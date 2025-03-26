;; Water Quality Monitoring Contract
;; Tracks critical parameters for fish health

;; Data Variables
(define-data-var measurement-counter uint u0)

;; Data Maps
(define-map water-quality-measurements
{ measurement-id: uint }
{
  pond-id: uint,
  recorder: principal,
  temperature: int,            ;; in tenths of degrees Celsius (e.g., 250 = 25.0 degrees C)
  ph: int,                     ;; in tenths (e.g., 72 = 7.2 pH)
  dissolved-oxygen: uint,      ;; in tenths of mg/L (e.g., 85 = 8.5 mg/L)
  ammonia: uint,               ;; in hundredths of mg/L (e.g., 25 = 0.25 mg/L)
  nitrite: uint,               ;; in hundredths of mg/L
  nitrate: uint,               ;; in tenths of mg/L
  turbidity: uint,             ;; in NTU (Nephelometric Turbidity Units)
  weather-conditions: (string-utf8 100),
  notes: (string-utf8 500),
  measurement-date: uint,
  critical-flag: bool
}
)

(define-map pond-measurements
{ pond-id: uint }
{ measurement-ids: (list 1000 uint) }
)

(define-map critical-measurements
{ pond-id: uint }
{ measurement-ids: (list 100 uint) }
)

;; Parameter thresholds for critical conditions
(define-data-var min-safe-temperature int 150)    ;; 15.0 degrees C
(define-data-var max-safe-temperature int 320)    ;; 32.0 degrees C
(define-data-var min-safe-ph int 65)              ;; 6.5 pH
(define-data-var max-safe-ph int 90)              ;; 9.0 pH
(define-data-var min-safe-oxygen uint u50)         ;; 5.0 mg/L
(define-data-var max-safe-ammonia uint u50)        ;; 0.50 mg/L
(define-data-var max-safe-nitrite uint u10)        ;; 0.10 mg/L
(define-data-var max-safe-nitrate uint u500)       ;; 50.0 mg/L

;; Record a water quality measurement
(define-public (record-measurement
  (pond-id uint)
  (temperature int)
  (ph int)
  (dissolved-oxygen uint)
  (ammonia uint)
  (nitrite uint)
  (nitrate uint)
  (turbidity uint)
  (weather-conditions (string-utf8 100))
  (notes (string-utf8 500)))
(let ((measurement-id (var-get measurement-counter))
      (is-critical (check-critical-conditions temperature ph dissolved-oxygen ammonia nitrite nitrate)))
  (begin
    ;; Store the measurement
    (map-set water-quality-measurements
      { measurement-id: measurement-id }
      {
        pond-id: pond-id,
        recorder: tx-sender,
        temperature: temperature,
        ph: ph,
        dissolved-oxygen: dissolved-oxygen,
        ammonia: ammonia,
        nitrite: nitrite,
        nitrate: nitrate,
        turbidity: turbidity,
        weather-conditions: weather-conditions,
        notes: notes,
        measurement-date: block-height,
        critical-flag: is-critical
      }
    )

    ;; Update pond-to-measurement mapping
    (let ((pond-list (default-to { measurement-ids: (list) } (map-get? pond-measurements { pond-id: pond-id }))))
      (map-set pond-measurements
        { pond-id: pond-id }
        { measurement-ids: (unwrap-panic (as-max-len? (append (get measurement-ids pond-list) measurement-id) u1000)) }
      )
    )

    ;; If critical, update critical measurements mapping
    (if is-critical
      (let ((critical-list (default-to { measurement-ids: (list) } (map-get? critical-measurements { pond-id: pond-id }))))
        (map-set critical-measurements
          { pond-id: pond-id }
          { measurement-ids: (unwrap-panic (as-max-len? (append (get measurement-ids critical-list) measurement-id) u100)) }
        )
      )
      true
    )

    ;; Increment counter and return measurement ID
    (var-set measurement-counter (+ measurement-id u1))
    (ok measurement-id)
  )
)
)

;; Helper function to check if conditions are critical
(define-private (check-critical-conditions
  (temperature int)
  (ph int)
  (dissolved-oxygen uint)
  (ammonia uint)
  (nitrite uint)
  (nitrate uint))
(or
  (< temperature (var-get min-safe-temperature))
  (> temperature (var-get max-safe-temperature))
  (< ph (var-get min-safe-ph))
  (> ph (var-get max-safe-ph))
  (< dissolved-oxygen (var-get min-safe-oxygen))
  (> ammonia (var-get max-safe-ammonia))
  (> nitrite (var-get max-safe-nitrite))
  (> nitrate (var-get max-safe-nitrate))
)
)

;; Update parameter thresholds
(define-public (update-thresholds
  (min-temperature int)
  (max-temperature int)
  (min-ph int)
  (max-ph int)
  (min-oxygen uint)
  (max-ammonia uint)
  (max-nitrite uint)
  (max-nitrate uint))
(begin
  (var-set min-safe-temperature min-temperature)
  (var-set max-safe-temperature max-temperature)
  (var-set min-safe-ph min-ph)
  (var-set max-safe-ph max-ph)
  (var-set min-safe-oxygen min-oxygen)
  (var-set max-safe-ammonia max-ammonia)
  (var-set max-safe-nitrite max-nitrite)
  (var-set max-safe-nitrate max-nitrate)
  (ok true)
)
)

;; Get measurement details by ID
(define-read-only (get-measurement (measurement-id uint))
(map-get? water-quality-measurements { measurement-id: measurement-id })
)

;; Get all measurements for a pond
(define-read-only (get-pond-measurements (pond-id uint))
(default-to { measurement-ids: (list) } (map-get? pond-measurements { pond-id: pond-id }))
)

;; Get critical measurements for a pond
(define-read-only (get-critical-measurements (pond-id uint))
(default-to { measurement-ids: (list) } (map-get? critical-measurements { pond-id: pond-id }))
)

;; Get the current parameter thresholds
(define-read-only (get-parameter-thresholds)
{
  min-temperature: (var-get min-safe-temperature),
  max-temperature: (var-get max-safe-temperature),
  min-ph: (var-get min-safe-ph),
  max-ph: (var-get max-safe-ph),
  min-oxygen: (var-get min-safe-oxygen),
  max-ammonia: (var-get max-safe-ammonia),
  max-nitrite: (var-get max-safe-nitrite),
  max-nitrate: (var-get max-safe-nitrate)
}
)

;; Get the total number of measurements
(define-read-only (get-measurement-count)
(var-get measurement-counter)
)

