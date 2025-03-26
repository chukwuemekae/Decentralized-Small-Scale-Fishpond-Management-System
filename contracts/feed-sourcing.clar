;; Feed Sourcing Contract
;; Validates sustainable inputs for fish nutrition

;; Data Variables
(define-data-var feed-counter uint u0)
(define-data-var supplier-counter uint u0)
(define-data-var purchase-counter uint u0)

;; Data Maps
(define-map feed-types
{ feed-id: uint }
{
  name: (string-utf8 100),
  protein-content: uint,           ;; in percentage
  fat-content: uint,               ;; in percentage
  carbohydrate-content: uint,      ;; in percentage
  ingredients: (list 20 (string-utf8 50)),
  sustainability-score: uint,      ;; 1-10 scale
  certification: (string-utf8 100),
  shelf-life: uint,                ;; in days
  registration-date: uint
}
)

(define-map feed-suppliers
{ supplier-id: uint }
{
  name: (string-utf8 100),
  location: (string-utf8 100),
  contact-info: (string-utf8 200),
  sustainability-rating: uint,     ;; 1-10 scale
  verification-status: bool,
  registration-date: uint
}
)

(define-map feed-purchases
{ purchase-id: uint }
{
  buyer: principal,
  feed-id: uint,
  supplier-id: uint,
  pond-id: uint,
  quantity: uint,                  ;; in kilograms
  price-per-kg: uint,
  total-price: uint,
  purchase-date: uint,
  delivery-date: uint,
  notes: (string-utf8 500)
}
)

(define-map pond-feed-purchases
{ pond-id: uint }
{ purchase-ids: (list 100 uint) }
)

(define-map supplier-feed-types
{ supplier-id: uint }
{ feed-ids: (list 50 uint) }
)

;; Register a new feed type
(define-public (register-feed-type
  (name (string-utf8 100))
  (protein-content uint)
  (fat-content uint)
  (carbohydrate-content uint)
  (ingredients (list 20 (string-utf8 50)))
  (sustainability-score uint)
  (certification (string-utf8 100))
  (shelf-life uint))
(let ((feed-id (var-get feed-counter)))
  (begin
    ;; Store the feed type
    (map-set feed-types
      { feed-id: feed-id }
      {
        name: name,
        protein-content: protein-content,
        fat-content: fat-content,
        carbohydrate-content: carbohydrate-content,
        ingredients: ingredients,
        sustainability-score: sustainability-score,
        certification: certification,
        shelf-life: shelf-life,
        registration-date: block-height
      }
    )

    ;; Increment counter and return feed ID
    (var-set feed-counter (+ feed-id u1))
    (ok feed-id)
  )
)
)

;; Register a new feed supplier
(define-public (register-supplier
  (name (string-utf8 100))
  (location (string-utf8 100))
  (contact-info (string-utf8 200))
  (sustainability-rating uint))
(let ((supplier-id (var-get supplier-counter)))
  (begin
    ;; Store the supplier
    (map-set feed-suppliers
      { supplier-id: supplier-id }
      {
        name: name,
        location: location,
        contact-info: contact-info,
        sustainability-rating: sustainability-rating,
        verification-status: false,
        registration-date: block-height
      }
    )

    ;; Increment counter and return supplier ID
    (var-set supplier-counter (+ supplier-id u1))
    (ok supplier-id)
  )
)
)

;; Verify a supplier
(define-public (verify-supplier (supplier-id uint))
(let ((supplier-data (map-get? feed-suppliers { supplier-id: supplier-id })))
  (if (is-some supplier-data)
    (begin
      (map-set feed-suppliers
        { supplier-id: supplier-id }
        (merge (unwrap-panic supplier-data) {
          verification-status: true
        })
      )
      (ok true)
    )
    (err u1) ;; Supplier doesn't exist
  )
)
)

;; Add a feed type to a supplier's offerings
(define-public (add-supplier-feed
  (supplier-id uint)
  (feed-id uint))
(let ((supplier-data (map-get? feed-suppliers { supplier-id: supplier-id }))
      (feed-data (map-get? feed-types { feed-id: feed-id })))
  (if (and (is-some supplier-data) (is-some feed-data))
    (let ((supplier-feeds (default-to { feed-ids: (list) } (map-get? supplier-feed-types { supplier-id: supplier-id }))))
      (begin
        (map-set supplier-feed-types
          { supplier-id: supplier-id }
          { feed-ids: (unwrap-panic (as-max-len? (append (get feed-ids supplier-feeds) feed-id) u50)) }
        )
        (ok true)
      )
    )
    (err u1) ;; Supplier or feed doesn't exist
  )
)
)

;; Record a feed purchase
(define-public (record-purchase
  (feed-id uint)
  (supplier-id uint)
  (pond-id uint)
  (quantity uint)
  (price-per-kg uint)
  (delivery-date uint)
  (notes (string-utf8 500)))
(let ((purchase-id (var-get purchase-counter))
      (total-price (* quantity price-per-kg)))
  (begin
    ;; Store the purchase
    (map-set feed-purchases
      { purchase-id: purchase-id }
      {
        buyer: tx-sender,
        feed-id: feed-id,
        supplier-id: supplier-id,
        pond-id: pond-id,
        quantity: quantity,
        price-per-kg: price-per-kg,
        total-price: total-price,
        purchase-date: block-height,
        delivery-date: delivery-date,
        notes: notes
      }
    )

    ;; Update pond-to-purchase mapping
    (let ((pond-purchases (default-to { purchase-ids: (list) } (map-get? pond-feed-purchases { pond-id: pond-id }))))
      (map-set pond-feed-purchases
        { pond-id: pond-id }
        { purchase-ids: (unwrap-panic (as-max-len? (append (get purchase-ids pond-purchases) purchase-id) u100)) }
      )
    )

    ;; Increment counter and return purchase ID
    (var-set purchase-counter (+ purchase-id u1))
    (ok purchase-id)
  )
)
)

;; Get feed type details
(define-read-only (get-feed-type (feed-id uint))
(map-get? feed-types { feed-id: feed-id })
)

;; Get supplier details
(define-read-only (get-supplier (supplier-id uint))
(map-get? feed-suppliers { supplier-id: supplier-id })
)

;; Get purchase details
(define-read-only (get-purchase (purchase-id uint))
(map-get? feed-purchases { purchase-id: purchase-id })
)

;; Get all feed types offered by a supplier
(define-read-only (get-supplier-feeds (supplier-id uint))
(default-to { feed-ids: (list) } (map-get? supplier-feed-types { supplier-id: supplier-id }))
)

;; Get all feed purchases for a pond
(define-read-only (get-pond-purchases (pond-id uint))
(default-to { purchase-ids: (list) } (map-get? pond-feed-purchases { pond-id: pond-id }))
)

;; Get sustainable feed types (score >= threshold)
(define-read-only (get-sustainable-feeds (threshold uint))
(ok {
  threshold: threshold,
  message: "To find sustainable feeds, please query individual feed types and filter by sustainability score"
})
)

;; Get the total number of registered feed types
(define-read-only (get-feed-type-count)
(var-get feed-counter)
)

;; Get the total number of registered suppliers
(define-read-only (get-supplier-count)
(var-get supplier-counter)
)

;; Get the total number of recorded purchases
(define-read-only (get-purchase-count)
(var-get purchase-counter)
)
