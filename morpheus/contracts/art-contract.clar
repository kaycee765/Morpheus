;; Dynamic NFT Marketplace Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-PRICE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; NFT definition
(define-non-fungible-token dynamic-nft uint)

;; Data variables
(define-data-var next-nft-id uint u1)
(define-data-var last-btc-price uint u50000) ;; Starting BTC price in USD
(define-data-var marketplace-fee uint u250) ;; 2.5% fee (250 basis points)

;; NFT metadata structure
(define-map nft-metadata uint {
    name: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    evolution-level: uint,
    creation-block: uint,
    last-interaction: uint,
    holder-score: uint
})

;; Marketplace listings
(define-map marketplace-listings uint {
    seller: principal,
    price: uint,
    listed-at: uint
})

;; Holder behavior tracking
(define-map holder-stats principal {
    total-owned: uint,
    total-transactions: uint,
    last-activity: uint,
    loyalty-score: uint
})

;; Evolution thresholds
(define-map evolution-thresholds uint {
    btc-price-threshold: uint,
    activity-threshold: uint,
    holder-threshold: uint
})

;; Initialize evolution thresholds
(map-set evolution-thresholds u1 {btc-price-threshold: u45000, activity-threshold: u100, holder-threshold: u50})
(map-set evolution-thresholds u2 {btc-price-threshold: u55000, activity-threshold: u200, holder-threshold: u100})
(map-set evolution-thresholds u3 {btc-price-threshold: u65000, activity-threshold: u300, holder-threshold: u200})

;; Read-only functions

;; Get NFT metadata
(define-read-only (get-nft-metadata (nft-id uint))
    (map-get? nft-metadata nft-id)
)

;; Get marketplace listing
(define-read-only (get-listing (nft-id uint))
    (map-get? marketplace-listings nft-id)
)

;; Get holder stats
(define-read-only (get-holder-stats (holder principal))
    (map-get? holder-stats holder)
)

;; Get NFT owner
(define-read-only (get-owner (nft-id uint))
    (nft-get-owner? dynamic-nft nft-id)
)

;; Calculate evolution level based on various factors
(define-read-only (calculate-evolution-level (nft-id uint))
    (let ((metadata (unwrap! (get-nft-metadata nft-id) u0))
          (current-btc-price (var-get last-btc-price))
          (current-block block-height)
          (creation-block (get creation-block metadata))
          (holder-score (get holder-score metadata)))
        (let ((block-age (- current-block creation-block))
              (price-factor (if (> current-btc-price u60000) u2 u1))
              (age-factor (if (> block-age u1000) u2 u1))
              (holder-factor (if (> holder-score u100) u2 u1)))
            (+ price-factor age-factor holder-factor)
        )
    )
)

;; Get current marketplace fee
(define-read-only (get-marketplace-fee)
    (var-get marketplace-fee)
)

;; Public functions

;; Mint new dynamic NFT
(define-public (mint-nft (name (string-ascii 64)) (description (string-ascii 256)) (image-uri (string-ascii 256)))
    (let ((nft-id (var-get next-nft-id)))
        (try! (nft-mint? dynamic-nft nft-id tx-sender))
        (map-set nft-metadata nft-id {
            name: name,
            description: description,
            image-uri: image-uri,
            evolution-level: u1,
            creation-block: block-height,
            last-interaction: block-height,
            holder-score: u0
        })
        (update-holder-stats tx-sender u1 u1)
        (var-set next-nft-id (+ nft-id u1))
        (ok nft-id)
    )
)

;; List NFT for sale
(define-public (list-nft (nft-id uint) (price uint))
    (let ((owner (unwrap! (nft-get-owner? dynamic-nft nft-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        (map-set marketplace-listings nft-id {
            seller: tx-sender,
            price: price,
            listed-at: block-height
        })
        (ok true)
    )
)

;; Remove NFT from marketplace
(define-public (unlist-nft (nft-id uint))
    (let ((listing (unwrap! (get-listing nft-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
        (map-delete marketplace-listings nft-id)
        (ok true)
    )
)

;; Buy NFT from marketplace
(define-public (buy-nft (nft-id uint))
    (let ((listing (unwrap! (get-listing nft-id) ERR-NOT-FOUND))
          (price (get price listing))
          (seller (get seller listing))
          (fee (/ (* price (var-get marketplace-fee)) u10000)))
        (try! (stx-transfer? (- price fee) tx-sender seller))
        (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
        (try! (nft-transfer? dynamic-nft nft-id seller tx-sender))
        (map-delete marketplace-listings nft-id)
        (update-holder-stats tx-sender u1 u1)
        (update-holder-stats seller u0 u1)
        (update-nft-interaction nft-id)
        (ok true)
    )
)

;; Transfer NFT (updates holder behavior)
(define-public (transfer-nft (nft-id uint) (recipient principal))
    (let ((owner (unwrap! (nft-get-owner? dynamic-nft nft-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? dynamic-nft nft-id tx-sender recipient))
        (update-holder-stats tx-sender u0 u1)
        (update-holder-stats recipient u1 u1)
        (update-nft-interaction nft-id)
        (ok true)
    )
)

;; Update BTC price (can be called by oracle or admin)
(define-public (update-btc-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set last-btc-price new-price)
        (ok true)
    )
)

;; Evolve NFT based on current conditions
(define-public (evolve-nft (nft-id uint))
    (let ((metadata (unwrap! (get-nft-metadata nft-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? dynamic-nft nft-id) ERR-NOT-FOUND))
          (new-evolution-level (calculate-evolution-level nft-id)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set nft-metadata nft-id (merge metadata {
            evolution-level: new-evolution-level,
            last-interaction: block-height
        }))
        (update-holder-stats tx-sender u0 u1)
        (ok new-evolution-level)
    )
)

;; Interact with NFT (increases holder score)
(define-public (interact-with-nft (nft-id uint))
    (let ((metadata (unwrap! (get-nft-metadata nft-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? dynamic-nft nft-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set nft-metadata nft-id (merge metadata {
            last-interaction: block-height,
            holder-score: (+ (get holder-score metadata) u10)
        }))
        (update-holder-stats tx-sender u0 u1)
        (ok true)
    )
)

;; Private functions

;; Update holder statistics
(define-private (update-holder-stats (holder principal) (owned-change uint) (tx-increment uint))
    (let ((current-stats (default-to {total-owned: u0, total-transactions: u0, last-activity: u0, loyalty-score: u0} 
                                   (get-holder-stats holder))))
        (map-set holder-stats holder {
            total-owned: (+ (get total-owned current-stats) owned-change),
            total-transactions: (+ (get total-transactions current-stats) tx-increment),
            last-activity: block-height,
            loyalty-score: (+ (get loyalty-score current-stats) (* tx-increment u5))
        })
    )
)

;; Update NFT interaction timestamp
(define-private (update-nft-interaction (nft-id uint))
    (let ((metadata (unwrap! (get-nft-metadata nft-id) false)))
        (map-set nft-metadata nft-id (merge metadata {
            last-interaction: block-height
        }))
        true
    )
)

;; Admin functions

;; Update marketplace fee (only contract owner)
(define-public (set-marketplace-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-PRICE) ;; Max 10% fee
        (var-set marketplace-fee new-fee)
        (ok true)
    )
)