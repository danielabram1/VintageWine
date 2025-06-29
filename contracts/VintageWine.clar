;; VintageWine: Premium Wine Collection and Authentication System
;; Version: 1.0.0

(define-constant ERR-ACCESS-DENIED (err u1))
(define-constant ERR-WINE-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-EXISTS (err u3))
(define-constant ERR-INVALID-STORAGE-STATE (err u4))
(define-constant ERR-INVALID-VINTAGE-YEAR (err u5))
(define-constant ERR-INVALID-WINE-TYPE (err u6))
(define-constant ERR-INVALID-RATING (err u7))
(define-constant ERR-INVALID-WINE-NAME (err u8))
(define-constant ERR-INVALID-VINEYARD (err u9))

(define-constant MIN-VINTAGE-YEAR u1850)

(define-data-var next-wine-id uint u1)

(define-map wine-collection
    uint
    {
        collector: principal,
        wine-name: (string-utf8 80),
        vineyard: (string-utf8 150),
        wine-type: (string-utf8 25),
        rating: (string-utf8 10),
        storage-state: (string-utf8 15),
        vintage-year: uint
    }
)

(define-private (validate-wine-type (wine-type (string-utf8 25)))
    (or 
        (is-eq wine-type u"Cabernet Sauvignon")
        (is-eq wine-type u"Chardonnay")
        (is-eq wine-type u"Pinot Noir")
        (is-eq wine-type u"Merlot")
        (is-eq wine-type u"Sauvignon Blanc")
        (is-eq wine-type u"Riesling")
    )
)

(define-private (validate-rating (rating (string-utf8 10)))
    (or 
        (is-eq rating u"Excellent")
        (is-eq rating u"Very Good")
        (is-eq rating u"Good")
        (is-eq rating u"Fair")
        (is-eq rating u"Poor")
    )
)

(define-private (validate-text-input (text (string-utf8 150)) (min-size uint) (max-size uint))
    (let 
        (
            (text-size (len text))
        )
        (and 
            (>= text-size min-size)
            (<= text-size max-size)
        )
    )
)

(define-public (add-wine-to-cellar 
    (wine-name (string-utf8 80))
    (vineyard (string-utf8 150))
    (wine-type (string-utf8 25))
    (rating (string-utf8 10))
    (vintage-year uint)
)
    (let
        (
            (wine-id (var-get next-wine-id))
        )
        (asserts! (validate-text-input wine-name u3 u80) ERR-INVALID-WINE-NAME)
        (asserts! (validate-text-input vineyard u5 u150) ERR-INVALID-VINEYARD)
        (asserts! (>= vintage-year MIN-VINTAGE-YEAR) ERR-INVALID-VINTAGE-YEAR)
        (asserts! (validate-wine-type wine-type) ERR-INVALID-WINE-TYPE)
        (asserts! (validate-rating rating) ERR-INVALID-RATING)
        
        (map-set wine-collection wine-id {
            collector: tx-sender,
            wine-name: wine-name,
            vineyard: vineyard,
            wine-type: wine-type,
            rating: rating,
            storage-state: u"cellared",
            vintage-year: vintage-year
        })
        (var-set next-wine-id (+ wine-id u1))
        (ok wine-id)
    )
)

(define-public (consume-wine (wine-id uint))
    (let
        (
            (wine (unwrap! (map-get? wine-collection wine-id) ERR-WINE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get collector wine)) ERR-ACCESS-DENIED)
        (asserts! (is-eq (get storage-state wine) u"cellared") ERR-INVALID-STORAGE-STATE)
        (ok (map-set wine-collection wine-id (merge wine { storage-state: u"consumed" })))
    )
)

(define-read-only (get-wine-info (wine-id uint))
    (ok (map-get? wine-collection wine-id))
)

(define-read-only (get-collector (wine-id uint))
    (ok (get collector (unwrap! (map-get? wine-collection wine-id) ERR-WINE-NOT-FOUND)))
)