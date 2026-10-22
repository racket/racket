#lang racket/base
(require racket/string
         "catalog.rkt"
         "print.rkt")

(provide pkg-search)

;; Scores `details` (as reported by a catalog) against one search term,
;; using the same weights as the package catalog website: a match as a
;; name prefix counts most, a match anywhere in the name next, and a
;; match in the description least. Matching is case-insensitive.
(define (term-score term name desc)
  (define t (string-downcase term))
  (cond
    [(string-prefix? (string-downcase name) t) 100]
    [(string-contains? (string-downcase name) t) 10]
    [(and desc (string-contains? (string-downcase desc) t)) 1]
    [else 0]))

(define (pkg-description details)
  (define d (hash-ref details 'description #f))
  (and (string? d) d))

(define (search-score name details terms)
  (for/sum ([term (in-list terms)])
    (term-score term name (pkg-description details))))

;; Returns the names in `details` that match any of `terms`, best
;; matches first; equally ranked names are sorted alphabetically.
(define (rank-search-results details terms)
  (define scored
    (for/list ([(name pkg-details) (in-hash details)])
      (define score (search-score name pkg-details terms))
      (and (positive? score) (vector score name))))
  (sort (filter values scored)
        (lambda (a b)
          (cond
            [(= (vector-ref a 0) (vector-ref b 0))
             (string-ci<? (vector-ref a 1) (vector-ref b 1))]
            [else (> (vector-ref a 0) (vector-ref b 0))]))))

(define (pkg-search terms)
  (cond
    [(null? terms)
     (pkg-error "no search terms provided")]
    [else
     (define details (get-all-pkg-details-from-catalogs))
     (define results (rank-search-results details terms))
     (cond
       [(null? results)
        (printf "No packages found\n")]
       [else
        (for ([result (in-list results)])
          (define name (vector-ref result 1))
          (define desc (pkg-description (hash-ref details name)))
          (printf "~a~a\n"
                  name
                  (if desc
                      (format " ~a" (regexp-replace* #rx"[\n\r]+" desc " "))
                      "")))
        (printf "~a packages found\n" (length results))])]))
