#lang racket/base
(require pkg/lib
         rackunit)

;; The `test-api' function is meant to be called via "tests-catalogs.rkt"
(provide test-api)

(define (test-api)
  (check-true (andmap string? (pkg-config-catalogs)))

  (define pkg-names (get-all-pkg-names-from-catalogs))
  (check-not-false (member "pkg-test1" pkg-names))
  (check-not-false (member "pkg-test2" pkg-names))
  
  (define details (get-all-pkg-details-from-catalogs))
  (check-not-false (hash-ref details "pkg-test1" #f))
  (check-not-false (hash-ref details "pkg-test2" #f))

  (check-equal? (hash-ref (hash-ref details "pkg-test1")
                          'source)
                "http://localhost:9997/pkg-test1.zip")
  (check-equal? (hash-ref (hash-ref details "pkg-test2")
                          'source)
                "http://localhost:9997/pkg-test2.zip")
  
  (define test1-details (get-pkg-details-from-catalogs "pkg-test1"))
  (check-equal? test1-details
                (hash-ref details "pkg-test1"))

  (define-values (cksum mods deps) 
    (get-pkg-content (pkg-desc "pkg-test1" #f #f #f #f)))
  (define-values (cksum1 mods1 deps1)
    (get-pkg-content (pkg-desc "http://localhost:9997/pkg-test1.zip" #f #f #f #f)))

  (check-equal? cksum cksum1)
  (check-equal? (sort mods string<? #:key cadr)
                '((lib "data/empty-set.rkt")
                  (lib "pkg-test1/conflict.rkt")
                  (lib "pkg-test1/main.rkt")
                  (lib "pkg-test1/number.rkt")
                  (lib "pkg-test1/update.rkt")))
  (check-equal? deps '())

  (define-values (cksum2 mods2 deps2) 
    (get-pkg-content (pkg-desc "pkg-test2" 'name #f #f #f)))
  (check-equal? (sort deps2 string<?) '("base" "pkg-test1")))
