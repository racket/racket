#lang racket/base
(require racket/list
         racket/string
         setup/getinfo
         racket/contract/base)

(provide
 (contract-out
  [get-module-suffixes (()
                        (#:mode (or/c 'preferred 'all-available 'no-planet 'no-user)
                         #:group (or/c 'all 'libs 'docs)
                         #:namespace (or/c #f namespace?))
                        . ->* .
                        (listof bytes?))]
  [get-module-suffix-regexp (()
                             (#:mode (or/c 'preferred 'all-available 'no-planet 'no-user)
                              #:group (or/c 'all 'libs 'docs)
                              #:namespace (or/c #f namespace?))
                             . ->* .
                             byte-regexp?)]))

(define (get-module-suffixes #:mode [key 'preferred]
                             #:group [group 'all]
                             #:namespace [namespace #f])
  (define fields (case group
                   [(all) '(module-suffixes doc-module-suffixes)]
                   [(libs) '(module-suffixes)]
                   [(docs) '(doc-module-suffixes)]))
  (define dirs (find-relevant-directories fields key))
  (define rkt-ht (if (memq 'module-suffixes fields)
                     (hash #"rkt" #t #"ss" #t #"scm" #t)
                     (hash)))
  (define init-ht (if (memq 'doc-module-suffixes fields)
                      (hash-set rkt-ht #"scrbl" #t)
                      rkt-ht))
  (define ht
    (for/fold ([ht init-ht]) ([dir (in-list dirs)])
      (define info (get-info/full dir #:namespace namespace))
      (for/fold ([ht init-ht]) ([field (in-list fields)])
        (define suffixes (info field (lambda () null)))
        (cond
         [(list? suffixes)
          (for/fold ([ht ht]) ([suffix (in-list suffixes)])
            (cond
             [(bytes? suffix) (hash-set ht suffix #t)]
             [else ht]))]))))
  (sort (hash-keys ht) bytes<?))

(define (get-module-suffix-regexp #:mode [key 'preferred]
                                  #:group [group 'all]
                                  #:namespace [namespace #f])
  (define suffixes
    (get-module-suffixes #:mode key #:group group #:namespace namespace))
  (byte-pregexp
   (bytes-append #"^(.*)\\.(?i:"
                 (apply bytes-append
                        (add-between suffixes #"|"))
                 #")$")))
