#lang racket/base
(require (for-syntax racket/base))
(require "test-suite-utils.rkt")

(define-syntax (test-search stx)
  (syntax-case stx ()
    [(_ args ...)
     (with-syntax ([line (syntax-line stx)])
       #'(test-search/proc line args ...))]))

(define (test-search/proc line txt string cs? rs bubble-table)
  (test
   (string->symbol (format "search.rkt: line ~a" line))
   (lambda (x) (equal? bubble-table x))
   (lambda ()
     (queue-sexp-to-mred
      `(let ([t (new (text:searching-mixin (editor:keymap-mixin text:basic%)))]
             [normalize
              (λ (ht) (sort (hash-table-map ht list)
                            (λ (x y) (string<=? (format "~s" (car x))
                                                (format "~s" (car y))))))])
         (send t insert ,txt)
         (send t set-searching-state ,string ,cs? ,rs)
         (send t get-search-bubbles))))))

(define default-color "plum")
(define light-color '(243 223 243))
(define dark-color "mediumorchid")

(test-search "" "aba" #t #f '())
(test-search "aba" "aba" #t #f
             `(((0 . 3) ,default-color)))

(test-search "aba aba" "aba" #t #f
             `(((0 . 3) ,default-color)
               ((4 . 7) ,default-color)))

(test-search "abaaba" "aba" #t #f
             `(((0 . 6) ,default-color)))

(test-search "abababa" "aba" #t #f
             `(((0 . 7) ,default-color)))

(test-search "Aba" "aba" #t #f '())
(test-search "Aba" "aba" #f #f `(((0 . 3) ,default-color)))

(test-search "" "aba" #t 0 '())

(test-search "aba" "aba" #f 0 `(((0 . 3) ,dark-color)))

(test-search "abababa" "aba" #f 0
             `(((0 . 7) ,light-color)))

(test-search "aba aba aba" "aba" #f 2
             `(((0 . 3) ,light-color)
               ((4 . 7) ,dark-color)
               ((8 . 11) ,light-color)))

(test-search "abababa" "aba" #f 2
             `(((0 . 7) ,light-color)))
