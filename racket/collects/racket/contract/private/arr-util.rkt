#lang racket/base

(require "application-arity-checking.rkt")
(provide split-doms
         sort-keywords
         valid-app-shapes-from-man/opts
         compute-quoted-src-expression)

(define (compute-quoted-src-expression stx)
  (define max-depth 4)
  (define max-width 5)
  (define max-str/kwd/sym-length 30)
  (define number-bound 1000000)
  
  (define (simple? ele)
    (or (and (symbol? ele)
             (< (string-length (symbol->string ele))
                max-str/kwd/sym-length))
        (and (keyword? ele)
             (< (string-length (keyword->string ele))
                max-str/kwd/sym-length))
        (boolean? ele)
        (char? ele)
        (null? ele)
        (and (string? ele)
             (< (string-length ele) max-str/kwd/sym-length))
        (and (number? ele)
             (simple-rational? (real-part ele))
             (simple-rational? (imag-part ele)))))
  
  (define (simple-rational? ele)
    (or (inexact? ele)
        (<= (- number-bound) (numerator ele) number-bound)
        (<= (denominator ele) number-bound)))
  
  (let loop ([stx stx]
             [depth max-depth])
    (cond
      [(zero? depth)
       (define ele (syntax-e stx))
       (if (simple? ele) ele '...)]
      [else
       (define lst (syntax->list stx))
       (cond
         [lst
          (if (<= (length lst) max-width)
              (for/list ([ele (in-list lst)])
                (loop ele (- depth 1)))
              (append (for/list ([ele (in-list lst)]
                                 [i (in-range (- max-width 1))])
                        (loop ele (+ depth 1)))
                      '(...)))]
         [else
          (define ele (syntax-e stx))
          (if (simple? ele) ele '...)])])))

;; split-doms : syntax identifier syntax -> syntax
;; given a sequence of keywords interpersed with other
;; stuff, splits out the keywords and sorts them,
;; and leaves the rest of the stuff in a row.
(define (split-doms stx name raw-doms)
  (let loop ([raw-doms raw-doms]
             [doms '()]
             [kwd-doms '()])
    (syntax-case raw-doms ()
      [() (list (reverse doms)
                (sort-keywords stx kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (not (keyword? (syntax-e #'arg))))
       (loop #'rest
             doms
             (cons #'(kwd arg) kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (keyword? (syntax-e #'arg)))
       (raise-syntax-error name
                           "cannot have two keywords in a row"
                           stx
                           #'kwd)]
      [(kwd)
       (keyword? (syntax-e #'kwd))
       (raise-syntax-error name
                           "cannot have a keyword at the end"
                           stx
                           #'kwd)]
      [(x . rest)
       (loop #'rest (cons #'x doms) kwd-doms)])))

;; sort-keywords : syntax (listof syntax[(kwd . whatever)] -> (listof syntax[(kwd . whatever)])
;; sorts a list of syntax according to the keywords in the list
(define (sort-keywords stx kwd/ctc-pairs)
  (define (insert x lst)
    (cond
      [(null? lst) (list x)]
      [else
       (let ([fst-kwd (syntax-e (car (syntax-e (car lst))))]
             [x-kwd (syntax-e (car (syntax-e x)))])
         (cond
           [(equal? x-kwd fst-kwd)
            (raise-syntax-error #f 
                                "duplicate keyword"
                                stx
                                (car x))]
           [(keyword<? x-kwd fst-kwd)
            (cons x lst)]
           [else (cons (car lst) (insert x (cdr lst)))]))]))
  
  (let loop ([pairs kwd/ctc-pairs])
    (cond
      [(null? pairs) null]
      [else (insert (car pairs) (loop (cdr pairs)))])))

(define (valid-app-shapes-from-man/opts min-arg-length num-of-opts rest? man-kwds opt-kwds)
  (define opt+man-dom-lengths
    (for/list ([i (in-range (+ num-of-opts 1))])
      (+ i min-arg-length)))
  (valid-app-shapes (if rest?
                        (append opt+man-dom-lengths 
                                (+ min-arg-length num-of-opts 1))
                        opt+man-dom-lengths)
                    man-kwds
                    opt-kwds))