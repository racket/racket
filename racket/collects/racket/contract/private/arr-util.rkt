#lang racket/base

(require "application-arity-checking.rkt")
(provide split-doms
         sort-keywords
         valid-app-shapes-from-man/opts)

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