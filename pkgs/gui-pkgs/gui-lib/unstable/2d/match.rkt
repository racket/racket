#lang racket/base
(require (for-syntax racket/base)
         racket/match)

(provide 2dmatch)
(define-syntax (2dmatch stx)
  (syntax-case stx ()
    [(_ widths heights [(cell ...) rhs ...] ...)
     (let ()
       
       ;; coord-to-content : hash[(list num num) -o> (listof syntax)]
       (define coord-to-content (make-hash))
       (define let-bindings '()) 
       
       (define main-args #f)
       
       ;; build up the coord-to-content mapping
       ;; side-effect: record need for let bindings to
       ;; cover the the situation where multiple cells
       ;; are joined together
       ;; (this code is similar to that in cond.rkt, but
       ;; my attempt at abstracting between them was unsuccessful)
       (for ([cells-stx (in-list (syntax->list #'((cell ...) ...)))]
             [rhses (in-list (syntax->list #'((rhs ...) ...)))])
         (define cells (syntax->datum cells-stx))
         (define on-boundary? (ormap (λ (lst) (or (= 0 (list-ref lst 0))
                                                  (= 0 (list-ref lst 1))))
                                     cells))
         (define rhses-lst (syntax->list rhses))
         (cond
           [(member (list 0 0) cells) 
            (unless (and rhses-lst (= 2 (length rhses-lst)))
              (raise-syntax-error '2dmatch "cell at 0,0 must contain two expressions"))
            (set! main-args rhses)]
           [on-boundary?
            (unless (and rhses-lst (= 1 (length rhses-lst)))
              (raise-syntax-error '2dmatch 
                                  (format 
                                   "cell at ~a,~a must contain exactly one match pattern, found ~a"
                                   (list-ref (car cells) 0) (list-ref (car cells) 1)
                                   (length rhses-lst))
                                  stx))
            (hash-set! coord-to-content (car cells) (car (syntax->list rhses)))]
           [else
            (when (null? (syntax-e rhses))
              (raise-syntax-error '2dmatch 
                                  (format "cell at ~a,~a should not be empty"
                                          (list-ref (car cells) 0)
                                          (list-ref (car cells) 1))
                                  stx))
            ;; this code will duplicate the rhses expressions
            ;; in the case that there are multiple cells in `cells'
            ;; it would be better to analyze the corresponding
            ;; match patterns and then stick the code in a function
            ;; whose arguments are the intersection of the bound variables
            ;; (supplying the arguments in a call in the cell)
            (for ([cell (in-list cells)])
              (hash-set! coord-to-content 
                         cell
                         (syntax->list rhses)))]))
       
       (define num-of-cols (length (syntax->list #'widths)))
       (define num-of-rows (length (syntax->list #'heights)))
       #`(let #,let-bindings
           (match* #,main-args
             #,@(for*/list ([x (in-range 1 num-of-cols)]
                            [y (in-range 1 num-of-rows)])
                  #`[(#,(hash-ref coord-to-content (list x 0))
                      #,(hash-ref coord-to-content (list 0 y)))
                     (let () #,@(hash-ref coord-to-content (list x y)))])
             [(_ _)
              (2dmatch-error #,@main-args)])))]))

(define (2dmatch-error a b)
  (error '2dmatch "no matching clauses for ~e and ~e" a b))
