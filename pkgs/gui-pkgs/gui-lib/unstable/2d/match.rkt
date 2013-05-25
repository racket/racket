#lang racket/base
(require (for-syntax racket/base)
         racket/match)

(provide 2dmatch)
(define-syntax (2dmatch stx)
  (syntax-case stx ()
    [(_ widths heights 
        [(cell ...) rhs ...] ...)
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
                                  (format "cell at ~a,~a must contain exactly one match pattern")
                                  stx))
            (hash-set! coord-to-content (car cells) (car (syntax->list rhses)))]
           [else
            (when (null? (syntax-e rhses))
              (raise-syntax-error '2dmatch 
                                  (format "cell at ~a,~a should not be empty"
                                          (list-ref (car cells) 0)
                                          (list-ref (car cells) 1))
                                  stx))
            (cond
              [(null? (cdr cells)) ;; only one cell:
               ;; => we don't need a let binding
               (hash-set! coord-to-content 
                          (car cells)
                          (syntax->list rhses))]
              [else
               (for ([cell (in-list cells)])
                 (define x (list-ref cell 0))
                 (define y (list-ref cell 1))
                 (with-syntax ([(id) (generate-temporaries (list (format "2dmatch~a-~a" x y)))]
                               [(rhs ...) rhses])
                   (set! let-bindings (cons #`[id (λ () rhs ...)]
                                            let-bindings))
                   (hash-set! coord-to-content cell (list #'(id)))))])]))
       
       (define num-of-cols (length (syntax->list #'widths)))
       (define num-of-rows (length (syntax->list #'heights)))
       #`(let #,let-bindings
           (match* #,main-args
             #,@(for*/list ([x (in-range 1 num-of-cols)]
                            [y (in-range 1 num-of-rows)])
                  #`[(#,(hash-ref coord-to-content (list x 0))
                      #,(hash-ref coord-to-content (list 0 y)))
                     (let () #,@(hash-ref coord-to-content (list x y)))]))))]))
