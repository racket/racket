#lang racket/base
(require (for-syntax racket/base
                     racket/match/parse
                     racket/match/patterns)
         racket/match)

(provide 2dmatch)
(define-syntax (2dmatch stx)
  (syntax-case stx ()
    [(_ widths heights [(cell ...) rhs ...] ...)
     (let ()
       
       ;; coord-to-content : hash[(list num num) -o> (listof syntax)]
       (define coord-to-content (make-hash))
       
       ;; pattern-vars : hash[(list num num) -o> (listof identifier)]
       ;; for each cell on the boundary, tell us which vars are 
       ;; bound in the corresponding pattern
       (define pattern-vars (make-hash))
       
       (define let-bindings '()) 
       
       (define main-args #f)
       
       (define (on-boundary? cells)
         (ormap (λ (lst) (or (= 0 (list-ref lst 0))
                             (= 0 (list-ref lst 1))))
                cells))
       
       (define (cell-stx-object cell) 
         (datum->syntax #f " " cell))
       
       ;; build up the coord-to-content mapping for the 
       ;; boundary cells and build up the pattern-vars table
       (for ([cells-stx (in-list (syntax->list #'((cell ...) ...)))]
             [rhses (in-list (syntax->list #'((rhs ...) ...)))])
         (define cells (syntax->datum cells-stx))
         (define rhses-lst (syntax->list rhses))
         (cond
           [(member (list 0 0) cells) 
            (unless (and rhses-lst (= 2 (length rhses-lst)))
              (raise-syntax-error '2dmatch "cell at 0,0 must contain two expressions"
                                  (cell-stx-object (car cells))))
            (with-syntax ([(left-x right-x) (generate-temporaries rhses)]
                          [(left-arg right-arg) rhses])
              (set! let-bindings (list* #`[right-x right-arg]
                                        #`[left-x left-arg]
                                        let-bindings))
              (set! main-args #'(left-x right-x)))]
           [(on-boundary? cells)
            (unless (and rhses-lst (= 1 (length rhses-lst)))
              (raise-syntax-error '2dmatch 
                                  (format 
                                   "cell at ~a,~a must contain exactly one match pattern, found ~a"
                                   (list-ref (car cells) 0) (list-ref (car cells) 1)
                                   (length rhses-lst))
                                  stx
                                  (cell-stx-object (car (syntax-e cells-stx)))))
            (define pat (car (syntax->list rhses)))
            (hash-set! pattern-vars (car cells) (bound-vars (parse pat)))
            (hash-set! coord-to-content (car cells) pat)]))
       
       ;; build up the coord-to-content mapping for the non-boundary cells
       ;; use the pattern-vars table to build up the let-bindings that
       ;; bind identifiers to functions that end up getting called in the match clauses
       (for ([cells-stx (in-list (syntax->list #'((cell ...) ...)))]
             [rhses (in-list (syntax->list #'((rhs ...) ...)))])
         (define cells (syntax->datum cells-stx))
         (define rhses-lst (syntax->list rhses))
         (unless (on-boundary? cells)
           (when (null? (syntax-e rhses))
             (raise-syntax-error '2dmatch 
                                 (format "cell at ~a,~a should not be empty"
                                         (list-ref (car cells) 0)
                                         (list-ref (car cells) 1))
                                 stx
                                 (cell-stx-object (car cells))))
           (define horizontal-vars (hash-ref pattern-vars (list (list-ref (car cells) 0) 0)))
           (define vertical-vars (hash-ref pattern-vars (list 0 (list-ref (car cells) 1))))
           
           (define (intersect vs1 vs2)
             (for/list ([v1 (in-list vs1)]
                        #:when (is-in? v1 vs2))
               v1))
           
           (define (is-in? v1 v2s)
             (for/or ([v2 (in-list v2s)])
               (free-identifier=? v1 v2)))
           
           (for ([cell (in-list (cdr cells))])
             (set! horizontal-vars (intersect horizontal-vars 
                                              (hash-ref pattern-vars (list (list-ref cell 0) 0))))
             (set! vertical-vars (intersect vertical-vars 
                                            (hash-ref pattern-vars (list 0 (list-ref cell 1))))))
           
           (with-syntax ([(id) (generate-temporaries (list (format "2d-~a-~a" 
                                                                   (list-ref (car cells) 0)
                                                                   (list-ref (car cells) 1))))])
             (define app #`(id #,@horizontal-vars #,@vertical-vars))
             (for ([cell (in-list cells)])
               (hash-set! coord-to-content cell app))
             (set! let-bindings
                   (cons #`[id (λ (#,@horizontal-vars #,@vertical-vars) #,@rhses)]
                         let-bindings)))))
       
       (define num-of-cols (length (syntax->list #'widths)))
       (define num-of-rows (length (syntax->list #'heights)))
       #`(let #,(reverse let-bindings)
           (match* #,main-args
             #,@(for*/list ([x (in-range 1 num-of-cols)]
                            [y (in-range 1 num-of-rows)])
                  #`[(#,(hash-ref coord-to-content (list x 0))
                      #,(hash-ref coord-to-content (list 0 y)))
                     #,(hash-ref coord-to-content (list x y))])
             [(_ _)
              (2dmatch-error #,@main-args)])))]))

(define (2dmatch-error a b)
  (error '2dmatch "no matching clauses for ~e and ~e" a b))
