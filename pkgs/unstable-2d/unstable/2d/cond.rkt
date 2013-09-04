#lang racket/base
(require (for-syntax racket/base))

(provide 2dcond)
(define-syntax (2dcond stx)
  (syntax-case stx ()
    [(_ widths heights 
        [(cell ...) rhs ...] ...)
     (let ()
       
       (define last-col (- (length (syntax->list #'widths)) 1))
       (define last-row (- (length (syntax->list #'heights)) 1))
       
       ;; coord-to-content : hash[(list num num) -o> (listof syntax)]
       (define coord-to-content (make-hash))
       (define let-bindings '()) 
       
       ;; build up the coord-to-content mapping
       ;; side-effect: record need for let bindings to
       ;; cover the the situation where multiple cells
       ;; are joined together
       (for ([cells-stx (in-list (syntax->list #'((cell ...) ...)))]
             [rhses (in-list (syntax->list #'((rhs ...) ...)))])
         (define cells (syntax->datum cells-stx))
         
         (cond
           [(member (list 0 0) cells)
            (unless (null? (syntax-e rhses))
              (raise-syntax-error '2dcond 
                                  "cell at 0,0 must be empty"
                                  stx))]
           [else
            (when (null? (syntax-e rhses))
              (raise-syntax-error '2dcond 
                                  (format "cell at ~a,~a must not be empty"
                                          (list-ref (car cells) 0)
                                          (list-ref (car cells) 1))
                                  stx))])
         
         (cond
           [(member (list 0 0) cells) (void)]
           [(and (or (member (list 0 last-row) cells)
                     (member (list last-col 0) cells))
                 (syntax-case rhses (else)
                   [(else) #t]
                   [_ #f]))
            ;; found an 'else' (in a reasonable place)
            ;; => treat it like a #t in that cell
            (hash-set! coord-to-content 
                       (car cells)
                       (list #'#t))]
           [(and 
             ;; only one cell:
             (null? (cdr cells))
             ;; not in the left-edge (questions)
             (not (= 0 (car (car cells)))))
            ;; then we don't need a let binding
            (hash-set! coord-to-content 
                       (car cells)
                       (syntax->list rhses))]
           [else
            (for ([cell (in-list cells)])
              (define x (list-ref cell 0))
              (define y (list-ref cell 1))
              (with-syntax ([(id) (generate-temporaries (list (format "2dcond~a-~a" x y)))]
                            [(rhs ...) rhses])
                (set! let-bindings (cons #`[id (λ () rhs ...)]
                                         let-bindings))
                (hash-set! coord-to-content cell (list #'(id)))))]))
       
       (define num-of-cols (length (syntax->list #'widths)))
       (define num-of-rows (length (syntax->list #'heights)))
       #`(let #,let-bindings
           #,(for/fold ([else-branch #'(2dcond-runtime-error #f)])
               ([x-flip (in-range 1 num-of-cols)])
               (define x (- num-of-cols x-flip))
               #`(if (let () #,@(hash-ref coord-to-content (list x 0)))
                     (cond
                       #,@(for/list ([y (in-range 1 num-of-rows)])
                            #`[(let () #,@(hash-ref coord-to-content (list 0 y)))
                               #,@(hash-ref coord-to-content (list x y))])
                       [else (2dcond-runtime-error #,x)])
                     #,else-branch))))]))

(define (2dcond-runtime-error dir)
  (define str
    (if dir
        (format "all of the y-direction questions were false (x coordinate ~a was true)"
                dir)
        "all of the x-direction questions were false"))
  (error '2dcond str))
