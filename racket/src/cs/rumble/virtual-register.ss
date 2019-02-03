;; We get a small number of virtual registers for fast,
;; pthread-specific bindings.

;; The last few virtual registers are reserved for use by the thread system
(meta define num-reserved-virtual-registers 2)

(meta define virtual-register-initial-values '())

(define-syntax (define-virtual-register stx)
  (syntax-case stx ()
    [(_ id init-val)
     (with-syntax ([pos (datum->syntax #'here (length virtual-register-initial-values))])
       (set! virtual-register-initial-values (cons #'init-val virtual-register-initial-values))
       (when (> (length virtual-register-initial-values) (- (virtual-register-count)
                                                            num-reserved-virtual-registers))
         (syntax-error stx "too many virtual-register definitions:"))
       #`(define-syntax id
           (syntax-rules ()
             [(_) (virtual-register pos)]
             [(_ v) (set-virtual-register! pos v)])))]))

(define-syntax (define-virtual-registers-init stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([(init ...)
                    (let loop ([l (reverse virtual-register-initial-values)]
                               [pos 0])
                      (cond
                       [(null? l) '()]
                       [else (cons (with-syntax ([pos (datum->syntax #'here pos)]
                                                 [init (car l)])
                                     #'(set-virtual-register! pos init))
                                   (loop (cdr l) (add1 pos)))]))])
       #'(define (id) init ...))]))
