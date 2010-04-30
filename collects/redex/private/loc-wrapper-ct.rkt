#lang scheme/base
(require (for-template scheme/base)
         (for-template "loc-wrapper-rt.ss")
         "term-fn.ss")
(provide to-lw/proc to-lw/uq/proc is-term-fn?)

;; this parameter allows define-metafunction to
;; communicate which name is the recursive calls
;; to the typesetting code, since the let-term-fn
;; won't have been expanded before to-lw/proc
;; is called.
(define is-term-fn? (make-parameter (λ (x) #f)))

(define (process-arg stx quote-depth)
  (define quoted? (quote-depth . > . 0))
  (define-values (op cl)
    (if (syntax? stx)
        (case (syntax-property stx 'paren-shape)
          [(#\{) (values "{" "}")]
          [(#\[) (values "[" "]")]
          [else (values "(" ")")])
        (values #f #f)))
  (define (reader-shorthand arg qd-delta mrk)
    #`(init-loc-wrapper 
       (list (init-loc-wrapper #,mrk
                               #,(syntax-line stx)
                               #,(syntax-column stx)
                               #,quoted?)
             'spring
             #,(process-arg arg (+ quote-depth qd-delta)))
       #,(syntax-line stx) 
       #,(syntax-column stx)
       #,quoted?))
  (define (handle-sequence qd-delta)
    #`(init-loc-wrapper 
       (list (init-loc-wrapper #,op #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
             #,@(map (λ (x) (process-arg x (+ qd-delta quote-depth))) (syntax->list stx))
             (init-loc-wrapper #,cl #f #f #,quoted?))
       #,(syntax-line stx) 
       #,(syntax-column stx)
       #,quoted?))
  (syntax-case* stx (name unquote quote unquote-splicing term) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
    ['a (reader-shorthand #'a +1 (if (= quote-depth 0) "" "'"))]
    [,a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ","))]
    [,@a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ",@"))]
    [(term a)
     (if (= quote-depth 0)
         #`(init-loc-wrapper 
            (list (init-loc-wrapper "" #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
                  'spring
                  #,(process-arg (cadr (syntax->list stx)) (+ quote-depth 1)))
            #,(syntax-line stx) 
            #,(syntax-column stx)
            #,quoted?)
         (handle-sequence +1))]
    [(a ...)
     (handle-sequence 0)]
    [(a b ... . c)
     #`(init-loc-wrapper 
        (list (init-loc-wrapper #,op #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
              #,@(map (λ (x) (process-arg x quote-depth)) (syntax->list (syntax (a b ...))))
              (init-loc-wrapper #," . " #f #f #,quoted?)
              #,(process-arg #'c quote-depth)
              (init-loc-wrapper #,cl #f #f #,quoted?))
        #,(syntax-line stx) 
        #,(syntax-column stx)
        #,quoted?)]
    [x 
     (and (identifier? #'x)
          (or (term-fn? (syntax-local-value #'x (λ () #f)))
              ((is-term-fn?) #'x)))
     #`(make-lw
        '#,(syntax-e #'x)
        #,(syntax-line stx) 
        #f
        #,(syntax-column stx)
        #f
        #f
        #t)]
    [x 
     (identifier? #'x)
     #`(init-loc-wrapper 
        '#,(syntax-e #'x)
        #,(syntax-line stx) 
        #,(syntax-column stx)
        #,quoted?)]
    [x 
     #`(init-loc-wrapper 
        #,(let ([base (syntax-e #'x)])
            (if (string? base)
                #`(rewrite-quotes #,(format "~s" base))
                (format "~s" (syntax-e #'x))))
        #,(syntax-line stx) 
        #,(syntax-column stx)
        #,quoted?)]))

(define (to-lw/proc stx) #`(add-spans #,(process-arg stx 1)))
(define (to-lw/uq/proc stx) #`(add-spans #,(process-arg stx 0)))
