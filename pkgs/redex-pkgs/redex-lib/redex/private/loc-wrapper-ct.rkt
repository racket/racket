#lang racket/base
(require (for-template racket/base)
         (for-template "loc-wrapper-rt.rkt")
         "term-fn.rkt")
(provide to-lw/proc to-lw/uq/proc)

(define (process-arg stx quote-depth)
  (define quoted? (quote-depth . > . 0))
  (define init-loc-wrapper/q? (if quoted? 'init-loc-wrapper/quoted 'init-loc-wrapper/unquoted)) 
  (define-values (op cl)
    (if (syntax? stx)
        (case (syntax-property stx 'paren-shape)
          [(#\{) (values "{" "}")]
          [(#\[) (values "[" "]")]
          [else (values "(" ")")])
        (values #f #f)))
  (define (reader-shorthand arg qd-delta mrk)
    #`#(#,init-loc-wrapper/q? 
        #(list #(#,init-loc-wrapper/q? #,mrk
                                      #,(syntax-line stx)
                                      #,(syntax-column stx))
               'spring
               #,(process-arg arg (+ quote-depth qd-delta)))
        #,(syntax-line stx) 
        #,(syntax-column stx)))
  (define (handle-sequence qd-delta)
    (with-syntax ([(others ...) (map (λ (x) (process-arg x (+ qd-delta quote-depth))) (syntax->list stx))]) 
      #`#(#,(if quoted?
                'init-loc-wrapper-sequence/quoted
                'init-loc-wrapper-sequence/unquoted)
          #,op #,(syntax-line stx) #,(syntax-column stx)
          others ...)))
  (syntax-case* stx (name unquote quote unquote-splicing term) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
    ['a (reader-shorthand #'a +1 (if (= quote-depth 0) "" "'"))]
    [,a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ","))]
    [,@a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ",@"))]
    [(term a)
     (if (= quote-depth 0)
         #`#(#,init-loc-wrapper/q?
             #(list #(#,init-loc-wrapper/q? "" #,(syntax-line stx) #,(syntax-column stx))
                    'spring
                    #,(process-arg (cadr (syntax->list stx)) (+ quote-depth 1)))
             #,(syntax-line stx) 
             #,(syntax-column stx))
         (handle-sequence +1))]
    [(a ...)
     (handle-sequence 0)]
    [(a b ... . c)
     #`#(#,init-loc-wrapper/q? 
         #(list #(#,init-loc-wrapper/q? #,op #,(syntax-line stx) #,(syntax-column stx))
                #,@(map (λ (x) (process-arg x quote-depth)) (syntax->list (syntax (a b ...))))
                #(#,init-loc-wrapper/q? #," . " #f #f)
                #,(process-arg #'c quote-depth)
                #(#,init-loc-wrapper/q? #,cl #f #f))
         #,(syntax-line stx) 
         #,(syntax-column stx))]
    [x 
     (and (identifier? #'x)
          (and (syntax-transforming?)
               (or (term-fn? (syntax-local-value #'x (λ () #f)))
                   (judgment-form? (syntax-local-value #'x (λ () #f))))))
     #`#(make-lw
         '#,(syntax-e #'x)
         #,(syntax-line stx) 
         #f
         #,(syntax-column stx)
         #f
         #f
         #t)]
    [x 
     (identifier? #'x)
     #`#(#,init-loc-wrapper/q? 
         '#,(syntax-e #'x)
         #,(syntax-line stx) 
         #,(syntax-column stx))]
    [x 
     #`#(#,init-loc-wrapper/q?
         #,(let ([base (syntax-e #'x)])
             (if (string? base)
                 #`#(rewrite-quotes #,(format "~s" base))
                 (format "~s" (syntax-e #'x))))
         #,(syntax-line stx) 
         #,(syntax-column stx))]))

(define (to-lw/proc stx [add-add-spans? #t]) 
  (if add-add-spans?
      #`(add-spans/interp-lws #,(process-arg stx 1))
      (process-arg stx 1)))
(define (to-lw/uq/proc stx [add-add-spans? #t])
  (if add-add-spans?
      #`(add-spans/interp-lws #,(process-arg stx 0))
      (process-arg stx 0)))
