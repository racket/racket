;;----------------------------------------------------------------------
;; with-syntax, generate-temporaries

(module with-stx '#%kernel
  (#%require "stx.rkt" "small-scheme.rkt" "stxcase.rkt"
             (for-syntax '#%kernel "stx.rkt" "stxcase.rkt" "stxloc.rkt" 
                         "sc.rkt" "qq-and-or.rkt" "cond.rkt"))

  (-define (with-syntax-fail stx)
    (raise-syntax-error
     'with-syntax
     "binding match failed"
     stx))

  (-define (with-datum-fail stx)
    (raise-syntax-error
     'with-datum
     "binding match failed"
     stx))

  ;; Partly from Dybvig
  (begin-for-syntax
   (define-values (gen-with-syntax)
     (let ([here-stx (quote-syntax here)])
       (lambda (x s-exp?)
         (syntax-case x ()
           ((_ () e1 e2 ...)
            (syntax/loc x (begin e1 e2 ...)))
           ((_ ((out in) ...) e1 e2 ...)
            (let ([ins (syntax->list (syntax (in ...)))])
              ;; Check for duplicates or other syntax errors:
              (get-match-vars (syntax _) x (syntax (out ...)) null)
              ;; Generate temps and contexts:
              (let ([tmps (map (lambda (x) (gen-temp-id 'ws)) ins)]
                    [heres (map (lambda (x)
                                  (datum->syntax
                                   x
                                   'here
                                   x))
                                ins)]
                    [outs (syntax->list (syntax (out ...)))])
                ;; Let-bind RHSs, then build up nested syntax-cases:
                (datum->syntax
                 here-stx
                 `(let ,(map (lambda (tmp here in)
                               `[,tmp ,(if s-exp?
                                           in
                                           `(datum->syntax 
                                             (quote-syntax ,here) 
                                             ,in))])
                             tmps heres ins)
                    ,(let loop ([tmps tmps][outs outs])
                       (cond
                        [(null? tmps)
                         (syntax (begin e1 e2 ...))]
                        [else `(syntax-case** #f #t ,(car tmps) () ,(if s-exp? 'eq? 'free-identifier=?) ,s-exp?
                                              [,(car outs) ,(loop (cdr tmps)
                                                                  (cdr outs))]
                                              [_ (,(if s-exp? 'with-datum-fail 'with-syntax-fail)
                                                  ;; Minimize the syntax structure we keep:
                                                  (quote-syntax ,(datum->syntax 
                                                                  #f 
                                                                  (syntax->datum (car outs))
                                                                  (car outs))))])])))
                 x)))))))))

  (-define-syntax with-syntax (lambda (stx) (gen-with-syntax stx #f)))
  (-define-syntax with-datum (lambda (stx) (gen-with-syntax stx #t)))

  (-define counter 0)
  (-define (append-number s)
    (set! counter (add1 counter))
    (string->symbol (format "~a~s" s counter)))

  (-define (generate-temporaries sl)
    (unless (stx-list? sl)
      (raise-argument-error 
       'generate-temporaries
       "(or/c list? syntax->list)"
       sl))
    (let ([l (stx->list sl)])
      (map (lambda (x) 
	     ((make-syntax-introducer)
	      (cond
	       [(symbol? x)
		(datum->syntax #f (append-number x))]
	       [(string? x)
		(datum->syntax #f (append-number x))]
	       [(keyword? x)
		(datum->syntax #f (append-number (keyword->string x)))]
	       [(identifier? x)
		(datum->syntax #f (append-number (syntax-e x)))]
	       [(and (syntax? x) (keyword? (syntax-e x)))
		(datum->syntax #f (append-number (keyword->string (syntax-e x))))]
	       [else 
		(datum->syntax #f (append-number 'temp))])))
	   l)))

  (#%provide with-syntax with-datum generate-temporaries))
