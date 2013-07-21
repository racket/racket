
(require lang/private/rewrite-error-message)

(define (strip-context v)
  ;; Just to be sure, remove all top-level context from the syntax object
  (cond
   [(syntax? v)
    (datum->syntax
     #f
     (strip-context (syntax-e v))
     v
     v)]
   [(pair? v) (cons (strip-context (car v))
                    (strip-context (cdr v)))]
   [else v]))
      
(define body-accum null)
(define-syntax (htdp-top stx)
  (syntax-case stx (quote)
    [(_ expr)
     #'(set! body-accum (append body-accum (list (strip-context #'expr))))]))
(define (htdp-top-pop w)
  (set! body-accum (let loop ([body-accum body-accum])
		     (if (null? (cdr body-accum))
			 null
			 (cons (car body-accum) (loop (cdr body-accum)))))))

(define teachpack-accum null)
(define-syntax (htdp-teachpack stx)
  (syntax-case stx ()
    [(_ lib) 
     #'(set! teachpack-accum (cons (strip-context (quote-syntax lib)) teachpack-accum))]))
(define (htdp-teachpack-pop)
  (set!  teachpack-accum (cdr teachpack-accum)))

(define previous-tp-accum #f)
(define previous-tp-lang #f)
(define (add-teachpacks lang)
  (cond
   [(null? teachpack-accum) lang]
   [(equal? teachpack-accum previous-tp-accum)
    `',previous-tp-lang]
   [else
    (let ([name (string->symbol (format "~a+tp~a" lang (gensym)))])
      (eval #`(module #,name mzscheme
                (define-syntax (bounce stx)
                  #'(begin
                      (require #,lang #,@(map (lambda (t)
                                                #`(quote #,t))
                                              teachpack-accum))
                      (provide (all-from #,lang)
                               #,@(map (lambda (tp)
                                         #`(all-from (quote #,tp)))
                                       teachpack-accum))))
                (bounce)))
      (set! previous-tp-accum teachpack-accum)
      (set! previous-tp-lang name)
      `',name)]))

(define htdp-syntax-test
  (case-lambda
   [(stx) (htdp-syntax-test stx #rx".")]
   [(stx rx)
    (error-test #`(module m #,(add-teachpacks current-htdp-lang)
		    #,@body-accum
		    #,(strip-context stx))
		(lambda (x)
		  (and (exn:fail:syntax? x)
                       (regexp-match (if (string? rx) (regexp-quote rx) rx) 
                                     (get-rewriten-error-message x))
                       (let ([locs ((exn:srclocs-accessor x) x)])
                         (and (not (empty? locs))
                              (andmap (lambda (s) (and (srcloc-source s)
                                                       (regexp-match #rx"htdp-test[/\\]tests" (srcloc-source s))
                                                       (srcloc-position s) (srcloc-span s))) 

                                      locs))))))]))

(require (only-in mzscheme 
                  [let mz-let]
                  [require mz-require]
                  [quote mz-quote]))

(define-syntax (htdp-test stx)
  (syntax-case stx ()
    [(_ expect f . args)
     #'(begin
	 (do-htdp-test #'(test expect f . args) #f #f)
	 (htdp-try-direct-module f . args))]))

(define-syntax (htdp-try-direct-module stx)
  (syntax-case stx ()
    [(_ 'nm expr)
     ;; double-check that there's no error, at least,
     ;;  when using the real module-begin:
     #'(mz-let ([name (gensym)])
               (eval
                #`(module #,name #,(add-teachpacks current-htdp-lang)
                    #,@body-accum
                    #,(strip-context #'expr)))
               (dynamic-require `',name #f))]
    [_ 
     (printf "~s\n" (syntax->datum stx))
     #'(void)]))

(define (htdp-string-to-pred exn?/rx)
  (if (or (regexp? exn?/rx) (string? exn?/rx))
      (lambda (x)
	(if (regexp-match exn?/rx (get-rewriten-error-message x))
            #t
            (begin
              (printf "written: ~s\n" (get-rewriten-error-message x))
              #f)))
      exn?/rx))

(define-syntax (htdp-err/rt-test stx)
  (syntax-case stx ()
    [(_ expr)
     #'(do-htdp-test #'expr #f exn:application:type?)]
    [(_ expr exn?)
     #'(do-htdp-test #'expr #f (htdp-string-to-pred exn?))]))

(define (exn-type-and-msg type-pred msg)
  (lambda (exn)
    (and (type-pred exn)
         (regexp-match (if (string? msg) (regexp-quote msg) msg) 
                       (get-rewriten-error-message exn)))))


(define (htdp-error-test stx)
  (do-htdp-test stx #t #f))

(module helper scheme/base
  (require (for-syntax scheme/base))
  (define-syntax (module-begin stx)
    (syntax-case stx ()
     [(_ the-test lang mb to-export (tp ...) . rest)
      (with-syntax ([(tp ...)
                     (map (lambda (tp)
                            (datum->syntax
                             tp
                             (list #'quote tp)
                             tp))
                          (syntax->list #'(tp ...)))])
        #`(#%module-begin
           (require (only-in 'tester [test the-test]))
           (require (except-in lang mb) tp ...)
           #,@(if (syntax->datum (syntax to-export))
                  (list (syntax (provide to-export)))
                  '())
           . rest))]))
  (provide (rename-out [module-begin #%module-begin])))
       
(module tester mzscheme
  (define test (namespace-variable-value 'test))
  (provide test))

(define (print-eval stx)
  (printf "~s\n" (syntax->datum stx))
  (eval stx))

(define (do-htdp-test stx stx-err? exn?)
  (let ([name (gensym 'm)])
    ((if stx-err? syntax-test print-eval)
     #`(module #,name 'helper
	 test
	 #,current-htdp-lang #%module-begin
         #f
         #,teachpack-accum
	 #,@body-accum
	 #,(strip-context stx)))
    (unless stx-err?
      (if exn?
	  (err/rt-test (eval #`(mz-require '#,name)) exn?)
	  (eval #`(mz-require '#,name))))))

(define-syntax (htdp-eval stx)
  (syntax-case stx ()
    [(_ arg) (syntax (do-htdp-eval #'arg))]))

(define (do-htdp-eval stx)
  (let ([name (gensym 'm)])
    (eval
     #`(module #,name 'helper
	 test
	 #,current-htdp-lang #%module-begin
         the-answer
         #,teachpack-accum
	 #,@body-accum
	 (define the-answer #,(strip-context stx))))
    (dynamic-require `',name 'the-answer)))
