
;;----------------------------------------------------------------------
;; more-scheme : case, do, etc. - remaining syntax

(module more-scheme '#%kernel
  (#%require "small-scheme.ss" "define.ss" '#%paramz
             (for-syntax '#%kernel "stx.ss" "small-scheme.ss" "stxcase-scheme.ss" "qqstx.ss"))

  (define-syntax case-test
    (lambda (x)
      (syntax-case x ()
	[(_ x (k))
	 (if (symbol? (syntax-e #'k))
	     (syntax (eq? x 'k))
	     (syntax (eqv? x 'k)))]
	[(_ x (k ...))
	 (syntax (memv x '(k ...)))])))

  ;; Mostly from Dybvig:
  (define-syntaxes (case old-case)
    (let ([go
           (lambda (x id=?)
             (syntax-case* x (else) id=?
               ((_ v)
                (syntax (#%expression (begin v (void)))))
               ((_ v (else e1 e2 ...))
                (syntax/loc x (#%expression (begin v e1 e2 ...))))
               ((_ v ((k ...) e1 e2 ...))
                (syntax/loc x (if (case-test v (k ...)) (begin e1 e2 ...) (void))))
               ((self v ((k ...) e1 e2 ...) c1 c2 ...)
                (syntax/loc x (let ((x v))
                                (if (case-test x (k ...))
                                    (begin e1 e2 ...)
                                    (self x c1 c2 ...)))))
               ((_ v (bad e1 e2 ...) . rest)
                (raise-syntax-error 
                 #f
                 "bad syntax (not a datum sequence)"
                 x
                 (syntax bad)))
               ((_ v clause . rest)
                (raise-syntax-error 
                 #f
                 "bad syntax (missing expression after datum sequence)"
                 x
                 (syntax clause)))
               ((_ . v)
                (not (null? (syntax-e (syntax v))))
                (raise-syntax-error 
                 #f
                 "bad syntax (illegal use of `.')"
                 x))))])
      (values
       (lambda (stx) (go stx free-identifier=?))
       (let ([else-stx (datum->syntax #f 'else)])
         (lambda (stx) (go stx (lambda (a b) (free-identifier=? a else-stx))))))))

  ;; From Dybvig:
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
	((_ ((var init . step) ...) (e0 e1 ...) c ...)
	 (with-syntax (((step ...)
			(map (lambda (v s)
			       (syntax-case s ()
				 (() v)
				 ((e) (syntax e))
				 (_ (raise-syntax-error 
				     #f
				     "bad variable syntax"
				     orig-x))))
			     (syntax->list (syntax (var ...)))
			     (syntax->list (syntax (step ...))))))
	   (syntax-case (syntax (e1 ...)) ()
	     (() (syntax/loc
		  orig-x
		  (let doloop ((var init) ...)
		    (if e0
                        (void)
			(begin c ... (doloop step ...))))))
	     ((e1 e2 ...)
	      (syntax/loc
	       orig-x
	       (let doloop ((var init) ...)
		 (if e0
		     (begin e1 e2 ...)
		     (begin c ... (doloop step ...))))))))))))
  
  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr1 expr ...)
	 (syntax (let () expr1 expr ...))]
	[(_ ([param val] ...) expr1 expr ...)
	 (with-syntax ([(p/v ...)
			(apply append
			       (map list
				    (syntax->list #'(param ...))
				    (syntax->list #'(val ...))))])
	   (syntax/loc stx
	     (with-continuation-mark
		 parameterization-key
		 (extend-parameterization
		  (continuation-mark-set-first #f parameterization-key)
		  p/v ...)
	       (let ()
		 expr1
		 expr ...))))])))

  (define-syntax parameterize*
    (syntax-rules ()
      [(_ () body1 body ...)
       (let () body1 body ...)]
      [(_ ([lhs1 rhs1] [lhs rhs] ...) body1 body ...)
       (parameterize ([lhs1 rhs1])
         (parameterize* ([lhs rhs] ...)
                        body1 body ...))]))

  (define (current-parameterization)
    (extend-parameterization (continuation-mark-set-first #f parameterization-key)))
  
  (define (call-with-parameterization paramz thunk)
    (unless (parameterization? paramz)
      (raise-type-error 'call-with-parameterization "parameterization" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-type-error 'call-with-parameterization "procedure (arity 0)" 1 paramz thunk))
    (with-continuation-mark
	parameterization-key
	paramz
      (thunk)))

  (define-syntax parameterize-break
    (lambda (stx)
      (syntax-case stx ()
	[(_ bool-expr expr1 expr ...)
	 (syntax/loc stx
	   (with-continuation-mark
	       break-enabled-key
	       (make-thread-cell (and bool-expr #t))
	     (begin
	       (check-for-break)
	       (let ()
		 expr1
		 expr ...))))])))
  
  (define-values (struct:break-paramz make-break-paramz break-paramz? break-paramz-ref break-paramz-set!)
    (make-struct-type 'break-parameterization #f 1 0 #f))

  (-define-struct break-parameterization (cell))
  
  (define (current-break-parameterization)
    (make-break-paramz (continuation-mark-set-first #f break-enabled-key)))
  
  (define (call-with-break-parameterization paramz thunk)
    (unless (break-paramz? paramz)
      (raise-type-error 'call-with-break-parameterization "break parameterization" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-type-error 'call-with-parameterization "procedure (arity 0)" 1 paramz thunk))
    (begin0
     (with-continuation-mark
	 break-enabled-key
	 (break-paramz-ref paramz 0)
       (begin
	 (check-for-break)
	 (thunk)))
     (check-for-break)))

  (define (select-handler/no-breaks e bpz l)
    (cond
     [(null? l)
      (raise e)]
     [((caar l) e)
      (begin0
       ((cdar l) e)
       (with-continuation-mark 
	   break-enabled-key
	   bpz
	 (check-for-break)))]
     [else
      (select-handler/no-breaks e bpz (cdr l))]))

  (define (select-handler/breaks-as-is e bpz l)
    (cond
     [(null? l)
      (raise e)]
     [((caar l) e)
      (with-continuation-mark 
	  break-enabled-key
	  bpz
	(begin
	  (check-for-break)
	  ((cdar l) e)))]
     [else
      (select-handler/breaks-as-is e bpz (cdr l))]))

  (define false-thread-cell (make-thread-cell #f))


  (define (check-with-handlers-in-context handler-prompt-key)
    (unless (continuation-prompt-available? handler-prompt-key) 
      (error 'with-handlers
             "exception handler used out of context")))

  (define handler-prompt-key (make-continuation-prompt-tag))

  (define-syntaxes (with-handlers with-handlers*)
    (let ([wh 
	   (lambda (disable-break?)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ () expr1 expr ...) (syntax/loc stx (let () expr1 expr ...))]
		 [(_ ([pred handler] ...) expr1 expr ...)
		  (with-syntax ([(pred-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-predicate) 
									    (syntax->list #'(pred ...))))]
				[(handler-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-handler) 
									       (syntax->list #'(handler ...))))])
		    (quasisyntax/loc stx
		      (let ([pred-name pred] ...
			    [handler-name handler] ...)
			;; Capture current break parameterization, so we can use it to
			;;  evaluate the body
			(let ([bpz (continuation-mark-set-first #f break-enabled-key)])
			  ;; Disable breaks here, so that when the exception handler jumps
			  ;;  to run a handler, breaks are disabled for the handler
			  (with-continuation-mark
			      break-enabled-key
			      false-thread-cell
			    (call-with-continuation-prompt
			     (lambda ()
			       ;; Restore the captured break parameterization for
			       ;;  evaluating the `with-handlers' body. In this
			       ;;  special case, no check for breaks is needed,
			       ;;  because bpz is quickly restored past call/ec.
			       ;;  Thus, `with-handlers' can evaluate its body in
			       ;;  tail position.
			       (with-continuation-mark 
				   break-enabled-key
				   bpz
                                 (with-continuation-mark 
                                     exception-handler-key
                                     (lambda (e)
                                       ;; Deliver a thunk to the escape handler:
                                       (abort-current-continuation
                                        handler-prompt-key
                                        (lambda ()
                                          (#,(if disable-break?
                                                 #'select-handler/no-breaks
                                                 #'select-handler/breaks-as-is)
                                           e bpz
                                           (list (cons pred-name handler-name) ...)))))
                                   (let ()
                                     expr1 expr ...))))
                             handler-prompt-key
			     ;; On escape, apply the handler thunk
			     (lambda (thunk) (thunk))))))))])))])
      (values (wh #t) (wh #f))))

  (define (call-with-exception-handler exnh thunk)
    ;; The `begin0' ensures that we don't overwrite an enclosing
    ;;  exception handler.
    (begin0
     (with-continuation-mark
         exception-handler-key
         exnh
       (thunk))
     (void)))

  (define-syntax set!-values
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr) (syntax (let-values ([() expr]) (void)))]
	[(_ (id) expr) (identifier? (syntax id)) (syntax (set! id expr))]
	[(_ (id ...) expr)
	 (let ([ids (stx->list (syntax (id ...)))])
	   (for-each
	    (lambda (id)
	      (unless (identifier? id)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    id)))
	    ids)
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate identifier"
				   stx
				   dup))))
	 (with-syntax ([(temp ...) (generate-temporaries (syntax (id ...)))])
	   (syntax/loc
	    stx
	    (let-values ([(temp ...) expr])
	      (set! id temp) ...)))])))

  (define-syntax let/cc
    (lambda (stx)
      (syntax-case stx ()
	[(_ var body1 body ...)
	 (syntax/loc stx (call/cc (lambda (var) body1 body ...)))])))

  (define-syntax fluid-let
    (lambda (stx)
      (syntax-case stx ()
	[(_ () body1 body ...) (syntax/loc stx (let () body1 body ...))]
	[(_ ([name val] ...) body1 body ...)
	 (with-syntax ([(tmp ...) (generate-temporaries (syntax (name ...)))])
	   (syntax/loc
	    stx
	    (let ([tmp val] ...)
	      (let ([swap
		     (lambda ()
		       (let ([s tmp])
			 (set! tmp name)
			 (set! name s))
		       ...)])
		(dynamic-wind
		    swap
		    (lambda () body1 body ...)
		    swap)))))])))

  (define-syntax time
    (lambda (stx)
      (syntax-case stx ()
	[(_ expr1 expr ...)
	 (syntax/loc
	  stx
	  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
	    (printf "cpu time: ~s real time: ~s gc time: ~s~n" cpu user gc)
	    (apply values v)))])))

  (define-syntax (log-it stx)
    (syntax-case stx ()
      [(_ id mode str-expr) 
       #'(let ([l (current-logger)])
           (when (log-level? l 'mode)
             (log-message l 'mode str-expr (current-continuation-marks))))]))
  (define-syntax (define-log stx)
    (syntax-case stx ()
      [(_ id mode) 
       #'(define-syntax (id stx)
           (syntax-case stx ()
             [(_ str-expr)
              #'(log-it id mode str-expr)]))]))
  (define-log log-fatal fatal)
  (define-log log-error error)
  (define-log log-warning warning)
  (define-log log-info info)
  (define-log log-debug debug)

  (#%provide case old-case do
             parameterize parameterize* current-parameterization call-with-parameterization
             parameterize-break current-break-parameterization call-with-break-parameterization
             with-handlers with-handlers* call-with-exception-handler
             set!-values
             let/cc fluid-let time
             log-fatal log-error log-warning log-info log-debug))
