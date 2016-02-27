
;;----------------------------------------------------------------------
;; more-scheme : case, do, etc. - remaining syntax

(module more-scheme '#%kernel
  (#%require "small-scheme.rkt" "define.rkt" '#%paramz "case.rkt" "logger.rkt"
             "member.rkt"
             (for-syntax '#%kernel "stx.rkt" "small-scheme.rkt" "stxcase-scheme.rkt" "qqstx.rkt"))

  ;; For `old-case`:
  (define-syntax case-test
    (lambda (x)
      (syntax-case x ()
        ;; For up to 3 elements, inline `eqv?' tests:
	[(_ x (k))
         (syntax (eqv? x 'k))]
	[(_ x (k1 k2))
         (syntax (let ([tmp x]) (if (eqv? tmp 'k1) #t (eqv? tmp 'k2))))]
	[(_ x (k1 k2 k3))
         (syntax (let ([tmp x]) (if (eqv? tmp 'k1) #t (if (eqv? tmp 'k2) #t (eqv? tmp 'k3)))))]
	[(_ x (k ...))
	 (syntax (memv x '(k ...)))])))

  ;; Mostly from Dybvig:
  (define-syntax (old-case x)
    (syntax-case* x (else) (let ([else-stx (datum->syntax #f 'else)])
                             (lambda (a b) (free-identifier=? a else-stx)))
      ((_ v)
       (syntax (#%expression (begin v (void)))))
      ((_ v (else e1 e2 ...))
       (syntax/loc x (#%expression (begin v (let-values () e1 e2 ...)))))
      ((_ v ((k ...) e1 e2 ...))
       (syntax/loc x (if (case-test v (k ...)) (let-values () e1 e2 ...) (void))))
      ((self v ((k ...) e1 e2 ...) c1 c2 ...)
       (syntax/loc x (let ((x v))
                       (if (case-test x (k ...))
                           (let-values () e1 e2 ...)
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
        x))))
  
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
	   (syntax/loc orig-x
             (let doloop ((var init) ...)
               (if e0
                   (begin (void) e1 ...)
                   (begin c ... (doloop step ...))))))))))
  
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
           (syntax-protect
            (syntax/loc stx
              (with-continuation-mark
                  parameterization-key
                  (extend-parameterization
                   (continuation-mark-set-first #f parameterization-key)
                   p/v ...)
                (let ()
                  expr1
                  expr ...)))))])))

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
      (raise-argument-error 'call-with-parameterization "parameterization?" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-argument-error 'call-with-parameterization "(-> any)" 1 paramz thunk))
    (with-continuation-mark
	parameterization-key
	paramz
      (thunk)))

  (define-syntax parameterize-break
    (lambda (stx)
      (syntax-case stx ()
	[(_ bool-expr expr1 expr ...)
         (syntax-protect
          (syntax/loc stx
            (with-continuation-mark
                break-enabled-key
                (make-thread-cell (and bool-expr #t))
              (begin
                (check-for-break)
                (let ()
                  expr1
                  expr ...)))))])))
  
  (define-values (struct:break-paramz make-break-paramz break-paramz? break-paramz-ref break-paramz-set!)
    (make-struct-type 'break-parameterization #f 1 0 #f))

  (-define-struct break-parameterization (cell))
  
  (define (current-break-parameterization)
    (make-break-paramz (continuation-mark-set-first #f break-enabled-key)))
  
  (define (call-with-break-parameterization paramz thunk)
    (unless (break-paramz? paramz)
      (raise-argument-error 'call-with-break-parameterization "break-parameterization?" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-argument-error 'call-with-parameterization "(-> any)" 1 paramz thunk))
    (begin0
     (with-continuation-mark
	 break-enabled-key
	 (break-paramz-ref paramz 0)
       (begin
	 (check-for-break)
	 (thunk)))
     (check-for-break)))

  (define (select-handler/no-breaks e bpz l)
    (with-continuation-mark 
        break-enabled-key
        ;; make a fresh thread cell so that the shared one isn't mutated
        (make-thread-cell #f)
      (let loop ([l l])
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
          (loop (cdr l))]))))

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

  (define (call-handled-body bpz handle-proc body-thunk)
    ;; Disable breaks here, so that when the exception handler jumps
    ;;  to run a handler, breaks are disabled for the handler
    (with-continuation-mark
        break-enabled-key
        false-thread-cell
      (call-with-continuation-prompt
       (lambda (bpz body-thunk)
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
                 ;; Deliver the exception to the escape handler:
                 (abort-current-continuation
                  handler-prompt-key
                  e))
             (body-thunk))))
       handler-prompt-key
       handle-proc
       bpz body-thunk)))
  
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
                    (syntax-protect
                     (quasisyntax/loc stx
                       (let-values ([(pred-name) pred] ...
                                    [(handler-name) handler] ...)
                         ;; Capture current break parameterization, so we can use it to
                         ;;  evaluate the body
                         (let ([bpz (continuation-mark-set-first #f break-enabled-key)])
                           (call-handled-body
                            bpz
                            (lambda (e)
                              (#,(if disable-break?
                                     #'select-handler/no-breaks
                                     #'select-handler/breaks-as-is)
                               e bpz
                               (list (cons pred-name handler-name) ...)))
                            (lambda ()
                              expr1 expr ...)))))))])))])
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
  
  (define-values (call/cc) call-with-current-continuation)

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
	    (printf "cpu time: ~s real time: ~s gc time: ~s\n" cpu user gc)
	    (apply values v)))])))

  (define-values (hash-update hash-update! hash-has-key? hash-ref!)
    (let* ([not-there (gensym)]
           [up (lambda (who mut? set ht key xform default)
                 (unless (and (hash? ht)
                              (if mut?
                                  (not (immutable? ht))
                                  (immutable? ht)))
                   (raise-argument-error who (if mut? "(and/c hash? (not/c immutable?))" "(and/c hash? immutable?)") ht))
                 (unless (and (procedure? xform)
                              (procedure-arity-includes? xform 1))
                   (raise-argument-error who "(any/c . -> . any/c)" xform))
                 (let ([v (hash-ref ht key default)])
                   (if (eq? v not-there)
                       (raise-mismatch-error who "no value found for key: " key)
                       (set ht key (xform v)))))])
      (let ([hash-update
             (case-lambda
              [(ht key xform default)
               (up 'hash-update #f hash-set ht key xform default)]
              [(ht key xform)
               (hash-update ht key xform not-there)])]
            [hash-update!
             (case-lambda
              [(ht key xform default)
               (up 'hash-update! #t hash-set! ht key xform default)]
              [(ht key xform)
               (hash-update! ht key xform not-there)])]
            [hash-has-key?
             (lambda (ht key)
               (unless (hash? ht)
                 (raise-argument-error 'hash-has-key? "hash?" 0 ht key))
               (not (eq? not-there (hash-ref ht key not-there))))]
            [hash-ref!
             (lambda (ht key new)
               (unless (and (hash? ht)
                            (not (immutable? ht)))
                 (raise-argument-error 'hash-ref! "(and/c hash? (not/c immutable?))" 0 ht key new))
               (let ([v (hash-ref ht key not-there)])
                 (if (eq? not-there v)
                   (let ([n (if (procedure? new) (new) new)])
                     (hash-set! ht key n)
                     n)
                   v)))])
        (values hash-update hash-update! hash-has-key? hash-ref!))))

  (#%provide case old-case do
             parameterize parameterize* current-parameterization call-with-parameterization
             parameterize-break current-break-parameterization call-with-break-parameterization
             (rename break-paramz? break-parameterization?)
             with-handlers with-handlers* call-with-exception-handler
             set!-values
             let/cc call/cc fluid-let time
             log-fatal log-error log-warning log-info log-debug define-logger
             hash-ref! hash-has-key? hash-update hash-update!))
