
;;----------------------------------------------------------------------
;; #%misc : file utilities, etc. - remaining functions

(module misc '#%kernel
  (#%require '#%utils ; built into racket
             "more-scheme.rkt" "small-scheme.rkt" "define.rkt"
             (for-syntax '#%kernel "stx.rkt" "stxcase-scheme.rkt" "stxcase.rkt"))
  
  ;; -------------------------------------------------------------------------

  (define-syntax define-syntax-rule
    (lambda (stx)
      (syntax-case stx ()
        [(dr (foo . pattern) template)
         (identifier? #'foo)
         (syntax/loc stx
           (define-syntax foo
             (lambda (x)
               (syntax-case** dr #t x () free-identifier=?
                              [(_ . pattern) (syntax/loc x template)]))))]
        [(dr (foo . pattern) template)
         (raise-syntax-error 'define-syntax-rule "expected an identifier" stx #'foo)]
        [(dr (foo . pattern))
         (raise-syntax-error 'define-syntax-rule "no template provided" stx)]
        [(dr (foo . pattern) template . etc)
         (raise-syntax-error 'define-syntax-rule "too many templates" stx #'etc)]
        [(dr head . template)
         (raise-syntax-error 'define-syntax-rule "invalid pattern" stx #'head)])))

  ;; -------------------------------------------------------------------------

  (define rationalize
    (letrec ([check (lambda (x) 
                      (unless (real? x) (raise-type-error 'rationalize "real" x)))]
	     [find-between 
	      (lambda (lo hi)
		(if (integer? lo)
		    lo
		    (let ([lo-int (floor lo)]
			  [hi-int (floor hi)])
		      (if (< lo-int hi-int)
			  (add1 lo-int)
			  (+ lo-int
			     (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int)))))))))]
             [do-find-between
              (lambda (lo hi)
                (cond
                 [(negative? lo) (- (find-between (- hi) (- lo)))]
                 [else (find-between lo hi)]))])
      (lambda (x within)
	(check x) (check within)
        (let* ([delta (abs within)]
               [lo (- x delta)]
               [hi (+ x delta)])
          (cond
           [(equal? x +nan.0) x]
           [(or (equal? x +inf.0) 
                (equal? x -inf.0))
            (if (equal? delta +inf.0) +nan.0 x)]
           [(equal? delta +inf.0) 0.0]
           [(not (= x x)) +nan.0]
           [(<= lo 0 hi) (if (exact? x) 0 0.0)]
           [(or (inexact? lo) (inexact? hi))
            (exact->inexact (do-find-between (inexact->exact lo) (inexact->exact hi)))]
           [else (do-find-between lo hi)])))))

  ;; -------------------------------------------------------------------------

  (define (read-eval-print-loop)
    (let repl-loop ()
      ;; This prompt catches all error escapes, including from read and print.
      (call-with-continuation-prompt
       (lambda ()
         (let ([v ((current-prompt-read))])
           (unless (eof-object? v)
             (call-with-values
                 (lambda () 
                   ;; This prompt catches escapes during evaluation.
                   ;; Unlike the outer prompt, the handler prints
                   ;; the results.
                   (call-with-continuation-prompt
                    (lambda ()
                      (let ([w (cons '#%top-interaction v)])
                        ((current-eval) (if (syntax? v)
                                            (namespace-syntax-introduce 
                                             (datum->syntax #f w v))
                                            w))))))
               (lambda results (for-each (current-print) results)))
             ;; Abort to loop. (Calling `repl-loop' directory would not be a tail call.)
             (abort-current-continuation (default-continuation-prompt-tag)))))
       (default-continuation-prompt-tag)
       (lambda args (repl-loop)))))

  (define load/cd
    (lambda (n)
      (unless (path-string? n)
	(raise-type-error 'load/cd "path or string (sans nul)" n))
      (let-values ([(base name dir?) (split-path n)])
	(if dir?
	    (raise
	     (exn:fail:filesystem
	      (string->immutable-string
	       (format "load/cd: cannot open a directory: ~s" n))
	      (current-continuation-marks)))
	    (if (not (path? base))
		(load n)
		(begin
		  (if (not (directory-exists? base))
		      (raise
		       (exn:fail:filesystem
			(string->immutable-string
			 (format 
			  "load/cd: directory of ~s does not exist (current directory is ~s)" 
			  n (current-directory)))
			(current-continuation-marks)))
                      (void))
		  (let ([orig (current-directory)])
		    (dynamic-wind
			(lambda () (current-directory base))
			(lambda () (load name))
			(lambda () (current-directory orig))))))))))

  (define (-load load name path)
    (unless (path-string? path) 
      (raise-type-error name "path or string (sans nul)" path))
    (if (complete-path? path)
	(load path)
	(let ([dir (current-load-relative-directory)])
	  (load (if dir (path->complete-path path dir) path)))))
  (define (load-relative path) (-load load 'load-relative path))
  (define (load-relative-extension path) (-load load-extension 'load-relative-extension path))
  
  ;; -------------------------------------------------------------------------

  (define-values (struct:guard make-guard guard? guard-ref guard-set!)
    (make-struct-type 'evt #f 1 0 #f (list (cons prop:evt 0)) (current-inspector) #f '(0)))

  (define (guard-evt proc)
    (unless (and (procedure? proc)
		 (procedure-arity-includes? proc 0))
      (raise-type-error 'guard-evt "procedure (arity 0)" proc))
    (make-guard (lambda (self) (proc))))

  (define (channel-get ch)
    (unless (channel? ch)
      (raise-type-error 'channel-get "channel" ch))
    (sync ch))

  (define (channel-try-get ch)
    (unless (channel? ch)
      (raise-type-error 'channel-try-get "channel" ch))
    (sync/timeout 0 ch))

  (define (channel-put ch val)
    (unless (channel? ch)
      (raise-type-error 'channel-put "channel" ch))
    (and (sync (channel-put-evt ch val)) (void)))

  ;; -------------------------------------------------------------------------

  (define (port? x) (or (input-port? x) (output-port? x)))

  (define displayln
    (case-lambda
     [(v) (displayln v (current-output-port))]
     [(v p) 
      (unless (output-port? p)
        (raise-type-error 'displayln "output port" 1 v p))
      (display v p)
      (newline p)]))

  ;; -------------------------------------------------------------------------

  (#%provide define-syntax-rule
             rationalize 
             path-string? path-replace-suffix path-add-suffix normal-case-path
             read-eval-print-loop
             load/cd
             load-relative load-relative-extension
             path-list-string->path-list find-executable-path
             collection-path collection-file-path load/use-compiled
             guard-evt channel-get channel-try-get channel-put
             port? displayln
             find-library-collection-paths))
