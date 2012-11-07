#lang racket/base

(require teachpack/2htdp/scribblings/img-eval
         racket/pretty
         racket/sandbox
         mzlib/pconvert
         file/convertible
         scribble/eval)

(provide
 ;; syntax: 
 ;; use with (define-module-local-eval e) ... (eval 'foo e)
 define-module-local-eval 

 ;; syntax: 
 ;; use with @interaction[#:eval (*sl-eval (define x ...) ...) ...] to create interactive examples 
 bsl-eval
 bsl+-eval
 isl-eval
 isl+-eval
 asl-eval)

;; this definition is a pile of hacks accumulated over the course of HtDP/2e writing 
;; there should be a better and simpler way to get this done 
(define-syntax-rule
  (*sl-eval module-lang reader def ...)
  ;; ===>>>
  (let ()
    (define me (make-img-eval))
    (me '(require (only-in racket empty? first rest cons? sqr true false)))
    (me '(require lang/posn))
    (me '(require racket/pretty))
    (me '(current-print pretty-print-handler))
    (me '(pretty-print-columns 65))
    (me 'def)
    ...
    (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
    (call-in-sandbox-context me (lambda () (sandbox-output 'string)))
    (call-in-sandbox-context me (lambda () (sandbox-error-output 'string)))
    (call-in-sandbox-context me (lambda ()
				  (current-print-convert-hook
				    (let ([prev (current-print-convert-hook)])
				      ;; tell `print-convert' to leave images as themselves:
				      (lambda (v basic sub)
					(if (convertible? v)
					    v
					    (prev v basic sub)))))

				  (pretty-print-size-hook
				    (let ([prev (pretty-print-size-hook)])
				      ;; tell `pretty-print' that we'll handle images specially:
				      (lambda (v w? op)
					(if (convertible? v) 1 (prev v w? op)))))
				  
				  (pretty-print-print-hook
				    (let ([prev (pretty-print-print-hook)])
				      ;; tell `pretty-print' how to handle images, which is
				      ;; by using `write-special':
				      (lambda (v w? op)
					(if (convertible? v) (write-special v op) (prev v w? op)))))

				  ((dynamic-require 'htdp/bsl/runtime 'configure)
				   (dynamic-require reader 'options))))
    (call-in-sandbox-context me (lambda () (namespace-require module-lang)))
    (interaction-eval #:eval me (require 2htdp/image))
    (interaction-eval #:eval me (require 2htdp/batch-io))
    ;; --- splice in the defs
    me
    #;
    (lambda x
      (with-handlers ([void (lambda (exn . more)
			      (define msg (exn-message exn))
			      (define x (get-rewriten-error-message exn))
			      (define s (open-output-string))
			      (define y
				(begin
				  (parameterize ([current-error-port s])
				    ((error-display-handler) x 'exn))
				  (get-output-string s)))
			      (displayln `(hello ,msg ,exn ,y))
			      x)])
	(apply me x)))))

(define-syntax-rule
  (bsl-eval def ...)
  (*sl-eval 'lang/htdp-beginner 'htdp/bsl/lang/reader def ...))

(define-syntax-rule
  (bsl+-eval def ...)
  (*sl-eval 'lang/htdp-beginner-abbr 'htdp/bsl+/lang/reader def ...))

(define-syntax-rule
  (isl-eval def ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (isl+-eval def ...)
  (*sl-eval 'lang/htdp-intermediate-lambda 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (asl-eval def ...)
  (*sl-eval 'lang/htdp-advanced 'htdp/asl/lang/reader def ...))

; (isl-eval+)

;; -----------------------------------------------------------------------------

;; (define-module-local-eval name-of-evaluator)
;; a make-base-eval whose namespace is initialized with the module where the macro is used 
(define-syntax-rule 
  (define-module-local-eval name)
  (begin
    (define-namespace-anchor ns)
    (define name 
      (parameterize ([sandbox-namespace-specs (list (lambda () (namespace-anchor->namespace ns)))]
                     [sandbox-error-output 'string]
                     [sandbox-output 'string])
        (make-base-eval)))))
