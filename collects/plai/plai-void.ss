
;; Like PLAI Advanced, but all functions must accept one
;;  argument, and the result is always changed to void.
;; To enforce void returns, tail call are broken, currently.
;; No `lambda', `local', `let', `let*', or `letrec'.

(module plai-void mzscheme
  (require (rename (lib "htdp-advanced.ss" "lang") plai-else else)
	   (rename (lib "htdp-advanced.ss" "lang") advanced-define define)
	   "private/datatype.ss"
           "test-harness.ss")

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-void stx)
    #'(begin
	(require (all-except (lib "htdp-advanced.ss" "lang")
			     lambda define local let let* letrec))
	(provide (all-from-except (lib "htdp-advanced.ss" "lang")
				  plai-else advanced-define))))
  (provide-void)
		   
  (provide (rename void-define define)
	   (rename void-type-case type-case)
	   define-type
	   require provide provide-type
           (all-from "test-harness.ss"))

  (define-syntax (void-define stx)
    (syntax-case stx ()
      [(_ id v)
       (identifier? #'id)
       #'(advanced-define id v)]
      [(_ (id) body)
       (identifier? #'id)
       #'(advanced-define (id) (begin body (void)))]
      [(_ (id x0 x ...) . rest)
       (andmap identifier? (syntax->list #'(id x0 x ...)))
       (raise-syntax-error
	#f
	"defined functions must accept no arguments in this language"
	stx)]
      [(_ . rest)
       #'(advanced-define . rest)]))

  (define-type-case void-type-case plai-else))
