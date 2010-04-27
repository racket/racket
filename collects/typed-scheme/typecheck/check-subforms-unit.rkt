#lang scheme/unit

(require "../utils/utils.ss"
	 syntax/kerncase
	 syntax/parse
         scheme/match unstable/debug
         "signatures.ss" "tc-metafunctions.ss"
         (types utils convenience union subtype)
	 (utils tc-utils)
	 (rep type-rep))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-subforms^)

;; find the subexpressions that need to be typechecked in an ignored form
;; syntax -> any
(define (check-subforms/with-handlers form [expected #f])
  (define handler-tys '())
  (define body-ty #f)    
  (define (get-result-ty t)
    (match t
      [(Function: 
	(list 
	 (arr: _ 
	       (Values: (list (Result: rngs _ _) ...)) 
	       _ _ (list (Keyword: _ _ #t) ...))))
       (apply Un rngs)]
      [_ (int-err "Internal error in get-result-ty: not a function type: ~n~a" t)]))
  (let loop ([form form])
    (parameterize ([current-orig-stx form])
      (syntax-parse form
        [stx
         ;; if this needs to be checked
         #:when (syntax-property form 'typechecker:with-type)
         ;; the form should be already ascribed the relevant type
	 (tc-expr form)]
        [stx
         ;; this is a handler function
         #:when (syntax-property form 'typechecker:exn-handler)
         (let ([t (tc-expr form)])
           (match t
	     [(tc-result1: 
	       (and t 
		    (Function: (list (arr: (list _) _ _ _ (list (Keyword: _ _ #f) ...)) ...))))
	      (set! handler-tys (cons (get-result-ty t) handler-tys))]
	     [(tc-results: t)
	      (tc-error "Exception handler must be a single-argument function, got ~n~a" t)]))]
        [stx
         ;; this is the body of the with-handlers
         #:when (syntax-property form 'typechecker:exn-body)
         (match-let ([(tc-results: ts) (tc-expr form)])
           (set! body-ty (-values ts)))]
        [(a . b)
	 (loop #'a)
	 (loop #'b)]
        [_ (void)])))
  (ret (apply Un body-ty handler-tys)))

;; syntax type -> any
(define (check-subforms/with-handlers/check form expected)
  (let loop ([form form])
    (parameterize ([current-orig-stx form])
      (kernel-syntax-case* form #f ()
        [stx
         ;; if this needs to be checked
         (syntax-property form 'typechecker:with-type)
         ;; the form should be already ascribed the relevant type
         (tc-expr form)]
        [stx
         ;; this is a hander function
         (syntax-property form 'typechecker:exn-handler)
         (tc-expr/check form (ret (-> (Un) (tc-results->values expected))))]
        [stx
         ;; this is the body of the with-handlers
         (syntax-property form 'typechecker:exn-body)
         (tc-expr/check form expected)]
        [(a . b)
         (begin
           (loop #'a)
           (loop #'b))]
        [_ (void)])))
  expected)

;; typecheck the expansion of a with-handlers form
;; syntax -> any
(define (check-subforms/ignore form)
  (let loop ([form form])
    (kernel-syntax-case* form #f ()
      [stx
       ;; if this needs to be checked
       (syntax-property form 'typechecker:with-type)
       ;; the form should be already ascribed the relevant type
       (tc-expr form)]
      [(a . b)
       (loop #'a)
       (loop #'b)]
      [_ (void)])))
