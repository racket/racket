#lang scheme/unit

(require syntax/kerncase
         syntax/struct
         syntax/stx
         scheme/match
         "type-contract.ss"
         "signatures.ss"
         "tc-structs.ss"
         "type-utils.ss"
         "utils.ss" ;; doesn't need tests
         "type-rep.ss" ;; doesn't need tests
         "unify.ss" ;; needs tests
         "infer.ss"
         "type-effect-convenience.ss" ;; maybe needs tests
         "union.ss"
         "subtype.ss" ;; has tests
         "internal-forms.ss" ;; doesn't need tests
         "planet-requires.ss" ;; doesn't need tests
         "type-env.ss" ;; maybe needs tests
         "parse-type.ss" ;; has tests
         "tc-utils.ss" ;; doesn't need tests
         "type-environments.ss" ;; doesn't need tests
         "lexical-env.ss" ;; maybe needs tests
         "type-annotation.ss" ;; has tests
         "type-name-env.ss" ;; maybe needs tests
         "init-envs.ss"
         "effect-rep.ss"
         "mutated-vars.ss")

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-subforms^)

;; find the subexpressions that need to be typechecked in an ignored form
;; syntax -> void
(define (check-subforms/with-handlers form)
  (define handler-tys '())
  (define body-ty #f)    
  (define (get-result-ty t)
    (match t
      [(Function: (list (arr: _ rngs _ _ _) ...)) (apply Un rngs)]
      [_ (tc-error "Internal error in get-result-ty: not a function type: ~n~a" t)]))
  (let loop ([form form])
    (parameterize ([current-orig-stx form])
      (kernel-syntax-case* form #f (#%app)
        [stx
         ;; if this needs to be checked
         (syntax-property form 'typechecker:with-type)
         ;; the form should be already ascribed the relevant type
         (void 
          (tc-expr form))]
        [stx
         ;; this is a hander function
         (syntax-property form 'typechecker:exn-handler)
         (let ([t (tc-expr/t form)])
           (unless (subtype t (-> (Un) Univ))
             (tc-error "Exception handler must be a single-argument function, got ~n~a"))
           (set! handler-tys (cons (get-result-ty t) handler-tys)))]
        [stx
         ;; this is the body of the with-handlers
         (syntax-property form 'typechecker:exn-body)
         (let ([t (tc-expr/t form)])
           (set! body-ty t))]
        [(a . b)
         (begin
           (loop #'a)
           (loop #'b))]
        [_ (void)])))
  (ret (apply Un body-ty handler-tys)))

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
         (tc-expr/check form (-> (Un) expected))]
        [stx
         ;; this is the body of the with-handlers
         (syntax-property form 'typechecker:exn-body)
         (tc-expr/check form expected)]
        [(a . b)
         (begin
           (loop #'a)
           (loop #'b))]
        [_ (void)])))
  (ret expected))

;; typecheck the expansion of a with-handlers form
;; syntax -> type
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
