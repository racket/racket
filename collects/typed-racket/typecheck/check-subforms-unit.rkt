#lang racket/unit

(require "../utils/utils.rkt"
         syntax/kerncase
         syntax/parse
         racket/match
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-funapp.rkt" "tc-subst.rkt"
         (types utils abbrev union subtype)
         (utils tc-utils)
         (rep type-rep))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-subforms^)

;; FIXME -- samth 7/15/11
;; This code is doing the wrong thing wrt the arguments of exception handlers.  
;; In particular, it allows them to be anything at all, but they might
;; get called with the wrong kind of arguments by the exception
;; mechanism.  The right thing is to use the exception predicate.

(define (transpose l) (apply map list l))

;; combine-types : Values * -> tc-results
(define (combine-types . args)
  (match args
    [(list (tc-results: tss) ...)
     (unless (apply = (map length tss))
       (tc-error "Exception handlers and body did not all have the same number of results: ~a" (map length tss)))
     ;; transpose and union
     (let ([ts* (transpose tss)])
       (ret (map (lambda (ts) (apply Un ts)) ts*)))]
    [_ (int-err "Internal error: unsupported exception result type in: ~a" args)]))

;; find the subexpressions that need to be typechecked in an ignored form
;; syntax -> any
(define (check-subforms/with-handlers form [expected #f])
  (define handler-tys '())
  (define body-ty #f)
  (define body-stx #f)
  ;; tc-result1 -> tc-results
  ;; The result of applying the function to a single argument of type (Un)
  ;; FIXME: (Un) is the wrong type, see above fixme
  (define (get-result-ty t)
    (match t
      [(tc-result1: (Function: _))
       (tc/funapp #'here #'(here) t (list (ret (Un))) #f)]
      [_ (int-err "Unsupported function type in get-result-ty: \n~a" t)]))
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
         (let ([t (single-value form)])
           (match t
             [(tc-result1: (Function: _))
              (set! handler-tys (cons (get-result-ty t) handler-tys))]
             [(tc-results: t)
              (tc-error "Exception handler must be a function, got \n~a" t)]))]
        [stx
         ;; this is the body of the with-handlers
         #:when (syntax-property form 'typechecker:exn-body)
         (set! body-stx form)
         (set! body-ty (tc-expr form))]
        [(a . b)
         (loop #'a)
         (loop #'b)]
        [_ (void)])))
  (apply combine-types body-ty handler-tys))

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
;; syntax -> void
(define (check-subforms/ignore form)
  (let loop ([form form])
    (kernel-syntax-case* form #f ()
      [stx
       ;; if this needs to be checked
       (syntax-property form 'typechecker:with-type)
       ;; the form should be already ascribed the relevant type
       (void (tc-expr form))]
      [(a . b)
       (loop #'a)
       (loop #'b)]
      [_ (void)])))
