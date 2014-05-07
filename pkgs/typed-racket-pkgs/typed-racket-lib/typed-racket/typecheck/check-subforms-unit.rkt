#lang racket/unit

(require "../utils/utils.rkt"
         syntax/parse
         racket/match
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-funapp.rkt"
         (types utils abbrev union resolve)
         (private syntax-properties)
         (utils tc-utils)
         (for-syntax racket/base syntax/parse)
         (for-template racket/base)
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

;; Does a depth first search of the syntax object. For each sub object it attempts to match it
;; against the provide syntax-parse patterns.
(define-syntax find-syntax
  (syntax-parser
    [(_ init-form [clause bodies ...+] ...)
     #'(let loop ([form init-form])
         (parameterize ([current-orig-stx form])
           (syntax-parse form
             #:literals (quote-syntax)
             [clause bodies ...] ...
             ;; avoid going under quote-syntax, nothing to typecheck
             [(quote-syntax . rst) (void)]
             [(a . b)
              (loop #'a)
              (loop #'b) ]
             [_ (void)])))]))

;; find the subexpressions that need to be typechecked in an ignored form
;; syntax -> any
(define (check-subforms/with-handlers form [expected #f])
  (define handler-tys '())
  (define body-ty #f)
  (define body-stx #f)
  ;; tc-result1 -> tc-results
  ;; The result of applying the function to a single argument of the type of its first argument
  ;; FIXME: This is the wrong type, see above fixme
  (define (get-result-ty t)
    (let loop ((t t))
      (match t
        [(Function: (list _ ... (arr: (list arg1) _ _ #f (list (Keyword: _ _ #f) ...)) _ ...))
         (tc/funapp #'here #'(here) (ret t) (list (ret arg1)) #f)]
        [(Function: (list _ ... (arr: '() _ (? values rest) #f (list (Keyword: _ _ #f) ...)) _ ...))
         (tc/funapp #'here #'(here) (ret t) (list (ret rest)) #f)]
        [(? needs-resolving? t)
         (loop (resolve t))]
        [(or (Poly: ns _) (PolyDots: (list ns ... _) _))
         (loop (instantiate-poly t (map (λ (n) Univ) ns)))]
        [_ (int-err "Unsupported function type in get-result-ty: \n~a" t)])))
  (find-syntax form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (tc-expr #'stx)]
    ;; this is a handler function
    [stx:exn-handler^
     (match (single-value #'stx)
       [(tc-result1: t)
        (set! handler-tys (cons (get-result-ty t) handler-tys))])]
    ;; this is the body of the with-handlers
    [stx:exn-body^
     (set! body-stx #'stx)
     (set! body-ty (tc-expr #'stx))])
  (apply combine-types body-ty handler-tys))

;; syntax tc-results -> tc-results
(define (check-subforms/with-handlers/check form expected)
  (define body-results #f)
  (find-syntax form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (tc-expr #'stx)]
    ;; this is a handler function
    [stx:exn-handler^
     (tc-expr/check #'stx (ret (-> (Un) (tc-results->values expected))))]
    ;; this is the body of the with-handlers
    [stx:exn-body^
     (set! body-results (tc-expr/check #'stx expected))])
  body-results)

;; typecheck the expansion of a with-handlers form
;; syntax -> void
(define (check-subforms/ignore form)
  (find-syntax form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (void (tc-expr #'stx))]))
