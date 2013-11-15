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

(define-syntax run-loop
  (syntax-parser
    [(_ init-form [clause bodies ...+] ...)
     #'(let loop ([form init-form])
         (parameterize ([current-orig-stx form])
           (syntax-parse form
             [clause bodies ...] ...
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
  ;; The result of applying the function to a single argument of type (Un)
  ;; FIXME: (Un) is the wrong type, see above fixme
  (define (get-result-ty t)
    (let loop ((t t))
      (match t
        [(Function: _)
         (tc/funapp #'here #'(here) (ret t) (list (ret (Un))) #f)]
        [(? needs-resolving? t)
         (loop (resolve t))]
        [(or (Poly: ns _) (PolyDots: (list ns ... _) _))
         (loop (instantiate-poly t (map (λ (n) Univ) ns)))]
        [_ (int-err "Unsupported function type in get-result-ty: \n~a" t)])))
  (run-loop form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (tc-expr #'stx)]
    ;; this is a handler function
    [stx:exn-handler^
     (let ([t (single-value #'stx)])
       (match t
         [(tc-result1: t)
          (set! handler-tys (cons (get-result-ty t) handler-tys))]))]
    ;; this is the body of the with-handlers
    [stx:exn-body^
     (set! body-stx #'stx)
     (set! body-ty (tc-expr #'stx))])
  (apply combine-types body-ty handler-tys))

;; syntax tc-results -> tc-results
(define (check-subforms/with-handlers/check form expected)
  (run-loop form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (tc-expr #'stx)]
    ;; this is a handler function
    [stx:exn-handler^
     (tc-expr/check #'stx (ret (-> (Un) (tc-results->values expected))))]
    ;; this is the body of the with-handlers
    [stx:exn-body^
     (tc-expr/check #'stx expected)])
  expected)

;; typecheck the expansion of a with-handlers form
;; syntax -> void
(define (check-subforms/ignore form)
  (run-loop form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (void (tc-expr #'stx))]))
