#lang racket/base

;; This module is used for saved values bindings.  This would have been
;; a very simple use case for an identifier macro, but this fail in one
;; important aspect: in some languages we can't rely on bindings that
;; we'll expand to to behave as usual.  Specifically, such an expansion
;; makes using saved values impossible in TR, since the expansion would
;; eventually be something that has no type (and even if we do some
;; specific hack and add a type, it would be a useless `Any').
;;
;; The solution is a pile of hair...  We define here a syntax-level
;; variable that holds the saved values parameter, and provide a macro
;; that sets it.  Then, xrepl calls us with the parameter values
;; injected directly into the syntax (=> 3d code), which is used to set
;; the variable.  (The syntax level gets instantiated multiple times, so
;; this way they all share the same parameter from xrepl.)  Finally, a
;; reference expands to a `quote' expression with the value injected
;; 3d-ly, and TR will be happy to see just a literal value as its
;; expansion.  (Well, mostly happily -- it makes using functions
;; impossible since TR will infer an `Any' type for them, but that case
;; is hopeless anyway.)

(require (for-syntax racket/base))

(define-for-syntax saved-values-param #f)

(provide set-saved-values-param!)
(define-syntax (set-saved-values-param! stx)
  (syntax-case stx ()
    [(_ p) (parameter? (syntax-e #'p))
           (begin (set! saved-values-param (syntax-e #'p))
                  (datum->syntax #'here (void)))]
    [_ (raise-syntax-error 'set-saved-values-param! "internal error")]))

(provide saved-value-ref)
(define-syntax (saved-value-ref stx)
  (define (ref id)
    (define (err fmt . args)
      (raise-syntax-error (syntax-e id) (apply format fmt args)))
    (unless (parameter? saved-values-param)
      (err "internal error: no saved-values"))
    (define saved (saved-values-param))
    (unless (list? saved) (err "internal error: saved-values isn't a list"))
    (define str (symbol->string (syntax-e id)))
    (define n
      (cond [(regexp-match? #rx"^([^0-9])+$" str) (string-length str)]
            [(regexp-match #rx"[0-9]+$" str)
             => (λ (m) (string->number (car m)))]
            [else (err "unknown name pattern for a saved-value reference")]))
    (unless (pair? saved)          (err "no saved values, yet"))
    (when (n . > . (length saved)) (err "no ~a saved values, yet" n))
    ;; the values are either `#f', or a weak box holding the value
    (define r
      (let ([b (list-ref saved (sub1 n))])
        (and b (or (weak-box-value b)
                   (err "saved value #~a was garbage-collected" n)))))
    #`'#,r)
  (syntax-case stx (set!)
    [(set! id . xs) (raise-syntax-error 'set! "cannot set history reference")]
    [(id . xs) (datum->syntax stx (cons (ref #'id) #'xs) stx)]
    [id (identifier? #'id) (ref #'id)]))
