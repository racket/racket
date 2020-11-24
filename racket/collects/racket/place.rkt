#lang racket/base
(require (prefix-in pl- '#%place)
         racket/match
         racket/place/dynamic
         (only-in "private/place.rkt"
                  start-place
                  start-place*)
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/free-vars))

(provide (all-from-out racket/place/dynamic)
         (protect-out place place*)
         place/context)


(define-for-syntax place-body-counter 0)

(define-for-syntax (place-form _in _out _err _start-place-func stx orig-stx)
  (syntax-case stx ()
    [(who ch body1 body ...)
     (if (eq? (syntax-local-context) 'module-begin)
         ;; when a `place' form is the only thing in a module body:
         #`(begin #,stx)
         ;; normal case:
         (let ()
           (unless (syntax-transforming-module-expression?)
             (raise-syntax-error #f "can only be used in a module" stx))
           (unless (identifier? #'ch)
             (raise-syntax-error #f "expected an identifier" stx #'ch))
           (set! place-body-counter (add1 place-body-counter))
           (define module-name-stx
             (datum->syntax stx
               (string->symbol
                (format "place-body-~a" place-body-counter))))
           (with-syntax ([internal-def-name
                          (syntax-local-lift-module
                           #`(module* #,module-name-stx #f
                               (provide main)
                               (define (main ch)
                                 body1 body ...)
                               ;; The existence of this submodule makes the
                               ;; enclosing submodule preserved by `raco exe`:
                               (module declare-preserve-for-embedding '#%kernel)))]
                         [in _in]
                         [out _out]
                         [err _err]
                         [start-place-func _start-place-func])
             #`(place/proc (#%variable-reference) '#,module-name-stx 'who start-place-func in out err))))]
     [(_ ch)
      (raise-syntax-error #f "expected at least one body expression" orig-stx)]))

(define-syntax (place stx)
  (place-form #'#f #'(current-output-port) #'(current-error-port) #'start-place stx stx))

(define-syntax (place* stx)
  (syntax-case stx ()
    [(pf #:in in #:out out #:err err ch body ...) (place-form #'in #'out #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in #:out out ch body ...)           (place-form #'in #'out #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:out out #:err err ch body ...)         (place-form #'#f #'out #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in #:err err ch body ...)           (place-form #'in #'#f  #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in ch body ...)                     (place-form #'in #'#f  #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:out out ch body ...)                   (place-form #'#f #'out #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:err err ch body ...)                   (place-form #'#f #'#f  #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf ch body ...)                             (place-form #'#f #'#f  #'#f   #'start-place* #'(pf ch body ...) stx)]))

(define (place/proc vr submod-name who start-place-func in out err)
  (define name
    (resolved-module-path-name
     (variable-reference->resolved-module-path
      vr)))
  (when (and (symbol? name)
             (not (module-predefined? `(quote ,name))))
     (error who "the enclosing module's resolved name is not a path or predefined"))
  (define submod-ref
    (match name
      [(? symbol?) `(submod (quote ,name) ,submod-name)]
      [(? path?) `(submod ,name ,submod-name)]
      [`(,p ,s ...) `(submod ,(if (symbol? p) `(quote ,p) p) ,@s ,submod-name)]))
  (start-place-func who submod-ref 'main in out err))

(define-syntax (place/context stx)
  (syntax-parse stx
    [(_ ch:id body:expr ...)
     (define b #'(lambda (ch) body ...))
     (define/with-syntax b* (local-expand b 'expression null))
     (define/with-syntax (fvs ...) (free-vars #'b*))
     (define/with-syntax (i ...) (for/list ([(v i) (in-indexed (syntax->list #'(fvs ...)))]) i))
     (define/with-syntax (v p) (generate-temporaries '(v p)))
     #'(let ()
         (define p (place ch (let* ([v (place-channel-get ch)]
                                    [fvs (vector-ref v i)] ...)
                               (b* ch))))
         (define vec (vector fvs ...))
         (for ([e (in-vector vec)]
               [n (in-list (syntax->list (quote-syntax (fvs ...))))])
           (unless (place-message-allowed? e)
             (raise-arguments-error 'place/context
                                    "free variable values must be allowable as place messages"
                                    (symbol->string (syntax-e n)) e)))
         (place-channel-put p vec)
         p)]))
