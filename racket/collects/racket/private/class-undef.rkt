#lang racket/base
(require (for-syntax racket/base
                     syntax/private/boundmap
                     syntax/kerncase))

(provide declare-field-use-start
         declare-field-initialization
         declare-field-use
         declare-inherit-use
         declare-field-assignment
         declare-this-escapes
         declare-super-new

         field-initialization-value
         
         detect-field-unsafe-undefined)

;; The `class` macros inject declarations into expansions
;; of the form `(begin (_declare-word _id ...) _expr ...)`
;; for each of the following `_declare-word`s:
(define-syntax declare-field-use-start #f) ; marks start of initialization
(define-syntax declare-field-initialization #f)
(define-syntax declare-field-use #f)
(define-syntax declare-inherit-use #f)
(define-syntax declare-field-assignment #f)
(define-syntax declare-this-escapes #f)
(define-syntax declare-super-new #f)

;; A wrapper around the RHS of an initlization assignment,
;; recognized by field identifier macros:
(define-syntax field-initialization-value #f)

;; A wrapper macro that runs the `need-undeed-check?` analysis
;; and adds a boolean argument to a call to `compose-class`:
(define-syntax (detect-field-unsafe-undefined stx)
  (syntax-case stx ()
    [(_ compose-class arg ... proc final)
     (let-values ([(exp exp-proc) (syntax-local-expand-expression #'proc)])
       (with-syntax ([exp-proc exp-proc]
                     [need-undef? (need-undefined-check? exp)])
         (syntax/loc stx
           (compose-class arg ... proc need-undef? final))))]))

;; Analysis to detect whether any field can be referenced while
;; its value is `unsafe-undefined`, based on `declare-...` annotations
;; inserted by macros.
(define-for-syntax (need-undefined-check? exp)
  ;; All local fields need to be initialized (i.e., assigned)
  ;; before a method call or `super-new`
  (define init-too-late? #f)
  ;; It's ok to use inherited fields only after `super-new` has
  ;; definitely been called:
  (define super-new? #f)

  ;; cloop returns #t if access-before-definition looks possible:
  (let cloop ([exp exp]
              [ready #f] ; table of initializations, after start
              [in-branch? #f])
    (define (loop e) (cloop e ready in-branch?))

    (kernel-syntax-case exp #f
      [_
       (identifier? exp)
       #f]

      ;; ----------------------------------------
      ;; Handle annotations at start of `begin`:

      [(begin) #f]
      [(begin '(decl) . body)
       (and (identifier? #'decl)
            (free-identifier=? #'decl #'declare-field-use-start))
       ;; Beginning of the class body; start tracking initialization
       ;; creating the `ready` table:
       (cloop #`(begin . body) (make-module-identifier-mapping) #f)]
      [(begin '(decl id ...) . body)
       (and (identifier? #'decl)
            (free-identifier=? #'decl #'declare-field-initialization))
       ;; A field is initialized. If this is after an action that
       ;; might read a field externally, it's too late. Otherwise,
       ;; assuming that we're not in a branch, the field is after here
       ;; initialized (but not before the right-hand side is evaluated):
       (let ([ids (syntax->list #'(id ...))])
         (or (and ready
                  init-too-late?
                  (ormap (lambda (id)
                           (not (module-identifier-mapping-get ready id (lambda () #f))))
                         ids)
                  (report #'body)
                  #t)
              ;; field is ready after RHS is evaluated:
             (begin0
              (loop #'(begin . body))
              (when ready
                (unless in-branch?
                  (for-each (lambda (id)
                              (module-identifier-mapping-put! ready id #t))
                            ids))))))]
      [(begin '(decl id ...) . body)
       (and (identifier? #'decl)
            (or (free-identifier=? #'decl #'declare-field-use)
                (free-identifier=? #'decl #'declare-field-assignment)))
       ;; A field is used or assigned. If tracking has started, make sure the
       ;; field is definitely initalized:
       (or (and ready
                (ormap (lambda (id)
                         (not (module-identifier-mapping-get ready id (lambda () #f))))
                       (syntax->list #'(id ...)))
                (report #'body)
                #t)
           (loop #'(begin . body)))]
      [(begin '(decl id ...) . body)
       (and (identifier? #'decl)
            (free-identifier=? #'decl #'declare-inherit-use))
       ;; It's ok to use an inherited field only if `super-new` has
       ;; definitely been called.
       (or (and ready
                (not super-new?)
                (report #'body)
                #t)
           (loop #'(begin . body)))]
      [(begin '(decl) . body)
       (and (identifier? #'decl)
            (free-identifier=? #'decl #'declare-this-escapes))
       ;; Any method call or explicit use of `this` means a field
       ;; might be accessed outside of the `class` declaration,
       ;; so any initialization afterward is too late:
       (begin
         (when ready (set! init-too-late? #t))
         (loop #'(begin . body)))]

      [(begin '(decl) . body)
       (and (identifier? #'decl)
            (free-identifier=? #'decl #'declare-super-new))
       ;; As long as we're not in a branch, `super-new` is definitely
       ;; called after here.
       (begin
         (when (and ready (not in-branch?)) (set! super-new? #t))
         (loop #'(begin '(declare-this-escapes) . body)))]

      ;; ----------------------------------------
      ;; Abstract interpretation of core forms.

      ;; We model order by calling `cloop` in order, which can mutate
      ;; `init-too-late?`, `super-new?` and `ready`. (Those could
      ;; have been threaded through, but local mutation is easier.)

      ;; We model both branches and delayed computation (via closures)
      ;; by recurring with a true `in-branch?`. In a branch, we
      ;; pessimistically ignore initialization (via local-field
      ;; assignment) and super-new`, and pessimistcally assume all
      ;; references, method calls, and inherite-field assignemtnt.
      ;; [Room for improvement: use a functional table in place of
      ;; `ready`, etc., and suitably split and merge.]

      [(begin exp . body)
       (or (loop #'exp)
           (loop #'(begin . body)))]

      [(#%plain-lambda _ exp ...)
       (cloop #'(begin exp ...) ready #t)]
      [(case-lambda clause ...)
       (ormap (lambda (clause)
                (cloop #`(#%plain-lambda . #,clause) ready #t))
              (syntax->list #'(clause ...)))]
      [(if tst thn els)
       (or (loop #'tst)
           (cloop #'thn ready #t)
           (cloop #'els ready #t))]
      [(begin0 exp ...)
       (loop #'(begin exp ...))]
      [(let-values ([(id ...) exp] ...) body-exp ...)
       (loop #'(begin exp ... body-exp ...))]
      [(letrec-values ([(id ...) exp] ...) body-exp ...)
       (loop #'(begin exp ... body-exp ...))]
      [(letrec-syntaxes+values _ ([(id ...) exp] ...) body-exp ...)
       (loop #'(begin exp ... body-exp ...))]
      [(set! id exp)
       (loop #'exp)]

      [(quote . _) #f]
      [(quote-syntax . _) #f]
      
      [(with-continuation-mark key val exp)
       (loop #'(begin key val exp))]
      
      [(#%plain-app exp ...)
       (loop #'(begin exp ...))]
      [(#%top . _) #f]
      [(#%variable-reference . _) #f]

      [(#%expression expr) (loop #'expr)]

      [_else (raise-syntax-error #f "unrecognized expression form" exp)])))

(define-for-syntax (report exprs)
  (when (pair? (syntax->list exprs))
    (define expr (car (syntax->list exprs)))
    (define s (srcloc->string (srcloc (syntax-source expr)
                                      (syntax-line expr)
                                      (syntax-column expr)
                                      (syntax-position expr)
                                      (syntax-span expr))))
    (log-message (make-logger 'optimizer (current-logger))
                 'debug
                 (format "chaperoning to prevent undefined access due to: ~.s~a~a"
                         (syntax->datum expr)
                         (if s " at: " "")
                         (or s ""))
                 expr)))
