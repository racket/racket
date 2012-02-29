#lang racket/base
(require racket/stxparam
         unstable/syntax
         syntax/parse/private/residual ;; keep abs. path
         (for-syntax racket/base
                     racket/list
                     syntax/kerncase
                     syntax/strip-context
                     racket/private/sc
                     racket/syntax
                     "rep-attrs.rkt"
                     "rep-data.rkt"))

(provide with
         fail-handler
         cut-prompt
         wrap-user-code

         fail
         try

         let-attributes
         let-attributes*
         let/unpack

         defattrs/unpack
         check-list^depth

         check-literal
         no-shadow
         curried-stxclass-parser
         app-argu)

#|
TODO: rename file

This file contains "runtime" (ie, phase 0) auxiliary *macros* used in
expansion of syntax-parse etc. This file must not contain any
reference that persists in a compiled program; those must go in
residual.rkt.
|#

;; == with ==

(define-syntax (with stx)
  (syntax-case stx ()
    [(with ([stxparam expr] ...) . body)
     (with-syntax ([(var ...) (generate-temporaries #'(stxparam ...))])
       (syntax/loc stx
         (let ([var expr] ...)
           (syntax-parameterize ((stxparam (make-rename-transformer (quote-syntax var)))
                                 ...)
             . body))))]))

;; == Control information ==

(define-syntax-parameter fail-handler
  (lambda (stx)
    (wrong-syntax stx "internal error: used out of context")))
(define-syntax-parameter cut-prompt
  (lambda (stx)
    (wrong-syntax stx "internal error: used out of context")))

(define-syntax-rule (wrap-user-code e)
  (with ([fail-handler #f]
         [cut-prompt #t])
    e))

(define-syntax-rule (fail fs)
  (fail-handler fs))

(define-syntax (try stx)
  (syntax-case stx ()
    [(try e0 e ...)
     (with-syntax ([(re ...) (reverse (syntax->list #'(e ...)))])
       (with-syntax ([(fh ...) (generate-temporaries #'(re ...))])
         (with-syntax ([(next-fh ...) (drop-right (syntax->list #'(fail-handler fh ...)) 1)]
                       [(last-fh) (take-right (syntax->list #'(fail-handler fh ...)) 1)])
           #'(let* ([fh (lambda (fs1)
                          (with ([fail-handler
                                  (lambda (fs2)
                                    (next-fh (cons fs1 fs2)))])
                            re))]
                    ...)
               (with ([fail-handler last-fh])
                 e0)))))]))

;; == Attributes

(define-for-syntax (parse-attr x)
  (syntax-case x ()
    [#s(attr name depth syntax?) #'(name depth syntax?)]))

(define-syntax (let-attributes stx)
  (syntax-case stx ()
    [(let-attributes ([a value] ...) . body)
     (with-syntax ([((name depth syntax?) ...)
                    (map parse-attr (syntax->list #'(a ...)))])
       (with-syntax ([(vtmp ...) (generate-temporaries #'(name ...))]
                     [(stmp ...) (generate-temporaries #'(name ...))])
         #'(letrec-syntaxes+values
               ([(stmp) (make-attribute-mapping (quote-syntax vtmp)
                                                'name 'depth 'syntax?)] ...)
               ([(vtmp) value] ...)
             (letrec-syntaxes+values
                 ([(name) (make-syntax-mapping 'depth (quote-syntax stmp))] ...)
                 ()
               . body))))]))

;; (let-attributes* (([id num] ...) (expr ...)) expr) : expr
;; Special case: empty attrs need not match number of value exprs.
(define-syntax let-attributes*
  (syntax-rules ()
    [(la* (() _) . body)
     (let () . body)]
    [(la* ((a ...) (val ...)) . body)
     (let-attributes ([a val] ...) . body)]))

;; (let/unpack (([id num] ...) expr) expr) : expr
;; Special case: empty attrs need not match packed length
(define-syntax (let/unpack stx)
  (syntax-case stx ()
    [(let/unpack (() packed) body)
     #'body]
    [(let/unpack ((a ...) packed) body)
     (with-syntax ([(tmp ...) (generate-temporaries #'(a ...))])
       #'(let-values ([(tmp ...) (apply values packed)])
           (let-attributes ([a tmp] ...) body)))]))

(define-syntax (defattrs/unpack stx)
  (syntax-case stx ()
    [(defattrs (a ...) packed)
     (with-syntax ([((name depth syntax?) ...)
                    (map parse-attr (syntax->list #'(a ...)))])
       (with-syntax ([(vtmp ...) (generate-temporaries #'(name ...))]
                     [(stmp ...) (generate-temporaries #'(name ...))])
         #'(begin (define-values (vtmp ...) (apply values packed))
                  (define-syntax stmp
                    (make-attribute-mapping (quote-syntax vtmp)
                                            'name 'depth 'syntax?))
                  ...
                  (define-syntax name (make-syntax-mapping 'depth (quote-syntax stmp)))
                  ...)))]))

;; (check-list^depth attr expr)
(define-syntax (check-list^depth stx)
  (syntax-case stx ()
    [(_ a expr)
     (with-syntax ([#s(attr name depth syntax?) #'a])
       (quasisyntax/loc #'expr
         (check-list^depth* 'name 'depth expr)))]))

;; (check-literal id phase-level-expr ctx) -> void
(define-syntax (check-literal stx)
  (syntax-case stx ()
    [(check-literal id used-phase-expr ctx)
     (let* ([ok-phases/ct-rel
             ;; id is bound at each of ok-phases/ct-rel
             ;; (phase relative to the compilation of the module in which the
             ;; 'syntax-parse' (or related) form occurs)
             (filter (lambda (p) (identifier-binding #'id p)) '(0 1 -1 #f))])
       ;; so we can avoid run-time call to identifier-binding if
       ;;   (+ (phase-of-enclosing-module) ok-phase/ct-rel) = used-phase
       (with-syntax ([ok-phases/ct-rel ok-phases/ct-rel])
         #`(check-literal* (quote-syntax id)
                           used-phase-expr
                           (phase-of-enclosing-module)
                           'ok-phases/ct-rel
                           ;; If context is not stripped, racket complains about
                           ;; being unable to restore bindings for compiled code;
                           ;; and all we want is the srcloc, etc.
                           (quote-syntax #,(strip-context #'ctx)))))]))

;; ====

(begin-for-syntax
 (define (check-shadow def)
   (syntax-case def ()
     [(_def (x ...) . _)
      (parameterize ((current-syntax-context def))
        (for ([x (in-list (syntax->list #'(x ...)))])
          (let ([v (syntax-local-value x (lambda _ #f))])
            (when (syntax-pattern-variable? v)
              (wrong-syntax
               x
               ;; FIXME: customize "~do pattern" vs "#:do block" as appropriate
               "definition in ~~do pattern must not shadow attribute binding")))))])))

(define-syntax (no-shadow stx)
  (syntax-case stx ()
    [(no-shadow e)
     (let ([ee (local-expand #'e (syntax-local-context)
                             (kernel-form-identifier-list))])
       (syntax-case ee (begin define-values define-syntaxes)
         [(begin d ...)
          #'(begin (no-shadow d) ...)]
         [(define-values . _)
          (begin (check-shadow ee)
                 ee)]
         [(define-syntaxes . _)
          (begin (check-shadow ee)
                 ee)]
         [_
          ee]))]))

(define-syntax (curried-stxclass-parser stx)
  (syntax-case stx ()
    [(_ class argu)
     (with-syntax ([#s(arguments (parg ...) (kw ...) _) #'argu])
       (let ([sc (get-stxclass/check-arity #'class #'class
                                           (length (syntax->list #'(parg ...)))
                                           (syntax->datum #'(kw ...)))])
         (with-syntax ([parser (stxclass-parser sc)])
           #'(lambda (x cx pr es fh cp rl success)
               (app-argu parser x cx pr es fh cp rl success argu)))))]))

(define-syntax (app-argu stx)
  (syntax-case stx ()
    [(aa proc extra-parg ... #s(arguments (parg ...) (kw ...) (kwarg ...)))
     #|
     Use keyword-apply directly?
        #'(keyword-apply proc '(kw ...) (list kwarg ...) parg ... null)
     If so, create separate no-keyword clause.
     |#
     ;; For now, let #%app handle it.
     (with-syntax ([((kw-part ...) ...) #'((kw kwarg) ...)])
       #'(proc kw-part ... ... extra-parg ... parg ...))]))
