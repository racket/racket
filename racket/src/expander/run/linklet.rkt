#lang racket/base
(require racket/unsafe/undefined
         racket/fasl
         "../common/set.rkt"
         "../syntax/datum-map.rkt"
         "../host/correlate.rkt"
         "../common/reflect-hash.rkt"
         "../boot/runtime-primitive.rkt"
         "correlated-to-host-syntax.rkt"
         "linklet-operation.rkt")

;; A "linklet" is the primitive form of separate (not necessarily
;; independent) compilation and linking. A `linklet` is serializable
;; linklet, and instantiation of a linklet produces an "instance"
;; given other instances to satisfy its imports. An instance, which
;; essentially just maps symbols to values, can also be created
;; directly, so it serves as the bridge between the worlds of values
;; and compiled objects.

;; A "linklet bundle" is similarly a primitive construct that is
;; essentially a mapping of symbols and fixnums to linklets, symbols,
;; and symbol lists. A bundle is used, for example, to implement a
;; module (which is a collection of linklets plus some static
;; metadata).

;; Finally, a "linklet directory" is a primitive construct that is a
;; mapping of #f to a bundle and symbols to linklet directories. The
;; intent is that individual linklet bundles can be efficiently
;; extracted from the marshaled form of a linklet directory --- the
;; primitive form of accessing an indvidual submodule.

;; For bootstrapping, we can implement linklets here by compiling
;; `linklet` to `lambda`. If the host Racket supports linklets, then
;; this is not necessary, except to the degree that `compile-linklet`
;; needs to be replaced with a variant that "compiles" to source.

(define (variable-reference-from-unsafe? x) #f)

;; See "linklet-operation.rkt":
(linklet-operations=> provide)

;; Helpers for "extract.rkt"
(provide linklet-compile-to-s-expr  ; a parameter; whether to "compile" to a source form
         linklet-as-s-expr?

         s-expr-linklet-importss+localss
         s-expr-linklet-exports+locals
         s-expr-linklet-body)

(struct linklet (compiled-proc  ; takes self instance plus instance arguments to run the linklet body
                 importss       ; list [length is 1 less than proc arity] of list of symbols
                 exports)       ; list of symbols
        #:prefab)

(struct instance (name        ; for debugging, typically a module name + phase
                  data        ; any value (e.g., a namespace)
                  variables)) ; symbol -> value

(define (make-instance name [data #f] [mode #f] . content)
  (define i (instance name data (make-hasheq)))
  (let loop ([content content])
    (cond
     [(null? content) (void)]
     [else
      (unless (symbol? (car content))
        (raise-argument-error 'make-instance
                              "symbol?"
                              (car content)))
      (when (null? (cdr content))
        (raise-arguments-error 'make-instance
                               "missing variable value"
                               "variable" (car content)))
      (instance-set-variable-value! i (car content) (cadr content) mode)
      (loop (cddr content))]))
  i)

(define (instance-variable-names i)
  (hash-keys (instance-variables i)))

(define (instance-variable-box i sym can-create?)
  (or (hash-ref (instance-variables i) sym #f)
      (if can-create?
          (let ([b (box undefined)])
            (hash-set! (instance-variables i) sym b)
            b)
          (error 'link "missing binding: ~s" sym))))

(define (instance-set-variable-value! i sym val [constant? #f])
  (set-box! (instance-variable-box i sym #t) val))

(define (instance-unset-variable! i sym)
  (set-box! (instance-variable-box i sym #t) undefined))

(define (instance-variable-value i sym [fail-k (lambda () (error "instance variable not found:" sym))])
  (define b (hash-ref (instance-variables i) sym #f))
  (cond
   [(and b
         (not (eq? (unbox b) undefined)))
    (unbox b)]
   [(procedure? fail-k) (fail-k)]
   [else fail-k]))

(define (instance-describe-variable! i sym desc)
  (void))

;; ----------------------------------------

(define undefined (gensym 'undefined))

(define (check-not-undefined val sym)
  (if (eq? val undefined)
      (check-not-unsafe-undefined unsafe-undefined sym)
      val))

;; ----------------------------------------

(define (primitive-table name)
  (cond
   [(eq? name '#%bootstrap-linklet) #f]
   [(eq? name '#%linklet) (linklet-operations=> reflect-hash)]
   [else
    (define mod-name `(quote ,name))
    (define-values (vars trans) (module->exports mod-name))
    (for/hasheq ([sym (in-list (map car (cdr (assv 0 vars))))])
      (values sym
              (dynamic-require mod-name sym)))]))

;; Bootstrap implementation doesn't support bytecode:
(define (primitive->compiled-position v) #f)
(define (compiled-position->primitive pos) #f)
(define (primitive-in-category? name cat-sym) #f)

;; ----------------------------------------

(struct variable-reference (instance primitive-varref))

(define (variable-reference->instance vr [ref-site? #f])
  (and (or ref-site?
           ;; It would be better to have a `variable-reference-anonymous?` predicate:
           (with-handlers ([exn:fail? (lambda (exn) #f)])
             (variable-reference->module-declaration-inspector
              (variable-reference-primitive-varref vr))))
       ;; Always returning ref-site instance; that's good enough to
       ;; bootstrap:
       (variable-reference-instance vr)))

(define variable-reference-constant?*
  (let ([variable-reference-constant?
         (lambda (vr)
           (variable-reference-constant? (variable-reference-primitive-varref vr)))])
    variable-reference-constant?))


(define variable-reference-from-unsafe?*
  (let ([variable-reference-from-unsafe?
         (lambda (vr)
           (variable-reference-from-unsafe? (variable-reference-primitive-varref vr)))])
    variable-reference-from-unsafe?))

;; ----------------------------------------

(define cu-namespace (make-empty-namespace))
(namespace-attach-module (current-namespace) ''#%builtin cu-namespace)
(parameterize ([current-namespace cu-namespace])
  (for ([name (in-list runtime-instances)])
    (namespace-require `',name))
  (namespace-require ''#%linklet)
  (namespace-set-variable-value! 'check-not-undefined check-not-undefined)
  (namespace-set-variable-value! 'instance-variable-box instance-variable-box)
  (namespace-set-variable-value! 'variable-reference variable-reference)
  (namespace-set-variable-value! 'variable-reference? variable-reference? #t)
  (namespace-set-variable-value! 'variable-reference->instance variable-reference->instance #t)
  (namespace-set-variable-value! 'variable-reference-constant? variable-reference-constant?* #t)
  (namespace-set-variable-value! 'variable-reference-from-unsafe? variable-reference-from-unsafe?* #t)
  ;; Needed when the host is RacketCS:
  (namespace-set-variable-value! 'fasl->s-exp/intern (lambda (v)
                                                       (fasl->s-exp v #:datum-intern? #t))))

;; ----------------------------------------

;; Compile a `linklet` to a plain `lambda`. Also, convert from the
;; notion of correlated that works for `compile-linklet` to the notion
;; of host syntax objects that works for `compile`.
(define (desugar-linklet c)
  (define imports (list-ref c 1))
  (define exports (list-ref c 2))
  (define bodys (list-tail c 3))
  (define inst-names (for/list ([import (in-list imports)]
                                [i (in-naturals)])
                       (string->symbol (format "in_~a" i))))
  (define import-box-bindings
    (for/list ([inst-imports (in-list imports)]
               [inst (in-list inst-names)]
               #:when #t
               [name (in-list inst-imports)])
      (define ext (if (symbol? name) name (car name)))
      (define int (if (symbol? name) name (cadr name)))
      `[(,int) (instance-variable-box ,inst ',ext #f)]))
  (define export-box-bindings
    (for/list ([name (in-list exports)])
      (define int (if (symbol? name) name (car name)))
      (define ext (if (symbol? name) name (cadr name)))
      `[(,int) (instance-variable-box self-inst ',ext #t)]))
  (define box-bindings (append import-box-bindings export-box-bindings))
  (define import-box-syms (apply seteq (map caar import-box-bindings)))
  (define box-syms (set-union import-box-syms
                              (apply seteq (map caar export-box-bindings))))
  (define (desugar e)
    (cond
     [(correlated? e)
      (correlate e (desugar (correlated-e e)))]
     [(symbol? e) (if (set-member? box-syms e)
                      (if (set-member? import-box-syms e)
                          `(unbox ,e)
                          `(check-not-undefined (unbox ,e) ',e))
                      e)]
     [(pair? e)
      (case (correlated-e (car e))
        [(quote) e]
        [(set!)
         (define-correlated-match m e '(set! var rhs))
         (if (set-member? box-syms (correlated-e (m 'var)))
             `(set-box! ,(m 'var) ,(desugar (m 'rhs)))
             `(set! ,(m 'var) ,(desugar (m 'rhs))))]
        [(define-values)
         (define-correlated-match m e '(define-values (id ...) rhs))
         (define ids (m 'id))
         (define tmps (map gensym (map correlated-e ids)))
         `(define-values ,(for/list ([id (in-list ids)]
                                     #:when (not (set-member? box-syms (correlated-e id))))
                            id)
           (let-values ([,tmps (let-values ([,ids ,(desugar (m 'rhs))])
                                 (values ,@ids))])
             (begin
               ,@(for/list ([id (in-list ids)]
                            [tmp (in-list tmps)]
                            #:when (set-member? box-syms (correlated-e id)))
                   `(set-box! ,id ,tmp))
               (values ,@(for/list ([id (in-list ids)]
                                    [tmp (in-list tmps)]
                                    #:when (not (set-member? box-syms (correlated-e id))))
                           tmp)))))]
        [(lambda)
         (define-correlated-match m e '(lambda formals body))
         `(lambda ,(m 'formals) ,(desugar (m 'body)))]
        [(case-lambda)
         (define-correlated-match m e '(case-lambda [formals body] ...))
         `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                                    [body (in-list (m 'body))])
                           `[,formals ,(desugar body)]))]
        [(#%variable-reference)
         (if (and (pair? (correlated-e (cdr (correlated-e e))))
                  (set-member? box-syms (correlated-e (correlated-cadr e))))
             ;; Using a plain `#%variable-reference` (for now) means
             ;; that all imported and exported variables count as
             ;; mutable:
             '(variable-reference self-inst (#%variable-reference))
             ;; Preserve info about a local identifier:
             `(variable-reference self-inst ,e))]
        [else (map desugar (correlated->list e))])]
     [else e]))
  (define (last-is-definition? bodys)
    (define p (car (reverse bodys)))
    (and (pair? p) (eq? (correlated-e (car p)) 'define-values)))
  (correlated->host-syntax
   `(lambda (self-inst ,@inst-names)
     (let-values ,box-bindings
       ,(cond
         [(null? bodys) '(void)]
         [else
          `(begin
            ,@(for/list ([body (in-list bodys)])
                (desugar body))
            ,@(if (last-is-definition? bodys)
                  '((void))
                  null))])))))

;; #:pairs? #f -> list of list of symbols
;; #:pairs? #t -> list of list of (cons ext-symbol int-symbol)
(define (extract-import-variables-from-expression c #:pairs? pairs?)
  (for/list ([is (in-list (unmarshal (list-ref c 1)))])
    (for/list ([i (in-list is)])
      (cond 
       [pairs? (if (symbol? i)
                   (cons i i)
                   (cons (car i) (cadr i)))]
       [else (if (symbol? i)
                 i
                 (car i))]))))

;; #:pairs? #f -> list of symbols
;; #:pairs? #t -> list of (cons ext-symbol int-symbol)
(define (extract-export-variables-from-expression c #:pairs? pairs?)
  (for/list ([e (in-list (unmarshal (list-ref c 2)))])
    (cond
     [pairs? (if (symbol? e)
                 (cons e e)
                 (cons (cadr e) (car e)))]
     [else (if (symbol? e)
               e
               (cadr e))])))

;; ----------------------------------------

(define orig-eval (current-eval))
(define orig-compile (current-compile))

(define linklet-compile-to-s-expr (make-parameter #f))

;; Compile to a serializable form
(define (compile-linklet c [name #f] [import-keys #f] [get-import (lambda (key) (values #f #f))] [options '(serializable)])
  (define l
    (cond
      [(linklet-compile-to-s-expr)
       (marshal (correlated->datum/lambda-name c))]
      [else
       (define plain-c (desugar-linklet c))
       (parameterize ([current-namespace cu-namespace]
                      [current-eval orig-eval]
                      [current-compile orig-compile])
         ;; Use a vector to list the exported variables
         ;; with the compiled bytecode
         (linklet (compile plain-c)
                  (marshal (extract-import-variables-from-expression c #:pairs? #f))
                  (marshal (extract-export-variables-from-expression c #:pairs? #f))))]))
  (if import-keys
      (values l import-keys) ; no imports added or removed
      l))

;; For re-optimizing:
(define (recompile-linklet linklet name [import-keys #f] [get-import (lambda (key) (values #f #f))])
  (if import-keys
      (values linklet import-keys)
      linklet))

;; Intended for JIT preparation
;; (and we could compile to a function here)
(define (eval-linklet c)
  c)

(define (linklet-virtual-machine-bytes)
  #"source")

(define (write-linklet-bundle-hash ld in)
  (write ld in))

(define (read-linklet-bundle-hash in)
  (read in))

;; Convert linklet to a procedure
(define (really-eval-linklet cl)
  (parameterize ([current-namespace cu-namespace]
                 [current-eval orig-eval]
                 [current-compile orig-compile])
    (if (linklet? cl)
        ;; Normal mode: compiled to struct
        (eval (linklet-compiled-proc cl))
        ;; Assume previously "compiled" to source:
        (or (hash-ref eval-cache cl #f)
            (let ([proc (eval (desugar-linklet (unmarshal cl)))])
              (hash-set! eval-cache cl proc)
              proc)))))
(define eval-cache (make-weak-hasheq))

;; Check whether we previously compiled a linket to source
(define (linklet-as-s-expr? cl)
  (not (linklet? cl)))

;; Instantiate
(define (instantiate-linklet linklet import-instances [target-instance #f] [use-prompt? #t])
  (cond
   [(not target-instance)
    ;; return newly created instance
    (define target-instance (make-instance 'anonymous))
    (instantiate-linklet linklet import-instances target-instance)
    target-instance]
   [else
    ;; return results via tail call
    (apply (really-eval-linklet linklet) target-instance import-instances)]))

;; ----------------------------------------

(define (linklet-import-variables linklet)
  (if (linklet? linklet)
      ;; Compiled to a prefab that includes metadata
      (linklet-importss linklet)
      ;; Previously "compiled" to source
      (extract-import-variables-from-expression linklet #:pairs? #f)))

(define (linklet-export-variables linklet)
  (if (linklet? linklet)
      ;; Compiled to a prefab that includes metadata
      (linklet-exports linklet)
      ;; Previously "compiled" to source
      (extract-export-variables-from-expression linklet #:pairs? #f)))

(define (s-expr-linklet-importss+localss linklet)
  (extract-import-variables-from-expression linklet #:pairs? #t))

(define (s-expr-linklet-exports+locals linklet)
  (extract-export-variables-from-expression linklet #:pairs? #t))

(define (s-expr-linklet-body linklet)
  (unmarshal (list-tail linklet 3)))

;; ----------------------------------------

(struct path-bytes (bstr) #:prefab)
(struct unreadable (str) #:prefab)
(struct void-value () #:prefab)
(struct srcloc-parts (source line column position span) #:prefab)

(define (marshal c)
  (datum-map c (lambda (tail? c)
                 (cond
                  [(path? c) (path-bytes (path->bytes c))]
                  [(and (symbol? c) (symbol-unreadable? c)) (unreadable (symbol->string c))]
                  [(void? c) (void-value)]
                  [(srcloc? c) (srcloc-parts (marshal (srcloc-source c))
                                             (marshal (srcloc-line c))
                                             (marshal (srcloc-column c))
                                             (marshal (srcloc-position c))
                                             (marshal (srcloc-span c)))]
                  [else c]))))

(define (unmarshal c)
  (datum-map c
             (lambda (tail? c)
               (cond
                [(path-bytes? c) (bytes->path (path-bytes-bstr c))]
                [(unreadable? c) (string->unreadable-symbol (unreadable-str c))]
                [(void-value? c) (void)]
                [(srcloc-parts? c)  (srcloc (marshal (srcloc-parts-source c))
                                            (marshal (srcloc-parts-line c))
                                            (marshal (srcloc-parts-column c))
                                            (marshal (srcloc-parts-position c))
                                            (marshal (srcloc-parts-span c)))]
                [else c]))))

;; Like `correlated->datum`, but preserves 'inferred-name information
;; by encoding it as a symbol in a `lambda` or `case-lambda` body.
;; Remove any existing symbol in the name position that might
;; otherwise be confused for the name. This conversion avoids parsing
;; expressions in general by relying on the fact that bindings are
;; renamed to avoid shadowing, `lambda`, `case-lambda`, or `quote`.
(define (correlated->datum/lambda-name c)
  (define (strip-potential-name-from-body body)
    (define-correlated-match m body #:try '(begin (quote _) body bodys ...))
    (cond
      [(and (m)
            (eq? 'begin (m 'begin))
            (eq? 'quote (m 'quote)))
       (strip-potential-name-from-body 
        (if (null? (m 'bodys))
            (m 'body)
            `(begin ,@(m 'bodys))))]
      [else body]))
  (let correlated->datum/lambda-name ([c c])
    (cond
      [(and (pair? c)
            (eq? (car c) 'lambda))
       (define-correlated-match m c '(lambda args body))
       `(lambda ,(correlated->datum (m 'args))
          ,(correlated->datum/lambda-name
            (strip-potential-name-from-body (m 'body))))]
      [(and (pair? c)
            (eq? (car c) 'case-lambda))
       (define-correlated-match m c '(case-lambda [argss bodys] ...))
       `(case-lambda
          ,@(for/list ([args (in-list (m 'argss))]
                       [body (in-list (m 'bodys))])
              `[,(correlated->datum args)
                ,(correlated->datum/lambda-name
                  (strip-potential-name-from-body body))]))]
      [(and (pair? c)
            (eq? (car c) 'quote))
       (correlated->datum c)]
      [(pair? c)
       (cons (correlated->datum/lambda-name (car c))
             (correlated->datum/lambda-name (cdr c)))]
      [(and (correlated? c)
            (let ([e (correlated-e c)])
              (and (pair? e)
                   (or (eq? 'lambda (car e))
                       (eq? 'case-lambda (car e)))))
            (correlated-property c 'inferred-name))
       => (lambda (name)
            (cond
              [(void? name)
               ;; Don't try to hide the name after all
               (correlated->datum/lambda-name (correlated-e c))]
              [else
               ;; Encode `name` as a symbol in the function body:
               (define lam (correlated->datum/lambda-name (correlated-e c)))
               (cond
                 [(eq? 'lambda (car lam))
                  (define-correlated-match m lam '(lambda args body))
                  `(lambda ,(m 'args) (begin (quote ,name) ,(m 'body)))]
                 [else
                  (define-correlated-match m lam '(case-lambda [argss bodys] ...))
                  (cond
                    [(null? (m 'argss))
                     ;; give up on naming an empty `case-lambda`
                     lam]
                    [else
                     `(case-lambda
                        [,(car (m 'argss)) (begin (quote ,name) ,(car (m 'bodys)))]
                        ,@(cddr lam))])])]))]
      [(correlated? c)
       (correlated->datum/lambda-name (correlated-e c))]
      [else
       (correlated->datum c)])))
