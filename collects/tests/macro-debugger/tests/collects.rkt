#lang scheme/base
(require rackunit
         rackunit/gui)
(require macro-debugger/model/debug
         scheme/path
         scheme/gui)
(provide big-libs-tests
         loadlib
         loadfile
         trace-modules)

;; loadlib : module-path symbol -> Deriv
(define (loadlib mod)
  (let ([resolved ((current-module-name-resolver) mod #f #f #f)])
    (loadfile (resolved-module-path-name resolved))))

;; loadfile : path symbol -> Deriv
(define (loadfile path)
  (define-values (base file dir?) (split-path path))
  (define expect-module
    (string->symbol (path->string (path-replace-suffix file #""))))
  (define-values (eh mnr)
    (make-handlers (current-eval)
                   (current-module-name-resolver)))
  #;(printf "Loading ~s\n" (path->string path))
  #;(printf "Expecting module named '~s'\n" expect-module)
  (parameterize ((current-load-relative-directory base)
                 (current-directory base)
                 (current-eval eh)
                 (current-module-name-resolver mnr))
    (let-values ([(e-expr deriv)
                  ((current-load) path expect-module)])
      (when (exn? e-expr)
        (raise e-expr))
      deriv)))

(define (make-handlers original-eval-handler original-module-name-resolver)
  (values
   (lambda (expr)
     (unless (syntax? expr)
       (raise-type-error 'eval-handler "syntax" expr))
     (trace/result expr))
   (lambda args
     (parameterize ((current-eval original-eval-handler)
                    (current-module-name-resolver original-module-name-resolver))
       (apply original-module-name-resolver args)))))

(define (test-libs name mods)
  (test-suite name
    (apply make-test-suite "Trace & Parse"
           (for/list ([m mods]) (test-lib/deriv m)))
    (apply make-test-suite "Reductions"
           (for/list ([m mods]) (test-lib/hide m hide-none-policy)))
    (apply make-test-suite "Standard hiding"
           (for/list ([m mods]) (test-lib/hide m standard-policy)))))

(define (test-lib/deriv m)
  (test-case (format "~s" m)
    (let ([deriv (loadlib m)])
      (check-pred deriv? deriv "Not a deriv")
      (check-pred ok-node? deriv "Expansion error"))))

(define (test-lib/hide m policy)
  (test-case (format "~s" m)
    (let ([deriv (loadlib m)])
      (check-steps deriv policy))))

(define (check-steps deriv policy)
  (define-values (steps binders uses stx exn)
    (parameterize ((macro-policy policy)) (reductions+ deriv)))
  (check-pred syntax? stx)
  (check-eq? exn #f)
  (check-true (list? steps) "Expected list for steps")
  (check-reduction-sequence steps))

(define (check-reduction-sequence steps)
  (cond [(null? steps) (void)]
        [(and (pair? steps) (step? (car steps)))
         (check-reduction-sequence (cdr steps))]
        [(and (pair? steps) (misstep? (car steps)))
         (check-eq? (cdr steps) '() "Stuff after misstep")]
        [else (fail "Bad reduction sequence")]))

(define (make-tracing-module-name-resolver omnr table)
  (case-lambda
    [(mod rel stx load?)
     (when load?
       (when (not rel)
         (hash-set! table mod #t))
       (when rel
         (let ([abs (rel+mod->mod rel mod)])
           (when abs (hash-set! table abs #t)))))
     (omnr mod rel stx load?)]
    [args
     (apply omnr args)]))

(define (rel+mod->mod rel mod)
  (define-values (base file dir?) (split-path (resolved-module-path-name rel)))
  (path->mod (simplify-path (build-path base mod))))

(define (path->mod path)
  (cond [(for/or ([c (current-library-collection-paths)]) (path->mod* path c))
         => (lambda (l)
              (string->symbol
               (path->string
                (path-replace-suffix (apply build-path l) #""))))]
        [else #f]))

(define (path->mod* path base)
  (let loop ([path (explode-path path)] [base (explode-path base)])
    (cond [(null? base) path]
          [(and (pair? path) (pair? base) (equal? (car path) (car base)))
           (loop (cdr path) (cdr base))]
          [else #f])))

(define (trace-modules mods)
  (define table (make-hash))
  (parameterize ((current-module-name-resolver
                  (make-tracing-module-name-resolver
                   (current-module-name-resolver)
                   table))
                 (current-namespace (make-gui-namespace)))
    (for ([mod mods])
         (dynamic-require mod #f))
    (let* ([loaded
            (hash-map table (lambda (k v) k))]
           [syms
            (for/list ([l loaded] #:when (symbol? l)) l)]
           [libs
            (for/list ([l loaded] #:when (and (pair? l) (eq? (car l) 'lib))) l)]
           [conv-libs
            (for/list ([l libs])
                      (string->symbol
                       (string-append
                        (apply string-append
                               (for/list ([d (cddr l)]) (string-append d "/")))
                        (path->string (path-replace-suffix (cadr l) #"")))))])
      (sort (append syms conv-libs)
            string<?
            #:key symbol->string
            #:cache-keys? #t))))

(define modules-from-framework (trace-modules '(framework)))
(define modules-from-typed-scheme
  #;(trace-modules '(typed-scheme))
  '(#|
    mzlib/contract
    mzlib/etc
    mzlib/file
    mzlib/kw
    mzlib/list
    mzlib/match
    mzlib/class
    mzlib/cm-accomplice
    mzlib/contract
    mzlib/etc
    mzlib/kw
    mzlib/list
    mzlib/pconvert
    mzlib/pconvert-prop
    mzlib/plt-match
    mzlib/pretty
    mzlib/private/increader
    mzlib/private/unit-compiletime
    mzlib/private/unit-keywords
    mzlib/private/unit-runtime
    mzlib/private/unit-syntax
    mzlib/shared
    mzlib/string
    mzlib/struct
    mzlib/trace
    mzlib/unit
    mzlib/unit-exptime
    mzscheme
    mzlib/plt-match
    scheme/base
    scheme/class
    scheme/contract
    scheme/include
    scheme/list
    scheme/match
    scheme/match/compiler
    scheme/match/define-forms
    scheme/match/gen-match
    scheme/match/legacy-match
    scheme/match/match
    scheme/match/match-expander
    scheme/match/parse
    scheme/match/parse-helper
    scheme/match/parse-legacy
    scheme/match/parse-quasi
    scheme/match/patterns
    scheme/match/reorder
    scheme/match/split-rows
    scheme/mzscheme
    scheme/nest
    scheme/private/class-internal
    scheme/contract/private/base
    scheme/contract/private/arrow
    scheme/contract/private/basic-opters
    scheme/contract/private/ds
    scheme/contract/private/ds-helpers
    scheme/contract/private/exists
    scheme/contract/private/guts
    scheme/contract/private/helpers
    scheme/contract/private/misc
    scheme/contract/private/opt
    scheme/contract/private/opt-guts
    scheme/private/define-struct
    scheme/private/define-struct
    scheme/private/for
    scheme/private/kw
    scheme/private/letstx-scheme
    scheme/private/list
    scheme/private/misc
    scheme/private/modbeg
    scheme/private/more-scheme
    scheme/private/namespace
    scheme/private/old-procs
    scheme/private/pre-base
    scheme/private/qqstx
    scheme/private/reqprov
    scheme/private/struct-info
    scheme/private/stx
    scheme/private/stxcase
    scheme/private/stxcase-scheme
    scheme/private/stxloc
    scheme/private/stxparamkey
    scheme/private/with-stx
    scheme/promise
    scheme/provide-transform
    scheme/require-syntax
    scheme/require-transform
    scheme/struct-info
    scheme/struct-info
    scheme/stxparam
    scheme/unit
    scheme/unit-exptime
    scheme/unit/lang
    srfi/1
    srfi/1/alist
    srfi/1/cons
    srfi/1/delete
    srfi/1/filter
    srfi/1/fold
    srfi/1/list
    srfi/1/lset
    srfi/1/misc
    srfi/1/predicate
    srfi/1/search
    srfi/1/selector
    srfi/1/util
    srfi/optional
    srfi/provider
    mzlib/struct
    syntax/boundmap
    syntax/boundmap
    syntax/context
    syntax/free-vars
    syntax/kerncase
    syntax/kerncase
    syntax/name
    syntax/path-spec
    syntax/private/boundmap
    syntax/struct
    syntax/struct
    syntax/stx
    syntax/stx
    mzlib/trace
    |#
    typed-scheme
    typed-scheme/minimal
    typed-scheme/private/base-env
    typed-scheme/private/base-types
    typed-scheme/private/check-subforms-unit
    typed-scheme/private/def-binding
    typed-scheme/private/effect-rep
    typed-scheme/private/extra-procs
    typed-scheme/private/free-variance
    typed-scheme/private/infer
    typed-scheme/private/infer-ops
    typed-scheme/private/init-envs
    typed-scheme/private/internal-forms
    typed-scheme/private/interning
    typed-scheme/private/lexical-env
    typed-scheme/private/mutated-vars
    typed-scheme/private/parse-type
    typed-scheme/private/planet-requires
    typed-scheme/private/prims
    typed-scheme/private/provide-handling
    typed-scheme/private/remove-intersect
    typed-scheme/private/rep-utils
    typed-scheme/private/require-contract
    typed-scheme/private/resolve-type
    typed-scheme/private/signatures
    typed-scheme/private/subtype
    typed-scheme/private/syntax-traversal
    typed-scheme/private/tables
    typed-scheme/private/tc-app-unit
    typed-scheme/private/tc-expr-unit
    typed-scheme/private/tc-if-unit
    typed-scheme/private/tc-lambda-unit
    typed-scheme/private/tc-let-unit
    typed-scheme/private/tc-structs
    typed-scheme/private/tc-toplevel
    typed-scheme/private/tc-utils
    typed-scheme/private/type-alias-env
    typed-scheme/private/type-annotation
    typed-scheme/private/type-comparison
    typed-scheme/private/type-contract
    typed-scheme/private/type-effect-convenience
    typed-scheme/private/type-effect-printer
    typed-scheme/private/type-env
    typed-scheme/private/type-environments
    typed-scheme/private/type-name-env
    typed-scheme/private/type-rep
    typed-scheme/private/type-utils
    typed-scheme/private/typechecker
    typed-scheme/private/unify
    typed-scheme/private/union
    typed-scheme/private/unit-utils
    typed-scheme/private/utils
    typed-scheme/typed-scheme))

(define big-libs-tests
  (test-libs "Collections" modules-from-typed-scheme))
