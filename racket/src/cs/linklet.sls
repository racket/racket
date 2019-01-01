(library (linklet)
  (export linklet?
          compile-linklet
          recompile-linklet
          eval-linklet
          instantiate-linklet

          read-on-demand-source

          linklet-import-variables
          linklet-export-variables
          
          instance?
          make-instance
          instance-name
          instance-data
          instance-variable-names
          instance-variable-value
          instance-set-variable-value!
          instance-unset-variable!
          instance-describe-variable!

          linklet-virtual-machine-bytes
          write-linklet-bundle-hash
          read-linklet-bundle-hash
          
          variable-reference?
          variable-reference->instance
          variable-reference-constant?
          variable-reference-from-unsafe?

          compile-enforce-module-constants
          compile-context-preservation-enabled
          compile-allow-set!-undefined
          current-compile-target-machine
          compile-target-machine?
          eval-jit-enabled
          load-on-demand-enabled

          primitive->compiled-position
          compiled-position->primitive
          primitive-in-category?

          omit-debugging?             ; not exported to racket
          platform-independent-zo-mode? ; not exported to racket
          linklet-performance-init!   ; not exported to racket
          linklet-performance-report! ; not exported to racket

          install-linklet-primitive-tables!  ; not exported to racket
          
          ;; schemify glue:
          make-internal-variable
          variable-set!
          variable-set!/check-undefined
          variable-ref
          variable-ref/no-check
          make-instance-variable-reference
          jitified-extract-closed
          jitified-extract
          schemify-table
          call-with-module-prompt)
  (import (chezpart)
          (only (chezscheme) printf)
          (rumble)
          (only (io)
                path?
                complete-path?
                path->string
                path->bytes
                bytes->path
                string->bytes/utf-8
                bytes->string/utf-8
                prop:custom-write
                write-bytes
                read-byte
                read-bytes
                open-output-bytes
                get-output-bytes
                file-position
                current-logger
                log-message
                sha1-bytes
                environment-variables-ref
                current-environment-variables
                find-system-path
                build-path
                format)
          (only (thread)
                current-process-milliseconds)
          (regexp)
          (schemify))

  (define linklet-compilation-mode
    (cond
     [(getenv "PLT_CS_JIT") 'jit]
     [(getenv "PLT_CS_MACH") 'mach]
     [else 'mach]))

  (define linklet-compilation-limit
    (and (eq? linklet-compilation-mode 'mach)
         (or (let ([s (getenv "PLT_CS_COMPILE_LIMIT")])
               (and s
                    (let ([n (string->number s)])
                      (and (real? n)
                           n))))
             10000)))

  (define no-future-jit-db? (getenv "PLT_NO_FUTURE_JIT_CACHE")) ; => don't calculate key for cache
  (define jit-db-path (let ([bstr (environment-variables-ref
                                   (|#%app| current-environment-variables)
                                   (string->utf8 "PLT_JIT_CACHE"))])
                        (cond
                         [(equal? bstr '#vu8()) #f] ; empty value disables the JIT cache
                         [(not bstr)
                          (build-path (find-system-path 'addon-dir)
                                      "cs-jit.sqlite")]
                         [else (bytes->path bstr)])))

  ;; For "main.sps" to select the default ".zo" directory name:
  (define platform-independent-zo-mode? (eq? linklet-compilation-mode 'jit))

  (define (primitive->compiled-position prim) #f)
  (define (compiled-position->primitive pos) #f)
  (define (primitive-in-category? sym cat) #f)

  (define root-logger (|#%app| current-logger))

  (define omit-debugging? (not (getenv "PLT_CS_DEBUG")))
  (define measure-performance? (getenv "PLT_LINKLET_TIMES"))

  (define compress-code? (cond
                          [(getenv "PLT_LINKLET_COMPRESS") #t]
                          [(getenv "PLT_LINKLET_NO_COMPRESS") #f]
                          [else
                           ;; Default selected at compile time, intended
                           ;; to be a `configure` option
                           (meta-cond
                            [(getenv "PLT_CS_MAKE_COMPRESSED") #t]
                            [else #f])]))

  (define gensym-on? (getenv "PLT_LINKLET_SHOW_GENSYM"))
  (define pre-lift-on? (getenv "PLT_LINKLET_SHOW_PRE_LIFT"))
  (define pre-jit-on? (getenv "PLT_LINKLET_SHOW_PRE_JIT"))
  (define lambda-on? (getenv "PLT_LINKLET_SHOW_LAMBDA"))
  (define post-lambda-on? (getenv "PLT_LINKLET_SHOW_POST_LAMBDA"))
  (define post-interp-on? (getenv "PLT_LINKLET_SHOW_POST_INTERP"))
  (define jit-demand-on? (getenv "PLT_LINKLET_SHOW_JIT_DEMAND"))
  (define known-on? (getenv "PLT_LINKLET_SHOW_KNOWN"))
  (define show-on? (or gensym-on?
                       pre-jit-on?
                       pre-lift-on?
                       post-lambda-on?
                       post-interp-on?
                       jit-demand-on?
                       known-on?
                       (getenv "PLT_LINKLET_SHOW")))
  (define show
    (case-lambda
     [(what v) (show show-on? what v)]
     [(on? what v)
      (when on?
        (printf ";; ~a ---------------------\n" what)
        (call-with-system-wind
         (lambda ()
           (parameterize ([print-gensym gensym-on?]
                          [print-extended-identifiers #t])
             (pretty-print (strip-jit-wrapper
                            (strip-nested-annotations
                             (correlated->annotation v))))))))
      v]))

  (include "linklet/version.ss")
  (include "linklet/write.ss")
  (include "linklet/read.ss")
  (include "linklet/annotation.ss")
  (include "linklet/performance.ss")
  (include "linklet/db.ss")

  ;; `compile`, `interpret`, etc. have `dynamic-wind`-based state
  ;; that need to be managed correctly when swapping Racket
  ;; engines/threads.
  (define (compile* e)
    (call-with-system-wind (lambda () (compile e))))
  (define (interpret* e)
    (call-with-system-wind (lambda () (interpret e))))
  (define (fasl-write* s o)
    (call-with-system-wind (lambda () (fasl-write s o))))
  (define (compile-to-port* s o)
    (call-with-system-wind (lambda () (compile-to-port s o))))

  (define (eval/foreign e mode)
    (performance-region
     mode
     (compile* e)))

  (define primitives (make-hasheq))
  (define (install-linklet-primitive-tables! . tables)
    (for-each
     (lambda (table)
       (hash-for-each table (lambda (k v) (hash-set! primitives k v))))
     tables))
  
  (define (outer-eval s format)
    (if (eq? format 'interpret)
        (interpret-linklet s primitives variable-ref variable-ref/no-check variable-set!
                           make-arity-wrapper-procedure)
        (compile* s)))

  (define (compile*-to-bytevector s)
    (let-values ([(o get) (open-bytevector-output-port)])
      (compile-to-port* (list `(lambda () ,s)) o)
      (get)))

  (define (compile-to-bytevector s format)
    (let ([bv (cond
               [(eq? format 'interpret)
                (let-values ([(o get) (open-bytevector-output-port)])
                  (fasl-write* s o)
                  (get))]
               [else (compile*-to-bytevector s)])])
      (if compress-code?
          (bytevector-compress bv)
          bv)))

  (define (eval-from-bytevector c-bv format)
    (let ([bv (if (bytevector-uncompressed-fasl? c-bv)
                  c-bv
                  (begin
                    (add-performance-memory! 'uncompress (bytevector-length c-bv))
                    (performance-region
                     'uncompress
                     (bytevector-uncompress c-bv))))])
      (add-performance-memory! 'faslin-code (bytevector-length bv))
      (cond
       [(eq? format 'interpret)
        (let ([r (performance-region
                  'faslin-code
                  (fasl-read (open-bytevector-input-port bv)))])
          (performance-region
           'outer
           (outer-eval r format)))]
       [else
        (performance-region
         'faslin-code
         (code-from-bytevector bv))])))

  (define (code-from-bytevector bv)
    (let ([i (open-bytevector-input-port bv)])
      (let ([r (load-compiled-from-port i)])
        (performance-region
         'outer
         (r)))))

  (define (bytevector-uncompressed-fasl? bv)
    ;; There's not actually a way to distinguish a fasl header from a
    ;; compression header, but the fasl header as a compression header
    ;; would mean a > 1GB uncompressed bytevector, so we can safely
    ;; assume that it's a fasl stream in that case.
    (and (> (bytevector-length bv) 8)
         (fx= 0 (bytevector-u8-ref bv 0))
         (fx= 0 (bytevector-u8-ref bv 1))
         (fx= 0 (bytevector-u8-ref bv 2))
         (fx= 0 (bytevector-u8-ref bv 3))
         (fx= (char->integer #\c) (bytevector-u8-ref bv 4))
         (fx= (char->integer #\h) (bytevector-u8-ref bv 5))
         (fx= (char->integer #\e) (bytevector-u8-ref bv 6))
         (fx= (char->integer #\z) (bytevector-u8-ref bv 7))))

  (define-values (lookup-code insert-code delete-code)
    (let ([get-procs!-maker
           (lambda (retry)
             (lambda args
               (let-values ([(lookup insert delete) (get-code-database-procedures)])
                 (set! lookup-code lookup)
                 (set! insert-code insert)
                 (set! delete-code delete)
                 (apply retry args))))])
      (values (get-procs!-maker (lambda (hash) (lookup-code hash)))
              (get-procs!-maker (lambda (hash code) (insert-code hash code)))
              (get-procs!-maker (lambda (hash) (delete-code hash))))))

  (define (add-code-hash a)
    (cond
     [no-future-jit-db? a]
     [else
      ;; Combine an annotation with a hash code in a vector
      (let-values ([(o get) (open-bytevector-output-port)])
        (fasl-write (cons (version) a) o)
        (vector (sha1-bytes (get)) a))]))

  (define-record-type wrapped-code
    (fields (mutable content) ; bytevector for 'lambda mode; annotation or (vector hash annotation) for 'jit mode
            arity-mask
            name)
    (nongenerative #{wrapped-code p6o2m72rgmi36pm8vy559b-0}))

  (define (force-wrapped-code wc)
    (let ([f (wrapped-code-content wc)])
      (if (procedure? f)
          f
          (performance-region
           'on-demand
           (let ([f (if (and (vector? f)
                             (or (not jit-db-path)
                                 (wrong-jit-db-thread?)))
                        (vector-ref f 1)
                        f)])
             (cond
              [(bytevector? f)
               (let* ([f (code-from-bytevector f)])
                 (wrapped-code-content-set! wc f)
                 f)]
              [(vector? f)
               (when jit-demand-on?
                 (show "JIT demand" (strip-nested-annotations (vector-ref f 1))))
               (let* ([hash (vector-ref f 0)]
                      [code (lookup-code hash)])
                 (cond
                  [code
                   (let* ([f (eval-from-bytevector code 'compile)])
                     (wrapped-code-content-set! wc f)
                     f)]
                  [else
                   (let ([code (compile-to-bytevector (vector-ref f 1) 'compile)])
                     (insert-code hash code)
                     (let* ([f (eval-from-bytevector code 'compile)])
                       (wrapped-code-content-set! wc f)
                       f))]))]
              [else
               (let ([f (compile* f)])
                 (when jit-demand-on?
                   (show "JIT demand" (strip-nested-annotations (wrapped-code-content wc))))
                 (wrapped-code-content-set! wc f)
                 f)]))))))

  (define (jitified-extract-closed wc)
    (let ([f (wrapped-code-content wc)])
      (if (#2%procedure? f)
          ;; previously forced, so no need for a wrapper
          f
          ;; make a wrapper that has the right arity and name
          ;; and that compiles/extracts when called:
          (make-jit-procedure (lambda () (force-wrapped-code wc))
                              (wrapped-code-arity-mask wc)
                              (wrapped-code-name wc)))))

  (define (jitified-extract wc)
    (let ([f (wrapped-code-content wc)])
      (if (#2%procedure? f)
          ;; previously forced, so no need for a wrapper
          f
          ;; make a wrapper that has the right arity and name
          ;; and that compiles/extracts when called:
          (lambda free-vars
            (make-jit-procedure (lambda ()
                                  (apply (force-wrapped-code wc)
                                         free-vars))
                                (wrapped-code-arity-mask wc)
                                (wrapped-code-name wc))))))

  (define (strip-jit-wrapper p)
    (cond
     [(wrapped-code? p)
      (vector (strip-jit-wrapper (strip-nested-annotations (wrapped-code-content p)))
              (wrapped-code-arity-mask p)
              (wrapped-code-name p))]
     [(pair? p)
      (cons (strip-jit-wrapper (car p)) (strip-jit-wrapper (cdr p)))]
     [else p]))
  
  ;; A linklet is implemented as a procedure that takes an argument
  ;; for each import plus an `variable` for each export, and calling
  ;; the procedure runs the linklet body.

  ;; A source linklet has a list of list of imports; those are all
  ;; flattened into a sequence of arguments for the linklet procedure,
  ;; followed by the arguments to receive the export `variable`s. Each
  ;; import is either a `variable` or the variable's value as
  ;; indicated by the "ABI" (which is based on information about which
  ;; exports of an imported linklet are constants).

  ;; A linklet also has a table of information about its 

  (define-record-type linklet
    (fields (mutable code) ; the procedure
            format         ; 'compile or 'interpret (where the latter may have compiled internal parts)
            (mutable preparation) ; 'faslable, 'faslable-strict, 'callable, or 'lazy
            importss-abi   ; ABI for each import, in parallel to `importss`
            exports-info   ; hash(sym -> known) for info about each export; see "known.rkt"
            name           ; name of the linklet (for debugging purposes)
            importss       ; list of list of import symbols
            exports)       ; list of export symbols
    (nongenerative #{linklet Zuquy0g9bh5vmeespyap4g-0}))

  (define (set-linklet-code linklet code preparation)
    (make-linklet code
                  (linklet-format linklet)
                  preparation
                  (linklet-importss-abi linklet)
                  (linklet-exports-info linklet)
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define compile-linklet
    (case-lambda
     [(c) (compile-linklet c #f #f (lambda (key) (values #f #f)) '(serializable))]
     [(c name) (compile-linklet c name #f (lambda (key) (values #f #f)) '(serializable))]
     [(c name import-keys) (compile-linklet c name import-keys (lambda (key) (values #f #f)) '(serializable))]
     [(c name import-keys get-import) (compile-linklet c name import-keys get-import '(serializable))]
     [(c name import-keys get-import options)
      (define serializable? (#%memq 'serializable options))
      (define use-prompt? (#%memq 'use-prompt options))
      (performance-region
       'schemify
       (define jitify-mode?
         (or (eq? linklet-compilation-mode 'jit)
             (and (linklet-bigger-than? c linklet-compilation-limit serializable?)
                  (log-message root-logger 'info 'linklet "compiling only interior functions for large linklet" #f)
                  #t)))
       (define format (if jitify-mode? 'interpret 'compile))
       ;; Convert the linklet S-expression to a `lambda` S-expression:
       (define-values (impl-lam importss exports new-import-keys importss-abi exports-info)
         (schemify-linklet (show "linklet" c)
                           serializable?
                           (not (#%memq 'uninterned-literal options))
                           jitify-mode?
                           (|#%app| compile-allow-set!-undefined)
                           #f ;; safe mode
                           (not use-prompt?)
                           prim-knowns
                           ;; Callback to get a specific linklet for a
                           ;; given import:
                           (lambda (key)
                             (lookup-linklet-or-instance get-import key))
                           import-keys))
       (define impl-lam/lifts
         (lift-in-schemified-linklet (show pre-lift-on? "pre-lift" impl-lam)))
       (define impl-lam/jitified
         (cond
           [(not jitify-mode?) impl-lam/lifts]
           [else
            (performance-region
             'jitify
             (jitify-schemified-linklet (case linklet-compilation-mode
                                          [(jit) (show pre-jit-on? "pre-jitified" impl-lam/lifts)]
                                          [else (show "schemified" impl-lam/lifts)])
                                        ;; don't need extract for non-serializable 'lambda mode
                                        (or serializable? (eq? linklet-compilation-mode 'jit))
                                        ;; need lift only for serializable JIT mode
                                        (and serializable? (eq? linklet-compilation-mode 'jit))
                                        ;; compilation threshold for ahead-of-time mode:
                                        (and (eq? linklet-compilation-mode 'mach)
                                             linklet-compilation-limit)
                                        ;; correlation -> lambda
                                        (case linklet-compilation-mode
                                          [(jit)
                                           ;; Preserve annotated `lambda` source for on-demand compilation:
                                           (lambda (expr arity-mask name)
                                             (let ([a (correlated->annotation (xify expr))])
                                               (make-wrapped-code (if serializable?
                                                                      (add-code-hash a)
                                                                      a)
                                                                  arity-mask
                                                                  name)))]
                                          [else
                                           ;; Compile an individual `lambda`:
                                           (lambda (expr arity-mask name)
                                             (performance-region
                                              'compile-nested
                                              (let ([code ((if serializable? compile*-to-bytevector compile*)
                                                           (show lambda-on? "lambda" (correlated->annotation expr)))])
                                                (if serializable?
                                                    (make-wrapped-code code arity-mask name)
                                                    code))))])))]))
       (define impl-lam/interpable
         (let ([impl-lam (case (and jitify-mode?
                                    linklet-compilation-mode)
                           [(mach) (show post-lambda-on? "post-lambda" impl-lam/jitified)]
                           [else (show "schemified" impl-lam/jitified)])])
           (if jitify-mode?
               (interpretable-jitified-linklet impl-lam correlated->datum)
               (correlated->annotation impl-lam))))
       (when known-on?
         (show "known" (hash-map exports-info (lambda (k v) (list k v)))))
       (performance-region
        'compile-linklet
        ;; Create the linklet:
        (let ([lk (make-linklet (call-with-system-wind
                                 (lambda ()
                                   ((if serializable? compile-to-bytevector outer-eval)
                                    (show (and jitify-mode? post-interp-on?) "post-interp" impl-lam/interpable)
                                    format)))
                                format
                                (if serializable? 'faslable 'callable)
                                importss-abi
                                exports-info
                                name
                                importss
                                exports)])
          (show "compiled" 'done)
          ;; In general, `compile-linklet` is allowed to extend the set
          ;; of linklet imports if `import-keys` is provided (e.g., for
          ;; cross-linklet optimization where inlining needs a new
          ;; direct import)
          (if import-keys
              (values lk new-import-keys)
              lk))))]))

  (define (lookup-linklet-or-instance get-import key)
    ;; Use the provided callback to get an linklet for the
    ;; import at `index`
    (cond
     [key
      (let-values ([(lnk/inst more-import-keys) (get-import key)])
        (cond
         [(linklet? lnk/inst)
          (values (linklet-exports-info lnk/inst)
                  ;; No conversion needed:
                  #f
                  more-import-keys)]
         [(instance? lnk/inst)
          (values (instance-hash lnk/inst)
                  variable->known
                  more-import-keys)]
         [else (values #f #f #f)]))]
     [else (values #f #f #f)]))

  (define (recompile-linklet lnk . args) lnk)

  ;; Intended to speed up reuse of a linklet in exchange for not being
  ;; able to serialize anymore
  (define (eval-linklet linklet)
    (case (linklet-preparation linklet)
      [(faslable)
       (set-linklet-code linklet (linklet-code linklet) 'lazy)]
      [(faslable-strict)
       (set-linklet-code linklet (eval-from-bytevector (linklet-code linklet) (linklet-format linklet)) 'callable)]
      [else
       linklet]))
     
  (define instantiate-linklet
    (case-lambda
     [(linklet import-instances)
      (instantiate-linklet linklet import-instances #f #f)]
     [(linklet import-instances target-instance)
      (instantiate-linklet linklet import-instances target-instance #f)]
     [(linklet import-instances target-instance use-prompt?)
      (cond
       [target-instance
        ;; Instantiate into the given instance and return the
        ;; result of the linklet body:
        (call/cc
         (lambda (k)
           (register-linklet-instantiate-continuation! k (instance-name target-instance))
           (when (eq? 'lazy (linklet-preparation linklet))
             ;; Trigger lazy conversion of code from bytevector
             (let ([code (eval-from-bytevector (linklet-code linklet) (linklet-format linklet))])
               (with-interrupts-disabled
                (when (eq? 'lazy (linklet-preparation linklet))
                  (linklet-code-set! linklet code)
                  (linklet-preparation-set! linklet 'callable)))))
           ;; Call the linklet:
           (performance-region
            'instantiate
            (apply
             (if (eq? 'callable (linklet-preparation linklet))
                 (linklet-code linklet)
                 (eval-from-bytevector (linklet-code linklet) (linklet-format linklet)))
             (make-variable-reference target-instance #f)
             (append (apply append
                            (map (make-extract-variables target-instance)
                                 import-instances
                                 (linklet-importss linklet)
                                 (linklet-importss-abi linklet)))
                     (create-variables target-instance
                                       (linklet-exports linklet)))))))]
       [else
        ;; Make a fresh instance, recur, and return the instance
        (let ([i (make-instance (linklet-name linklet))])
          (instantiate-linklet linklet import-instances i use-prompt?)
          i)])]))
              
  (define (linklet-import-variables linklet)
    (linklet-importss linklet))

  (define (linklet-export-variables linklet)
    (linklet-exports linklet))

  ;; ----------------------------------------

  ;; A potentially mutable import or definition is accessed through
  ;; the indirection of a `variable`; accessing a variable may include
  ;; a check for undefined, since going through a `variable`
  ;; sacrifices the undefined check of the host Scheme
    
  (define-record variable (val
                           name
                           constance  ; #f (mutable), 'constant, or 'consistent (always the same shape)
                           inst-box)) ; weak pair with instance in `car`

  ;; Can't use `unsafe-undefined`, because the expander expects to be
  ;; able to store `unsafe-undefined` in variables
  (define variable-undefined (gensym 'undefined))

  (define (make-internal-variable name)
    (make-variable variable-undefined name #f (cons #!bwp #f)))

  (define (do-variable-set! var val constance as-define?)
    (cond
     [(variable-constance var)
      (cond
       [as-define?
        (raise
         (|#%app|
          exn:fail:contract:variable
          (string-append "define-values: assignment disallowed;\n"
                         " cannot re-define a constant\n"
                         "  constant: " (symbol->string (variable-name var)) "\n"
                         "  in module:" (variable-module-name var))
          (current-continuation-marks)
          (variable-name var)))]
       [else
        (raise
         (|#%app|
          exn:fail:contract:variable
          (string-append (symbol->string (variable-name var))
                         ": cannot modify constant")
          (current-continuation-marks)
          (variable-name var)))])]
     [else
      (set-variable-val! var val)
      (when constance
        (set-variable-constance! var constance))]))

  (define (variable-set! var val constance)
    (do-variable-set! var val constance #f))

  (define (variable-set!/check-undefined var val constance)
    (when (eq? (variable-val var) variable-undefined)
      (raise-undefined var #t))
    (variable-set! var val constance))

  (define (variable-ref var)
    (let ([v (variable-val var)])
      (if (eq? v variable-undefined)
          (raise-undefined var #f)
          v)))

  (define (variable-ref/no-check var)
    (variable-val var))

  ;; Find variables or values needed from an instance for a linklet's
  ;; imports
  (define (make-extract-variables target-inst)
    (lambda (inst syms imports-abi)
      (let ([ht (instance-hash inst)])
        (map (lambda (sym import-abi)
               (let ([var (or (hash-ref ht sym #f)
                              (raise-linking-failure "is not exported" target-inst inst sym))])
                 (when (eq? (variable-val var) variable-undefined)
                   (raise-linking-failure "is uninitialized" target-inst inst sym))
                 (if import-abi
                     (variable-val var)
                     var)))
             syms
             imports-abi))))

  (define (raise-linking-failure why target-inst inst sym)
    (raise-arguments-error 'instantiate-linklet
                           (string-append "mismatch;\n"
                                          " reference to a variable that " why ";\n"
                                          " possibly, bytecode file needs re-compile because dependencies changed")
                           "name" (unquoted-printing-string (symbol->string sym))
                           "exporting instance" (unquoted-printing-string (format "~a" (instance-name inst)))
                           "importing instance" (unquoted-printing-string (format "~a" (instance-name target-inst)))))

  (define (identify-module var)
    (let ([i (car (variable-inst-box var))])
      (cond
       [(eq? i #!bwp)
        ""]
       [(instance-name i)
        => (lambda (name)
             (#%format "\n  module: ~a" name))]
       [else ""])))

  (define (raise-undefined var set?)
    (raise
     (|#%app|
      exn:fail:contract:variable
      (cond
       [set?
        (string-append "set!: assignment disallowed;\n"
                       " cannot set variable before its definition\n"
                       "  variable: " (symbol->string (variable-name var))
                       (identify-module var))]
       [else
        (string-append (symbol->string (variable-name var))
                       ": undefined;\n cannot reference undefined identifier"
                       (identify-module var))])
      (current-continuation-marks)
      (variable-name var))))

  ;; Create the variables needed for a linklet's exports
  (define (create-variables inst syms)
    (let ([ht (instance-hash inst)]
          [inst-box (weak-cons inst #f)])
      (map (lambda (sym)
             (or (hash-ref ht sym #f)
                 (let ([var (make-variable variable-undefined sym #f inst-box)])
                   (hash-set! ht sym var)
                   var)))
           syms)))

  (define (variable->known var)
    (let ([desc (cdr (variable-inst-box var))])
      (cond
       [(and (pair? desc) (or (#%memq (car desc) '(procedure
                                                   procedure/succeeds
                                                   procedure/pure)))
             (pair? (cdr desc)) (exact-integer? (cadr desc)))
        (case (car desc)
          [(procedure/pure) (known-procedure/pure (cadr desc))]
          [(procedure/succeeds) (known-procedure/succeeds (cadr desc))]
          [else (known-procedure (cadr desc))])]
       [else
        (let ([constance (variable-constance var)])
          (cond
           [(not constance) #f]
           [(and (eq? constance 'consistent)
                 (#%procedure? (variable-val var)))
            (known-procedure (#%procedure-arity-mask (variable-val var)))]
           [else a-known-constant]))])))

  (define (check-variable-set var sym)
    (when (eq? (variable-val var) variable-undefined)
      (raise
       (|#%app|
        exn:fail:contract:variable
        (string-append "define-values: skipped variable definition;\n"
                       " cannot continue without defining variable\n"
                       "  variable: " (symbol->string sym) "\n"
                       "  in module: " (variable-module-name var))
        (current-continuation-marks)
        (variable-name var)))))
  
  (define (variable-describe! var desc)
    (set-variable-inst-box! var (weak-cons (car (variable-inst-box var))
                                           desc)))

  (define (variable-module-name var)
    (let ([i (car (variable-inst-box var))])
      (if (eq? i #!bwp)
          "[unknown]"
          (format "~a" (instance-name i)))))

  ;; ----------------------------------------

  ;; An instance represents the instantiation of a linklet
  (define-record-type (instance new-instance instance?)
    (fields name
            data
            hash)) ; symbol -> variable

  (define-record-type data-with-describes
    (fields data
            describes))

  (define make-instance
    (case-lambda
     [(name) (make-instance name #f)]
     [(name data) (make-instance name data #f)]
     [(name data constance . content)
      (let* ([ht (make-hasheq)]
             [inst (new-instance name data ht)]
             [inst-box (weak-cons inst #f)])
        (check-constance 'make-instance constance)
        (let loop ([content content])
          (cond
           [(null? content) (void)]
           [else
            (hash-set! ht (car content) (make-variable (cadr content) (car content) constance inst-box))
            (loop (cddr content))]))
        inst)]))

  (define (instance-variable-names i)
    (hash-map (instance-hash i) (lambda (k v) k)))

  (define instance-variable-value
    (case-lambda
     [(i sym fail-k)
      (let* ([var (hash-ref (instance-hash i) sym variable-undefined)]
             [v (if (eq? var variable-undefined)
                    variable-undefined
                    (variable-val var))])
        (if (eq? v variable-undefined)
            (fail-k)
            v))]
     [(i sym)
      (instance-variable-value i
                               sym
                               (lambda ()
                                 (raise-argument-error
                                  'instance-variable-value
                                  "instance variable not found"
                                  "name" sym)))]))

  (define instance-set-variable-value!
    (case-lambda
     [(i k v) (instance-set-variable-value! i k v #f)]
     [(i k v mode)
      (unless (instance? i)
        (raise-argument-error 'instance-set-variable-value! "instance?" i))
      (unless (symbol? k)
        (raise-argument-error 'instance-set-variable-value! "symbol?" i))
      (check-constance 'instance-set-variable-value! mode)
      (let ([var (or (hash-ref (instance-hash i) k #f)
                     (let ([var (make-variable variable-undefined k #f (weak-cons i #f))])
                       (hash-set! (instance-hash i) k var)
                       var))])
        (variable-set! var v mode))]))

  (define (instance-unset-variable! i k)
    (unless (instance? i)
      (raise-argument-error 'instance-unset-variable! "instance?" i))
    (unless (symbol? k)
      (raise-argument-error 'instance-unset-variable! "symbol?" i))
    (let ([var (hash-ref (instance-hash i) k #f)])
      (when var
        (set-variable-val! var variable-undefined))))

  (define (instance-describe-variable! i k desc)
    (unless (instance? i)
      (raise-argument-error 'instance-describe-variable! "instance?" i))
    (unless (symbol? k)
      (raise-argument-error 'instance-describe-variable! "symbol?" k))
    (let ([var (hash-ref (instance-hash i) k #f)])
      (when var
        (variable-describe! var desc))))

  (define (check-constance who mode)
    (unless (or (not mode) (eq? mode 'constant) (eq? mode 'consistent))
      (raise-argument-error who "(or/c #f 'constant 'consistant)" mode)))

  ;; --------------------------------------------------

  (define-record variable-reference (instance      ; the use-site instance
                                     var-or-info)) ; the referenced variable, 'constant, 'mutable, #f, or 'primitive
              
  (define variable-reference->instance
    (case-lambda
     [(vr ref?)
      (if ref?
          (variable-reference-instance vr)
          (variable-reference->instance vr))]
     [(vr)
      (let ([v (variable-reference-var-or-info vr)])
        (cond
         [(not v) ;; anonymous
          #f]
         [(variable? v)
          (let ([i (car (variable-inst-box v))])
            (if (eq? i #!bwp)
                (variable-reference->instance vr #t)
                i))]
         [(eq? v 'primitive)
          ;; FIXME: We don't have the right primitive instance name
          ;; ... but '#%kernel is usually right.
          '|#%kernel|]
         [else
          ;; Local variable, so same as use-site
          (variable-reference->instance vr #t)]))]))

  (define (variable-reference-constant? vr)
    (let ([v (variable-reference-var-or-info vr)])
      (cond
       [(variable? v)
        (and (variable-constance v) #t)]
       [(eq? v 'mutable) #f]
       [else (and v #t)])))

  (define (variable-reference-from-unsafe? vr)
    #f)

  (define (make-instance-variable-reference vr v)
    (make-variable-reference (variable-reference-instance vr) v))

  ;; --------------------------------------------------

  (define module-prompt-handler
    (lambda (arg)
      (abort-current-continuation
       (default-continuation-prompt-tag)
       arg)))

  (define call-with-module-prompt
    (case-lambda
     [(proc)
      ;; No bindings to set or check, so just call `proc` in a prompt
      (call-with-continuation-prompt
       proc
       (default-continuation-prompt-tag)
       module-prompt-handler)]
     [(proc syms modes var)
      ;; Common case: one binding to set/check
      (call-with-continuation-prompt
       (lambda ()
         (do-variable-set! var (proc) (car modes) #t))
       (default-continuation-prompt-tag)
       module-prompt-handler)
      (check-variable-set var (car syms))]
     [(proc syms modes . vars)
      ;; General case: many bindings to set/check
      (call-with-continuation-prompt
       (lambda ()
         (call-with-values proc
           (lambda vals
             (unless (= (length syms) (length vals))
               (raise-binding-result-arity-error syms vals))
             (let loop ([vars vars] [vals vals] [modes modes])
               (unless (null? vars)
                 (do-variable-set! (car vars) (car vals) (car modes) #t)
                 (loop (cdr vars) (cdr vals) (cdr modes)))))))
       (default-continuation-prompt-tag)
       module-prompt-handler)
      (let loop ([vars vars] [syms syms])
        (unless (null? vars)
          (check-variable-set (car vars) (car syms))
          (loop (cdr vars) (cdr syms))))]))

  ;; --------------------------------------------------
  
  (define compile-enforce-module-constants
    (make-parameter #t (lambda (v) (and v #t))))

  (define compile-context-preservation-enabled
    (make-parameter #f (lambda (v) (and v #t))))

  (define compile-allow-set!-undefined
    (make-parameter #f (lambda (v) (and v #t))))

  (define current-compile-target-machine
    (make-parameter (machine-type) (lambda (v)
                                     (unless (or (not v)
                                                 (and (symbol? v)
                                                      (compile-target-machine? v)))
                                       (raise-argument-error 'current-compile-target-machine
                                                             "(or/c #f (and/c symbol? compile-target-machine?))"
                                                             v))
                                     v)))

  (define (compile-target-machine? v)
    (unless (symbol? v)
      (raise-argument-error 'compile-target-machine? "symbol?" v))
    (eq? v (machine-type)))

  (define eval-jit-enabled
    (make-parameter #t (lambda (v) (and v #t))))
  
  (define load-on-demand-enabled
    (make-parameter #t (lambda (v) (and v #t))))

  ;; --------------------------------------------------

  (define-syntax primitive-table
    (syntax-rules ()
      [(_ id ...)
       (let ([ht (make-hasheq)])
         (hash-set! ht 'id id) ...
         ht)]))

  (define schemify-table
    (primitive-table
     variable-set!
     variable-set!/check-undefined
     variable-ref
     variable-ref/no-check
     make-instance-variable-reference
     unbox/check-undefined
     set-box!/check-undefined
     jitified-extract
     jitified-extract-closed))

  ;; --------------------------------------------------

  (when omit-debugging?
    (generate-inspector-information (not omit-debugging?))
    (generate-procedure-source-information #t))

  (when measure-performance?
    (#%$enable-pass-timing #t)
    (#%$clear-pass-stats))

  (set-foreign-eval! eval/foreign)

  (expand-omit-library-invocations #t))
