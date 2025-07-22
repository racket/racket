(library (linklet)
  (export linklet?
          compile-linklet
          expand/optimize-linklet            ; for optimization test suite
          recompile-linklet
          eval-linklet
          instantiate-linklet

          read-on-demand-source

          linklet-import-variables
          linklet-export-variables
          linklet-fasled-code+arguments      ; for tools like `raco decompile`
          linklet-interpret-jitified?        ; for `raco decompile`
          linklet-interpret-jitified-extract ; for `raco decompile`
          linklet-add-target-machine-info    ; helps cross-compilation
          linklet-summarize-target-machine-info ; helps cross-compilation
          
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
          linklet-cross-machine-type
          write-linklet-bundle-hash
          read-linklet-bundle-hash
          
          variable-reference?
          variable-reference->instance
          variable-reference-constant?
          variable-reference-from-unsafe?

          add-cross-compiler!        ; not exported to racket

          compile-enforce-module-constants
          compile-context-preservation-enabled
          compile-allow-set!-undefined
          current-compile-target-machine
          current-compile-realm
          compile-target-machine?
          eval-jit-enabled
          load-on-demand-enabled

          primitive->compiled-position
          compiled-position->primitive
          primitive-in-category?
          primitive-lookup

          omit-debugging?             ; not exported to racket
          platform-independent-zo-mode? ; not exported to racket
          linklet-performance-init!   ; not exported to racket
          linklet-performance-report! ; not exported to racket

          install-linklet-primitive-tables!  ; not exported to racket
          
          ;; schemify glue:
          make-internal-variable
          variable-set!
          variable-set!/define
          variable-set!/check-undefined
          variable-ref
          variable-ref/no-check
          set-consistent-variables!/define
          make-instance-variable-reference
          jitified-extract-closed
          jitified-extract
          schemify-table
          call-with-module-prompt)
  (import (chezpart)
          (rename (rumble)
                  [raise-argument-error raise-argument-error/primitive]
                  [raise-argument-error/user raise-argument-error]
                  [raise-arguments-error raise-arguments-error/primitive]
                  [raise-arguments-error/user raise-arguments-error]
                  [raise-range-error raise-range-error/primitive]
                  [raise-range-error/user raise-range-error])
          (only (io)
                path?
                complete-path?
                split-path
                path->string
                path-element->string
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
                format
                error-print-source-location
                ;; Used by cross-compiler:
                get-original-error-port
                subprocess
                write-string
                write-bytes
                flush-output
                read-bytes
                path->complete-path
                file-exists?)
          (only (thread)
                current-process-milliseconds
                ;; Used by cross-compiler:
                unsafe-make-custodian-at-root
                current-custodian
                custodian-shutdown-all
                thread
                make-channel
                channel-put
                channel-get
                make-will-executor
                will-register
                will-try-execute)
          (regexp)
          (schemify))

  (define linklet-compilation-mode
    (cond
     [(getenv "PLT_CS_JIT") 'jit]
     [(getenv "PLT_CS_MACH") 'mach]
     [(getenv "PLT_CS_INTERP") 'interp]
     [else 'mach]))

  (define linklet-compilation-limit
    (and (eq? linklet-compilation-mode 'mach)
         (or (let ([s (getenv "PLT_CS_COMPILE_LIMIT")])
               (and s
                    (let ([n (string->number s)])
                      (and (real? n)
                           n))))
             10000)))

  ;; For "main.sps" to select the default ".zo" directory name:
  (define platform-independent-zo-mode? (not (eq? linklet-compilation-mode 'mach)))

  (define (primitive->compiled-position prim) #f)
  (define (compiled-position->primitive pos) #f)
  (define (primitive-in-category? sym cat) #f)

  (define (primitive-lookup sym)
    (unless (symbol? sym)
      (raise-argument-error 'primitive-lookup "symbol?" sym))
    (call-with-system-wind
     (lambda ()
       (guard
        (c [else #f])
        (eval sym)))))

  (define root-logger (|#%app| current-logger))

  (define omit-debugging? (not (getenv "PLT_CS_DEBUG")))
  (define measure-performance? (getenv "PLT_LINKLET_TIMES"))

  ;; The difference between this and `PLT_CS_INTERP` is that
  ;; this one keeps using existing compiled code in a machine-specific
  ;; "compiled" directory:
  (define default-compile-quick? (getenv "PLT_LINKLET_COMPILE_QUICK"))

  (define compress-code? (cond
                          [(getenv "PLT_LINKLET_COMPRESS") #t]
                          [(getenv "PLT_LINKLET_NO_COMPRESS") #f]
                          [else
                           ;; Default selected at compile time, intended
                           ;; to be a `configure` option
                           (meta-cond
                            [(getenv "PLT_CS_MAKE_COMPRESSED") #t]
                            [else #f])]))

  ;; Note: compressing data also compresses serialized code, which is
  ;; a redundant layer of compression if `compress-code?`
  (define compress-data? (cond
                           [(getenv "PLT_LINKLET_COMPRESS_DATA") #t]
                           [(getenv "PLT_LINKLET_NO_COMPRESS_DATA") #f]
                           [else
                            ;; Default selected at compile time, as above
                            (meta-cond
                             [(getenv "PLT_CS_MAKE_COMPRESSED_DATA") #t]
                             [else #f])]))

  (define gensym-mode (cond [(getenv "PLT_LINKLET_SHOW_GENSYM") #t]
			    [else 'pretty/suffix]))
  (define pre-jit-on? (getenv "PLT_LINKLET_SHOW_PRE_JIT"))
  (define lambda-on? (getenv "PLT_LINKLET_SHOW_LAMBDA"))
  (define post-lambda-on? (getenv "PLT_LINKLET_SHOW_POST_LAMBDA"))
  (define post-interp-on? (getenv "PLT_LINKLET_SHOW_POST_INTERP"))
  (define jit-demand-on? (getenv "PLT_LINKLET_SHOW_JIT_DEMAND"))
  (define literals-on? (getenv "PLT_LINKLET_SHOW_LITERALS"))
  (define known-on? (getenv "PLT_LINKLET_SHOW_KNOWN"))
  (define passes-on (cond [(getenv "PLT_LINKLET_SHOW_PASSES")
                           =>
                           (lambda (e)
                             (let lp ([p (open-string-input-port e)])
                               (define pass (read p))
                               (cond
                                 [(eq? pass 'all) '(#t)]
                                 [(symbol? pass) (cons pass (lp p))]
                                 [else '()])))]
                          [else '()]))
  (define cp0-on? (getenv "PLT_LINKLET_SHOW_CP0"))
  (define assembly-on? (getenv "PLT_LINKLET_SHOW_ASSEMBLY"))
  (define show-on? (or (eq? #t gensym-mode)
                       pre-jit-on?
                       lambda-on?
                       post-lambda-on?
                       post-interp-on?
                       jit-demand-on?
                       literals-on?
                       known-on?
                       cp0-on?
                       (pair? passes-on)
                       assembly-on?
                       (getenv "PLT_LINKLET_SHOW")))
  (define show
    (case-lambda
     [(what v) (show show-on? what v)]
     [(on? what v)
      (when on?
        (trace-printf ";; compile-linklet: step: ~a\n" what)
        (trace-printf ";; ---------------------\n")
        (call-with-system-wind
         (lambda ()
           (parameterize ([print-gensym gensym-mode]
                          [print-extended-identifiers #t])
             (pretty-print (strip-jit-wrapper
                            (strip-nested-annotations
                             (correlated->annotation v)))
                           (#%current-error-port))))))
      v]))

  (define (large-message info)
    (define name (hash-ref info 'module #f))
    (define phase (hash-ref info 'phase 0))
    (string-append "compiling only interior functions for large linklet"
                   (if mod
                       (format ": ~a" name)
                       "")
                   (if (eqv? phase 0)
                       ""
                       (format " (phase ~a)" phase))))

  (include "linklet/trace.ss")
  (include "linklet/check.ss")
  (include "linklet/version.ss")
  (include "linklet/write.ss")
  (include "linklet/read.ss")
  (include "linklet/annotation.ss")
  (include "linklet/performance.ss")

  (define (trace-compile inner-compile)
    (let* ([print-header (lambda ()
                           (trace-printf ";; compile-linklet: passes:")
                           (for-each (lambda (p)
                                       (define pass
                                         (if (eq? p #t) 'all p))
                                       (trace-printf " ~a" pass))
                                     (if assembly-on?
                                         (append passes-on '(assembly))
                                         passes-on))
                           (trace-printf "\n;; ---------------------\n"))]
           [wrapped-compile (lambda ()
                              (if (not (null? passes-on))
                                (parameterize ([print-gensym gensym-mode]
                                               [print-extended-identifiers #t]
                                               [#%$np-tracer passes-on])
                                  (inner-compile))
                                (inner-compile)))])
      (when (or (not (null? passes-on)) assembly-on?)
        (print-header))
      (if assembly-on?
          (parameterize ([#%$assembly-output (#%current-error-port)])
            (wrapped-compile))
          (wrapped-compile))))

  ;; `compile`, `interpret`, etc. have `dynamic-wind`-based state
  ;; that need to be managed correctly when swapping Racket
  ;; engines/threads.
  (define compile*
    (case-lambda
     [(e realm unsafe?)
      (call-with-system-wind (lambda ()
                               (parameterize ([optimize-level (if unsafe?
                                                                  3
                                                                  (optimize-level))]
                                              [compile-procedure-realm realm])
                                 (trace-compile (lambda () (compile e))))))]
     [(e) (compile* e #f #f)]))
  (define (interpret* e) ; result is not safe for space
    (call-with-system-wind (lambda () (interpret e))))
  (define (fasl-write* s o)
    (call-with-system-wind (lambda ()
                             (parameterize ([fasl-compressed compress-data?])
                               (fasl-write s o)))))
  (define (fasl-write/literals* s quoteds o)
    (call-with-system-wind (lambda ()
                             (call-getting-literals
                              quoteds
                              (lambda (pred)
                                (fasl-write s o pred))))))
  (define (fasl-write-code* s quoteds o)
    (call-with-system-wind (lambda ()
                             (parameterize ([fasl-compressed compress-code?])
                               (call-getting-literals
                                quoteds
                                (lambda (pred)
                                  (fasl-write s o pred 'omit-rtds)))))))
  (define (compile-to-port* s quoteds o realm unsafe?)
    (call-with-system-wind (lambda ()
                             (parameterize ([fasl-compressed compress-code?]
                                            [optimize-level (if unsafe?
                                                                3
                                                                (optimize-level))]
                                            [compile-procedure-realm realm])
                               (trace-compile
                                (lambda ()
                                  (call-getting-literals
                                   quoteds
                                   (lambda (pred)
                                     ;; If arguments change here, then probably they should change in
                                     ;; "cross-serve.ss", too:
                                     (compile-to-port s o #f #f #f (machine-type) #f pred 'omit-rtds)))))))))
  (define (expand/optimize* e unsafe?)
    (call-with-system-wind (lambda ()
                             (parameterize ([optimize-level (if unsafe?
                                                                3
                                                                (optimize-level))])
                               (#%expand/optimize e)))))

  (define (call-getting-literals quoteds proc)
    ;; `quoteds` is a list of literal values detected by schemify,
    ;; but we may discover srclocs attached as procedure names
    (let ([literals '()])
      (proc (lambda (v)
              (and (or (srcloc? v)
                       (and quoteds
                            (hash-ref quoteds v #f)))
                   (begin
                     (set! literals (cons v literals))
                     #t))))
      (list->vector (reverse literals))))

  (define (eval/foreign e mode)
    (performance-region
     mode
     (compile* e #f #t)))

  (define primitives (unsafe-make-hasheq)) ; hash of sym -> known
  (define primitive-tables '())     ; list of (cons sym hash)

  ;; Arguments are `(cons <sym> <hash-table>)`
  (define (install-linklet-primitive-tables! . tables)
    (set! primitive-tables tables)
    (for-each
     (lambda (table)
       (hash-for-each (cdr table) (lambda (k v) (hash-set! primitives k v))))
     tables)
    (unsafe-hash-seal! primitives)
    ;; propagate table to the rumble layer
    (install-primitives-table! primitives))

  ;; Runs the result of `interpretable-jitified-linklet`
  (define (run-interpret s)
    (interpret-linklet s))

  (define (lambda->linklet-lambda s)
    ;; Replace `lambda` with `$lambda/lift-barrier`, which prevents
    ;; the compiler from converting functions in the immediate linklet
    ;; body to take closure elements are arguments; at the level of a
    ;; linklet, it's better to create all of the closures on instantiation
    (cons '$lambda/lift-barrier (cdr s)))

  (define (compile-to-proc s format realm unsafe?)
    (if (eq? format 'interpret)
        (run-interpret s)
        (compile* (lambda->linklet-lambda s) realm unsafe?)))

  ;; returns code bytevector and literals vector
  (define (compile*-to-bytevector s quoteds realm unsafe?)
    (let-values ([(o get) (open-bytevector-output-port)])
      (let ([literals (compile-to-port* (list s) quoteds o realm unsafe?)])
        (values (get) literals))))

  ;; returns code bytevector and literals vector
  (define (compile-to-bytevector s quoteds format realm unsafe?)
    (cond
      [(eq? format 'interpret)
       (let-values ([(o get) (open-bytevector-output-port)])
         (let ([literals (fasl-write-code* s quoteds o)])
           (values (get) literals)))]
      [else (compile*-to-bytevector (lambda->linklet-lambda s) quoteds realm unsafe?)]))

  ;; returns code bytevector and literals vector
  (define (cross-compile-to-bytevector machine s quoteds format realm unsafe?)
    (cond
      [(eq? format 'interpret) (cross-fasl-to-string machine s quoteds 'code)]
      [else (cross-compile machine (lambda->linklet-lambda s) quoteds realm unsafe?)]))

  (define (eval-from-bytevector bv literals format)
    (add-performance-memory! 'faslin-code (bytevector-length bv))
    (cond
      [(eq? format 'interpret)
       (let ([r (performance-region
                 'faslin-code
                 (fasl-read (open-bytevector-input-port bv) 'load literals))])
         (run-interpret r))]
      [else
       (performance-region
        'faslin-code
        (code-from-bytevector bv literals))]))

  (define (code-from-bytevector bv literals)
    (let ([i (open-bytevector-input-port bv)])
      (load-compiled-from-port i literals)))

  (define (extract-literals v)
    (performance-region
     'faslin-literals
     (force-unfasl-literals v)))

  (define (compiler-query v)
    (call-with-system-wind
     (lambda ()
       (cond
         [(#%equal? v '(system-type)) (system-type)]
         [else (expand/optimize v)]))))

  (define-record-type wrapped-code
    (fields (mutable content) ; bytevector for 'lambda mode; annotation or (vector hash annotation) for 'jit mode
            literals
            arity-mask
            name ; #f, symbol, or boxed (=> method) #f or symbol
            realm)
    (nongenerative #{wrapped-code ca80s538oixlhvqqyokefc87g-0}))

  (define (force-wrapped-code wc)
    (let ([f (wrapped-code-content wc)])
      (if (procedure? f)
          f
          (performance-region
           'on-demand
           (cond
             [(bytevector? f)
              (let* ([f (code-from-bytevector f (wrapped-code-literals wc))])
                (wrapped-code-content-set! wc f)
                f)]
             [else
              (let ([f (compile* f)])
                (when jit-demand-on?
                  (show "JIT demand" (strip-nested-annotations (wrapped-code-content wc))))
                (wrapped-code-content-set! wc f)
                f)])))))

  (define (jitified-extract-closed wc)
    (let ([f (wrapped-code-content wc)])
      (if (#2%procedure? f)
          ;; previously forced, so no need for a wrapper
          f
          ;; make a wrapper that has the right arity and name
          ;; and that compiles/extracts when called:
          (make-jit-procedure (lambda () (force-wrapped-code wc))
                              (wrapped-code-arity-mask wc)
                              (wrapped-code-name wc)
                              (wrapped-code-realm wc)))))

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
                                (wrapped-code-name wc)
                                (wrapped-code-realm wc))))))

  (define (strip-jit-wrapper p)
    (cond
     [(wrapped-code? p)
      (vector (strip-jit-wrapper (strip-nested-annotations (wrapped-code-content p)))
              (wrapped-code-arity-mask p)
              (wrapped-code-name p)
              (wrapped-code-realm p))]
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

  ;; A linklet also has a table of information about its exports. That
  ;; known-value information is used by schemify to perform
  ;; cross-linklet inlining and related optimizations.

  (define-record-type linklet
    (fields (mutable code) ; the procedure or interpretable form
            literals       ; vector of literals, including paths, that have to be serialized by racket/fasl
            format         ; 'compile or 'interpret (where the latter may have compiled internal parts)
            (mutable preparation) ; 'faslable, 'faslable-strict, 'faslable-unsafe, 'callable, 'lazy, or (cons 'cross <machine>)
            importss-abi   ; ABI for each import, in parallel to `importss`
            (mutable exports-info) ; hash(target -> hash(sym -> known)) for info about export; see "known.rkt"; unfasl on demand
            name           ; name of the linklet (for debugging purposes)
            importss       ; list of list of import symbols
            exports)       ; list of export symbol-or-pair, pair is (cons export-symbol src-symbol)
    (nongenerative #{linklet Zuquy0g9bh5vmeespyap4g-3}))

  (define (linklet->vector l)
    (vector '|#%linklet|
            (linklet-code l)
            (linklet-literals l)
            (linklet-format l)
            (linklet-preparation l)
            (linklet-importss-abi l)
            (linklet-exports-info l)
            (linklet-name l)
            (linklet-importss l)
            (linklet-exports l)))

  (define (linklet-vector? l)
    (and (vector? l)
         (= 10 (vector-length l))
         (eq? '|#%linklet| (vector-ref l 0))))

  (define (vector->linklet v)
    (make-linklet (vector-ref v 1)
                  (vector-ref v 2)
                  (vector-ref v 3)
                  (vector-ref v 4)
                  (vector-ref v 5)
                  (vector-ref v 6)
                  (vector-ref v 7)
                  (vector-ref v 8)
                  (vector-ref v 9)))

  (define (set-linklet-code linklet code preparation)
    (make-linklet code
                  (linklet-literals linklet)
                  (linklet-format linklet)
                  preparation
                  (linklet-importss-abi linklet)
                  (linklet-exports-info linklet)
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define (set-linklet-literals linklet literals)
    (make-linklet (linklet-code linklet)
                  literals
                  (linklet-format linklet)
                  (linklet-preparation linklet)
                  (linklet-importss-abi linklet)
                  (linklet-exports-info linklet)
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define (set-linklet-preparation linklet preparation)
    (make-linklet (linklet-code linklet)
                  (linklet-literals linklet)
                  (linklet-format linklet)
                  preparation
                  (linklet-importss-abi linklet)
                  (linklet-exports-info linklet)
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define (set-linklet-exports-info linklet exports-info)
    (make-linklet (linklet-code linklet)
                  (linklet-literals linklet)
                  (linklet-format linklet)
                  (linklet-preparation linklet)
                  (linklet-importss-abi linklet)
                  exports-info
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define (linklet-pack-exports-info! l)
    (let ([info (linklet-exports-info l)])
      (when (hash? info)
        (let ([new-info (->fasl info fixup-correlated-srclocs)])
          (linklet-exports-info-set! l new-info)))))

  (define (linklet-unpack-exports-info! l)
    (let ([info (linklet-exports-info l)])
      (unless (hash? info)
        (let ([new-info
               (cond
                [(not info) (hasheq)]
                [else (fasl-> info)])])
          (linklet-exports-info-set! l new-info)))))

  (define compile-linklet
    (case-lambda
     [(c) (compile-linklet c #f #f #f '(serializable))]
     [(c info) (compile-linklet c info #f #f '(serializable))]
     [(c info import-keys) (compile-linklet c info import-keys #f '(serializable))]
     [(c info import-keys get-import) (compile-linklet c info import-keys get-import '(serializable))]
     [(c info import-keys get-import options)
      (do-compile-linklet 'compile-linklet c info import-keys get-import options #f)]))

  (define expand/optimize-linklet ; for testing
    (case-lambda
     [(c) (do-compile-linklet 'expand/optimize-linklet c #f #f #f '() #t)]
     [(c info import-keys get-import options)
      (do-compile-linklet 'expand/optimize-linklet c info import-keys get-import options #t)]))

  (define do-compile-linklet
    (lambda (who c info-or-name import-keys get-import options just-expand?)
      ;; If `info-or-name` is not a hash, assume it may be a `name`
      ;; value for compatibility with previous versions.
      (define info
        (if (hash? info-or-name)
            info-or-name
            (hasheq 'name info-or-name)))
      (define name (hash-ref info 'name #f))
      (define check-result (check-compile-args who import-keys get-import options))
      (define serializable? (#%memq 'serializable options))
      (define use-prompt? (#%memq 'use-prompt options))
      (define unsafe? (and (#%memq 'unsafe options) #t))
      (define unlimited-compile? (#%memq 'unlimited-compile options))
      (define cross-machine (and serializable?
                                 (let ([m (|#%app| current-compile-target-machine)])
                                   (and (not (eq? m (machine-type)))
                                        m))))
      (define target-machine (or cross-machine (machine-type)))
      (define realm (let ([v (|#%app| current-compile-realm)])
                      (if (eq? v 'racket) #f v)))
      (define enforce-constant? (|#%app| compile-enforce-module-constants))
      (define inline? (not (|#%app| compile-context-preservation-enabled)))
      (define quick-mode? (and (not just-expand?)
                               (or default-compile-quick?
                                   (and (not serializable?)
                                        (#%memq 'quick options)))))
      (define serializable?-box (and serializable? (box #f)))
      (define sfd-cache (if serializable?
                            ;; For determinism: a fresh, non-weak cache per linklet
                            (make-hash)
                            ;; For speed and more flexible sharing: a weak, place-local cache
                            (get-nonserializable-sfd-cache)))

      (when show-on?
        (hash-for-each info (lambda (k v)
                              (trace-printf ";; ~a: ~a: ~a\n" who k v))))

      (performance-region
       'schemify
       (define jitify-mode?
         (and (not just-expand?)
              (not quick-mode?)
              (or (eq? linklet-compilation-mode 'jit)
                  (and (eq? linklet-compilation-mode 'mach)
                       (not unlimited-compile?)
                       (linklet-bigger-than? c linklet-compilation-limit serializable?)
                       (log-message root-logger 'info 'linklet (large-message info) #f)
                       #t))))
       (define format (if (or jitify-mode?
                              quick-mode?
                              (and (eq? linklet-compilation-mode 'interp)
                                   (not just-expand?)))
                          'interpret
                          'compile))
       ;; Convert the linklet S-expression to a `lambda` S-expression:
       (define-values (impl-lam importss exports new-import-keys importss-abi exports-info)
         (schemify-linklet (show "linklet" c)
                           serializable?-box
                           (not (#%memq 'uninterned-literal options))
                           (if (eq? format 'interpret) 'interp 'compile) ; target
                           (|#%app| compile-allow-set!-undefined)
                           unsafe?
                           enforce-constant?
                           inline?
                           (not use-prompt?)
                           prim-knowns
                           primitives
                           (if cross-machine
                               (lambda (v) (cross-compiler-query cross-machine v))
                               compiler-query)
                           ;; Callback to get a specific linklet for a
                           ;; given import:
                           (if get-import
                               (lambda (key) (lookup-linklet-or-instance get-import key target-machine))
                               (lambda (key) (values #f #f #f)))
                           import-keys))
       (define impl-lam/jitified
         (cond
           [(not jitify-mode?) impl-lam]
           [else
            (performance-region
             'jitify
             (jitify-schemified-linklet (case linklet-compilation-mode
                                          [(jit) (show pre-jit-on? "pre-jitified" impl-lam)]
                                          [else (show "schemified" impl-lam)])
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
                                           (lambda (expr arity-mask name method?)
                                             (let ([a (correlated->annotation (xify expr) serializable? sfd-cache)])
                                               (make-wrapped-code a
                                                                  #f
                                                                  arity-mask
                                                                  (extract-inferred-name expr name method?)
                                                                  realm)))]
                                          [else
                                           ;; Compile an individual `lambda`:
                                           (lambda (expr-in arity-mask name method?)
                                             (performance-region
                                              'compile-nested
                                              (let ([expr (show lambda-on? "lambda" (correlated->annotation expr-in serializable? sfd-cache))])
                                                (if serializable?
                                                    (let ([quoteds (unbox serializable?-box)])
                                                      (let-values ([(code literals) (if cross-machine
                                                                                        (cross-compile cross-machine expr quoteds realm unsafe?)
                                                                                        (compile*-to-bytevector expr quoteds realm unsafe?))])
                                                        (make-wrapped-code code literals arity-mask (extract-inferred-name expr-in name method?) realm)))
                                                    (compile* expr realm unsafe?)))))])))]))
       (define impl-lam/interpable
         (let ([impl-lam (case (and jitify-mode?
                                    linklet-compilation-mode)
                           [(mach) (show post-lambda-on? "post-lambda" impl-lam/jitified)]
                           [else (show "schemified" impl-lam/jitified)])])
           (if (eq? format 'interpret)
               (interpretable-jitified-linklet impl-lam serializable? realm)
               (correlated->annotation impl-lam serializable? sfd-cache))))
       (when known-on?
         (show "known" (hash-map exports-info (lambda (k v) (list k v)))))
       (when (and cp0-on? (eq? format 'compile))
         (show "cp0" (expand/optimize* (correlated->annotation impl-lam/jitified) unsafe?)))
       (performance-region
        'compile-linklet
        ;; Create the linklet:
        (let ([impl (show (and (eq? format 'interpret) post-interp-on?) "post-interp" impl-lam/interpable)])
          (cond
            [just-expand? (expand/optimize* impl unsafe?)]
            [else
             (let-values ([(code literals)
                           (if serializable?
                               (let ([quoteds (unbox serializable?-box)])
                                 (if cross-machine
                                     (cross-compile-to-bytevector cross-machine impl quoteds format realm unsafe?)
                                     (compile-to-bytevector impl quoteds format realm unsafe?)))
                               (values (compile-to-proc impl format realm unsafe?) '#()))])
               (when literals-on?
                 (show "literals" literals))
               (let ([lk (make-linklet code
                                       literals
                                       format
                                       (if serializable? (if cross-machine (cons 'cross cross-machine) 'faslable) 'callable)
                                       importss-abi
                                       (hasheq target-machine exports-info)
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
                     lk)))]))))))

  (define (lookup-linklet-or-instance get-import key target-machine)
    ;; Use the provided callback to get an linklet for the
    ;; import at `index`
    (cond
     [key
      (let-values ([(lnk/inst more-import-keys) (get-import key)])
        (cond
         [(linklet? lnk/inst)
          (linklet-unpack-exports-info! lnk/inst)
          (let ([ei (linklet-exports-info lnk/inst)])
            (values (and ei
                         (hash-ref ei target-machine (hasheq)))
                    ;; No conversion needed:
                    #f
                    more-import-keys))]
         [(instance? lnk/inst)
          (values (instance-hash lnk/inst)
                  variable->known
                  more-import-keys)]
         [else (values #f #f #f)]))]
     [else (values #f #f #f)]))

  (define recompile-linklet
    (case-lambda
     [(lnk) (recompile-linklet lnk #f #f #f '(serializable))]
     [(lnk info) (recompile-linklet lnk info #f #f '(serializable))]
     [(lnk info import-keys) (recompile-linklet lnk info import-keys #f '(serializable))]
     [(lnk info import-keys get-import) (recompile-linklet lnk info import-keys get-import '(serializable))]
     [(lnk info import-keys get-import options)
      (unless (linklet? lnk)
        (raise-argument-error 'recompile-linklet "linklet?" lnk))
      (check-compile-args 'recompile-linklet import-keys get-import options)
      (if import-keys
          (values lnk import-keys)
          lnk)]))

  ;; Intended to speed up reuse of a linklet in exchange for not being
  ;; able to serialize anymore
  (define (eval-linklet linklet)
    (case (linklet-preparation linklet)
      [(faslable)
       (set-linklet-code linklet (linklet-code linklet) 'lazy)]
      [(faslable-strict)
       (set-linklet-code linklet
                         (eval-from-bytevector (linklet-code linklet)
                                               (extract-literals (linklet-literals linklet))
                                               (linklet-format linklet))
                         'callable)]
      [(faslable-unsafe)
       (raise (|#%app|
               exn:fail
               "eval-linklet: cannot use unsafe linklet loaded with non-original code inspector"
               (current-continuation-marks)))]
      [else
       linklet]))

  (define instantiate-linklet
    (case-lambda
     [(linklet import-instances)
      (instantiate-linklet linklet import-instances #f #f)]
     [(linklet import-instances target-instance)
      (instantiate-linklet linklet import-instances target-instance #f)]
     [(linklet import-instances target-instance use-prompt?)
      (unless (linklet? linklet)
        (raise-argument-error 'instantiate-linklet "linklet?" linklet))
      (let loop ([l import-instances])
        (unless (null? l)
          (if (and (pair? l)
                   (instance? (car l)))
              (loop (cdr l))
              (raise-argument-error 'instantiate-linklet "(listof instance?)" import-instances))))
      (cond
       [target-instance
        (unless (instance? target-instance)
          (raise-argument-error 'instantiate-linklet "(or/c instance? #f)" target-instance))
        ;; Instantiate into the given instance and return the
        ;; result of the linklet body:
        (with-continuation-mark
          linklet-instantiate-key (instance-name target-instance)
          (begin
           ;; Trigger lazy conversion of code from bytevector, but
           ;; beware that multiple thread might try to do that at once
           (let-values ([(prep code) (with-interrupts-disabled
                                      (values (linklet-preparation linklet)
                                              (linklet-code linklet)))])
             (when (eq? 'lazy prep)
               (let ([code (eval-from-bytevector code
                                                 (extract-literals (linklet-literals linklet))
                                                 (linklet-format linklet))])
                 (with-interrupts-disabled
                  (when (eq? 'lazy (linklet-preparation linklet))
                    (linklet-code-set! linklet code)
                    (linklet-preparation-set! linklet 'callable))))))
           ;; Call the linklet:
           (performance-region
            'instantiate
            ((if use-prompt?
                 ;; For per-form prompts with in a module linklet,
                 ;; rely on 'use-prompt provided at compile time.
                 ;; But this one is useful for top-level forms.
                 call-with-module-prompt
                 (lambda (thunk) (thunk)))
             (lambda ()
               (apply
                (if (eq? 'callable (linklet-preparation linklet))
                    (linklet-code linklet)
                    (eval-from-bytevector (linklet-code linklet)
                                          (extract-literals (linklet-literals linklet))
                                          (linklet-format linklet)))
                (make-variable-reference target-instance #f)
                (extract-imported-variabless target-instance
                                             import-instances
                                             (linklet-importss linklet)
                                             (linklet-importss-abi linklet)
                                             (create-variables target-instance
                                                               (linklet-exports linklet)))))))))]
       [else
        ;; Make a fresh instance, recur, and return the instance
        (let ([i (make-instance (linklet-name linklet))])
          (instantiate-linklet linklet import-instances i use-prompt?)
          i)])]))
              
  (define (linklet-import-variables linklet)
    (unless (linklet? linklet)
        (raise-argument-error 'linklet-import-variables "linklet?" linklet))
    (linklet-importss linklet))

  (define (linklet-export-variables linklet)
    (unless (linklet? linklet)
        (raise-argument-error 'linklet-export-variables "linklet?" linklet))
    (map (lambda (e) (if (pair? e) (car e) e)) (linklet-exports linklet)))

  (define (linklet-fasled-code+arguments linklet)
    (unless (linklet? linklet)
      (raise-argument-error 'linklet-fasled-code+arguments "linklet?" linklet))
    (case (linklet-preparation linklet)
      [(faslable faslable-strict faslable-unsafe lazy)
       (values (linklet-format linklet) (linklet-code linklet) (extract-literals (linklet-literals linklet)))]
      [else (values #f #f #f #f)]))

  (define (linklet-interpret-jitified? v)
    (wrapped-code? v))

  (define (linklet-interpret-jitified-extract v)
    (unless (wrapped-code? v)
      (raise-argument-error 'linklet-interpret-jitified-extract "linklet-interpret-jitified?" v))
    (force-wrapped-code v))

  (define (linklet-add-target-machine-info linklet other-linklet)
    (unless (linklet? linklet)
      (raise-argument-error 'linklet-add-target-machine-info "linklet?" linklet))
    (let ([from-hash? (hash? other-linklet)])
      (unless (or from-hash?
                  (linklet? other-linklet))
        (raise-argument-error 'linklet-add-target-machine-info "(or/c linklet? hash?)" other-linklet))
      (linklet-unpack-exports-info! linklet)
      (unless from-hash?
        (linklet-unpack-exports-info! other-linklet))
      (unless from-hash?
        ;; sanity check:
        (let ([ht (unsafe-make-hasheq)])
          (for-each (lambda (ex) (hash-set! ht ex #t)) (linklet-export-variables linklet))
          (let ([exs (linklet-export-variables other-linklet)])
            (unless (and (= (length exs) (hash-count ht))
                         (andmap (lambda (ex) (hash-ref ht ex #f)) exs))
              (raise-arguments-error 'linklet-add-target-machine-info
                                     "linklets are not compatible")))))
      ;; merge:
      (let ([other-ei (if from-hash? other-linklet (or (linklet-exports-info other-linklet) (hasheq)))])
        (set-linklet-exports-info linklet
                                  (let loop ([ei (or (linklet-exports-info linklet) (hasheq))]
                                             [keys (hash-map other-ei (lambda (k v) k))])
                                    (cond
                                      [(null? keys) ei]
                                      [else (let ([key (car keys)])
                                              (loop (hash-set ei key (hash-ref other-ei key))
                                                    (cdr keys)))]))))))

  (define (linklet-summarize-target-machine-info linklet)
    (unless (linklet? linklet)
      (raise-argument-error 'linklet-add-target-machine-info "linklet?" linklet))
    (linklet-unpack-exports-info! linklet)
    (linklet-exports-info linklet))

  ;; ----------------------------------------

  ;; A potentially mutable import or definition is accessed through
  ;; the indirection of a `variable`; accessing a variable may include
  ;; a check for undefined, since going through a `variable`
  ;; sacrifices the undefined check of the host Scheme
    
  (define-record variable (val
                           name
                           source-name
                           constance  ; #f (mutable), 'constant, or 'consistent (always the same shape)
                           inst-box)) ; weak pair with instance in `car`

  ;; Can't use `unsafe-undefined`, because the expander expects to be
  ;; able to store `unsafe-undefined` in variables
  (define variable-undefined (gensym 'undefined))

  (define (make-internal-variable name)
    (make-variable variable-undefined name name #f (cons #!bwp #f)))

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
                         "  constant: " (symbol->string (variable-source-name var)) "\n"
                         "  in module:" (variable-module-name var))
          (current-continuation-marks)
          (variable-name var)))]
       [else
        (raise
         (|#%app|
          exn:fail:contract:variable
          (string-append (symbol->string (variable-source-name var))
                         ": cannot modify a constant")
          (current-continuation-marks)
          (variable-name var)))])]
     [else
      (set-variable-val! var val)
      (when constance
        (set-variable-constance! var constance))]))

  (define (variable-set! var val)
    (do-variable-set! var val #f #f))

  (define (variable-set!/define var val constance)
    (do-variable-set! var val constance #t))

  (define (variable-set!/check-undefined var val)
    (when (eq? (variable-val var) variable-undefined)
      (raise-undefined var #t))
    (variable-set! var val))

  (define (variable-ref var)
    (let ([v (variable-val var)])
      (if (eq? v variable-undefined)
          (raise-undefined var #f)
          v)))

  (define (variable-ref/no-check var)
    (variable-val var))

  (define (set-consistent-variables!/define vars vals)
    (let loop ([i 0])
      (unless (fx= i (#%vector-length vars))
        (variable-set!/define (#%vector-ref vars i) (#%vector-ref vals i) 'consistent)
        (loop (fx+ i 1)))))

  ;; Find variables or values needed from an instance for a linklet's
  ;; imports
  (define (extract-imported-variabless target-inst insts symss imports-abis accum)
    (cond
      [(null? insts)
       (unless (null? symss) (raise-import-count-mismatch target-inst))
       accum]
      [(null? symss) (raise-import-count-mismatch target-inst)]
      [else (extract-imported-variables
             target-inst (car insts) (car symss) (car imports-abis)
             (extract-imported-variabless target-inst (cdr insts) (cdr symss) (cdr imports-abis)
                                          accum))]))
  (define (extract-imported-variables target-inst inst syms imports-abi accum)
    (cond
     [(null? syms) accum]
     [else
      (let ([sym (car syms)]
            [import-abi (car imports-abi)])
        (let ([var (or (eq-hashtable-ref (hash->eq-hashtable (instance-hash inst)) sym #f) ; raw hashtable avoids unnecessary lock
                       (raise-linking-failure "is not exported" target-inst inst sym))])
          (when (eq? (variable-val var) variable-undefined)
            (raise-linking-failure "is uninitialized" target-inst inst sym))
          (let ([v (if import-abi
                       ;; The import ABI encoding is generated by schemify and checked here.
                       ;; Currently, an ABI requirement specifies 'proc, a number of fields for
                       ;; a structure type, or #t for "any values"
                       (let ([v (variable-val var)])
                         (cond
                           [(eq? import-abi 'proc)
                            (unless (#%procedure? v)
                              (raise-linking-failure "was expected to have a procedure value" target-inst inst sym))]
                           [(fixnum? import-abi)
                            (let ([v (unsafe-strip-impersonator v)])
                              (unless (and (record-type-descriptor? v)
                                           (eqv? import-abi (#%$record-type-field-count v)))
                                (raise-linking-failure (#%format "was expected to have a struct-type value with ~a fields" import-abi)
                                                       target-inst inst sym)))])
                         v)
                       var)])
            (cons v
                  (extract-imported-variables target-inst inst (cdr syms) (cdr imports-abi) accum)))))]))

  (define (raise-linking-failure why target-inst inst sym)
    (raise-arguments-error 'instantiate-linklet
                           (string-append "mismatch;\n"
                                          " reference to a variable that " why "")
                           "name" (unquoted-printing-string (symbol->string sym))
                           "exporting instance" (unquoted-printing-string (format "~a" (instance-name inst)))
                           "importing instance" (unquoted-printing-string (format "~a" (instance-name target-inst)))
                           "possible reason" (unquoted-printing-string
                                              "modules need to be recompiled because dependencies changed")
                           "possible solution" (unquoted-printing-string
                                                "running `racket -y`, `raco make`, or `raco setup`")))

  (define (raise-import-count-mismatch inst)
    (raise-arguments-error 'instantiate-linklet
                           (string-append "mismatch;\n"
                                          " given import instance count does not match expected")
                           "importing instance name" (unquoted-printing-string (format "~a" (instance-name inst)))))

  (define (identify-module var)
    (cond
     [(error-print-source-location)
      (let ([i (car (variable-inst-box var))])
        (cond
         [(eq? i #!bwp)
          ""]
         [(instance-name i)
          => (lambda (name)
               (#%format "\n  in module: ~a" name))]
         [else ""]))]
     [else ""]))

  (define (indentify-internal-name var)
    (cond
     [(error-print-source-location)
      (cond
       [(eq? (variable-name var) (variable-source-name var))
        ""]
       [else
        (string-append "\n  internal name: "
                       (symbol->string  (variable-name var)))])]
     [else ""]))

  (define (raise-undefined var set?)
    (raise
     (|#%app|
      exn:fail:contract:variable
      (cond
       [set?
        (string-append "set!: assignment disallowed;\n"
                       " cannot set variable before its definition\n"
                       "  variable: " (symbol->string (variable-source-name var))
                       (identify-module var)
                       (indentify-internal-name var))]
       [else
        (string-append (symbol->string (variable-source-name var))
                       ": undefined;\n cannot reference an identifier before its definition"
                       (identify-module var)
                       (indentify-internal-name var))])
      (current-continuation-marks)
      (variable-name var))))

  ;; Create the variables needed for a linklet's exports; assumes that
  ;; the instance-hashtable lock is currently held
  (define (create-variables inst syms-or-pairs)
    (let ([ht (instance-hash inst)]
          [inst-box (weak-cons inst #f)])
      (let ([raw-ht (hash->eq-hashtable ht)])
        (with-interrupts-disabled ; since we test and add to `raw-ht`, and it might be shared
         (create-the-variables syms-or-pairs raw-ht inst-box)))))
  (define (create-the-variables syms-or-pairs raw-ht inst-box)
    (cond
     [(null? syms-or-pairs) '()]
     [else
      (let ([sym-or-pair (car syms-or-pairs)])
        (let-values ([(sym src-sym)
                      (if (pair? sym-or-pair)
                          (values (car sym-or-pair) (cdr sym-or-pair))
                          (values sym-or-pair sym-or-pair))])
          (cons (or (eq-hashtable-ref raw-ht sym #f)
                    (let ([var (make-variable variable-undefined sym src-sym #f inst-box)])
                      (eq-hashtable-set! raw-ht sym var)
                      var))
                (create-the-variables (cdr syms-or-pairs) raw-ht inst-box))))]))

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
      (let* ([ht (unsafe-make-hasheq)]
             [raw-ht (hash->eq-hashtable ht)] ; note: raw-ht isn't shared, yet
             [inst (new-instance name data ht)]
             [inst-box (weak-cons inst #f)])
        (check-constance 'make-instance constance)
        (let loop ([content content])
          (cond
           [(null? content) (void)]
           [else
            (let ([name (car content)])
              (eq-hashtable-set! raw-ht (car content) (make-variable (cadr content) name name constance inst-box)))
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
                                 (raise-arguments-error
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
                     (let ([var (make-variable variable-undefined k k #f (weak-cons i #f))])
                       (hash-set! (instance-hash i) k var)
                       var))])
        (do-variable-set! var v mode #f))]))

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
      (raise-argument-error who "(or/c #f 'constant 'consistent)" mode)))

  ;; --------------------------------------------------

  (define-record variable-reference (instance      ; the use-site instance
                                     var-or-info)) ; the referenced variable, 'constant, 'mutable, #f, or primitive name
              
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
         [(or (eq? v 'constant) (eq? v 'mutable))
          ;; Local variable, so same as use-site
          (variable-reference->instance vr #t)]
         [else
          (or (#%ormap (lambda (table)
                         (and (hash-ref (cdr table) v #f)
                              (car table)))
                       primitive-tables)
              ;; Fallback, just in case
              '|#%kernel|)]))]))

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
    (lambda args
      (apply
       abort-current-continuation
       (default-continuation-prompt-tag)
       args)))

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
               (raise-definition-result-arity-error syms vals))
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

  (include "linklet/cross-compile.ss")
  
  (define compile-enforce-module-constants
    (make-parameter #t (lambda (v) (and v #t)) 'compile-enforce-module-constants))

  (define compile-context-preservation-enabled
    (make-parameter #f (lambda (v) (and v #t)) 'compile-context-preservation-enabled))

  (define compile-allow-set!-undefined
    (make-parameter #f (lambda (v) (and v #t)) 'compile-allow-set!-undefined))

  (define current-compile-target-machine
    (make-parameter (machine-type) (lambda (v)
                                     (unless (or (not v)
                                                 (and (symbol? v)
                                                      (compile-target-machine? v)))
                                       (raise-argument-error 'current-compile-target-machine
                                                             "(or/c #f (and/c symbol? compile-target-machine?))"
                                                             v))
                                     v)
				     'current-compile-target-machine))

  (define (compile-target-machine? v)
    (unless (symbol? v)
      (raise-argument-error 'compile-target-machine? "symbol?" v))
    (or (eq? v (machine-type))
        (and (#%assq v cross-machine-types)
             #t)))

  (define current-compile-realm
    (make-parameter 'racket (lambda (v)
                              (unless (symbol? v)
                                (raise-argument-error 'current-compile-realm "symbol?" v))
                              v)
                    'current-compile-realm))

  (define eval-jit-enabled
    (make-parameter #t (lambda (v) (and v #t)) 'eval-jit-enabled))
  
  (define load-on-demand-enabled
    (make-parameter #t (lambda (v) (and v #t)) 'load-on-demand-enabled))

  ;; --------------------------------------------------

  (define-syntax primitive-table
    (syntax-rules ()
      [(_ id ...)
       (let ([ht (unsafe-make-hasheq)])
         (hash-set! ht 'id id) ...
         ht)]))

  (define schemify-table
    (primitive-table
     make-internal-variable
     variable-set!
     variable-set!/check-undefined
     variable-set!/define
     variable-ref
     variable-ref/no-check
     variable-set!/define
     set-consistent-variables!/define
     make-instance-variable-reference
     unbox/check-undefined
     set-box!/check-undefined
     jitified-extract
     jitified-extract-closed))

  ;; --------------------------------------------------

  (interpreter-link! primitives
                     correlated->datum
                     make-internal-variable
                     variable-ref variable-ref/no-check
                     variable-set! variable-set!/define
                     make-interp-procedure
                     decode-procedure-name)

  (set-foreign-eval! eval/foreign)

  (include "linklet/config.ss"))
