(top-level-program
 (import (except (chezpart)
                 eval
                 read)
         (rumble)
         (only (expander)
               boot
               current-command-line-arguments
               use-compiled-file-paths
               current-library-collection-links
               find-library-collection-links
               current-library-collection-paths
               find-library-collection-paths
               use-collection-link-paths
               current-compiled-file-roots
               find-main-config
               executable-yield-handler
               load-on-demand-enabled
               use-user-specific-search-paths
               eval
               read
               load
               dynamic-require
               namespace-require
               embedded-load
               module-path?
               module-declared?
               module->language-info
               module-path-index-join
               identifier-binding
               namespace-datum-introduce
               datum->kernel-syntax
               namespace-variable-value
               version
               exit
               compile-keep-source-locations!
               expander-place-init!
               path-list-string->path-list)
         (regexp)
         (io)
         (thread)
         (only (linklet)
               omit-debugging?
               platform-independent-zo-mode?
               linklet-performance-init!
               linklet-performance-report!
               current-compile-target-machine
               compile-target-machine?
               add-cross-compiler!))

 (linklet-performance-init!)
 (unless omit-debugging?
   (compile-keep-source-locations! #t))

 (define-syntax seq (syntax-rules () [(_ expr ...) (define dummy (let () expr ... (void)))]))

 (define (run the-command-line-arguments/maybe-bytes)
   (define the-command-line-arguments
     (map (lambda (s) (if (bytes? s)
                          (bytes->string/locale s #\?)
                          s))
          the-command-line-arguments/maybe-bytes))
   (define (find-original-bytes s)
     ;; In case `the-command-line-arguments/maybe-bytes` has bytes,
     ;; try to get the original byte string for `s`
     (let loop ([args the-command-line-arguments]
                [args/maybe-bytes the-command-line-arguments/maybe-bytes])
       (cond
        [(null? args) s]
        [(eq? (car args) s) (car args/maybe-bytes)]
        [else (loop (cdr args) (cdr args/maybe-bytes))])))
   (define (->path s)
     (cond
      [(path? s) s]
      [(bytes? s) (bytes->path s)]
      [else (string->path s)]))

   (define (getenv-bytes str)
     (environment-variables-ref (|#%app| current-environment-variables) (string->utf8 str)))

   (define builtin-argc 9)
   (seq
    (unless (>= (length the-command-line-arguments) builtin-argc)
      (error 'racket (string-append
		      "expected `exec-file`, `run-file`, `collects`, and `etc` paths"
		      " plus `segment-offset`, `cs-compiled-subdir?`, `is-gui?`,"
		      " `wm-is-gracket-or-x11-arg-count`, and `gracket-guid-or-x11-args`"
		      " to start")))
    (set-exec-file! (->path (list-ref the-command-line-arguments/maybe-bytes 0)))
    (set-run-file! (->path (list-ref the-command-line-arguments/maybe-bytes 1))))
   (define-values (init-collects-dir collects-pre-extra)
     (let ([s (list-ref the-command-line-arguments/maybe-bytes 2)])
       (cond
        [(or (equal? s "")
             (equal? s '#vu8()))
         (values 'disable '())]
        [(or (string? s) (bytevector? s)) (values (->path s) '())]
        [else (let ([s (reverse s)])
                (values (->path (car s))
                        (map ->path (cdr s))))])))
   (define init-config-dir (->path (or (getenv-bytes "PLTCONFIGDIR")
                                       (list-ref the-command-line-arguments/maybe-bytes 3))))
   (define segment-offset (#%string->number (list-ref the-command-line-arguments 4)))
   (define cs-compiled-subdir? (string=? "true" (list-ref the-command-line-arguments 5)))
   (define gracket? (string=? "true" (list-ref the-command-line-arguments 6)))
   (define wm-is-gracket-or-x11-arg-count (string->number (list-ref the-command-line-arguments 7)))
   (define gracket-guid-or-x11-args (list-ref the-command-line-arguments 8))

   (seq
    (when (foreign-entry? "racket_exit")
      (#%exit-handler (foreign-procedure "racket_exit" (int) void)))

    (when (eq? 'windows (system-type))
      (unsafe-register-process-global (string->bytes/utf-8 "PLT_WM_IS_GRACKET")
				      (ptr-add #f wm-is-gracket-or-x11-arg-count))
      (unsafe-register-process-global (string->bytes/utf-8 "PLT_GRACKET_GUID")
				      (bytes-append (string->bytes/utf-8 gracket-guid-or-x11-args) #vu8(0))))
    (when (eq? 'macosx (system-type))
      (when gracket?
        (unsafe-register-process-global (string->bytes/utf-8 "PLT_IS_FOREGROUND_APP")
				        (ptr-add #f 1))))
    (when (eq? 'unix (system-type))
      (when gracket?
        (unsafe-register-process-global (string->bytes/utf-8 "PLT_X11_ARGUMENT_COUNT")
				        (ptr-add #f wm-is-gracket-or-x11-arg-count))
        (unsafe-register-process-global (string->bytes/utf-8 "PLT_X11_ARGUMENTS")
				        (ptr-add #f (#%string->number gracket-guid-or-x11-args 16))))))

   (define compiled-file-paths
     (list (->path (cond
                    [cs-compiled-subdir?
                     (build-path "compiled"
                                 (cond
                                  [(getenv-bytes "PLT_ZO_PATH")
                                   => (lambda (s)
                                        (unless (and (not (equal? s #vu8()))
                                                     (relative-path? (->path s)))
                                          (error 'racket "PLT_ZO_PATH environment variable is not a valid path"))
                                        (->path s))]
                                  [platform-independent-zo-mode? "cs"]
                                  [else (symbol->string (machine-type))]))]
                    [else "compiled"]))))
   (define user-specific-search-paths? #t)
   (define load-on-demand? #t)
   (define compile-target-machine (if (getenv "PLT_COMPILE_ANY")
                                      #f
                                      (machine-type)))
   (define compiled-roots-path-list-string (getenv "PLTCOMPILEDROOTS"))
   (define embedded-load-in-places #f)

   (define (see saw . args)
     (let loop ([saw saw] [args args])
       (if (null? args)
           saw
           (loop (hash-set saw (car args) #t) (cdr args)))))
   (define (saw? saw tag)
     (hash-ref saw tag #f))
   (define (saw-something? saw)
     (positive? (hash-count saw)))

   (define rx:logging-spec (pregexp "^[\\s]*(none|fatal|error|warning|info|debug)(?:@([^\\s @]+))?(.*)$"))
   (define rx:all-whitespace (pregexp "^[\\s]*$"))
   (define (parse-logging-spec which str where exit-on-fail?)
     (define (fail)
       (let ([msg (string-append
                   which " <levels> " where " must be one of the following\n"
                   " <level>s:\n"
                   "   none fatal error warning info debug\n"
                   "or up to one such <level> in whitespace-separated sequence of\n"
                   "   <level>@<name>\n"
                   "given: " str)])
         (cond
          [exit-on-fail?
           (raise-user-error 'racket msg)]
          [else
           (eprintf "~a\n" msg)])))
     (let loop ([str str] [default #f])
       (let ([m (regexp-match rx:logging-spec str)])
         (cond
          [m
           (let ([level (string->symbol (cadr m))]
                 [topic (caddr m)])
             (cond
              [topic
               (cons level (cons (string->symbol topic) (loop (cadddr m) default)))]
              [default (fail)]
              [else (loop (cadddr m) level)]))]
          [(regexp-match? rx:all-whitespace str)
           (if default (list default) null)]
          [else (fail)]))))

   (define (configure-runtime m)
     ;; New-style configuration through a `configure-runtime` submodule:
     (let ([config-m (module-path-index-join '(submod "." configure-runtime) m)])
       (when (module-declared? config-m #t)
         (dynamic-require config-m #f)))
     ;; Old-style configuration with module language info:
     (let ([info (module->language-info m #t)])
       (when (and (vector? info) (= 3 (vector-length info)))
         (let* ([info-load (lambda (info)
                             ((dynamic-require (vector-ref info 0) (vector-ref info 1)) (vector-ref info 2)))]
                [get (info-load info)]
                [infos (get 'configure-runtime '())])
           (unless (and (list? infos)
                        (andmap (lambda (info) (and (vector? info) (= 3 (vector-length info))))
                                infos))
             (raise-argument-error 'runtime-configure "(listof (vector any any any))" infos))
           (for-each info-load infos)))))

   (define need-runtime-configure? #t)
   (define (namespace-require+ mod)
     (let ([m (module-path-index-join mod #f)])
       (when need-runtime-configure?
         (configure-runtime m)
         (set! need-runtime-configure? #f))
       (namespace-require m)
       ;; Run `main` submodule, if any:
       (let ([main-m (module-path-index-join '(submod "." main) m)])
         (when (module-declared? main-m #t)
           (dynamic-require main-m #f)))))

   (define (get-repl-init-filename)
     (call-with-continuation-prompt
      (lambda ()
        (or (let ([p (build-path (find-system-path 'addon-dir)
                                 (if gracket?
                                     "gui-interactive.rkt"
                                     "interactive.rkt"))])
              (and (file-exists? p) p))
            (let ([config-fn (build-path (find-main-config) "config.rktd")])
              (and (file-exists? config-fn)
                   (hash-ref (call-with-input-file config-fn read)
                             (if gracket? 'gui-interactive-file 'interactive-file)
                             #f)))
            (if gracket? 'racket/gui/interactive 'racket/interactive)))
      (default-continuation-prompt-tag)
      (lambda args #f)))

   (define init-library (if gracket?
                            '(lib "racket/gui/init")
                            '(lib "racket/init")))
   (define loads '())
   (define repl? #f)
   (define repl-init? #t)
   (define version? #f)
   (define text-repl? (not gracket?))
   (define yield? #t)
   (define stderr-logging-arg #f)
   (define stdout-logging-arg #f)
   (define syslog-logging-arg #f)
   (define runtime-for-init? #t)
   (define exit-value 0)
   (define host-collects-dir #f)
   (define host-config-dir #f)
   (define addon-dir #f)
   (define rev-collects-post-extra '())

   (define (no-init! saw)
     (unless (saw? saw 'top)
       (set! init-library #f)))

   (define (next-arg what flag within-flag args)
     (let loop ([args (cdr args)] [accum '()])
       (cond
        [(null? args)
         (error 'racket "missing ~a after ~a switch" what (or within-flag flag))]
        [(pair? (car args))
         (loop (cdr args) (cons (car args) accum))]
        [else
         (values (car args) (append (reverse accum) (cdr args)))])))

   (define (check-path-arg what flag within-flag)
     (when (equal? what "")
       (error 'racket "empty ~a after ~a switch" what (or within-flag flag))))

   (define (raise-bad-switch arg within-arg)
     (raise-user-error 'racket "bad switch: ~a~a"
                       arg
                       (if within-arg
                           (format " within: ~a" within-arg)
                           "")))

   (define (no-front!)
     (unsafe-register-process-global (string->bytes/utf-8 "Racket-GUI-no-front") #vu8(1)))

   (define (add-namespace-require-load! mod-path arg)
     (unless (module-path? mod-path)
       (raise-user-error 'require
                         "bad module path: ~V derived from command-line argument: ~a"
                         mod-path
                         arg))
     (set! loads
           (cons (lambda () (namespace-require+ mod-path))
                 loads)))

   (include "main/help.ss")

   (define-syntax string-case
     ;; Assumes that `arg` is a variable
     (syntax-rules ()
       [(_ arg [else body ...])
        (let () body ...)]
       [(_ arg [(str ...) body ...] rest ...)
        (if (or (string=? arg str) ...)
            (let () body ...)
            (string-case arg rest ...))]))

   (define remaining-command-line-arguments '#())

   (seq
    (let flags-loop ([args (list-tail the-command-line-arguments builtin-argc)]
                     [saw (hasheq)])
      ;; An element of `args` can become `(cons _arg _within-arg)`
      ;; due to splitting multiple flags with a single "-"
      (define (loop args) (flags-loop args (see saw 'something)))
      ;; Called to handle remaining non-switch arguments:
      (define (finish args saw)
        (cond
         [(and (pair? args)
               (not (saw? saw 'non-config)))
          (loop (cons "-u" args))]
         [else
          (set! remaining-command-line-arguments (vector->immutable-vector
                                                  (list->vector args)))
          (when (and (null? args) (not (saw? saw 'non-config)))
            (set! repl? #t)
            (when text-repl?
              (set! version? #t)))]))
      ;; Dispatch on first argument:
      (if (null? args)
          (finish args saw)
          (let* ([arg (car args)]
                 [within-arg (and (pair? arg) (cdr arg))]
                 [arg (if (pair? arg) (car arg) arg)])
            (string-case
             arg
             [("-l" "--lib")
              (let-values ([(lib-name rest-args) (next-arg "library name" arg within-arg args)])
                (add-namespace-require-load! `(lib ,lib-name) lib-name)
                (no-init! saw)
                (flags-loop rest-args (see saw 'non-config 'lib)))]
             [("-t" "--require")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (add-namespace-require-load! `(file ,file-name) file-name)
                (no-init! saw)
                (flags-loop rest-args (see saw 'non-config 'lib)))]
             [("-p")
              (let-values ([(package rest-args) (next-arg "package" arg within-arg args)])
                (add-namespace-require-load! `(planet ,package) package)
                (no-init! saw)
                (flags-loop rest-args (see saw 'non-config 'lib)))]
             [("-u" "--require-script")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (add-namespace-require-load! `(file ,file-name) file-name)
                (no-init! saw)
                (set-run-file! (string->path file-name))
                (flags-loop (cons "--" rest-args) (see saw 'non-config 'lib)))]
             [("-f" "--load")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (set! loads (cons (lambda () (load file-name))
                                  loads))
                (flags-loop rest-args (see saw 'non-config)))]
             [("-r" "--script")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (set! loads (cons (lambda () (load file-name))
                                  loads))
                (set-run-file! (string->path file-name))
                (flags-loop (cons "--" rest-args) (see saw 'non-config)))]
             [("-e" "--eval")
              (let-values ([(expr rest-args) (next-arg "expression" arg within-arg args)])
                (set! loads
                      (cons
                       (lambda ()
                         (call-with-values (lambda ()
                                             (call-with-continuation-prompt
                                              (lambda ()
                                                (eval `(|#%top-interaction| . ,(read (open-input-string expr)))))
                                              (default-continuation-prompt-tag)
                                              (lambda (proc)
                                                ;; continue escape to set error status:
                                                (abort-current-continuation (default-continuation-prompt-tag) proc))))
                           (lambda vals
                             (for-each (lambda (v)
                                         (|#%app| (|#%app| current-print) v)
                                         (flush-output))
                                       vals))))
                       loads))
                (flags-loop rest-args (see saw 'non-config)))]
             [("-k")
              (let*-values ([(n rest-args) (next-arg "starting and ending offsets" arg within-arg args)]
                            [(m rest-args) (next-arg "first ending offset" arg within-arg (cons "-k" rest-args))]
                            [(p rest-args) (next-arg "second ending offset" arg within-arg (cons "-k" rest-args))])
                (let* ([add-segment-offset
                        (lambda (s what)
                          (let ([n (#%string->number s)])
                            (unless (exact-integer? n)
                              (raise-user-error 'racket "bad ~a: ~a" what s))
                            (#%number->string (+ n segment-offset))))]
                       [n (add-segment-offset n "starting offset")]
                       [m (add-segment-offset m "first ending offset")]
                       [p (add-segment-offset p "second ending offset")])
                  (set! loads
                        (cons
                         (lambda ()
                           (set! embedded-load-in-places (list n m))
                           (embedded-load n m #f #t)
                           (embedded-load m p #f #f))
                         loads)))
                (no-init! saw)
                (flags-loop rest-args (see saw 'non-config)))]
             [("-m" "--main")
              (set! loads (cons (lambda () (call-main))
                                loads))
              (flags-loop (cdr args) (see saw 'non-config))]
             [("-i" "--repl") 
              (set! repl? #t)
              (set! version? #t)
              (flags-loop (cdr args) (see saw 'non-config 'top))]
             [("-n" "--no-lib")
              (set! init-library #f)
              (flags-loop (cdr args) (see saw 'non-config))]
             [("-V" "--no-yield")
              (set! yield? #f)
              (set! version? #t)
              (flags-loop (cdr args) (see saw 'non-config))]
             [("-v" "--version") 
              (set! version? #t)
              (no-init! saw)
              (flags-loop (cdr args) (see saw 'non-config))]
             [("-c" "--no-compiled")
              (set! compiled-file-paths '())
              (loop (cdr args))]
             [("-I")
              (let-values ([(lib-name rest-args) (next-arg "library name" arg within-arg args)])
                (when init-library
                  (set! init-library `(lib ,lib-name)))
                (loop rest-args))]
             [("-A" "--addon")
              (let-values ([(addon-path rest-args) (next-arg "addon directory" arg within-arg args)])
                (set! addon-dir (find-original-bytes addon-path))
                (loop rest-args))]
             [("-X" "--collects")
              (let-values ([(collects-path rest-args) (next-arg "collects path" arg within-arg args)])
                (cond
                 [(equal? collects-path "")
                  (set! init-collects-dir 'disable)]
                 [else 
                  (check-path-arg "collects path" arg within-arg)
                  (set! init-collects-dir (path->complete-path (->path (find-original-bytes collects-path))))])
                (loop rest-args))]
             [("-S" "--search")
              (let-values ([(collects-path rest-args) (next-arg "path" arg within-arg args)])
                (check-path-arg "collects path" collects-path within-arg)
                (set! rev-collects-post-extra (cons (->path (find-original-bytes collects-path)) rev-collects-post-extra))
                (loop rest-args))]
             [("-G" "--config")
              (let-values ([(config-path rest-args) (next-arg "config path" arg within-arg args)])
                (check-path-arg "config path" config-path within-arg)
                (set! init-config-dir (path->complete-path (->path (find-original-bytes config-path))))
                (loop rest-args))]
             [("-C" "--cross")
              (set! host-config-dir init-config-dir)
              (set! host-collects-dir init-collects-dir)
              (set-cross-mode! 'force)
              (loop (cdr args))]
             [("-U" "--no-user-path")
              (set! user-specific-search-paths? #f)
              (loop (cdr args))]
             [("-R" "--compiled")
              (let-values ([(paths rest-args) (next-arg "path list" arg within-arg args)])
                (set! compiled-roots-path-list-string paths)
                (loop rest-args))]
             [("-d" "--no-delay")
              (set! load-on-demand? #t)
              (loop (cdr args))]
             [("-b" "--binary")
              (loop (cdr args))]
             [("-q" "--no-init-file")
              (set! repl-init? #f)
              (loop (cdr args))]
             [("-W" "--stderr")
              (let-values ([(spec rest-args) (next-arg "stderr level" arg within-arg args)])
                (set! stderr-logging-arg (parse-logging-spec "stderr" spec (format "after ~a switch" (or within-arg arg)) #t))
                (loop rest-args))]
             [("-O" "--stdout")
              (let-values ([(spec rest-args) (next-arg "stdout level" arg within-arg args)])
                (set! stdout-logging-arg (parse-logging-spec "stdout" spec (format "after ~a switch" (or within-arg arg)) #t))
                (loop rest-args))]
             [("-L" "--syslog")
              (let-values ([(spec rest-args) (next-arg "syslog level" arg within-arg args)])
                (set! syslog-logging-arg (parse-logging-spec "syslog" spec (format "after ~a switch" (or within-arg arg)) #t))
                (loop rest-args))]
             [("-N" "--name")
              (let-values ([(name rest-args) (next-arg "name" arg within-arg args)])
                (set-run-file! (string->path name))
                (loop rest-args))]
             [("-J")
              (cond
               [gracket?
                (let-values ([(wm-class rest-args) (next-arg "WM_CLASS string" arg within-arg args)])
                  (unsafe-register-process-global (string->bytes/utf-8 "Racket-GUI-wm-class")
                                                  (bytes-append (string->bytes/utf-8 wm-class) #vu8(0)))
                  (loop rest-args))]
               [else
                (raise-bad-switch arg within-arg)])]
             [("-K" "--back")
              (cond
               [gracket?
                (no-front!)
                (loop (cdr args))]
               [else
                (raise-bad-switch arg within-arg)])]
             [("-z" "--text-repl")
              (cond
               [gracket?
                (no-front!)
                (set! text-repl? #t)
                (loop (cdr args))]
               [else
                (raise-bad-switch arg within-arg)])]
             [("-M" "--compile-any")
              (set! compile-target-machine #f)
              (loop (cdr args))]
             [("--compile-machine")
              (let-values ([(mach-str rest-args) (next-arg "target machine" arg within-arg args)])
                (let ([mach (string->symbol mach-str)])
                  (unless (compile-target-machine? mach)
                    (raise-user-error 'racket "machine not supported as a compile target: ~a" mach))
                  (set! compile-target-machine mach))
                (loop rest-args))]
             [("--cross-compiler")
              (let-values ([(mach rest-args) (next-arg "target machine" arg within-arg args)])
                (let-values ([(xpatch-dir rest-args) (next-arg "cross-compiler path" arg within-arg (cons arg rest-args))])
                  (add-cross-compiler! (string->symbol mach)
                                       (path->complete-path (->path (find-original-bytes xpatch-dir)))
                                       (find-system-path 'exec-file))
                  (loop rest-args)))]
             [("--cross-server")
              (let-values ([(scheme-xpatch-file rest-args) (next-arg "target machine" arg within-arg args)])
                (let-values ([(scheme-xpatch-file rest-args) (next-arg "compiler xpatch path" arg within-arg (cons arg rest-args))])
                  (let-values ([(scheme-xpatch-file rest-args) (next-arg "library xpatch path" arg within-arg (cons arg rest-args))])
                    (when (or (saw-something? saw)
                              (not (null? rest-args)))
                      (raise-user-error 'racket "--cross-server cannot be combined with any other arguments"))
                    (raise-user-error 'racket "--cross-server should have been handled earlier"))))
              (flags-loop null (see saw 'non-config))]
             [("-j" "--no-jit")
              (loop (cdr args))]
             [("-h" "--help")
              (show-help)
              (exit)]
             [("--")
              (cond
               [(or (null? (cdr args)) (not (pair? (cadr args))))
                (finish (cdr args) saw)]
               [else
                ;; Need to handle more switches from a combined flag
                (loop (cons (cadr args) (cons (car args) (cddr args))))])]
             [else
              (cond
               [(and (eqv? (string-ref arg 0) #\-)
                     (> (string-length arg) 1))
                (cond
                 [(and (> (string-length arg) 2)
                       (not (eqv? (string-ref arg 1) #\-)))
                  ;; Split flags
                  (loop (append (map (lambda (c) (cons (string #\- c) arg))
                                     (cdr (string->list arg)))
                                (cdr args)))]
                 [else
                  (raise-bad-switch arg within-arg)])]
               [else
                ;; Non-flag argument
                (finish args saw)])])))))

   (define (call-main)
     (let ([m (namespace-datum-introduce 'main)])
       (unless (identifier-binding m)
         (namespace-variable-value 'main #f
                                   (lambda ()
                                     (error "main: not defined or required into the top-level environment"))))
       (call-with-values (lambda () (eval (datum->kernel-syntax
                                           (cons m (vector->list remaining-command-line-arguments)))))
         (lambda results
           (let ([p (|#%app| current-print)])
             (for-each (lambda (v) (|#%app| p v)) results))))))

   ;; Set up GC logging
   (define-values (struct:gc-info make-gc-info gc-info? gc-info-ref gc-info-set!)
     (make-struct-type 'gc-info #f 10 0 #f null 'prefab #f '(0 1 2 3 4 5 6 7 8 9)))
   (define (K plus n)
     (let* ([s (number->string (quotient (abs n) 1000))]
            [len (string-length s)]
            [len2 (+ len
                     (quotient (sub1 len) 3)
                     (if (or (< n 0)
                             (not (eq? "" plus)))
                         1
                         0)
                     1)]
            [s2 (make-string len2)])
       (string-set! s2 (sub1 len2) #\K)
       (let loop ([i len]
                  [j (sub1 len2)]
                  [digits 0])
         (cond
          [(zero? i)
           (cond
            [(< n 0) (string-set! s2 0 #\-)]
            [(not (eq? plus "")) (string-set! s2 0 (string-ref plus 0))])
           s2]
          [(= 3 digits)
           (let ([j (sub1 j)])
             (string-set! s2 j #\,)
             (loop i j 0))]
          [else
           (let ([i (sub1 i)]
                 [j (sub1 j)])
             (string-set! s2 j (string-ref s i))
             (loop i j (add1 digits)))]))))
   (define minor-gcs 0)
   (define major-gcs 0)
   (define auto-gcs 0)
   (define peak-mem 0)
   (seq
    (set-garbage-collect-notify!
     (let ([root-logger (|#%app| current-logger)])
       ;; This function can be called in any Chez Scheme thread
       (lambda (gen pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                    post-allocated post-allocated+overhead post-time post-cpu-time)
         (let ([minor? (< gen (collect-maximum-generation))])
           (if minor?
               (set! minor-gcs (add1 minor-gcs))
               (set! major-gcs (add1 major-gcs)))
           (set! peak-mem (max peak-mem pre-allocated))
           (let ([debug-GC? (log-level?* root-logger 'debug 'GC)])
             (when (or debug-GC?
                       (and (not minor?)
                            (log-level?* root-logger 'debug 'GC:major)))
               (let ([delta (- pre-allocated post-allocated)])
                 (log-message* root-logger 'debug (if debug-GC? 'GC 'GC:major)
                               (chez:format "GC: 0:~a~a @ ~a(~a); free ~a(~a) ~ams @ ~a"
                                            (if minor? "min" "MAJ") gen
                                            (K "" pre-allocated) (K "+" (- pre-allocated+overhead pre-allocated))
                                            (K "" delta) (K "+" (- (- pre-allocated+overhead post-allocated+overhead)
                                                                   delta))
                                            (- post-cpu-time pre-cpu-time) pre-cpu-time)
                               (make-gc-info (if minor? 'minor 'major) pre-allocated pre-allocated+overhead 0
                                             post-allocated post-allocated+overhead
                                             pre-cpu-time post-cpu-time
                                             pre-time post-time)
                               #f
                               ;; in interrupt:
                               #t)))))))))
   (seq
    (|#%app| exit-handler
     (let ([orig (|#%app| exit-handler)]
           [root-logger (|#%app| current-logger)])
       (lambda (v)
         (when (log-level? root-logger 'info 'GC)
           (log-message root-logger 'info 'GC
                        (chez:format "0:atexit peak ~a; alloc ~a; major ~a; minor ~a; ~ams"
                                     (K "" peak-mem)
                                     (K "" (- (+ (bytes-deallocated) (bytes-allocated)) (initial-bytes-allocated)))
                                     major-gcs
                                     minor-gcs
                                     (let ([t (sstats-gc-cpu (statistics))])
                                       (+ (* (time-second t) 1000)
                                          (quotient (time-nanosecond t) 1000000))))
                        #f))
         (linklet-performance-report!)
         (|#%app| orig v)))))

   (define stderr-logging
     (or stderr-logging-arg
         (let ([spec (getenv "PLTSTDERR")])
           (if spec
               (parse-logging-spec "stderr" spec "in PLTSTDERR environment variable" #f)
               '(error)))))

   (define stdout-logging
     (or stdout-logging-arg
         (let ([spec (getenv "PLTSTDOUT")])
           (if spec
               (parse-logging-spec "stdout" spec "in PLTSTDOUT environment variable" #f)
               '()))))

   (define syslog-logging
     (or syslog-logging-arg
         (let ([spec (getenv "PLTSYSLOG")])
           (if spec
               (parse-logging-spec "syslog" spec "in PLTSYSLOG environment variable" #f)
               '()))))

   (define (initialize-place!)
     (|#%app| current-command-line-arguments remaining-command-line-arguments)
     (|#%app| use-compiled-file-paths compiled-file-paths)
     (|#%app| use-user-specific-search-paths user-specific-search-paths?)
     (|#%app| load-on-demand-enabled load-on-demand?)
     (unless (eq? compile-target-machine (machine-type))
       (|#%app| current-compile-target-machine compile-target-machine))
     (boot)
     (when (and stderr-logging
                (not (null? stderr-logging)))
       (apply add-stderr-log-receiver! (|#%app| current-logger) stderr-logging))
     (when (and stdout-logging
                (not (null? stdout-logging)))
       (apply add-stdout-log-receiver! (|#%app| current-logger) stdout-logging))
     (when (and syslog-logging
                (not (null? syslog-logging)))
       (apply add-syslog-log-receiver! (|#%app| current-logger) syslog-logging))
     (when host-collects-dir
       (set-host-collects-dir! host-collects-dir))
     (when host-config-dir
       (set-host-config-dir! host-config-dir))
     (cond
      [(eq? init-collects-dir 'disable)
       (|#%app| use-collection-link-paths #f)
       (set-collects-dir! (build-path 'same))]
      [else
       (set-collects-dir! init-collects-dir)])
     (set-config-dir! init-config-dir)
     (unless (eq? init-collects-dir 'disable)
       (|#%app| current-library-collection-links
        (find-library-collection-links))
       (|#%app| current-library-collection-paths
        (find-library-collection-paths collects-pre-extra (reverse rev-collects-post-extra))))
     (when compiled-roots-path-list-string
       (|#%app| current-compiled-file-roots
        (let ([s (regexp-replace* "@[(]version[)]"
                                  compiled-roots-path-list-string
                                  (version))])
          (path-list-string->path-list s (list (build-path 'same)))))))

   (set-make-place-ports+fds! make-place-ports+fds)

   (set-start-place!
    (lambda (pch mod sym in out err cust plumber)
      (io-place-init! in out err cust plumber)
      (regexp-place-init!)
      (expander-place-init!)
      (initialize-place!)
      (when embedded-load-in-places
        (let-values ([(n m) (apply values embedded-load-in-places)])
          (embedded-load n m #f #t)))
      (lambda ()
        (let ([f (dynamic-require mod sym)])
          (f pch)))))

   (let ([a (or addon-dir
                (getenv-bytes "PLTADDONDIR"))])
     (when a
       (set-addon-dir! (path->complete-path (->path a)))))

   (when (getenv "PLT_STATS_ON_BREAK")
     (keyboard-interrupt-handler
      (let ([orig (keyboard-interrupt-handler)])
        (lambda args
          (dump-memory-stats)
          (apply orig args)))))

   (when version?
     (printf "Welcome to Racket v~a [cs].\n" (version)))
   (call-in-main-thread
    (lambda ()
      (initialize-place!)

      (when init-library
        (namespace-require+ init-library))

      (call-with-continuation-prompt
       (lambda ()
         (for-each (lambda (ld) (ld))
                   (reverse loads)))
       (default-continuation-prompt-tag)
       ;; If any load escapes, then set the exit value and
       ;; stop running loads (but maybe continue with the REPL)
       (lambda (proc)
         (set! exit-value 1)
         ;; Let the actual default handler report an arity mismatch, etc.
         (call-with-continuation-prompt
          (lambda () (abort-current-continuation (default-continuation-prompt-tag) proc)))))

      (when repl?
        (set! exit-value 0)
        (when repl-init?
          (let ([m (get-repl-init-filename)])
            (when m
              (call-with-continuation-prompt
               (lambda () (dynamic-require m 0))
               (default-continuation-prompt-tag)
               (lambda args (set! exit-value 1))))))
        (|#%app| (if text-repl?
                     (dynamic-require 'racket/base 'read-eval-print-loop)
                     (dynamic-require 'racket/gui/init 'graphical-read-eval-print-loop)))
        (when text-repl?
          (newline)))

      (when yield?
        (|#%app| (|#%app| executable-yield-handler) exit-value))

      (exit exit-value))))

 (define the-command-line-arguments
   (or (and (top-level-bound? 'bytes-command-line-arguments)
            (top-level-value 'bytes-command-line-arguments))
       (command-line-arguments)))

 (if (null? the-command-line-arguments)
     ;; Assume that we're running as a boot file
     (scheme-start (lambda args (run args)))
     ;; Assume that we're running as a script
     (run the-command-line-arguments)))
