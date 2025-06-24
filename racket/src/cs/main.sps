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
               current-load/use-compiled
               find-compiled-file-roots
               find-main-config
               read-installation-configuration-table
               get-installation-name
               executable-yield-handler
               load-on-demand-enabled
               use-user-specific-search-paths
               use-compiled-file-check
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
               add-cross-compiler!
               primitive-lookup))

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
     (environment-variables-ref (current-environment-variables) (string->utf8 str)))

   (define (startup-warning fmt . args)
     (#%fprintf (#%current-error-port) "~a: " (path->string (find-system-path 'exec-file)))
     (if (null? args)
         (#%display fmt (#%current-error-port))
         (#%apply #%fprintf (#%current-error-port) fmt args))
     (#%newline (#%current-error-port)))

   (define (startup-error fmt . args)
     (apply startup-warning fmt args)
     (exit 1))

   (define builtin-argc 11)
   (seq
    (unless (>= (length the-command-line-arguments) builtin-argc)
      (startup-error (string-append
		      "expected `embedded-interactive-mode?`,"
                      " `exec-file`, `run-file`, `collects`, and `etc` paths"
		      " plus `k-file`, `segment-offset`, `cs-compiled-subdir?`, `is-gui?`,"
		      " `wm-is-gracket-or-x11-arg-count`, and `gracket-guid-or-x11-args`"
		      " to start")))
    (set-exec-file! (->path (list-ref the-command-line-arguments/maybe-bytes 1)))
    (set-run-file! (->path (list-ref the-command-line-arguments/maybe-bytes 2))))
   (define embedded-interactive-mode? (string=? "true" (list-ref the-command-line-arguments 0)))
   (define-values (init-collects-dir collects-pre-extra)
     (let ([s (list-ref the-command-line-arguments/maybe-bytes 3)])
       (cond
        [(or (equal? s "")
             (equal? s '#vu8()))
         (values 'disable '())]
        [(or (string? s) (bytevector? s)) (values (->path s) '())]
        [else (let ([s (reverse s)])
                (values (->path (car s))
                        (map ->path (cdr s))))])))
   (define init-config-dir (->path (or (getenv-bytes "PLTCONFIGDIR")
                                       (list-ref the-command-line-arguments/maybe-bytes 4))))
   (define k-executable-path (let ([s (list-ref the-command-line-arguments/maybe-bytes 5)])
                               (and (not (or (equal? s "") (equal? s '#vu8())))
                                    s)))
   (define segment-offset (#%string->number (list-ref the-command-line-arguments 6)))
   (define cs-compiled-subdir? (string=? "true" (list-ref the-command-line-arguments 7)))
   (define gracket? (string=? "true" (list-ref the-command-line-arguments 8)))
   (define wm-is-gracket-or-x11-arg-count (string->number (list-ref the-command-line-arguments 9)))
   (define gracket-guid-or-x11-args (list-ref the-command-line-arguments 10))

   (seq
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
				        (ptr-add #f (#%string->number (substring gracket-guid-or-x11-args 2) 16))))))

   (define compiled-file-paths
     (list (cond
             [(getenv-bytes "PLT_ZO_PATH")
              => (lambda (s)
                   (unless (and (not (equal? s #vu8()))
                                (relative-path? (->path s)))
                     (startup-error "PLT_ZO_PATH environment variable is not a valid path"))
                   (->path s))]
             [cs-compiled-subdir?
              (build-path "compiled"
                          (->path
                           (cond
                             [platform-independent-zo-mode? "cs"]
                             [else (symbol->string (machine-type))])))]
             [else "compiled"])))
   (define make? #f)
   (define user-specific-search-paths? #t)
   (define load-on-demand? #t)
   (define compile-target-machine (if (getenv "PLT_COMPILE_ANY")
                                      #f
                                      (machine-type)))
   (define compiled-roots-path-list-string (getenv "PLTCOMPILEDROOTS"))
   (define embedded-load-in-places '())

   (define init-compiled-file-check (let ([s (getenv "PLT_COMPILED_FILE_CHECK")])
                                      (cond
                                        [(not s) (use-compiled-file-check)]
                                        [(string=? s "modify-seconds") 'modify-seconds]
                                        [(string=? s "exists") 'exists]
                                        [else
                                         (startup-warning
                                          (string-append "unrecognized value for PLT_COMPILED_FILE_CHECK;\n"
                                                         " recognized values are \"modify-seconds\" and \"exists\"\n"
                                                         "  unrecognized value: ~s")
                                          s)
                                         (use-compiled-file-check)])))

   (define (see saw . args)
     (let loop ([saw saw] [args args])
       (if (null? args)
           saw
           (loop (hash-set saw (car args) #t) (cdr args)))))
   (define (saw? saw tag)
     (hash-ref saw tag #f))
   (define (saw-something? saw)
     (positive? (hash-count saw)))

   (define rx:logging-spec (pregexp "^[\\s]*(none|fatal|error|warning|info|debug)(?:@([^\\s @]+))?()"))
   (define rx:all-whitespace (pregexp "^[\\s]*$"))
   (define (parse-logging-spec which str where exit-on-fail? default)
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
           (startup-error msg)]
          [else
           (eprintf "~a\n" msg)
           default])))
     (let loop ([start-pos 0] [default #f])
       (let ([m (regexp-match-positions rx:logging-spec str start-pos)])
         (define (extract p) (and p (substring str (car p) (cdr p))))
         (cond
          [m
           (let ([level (string->symbol (extract (cadr m)))]
                 [topic (extract (caddr m))])
             (cond
              [topic
               (cons level (cons (string->symbol topic) (loop (cdr (cadddr m)) default)))]
              [default (fail)]
              [else (loop (cdr (cadddr m)) level)]))]
          [(regexp-match? rx:all-whitespace str start-pos)
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

   (define (get-repl-init-filename config)
     (or (let ([p (build-path (find-system-path 'addon-dir)
                              (if gracket?
                                  "gui-interactive.rkt"
                                  "interactive.rkt"))])
           (and (file-exists? p) p))
         (hash-ref config
                   (if gracket? 'gui-interactive-file 'interactive-file)
                   #f)
         (if gracket? 'racket/gui/interactive 'racket/interactive)))

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
   (define addon-dir #f)
   (define host-collects-dir #f)
   (define host-config-dir #f)
   (define host-addon-dir 'inherit)
   (define rev-collects-post-extra '())

   (define (no-init! saw)
     (unless (saw? saw 'top)
       (set! init-library #f)))

   (define (next-arg what flag within-flag args)
     (let loop ([args (cdr args)] [accum '()])
       (cond
        [(null? args)
         (startup-error "missing ~a after ~a switch" what (or within-flag flag))]
        [(pair? (car args))
         (loop (cdr args) (cons (car args) accum))]
        [else
         (values (car args) (append (reverse accum) (cdr args)))])))

   (define (check-path-arg path what flag within-flag)
     (when (equal? path "")
       (startup-error "empty ~a after ~a switch" what (or within-flag flag))))

   (define (raise-bad-switch arg within-arg)
     (startup-error "bad switch: ~a~a"
                    arg
                    (if within-arg
                        (format " within: ~a" within-arg)
                        "")))

   (define (no-front!)
     (unsafe-register-process-global (string->bytes/utf-8 "Racket-GUI-no-front") #vu8(1)))

   (define (add-namespace-require-load! mod-path arg)
     (unless (module-path? mod-path)
       (startup-error "bad module path: ~a derived from command-line argument: ~a"
                      (format "~v" mod-path)
                      arg))
     (set! loads
           (cons (lambda () (namespace-require+ mod-path))
                 loads)))

   (include "main/help.ss")
   (include "main/eval-all.ss")

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
          (cond
           [(and (null? args) (not (saw? saw 'non-config)))
            (set! repl? #t)
            (when text-repl?
              (set! version? #t))]
           [else
            (no-init! saw)])]))
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
                (check-path-arg file-name "file name" arg within-arg)
                (set-run-file! (string->path file-name))
                (flags-loop (cons "--" rest-args) (see saw 'non-config 'lib)))]
             [("-f" "--load")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (set! loads (cons (lambda ()
                                    (if (equal? file-name "-")
                                        (eval-all (current-input-port))
                                        (load file-name)))
                                  loads))
                (flags-loop rest-args (see saw 'non-config 'top)))]
             [("-r" "--script")
              (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
                (set! loads (cons (lambda () (load file-name))
                                  loads))
                (check-path-arg file-name "file name" arg within-arg)
                (set-run-file! (string->path file-name))
                (flags-loop (cons "--" rest-args) (see saw 'non-config 'top)))]
             [("-e" "--eval")
              (let-values ([(expr rest-args) (next-arg "expression" arg within-arg args)])
                (set! loads
                      (cons
                       (lambda ()
                         (define i (open-input-string expr))
                         (eval-all i))
                       loads))
                (flags-loop rest-args (see saw 'non-config 'top)))]
             [("-k" "-Y")
              (let*-values ([(f rest-args) (if (equal? arg "-Y")
                                               (next-arg "file" arg within-arg args)
                                               (values #f (cdr args)))]
                            [(n rest-args) (next-arg "starting and ending offsets" arg within-arg (cons arg rest-args))]
                            [(m rest-args) (next-arg "first ending offset" arg within-arg (cons arg rest-args))]
                            [(p rest-args) (next-arg "second ending offset" arg within-arg (cons arg rest-args))])
                (let* ([add-segment-offset
                        (lambda (s what)
                          (let ([n (#%string->number s)])
                            (unless (exact-integer? n)
                              (startup-error "bad ~a: ~a" what s))
                            (#%number->string (+ n (if f 0 segment-offset)))))]
                       [n (add-segment-offset n "starting offset")]
                       [m (add-segment-offset m "first ending offset")]
                       [p (add-segment-offset p "second ending offset")]
                       [f (or f k-executable-path)])
                  (set! loads
                        (cons
                         (lambda ()
                           (set! embedded-load-in-places (cons (list f n m #f) embedded-load-in-places))
                           (embedded-load n m #f #t f)
                           (embedded-load m p #f #f f))
                         loads)))
                (no-init! saw)
                (flags-loop rest-args (see saw 'non-config)))]
             [("-m" "--main")
              (set! loads (cons (lambda () (call-main))
                                loads))
              (flags-loop (cdr args) (see saw 'non-config 'top))]
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
              (flags-loop (cdr args) (see saw 'non-config))]
             [("-y" "--make")
              (set! make? #t)
              (loop (cdr args))]
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
                  (check-path-arg collects-path "collects path" arg within-arg)
                  (set! init-collects-dir (path->complete-path (->path (find-original-bytes collects-path))))])
                (loop rest-args))]
             [("-S" "--search")
              (let-values ([(collects-path rest-args) (next-arg "path" arg within-arg args)])
                (check-path-arg collects-path "collects path" collects-path within-arg)
                (let ([path (path->complete-path (->path (find-original-bytes collects-path)))])
                  (set! rev-collects-post-extra (cons path rev-collects-post-extra)))
                (loop rest-args))]
             [("-G" "--config")
              (let-values ([(config-path rest-args) (next-arg "config path" arg within-arg args)])
                (check-path-arg config-path "config path" config-path within-arg)
                (set! init-config-dir (path->complete-path (->path (find-original-bytes config-path))))
                (loop rest-args))]
             [("-C" "--cross")
              (unless (saw? saw 'cross)
                (set! host-config-dir init-config-dir)
                (set! host-collects-dir init-collects-dir)
                (set! host-addon-dir addon-dir)
                (set-cross-mode! 'force))
              (flags-loop (cdr args) (see saw 'cross))]
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
                (set! stderr-logging-arg (parse-logging-spec "stderr" spec (format "after ~a switch" (or within-arg arg)) #t #f))
                (loop rest-args))]
             [("-O" "--stdout")
              (let-values ([(spec rest-args) (next-arg "stdout level" arg within-arg args)])
                (set! stdout-logging-arg (parse-logging-spec "stdout" spec (format "after ~a switch" (or within-arg arg)) #t #f))
                (loop rest-args))]
             [("-L" "--syslog")
              (let-values ([(spec rest-args) (next-arg "syslog level" arg within-arg args)])
                (set! syslog-logging-arg (parse-logging-spec "syslog" spec (format "after ~a switch" (or within-arg arg)) #t #f))
                (loop rest-args))]
             [("-N" "--name")
              (let-values ([(name rest-args) (next-arg "name" arg within-arg args)])
                (check-path-arg name "name" arg within-arg)
                (set-run-file! (string->path name))
                (loop rest-args))]
             [("-E" "--exec")
              (let-values ([(name rest-args) (next-arg "name" arg within-arg args)])
                (check-path-arg name "name" arg within-arg)
                (set-exec-file! (string->path name))
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
                    (startup-error "machine not supported as a compile target: ~a" mach))
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
                      (startup-error "--cross-server cannot be combined with any other arguments"))
                    (startup-error "--cross-server should have been handled earlier"))))
              (flags-loop null (see saw 'non-config))]
             [("-j" "--no-jit")
              (loop (cdr args))]
             [("-Z")
              (let-values ([(ignored rest-args) (next-arg "argument to ignore" arg within-arg args)])
                (flags-loop rest-args saw))]
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
               [(and (> (string-length arg) 1)
                     (eqv? (string-ref arg 0) #\-))
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
           (let ([p (current-print)])
             (for-each (lambda (v) (|#%app| p v)) results))))))

   ;; Set up GC logging
   (define-values (struct:gc-info make-gc-info gc-info? gc-info-ref gc-info-set!)
     (make-struct-type 'gc-info #f 10 0 #f null 'prefab #f '(0 1 2 3 4 5 6 7 8 9)))
   (define (K plus n)
     (let* ([s (number->string (quotient (abs n) 1024))]
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
   (seq
    (set-garbage-collect-notify!
     (let ([root-logger (current-logger)])
       ;; This function can be called in any Chez Scheme thread
       (lambda (gen pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                    post-allocated post-allocated+overhead proper-post-time proper-post-cpu-time
                    post-time post-cpu-time)
         (let ([minor? (< gen (collect-maximum-generation))])
           (if minor?
               (set! minor-gcs (add1 minor-gcs))
               (set! major-gcs (add1 major-gcs)))
           (let ([debug-GC? (log-level?* root-logger 'debug 'GC)]
                 [debug-GC:major? (and (not minor?)
                                       (log-level?* root-logger 'debug 'GC:major))])
             (when (or debug-GC? debug-GC:major?)
               (let ([delta (- pre-allocated post-allocated)]
                     [account-str (let ([proper (if (= post-cpu-time pre-cpu-time)
                                                    100
                                                    (quotient (* 100 (- proper-post-cpu-time pre-cpu-time))
                                                              (- post-cpu-time pre-cpu-time)))])
                                    (if (fx>= proper 99)
                                        ""
                                        (string-append "[" (number->string (fx- 100 proper)) "%]")))])
                 (let ([msg (chez:format "GC: 0:~a~a @ ~a(~a); free ~a(~a) ~ams~a @ ~a"
                                         (if minor? "min" "MAJ") gen
                                         (K "" pre-allocated) (K "+" (- pre-allocated+overhead pre-allocated))
                                         (K "" delta) (K "+" (- (- pre-allocated+overhead post-allocated+overhead)
                                                                delta))
                                         (- post-cpu-time pre-cpu-time)
                                         account-str
                                         pre-cpu-time)]
                       [data (make-gc-info (if minor? 'minor 'major) pre-allocated pre-allocated+overhead 0
                                           post-allocated post-allocated+overhead
                                           pre-cpu-time post-cpu-time
                                           pre-time post-time)]
                       [in-interrupt? #t])
                   (when debug-GC?
                     (log-message* root-logger 'debug 'GC msg data #f in-interrupt?))
                   (when debug-GC:major?
                     (log-message* root-logger 'debug 'GC:major msg data #f in-interrupt?)))))))))))

   (define (initialize-exit-handler!)
     (#%exit-handler
      (let ([orig (#%exit-handler)]
            [root-logger (current-logger)])
        (lambda (v)
          (when gcs-on-exit?
            (collect-garbage)
            (collect-garbage))
          (let ([info-GC? (log-level?* root-logger 'info 'GC)]
                [info-GC:major? (log-level?* root-logger 'info 'GC:major)])
            (when (or info-GC? info-GC:major?)
              (let* ([peak-mem (current-memory-use 'peak)]
                     [msg (chez:format "GC: 0:atexit peak ~a(~a); alloc ~a; major ~a; minor ~a; ~ams"
                                       (K "" peak-mem)
                                       (K "+" (- (maximum-memory-bytes) peak-mem))
                                       (K "" (- (+ (bytes-deallocated) (bytes-allocated)) (initial-bytes-allocated)))
                                       major-gcs
                                       minor-gcs
                                       (let ([t (sstats-gc-cpu (statistics))])
                                         (+ (* (time-second t) 1000)
                                            (quotient (time-nanosecond t) 1000000))))])
                (when info-GC?
                  (log-message root-logger 'info 'GC msg #f #f))
                (when info-GC:major?
                  (log-message root-logger 'info 'GC:major msg #f #f)))))
          (linklet-performance-report!)
          (custodian-shutdown-root-at-exit)
          (|#%app| orig v)))))

   (define stderr-logging
     (or stderr-logging-arg
         (let ([spec (getenv "PLTSTDERR")])
           (if spec
               (parse-logging-spec "stderr" spec "in PLTSTDERR environment variable" #f '(error))
               '(error)))))

   (define stdout-logging
     (or stdout-logging-arg
         (let ([spec (getenv "PLTSTDOUT")])
           (if spec
               (parse-logging-spec "stdout" spec "in PLTSTDOUT environment variable" #f '())
               '()))))

   (define syslog-logging
     (or syslog-logging-arg
         (let ([spec (getenv "PLTSYSLOG")])
           (if spec
               (parse-logging-spec "syslog" spec "in PLTSYSLOG environment variable" #f '())
               '()))))

   (define gcs-on-exit? (and (getenv "PLT_GCS_ON_EXIT") #t))

   (define config
     (let ()
       (when host-collects-dir
         (set-host-collects-dir! host-collects-dir))
       (when host-config-dir
         (set-host-config-dir! host-config-dir))
       (cond
         [(eq? init-collects-dir 'disable)
          (set-collects-dir! (build-path 'same))]
         [else
          (set-collects-dir! init-collects-dir)])
       (set-config-dir! init-config-dir)
       ;; Beware: using Racket I/O outside of the initial thread, but
       ;; enough is in place on startup to read a configuration file
       (read-installation-configuration-table)))

   (define (initialize-place!)
     (current-command-line-arguments remaining-command-line-arguments)
     (use-compiled-file-paths compiled-file-paths)
     (use-user-specific-search-paths user-specific-search-paths?)
     (load-on-demand-enabled load-on-demand?)
     (unless (eq? compile-target-machine (machine-type))
       (current-compile-target-machine compile-target-machine))
     (use-compiled-file-check init-compiled-file-check)
     (boot)
     (when (and stderr-logging
                (not (null? stderr-logging)))
       (apply add-stderr-log-receiver! (current-logger) stderr-logging))
     (when (and stdout-logging
                (not (null? stdout-logging)))
       (apply add-stdout-log-receiver! (current-logger) stdout-logging))
     (when (and syslog-logging
                (not (null? syslog-logging)))
       (apply add-syslog-log-receiver! (current-logger) syslog-logging))
     (when (eq? init-collects-dir 'disable)
       (use-collection-link-paths #f))
     (let ([name (get-installation-name config)])
       (unless (eq? init-collects-dir 'disable)
         (current-library-collection-links
          (find-library-collection-links config name))
         (current-library-collection-paths
          (find-library-collection-paths collects-pre-extra (reverse rev-collects-post-extra) config name)))
       (let ([roots (find-compiled-file-roots config)])
         (if compiled-roots-path-list-string
             (current-compiled-file-roots
              (let ([s (regexp-replace* "@[(]version[)]"
                                        compiled-roots-path-list-string
                                        (version))])
                (path-list-string->path-list s roots)))
             (current-compiled-file-roots roots)))))

   ;; Called when Racket is embedded in a larger application:
   (define (register-embedded-entry-info! escape)
     (let ([resume-k #f]) ;; to get back to Racket thread; expects a thunk
       ((call/cc ;; Scheme-level `call/cc` to escape Racket's thread-engine loop
         (lambda (init-resume-k)
           (set! resume-k init-resume-k)
           (set-top-level-value!
            'embedded-racket-entry-info
            ;; A vector of specific functions:
            (vector
             ;; Resume the main Racket thread to apply `proc` to `args`,
             ;; and return a list of result values; no exception handling
             ;; or other such protections
             (lambda (proc args)
               (call/cc ;; Scheme-level `call/cc` to escape engine loop
                (lambda (entry-point-k)
                  (resume-k
                   (lambda ()
                     (let-values ([vals (apply proc args)])
                       ((call/cc
                         (lambda (latest-resume-k)
                           (set! resume-k init-resume-k)
                           (entry-point-k vals))))))))))
             ;; Functions that are useful to apply and that
             ;; provide access to everything else:
             primitive-lookup
             eval
             dynamic-require
             namespace-require
             ;; bstr as #f => use path, start, and end
             ;; path as #f => find executable
             ;; end as #f => use file size
             (lambda (path start end bstr as-predefined?)
               (embedded-load start end bstr as-predefined? path)
               (when as-predefined?
                 (set! embedded-load-in-places (cons (list path start end bstr) embedded-load-in-places))))))
           (escape))))))

   (set-make-place-ports+fds! make-place-ports+fds)

   (set-prepare-for-place!
    (lambda ()
      ;; Force visit of modules to make sure that we don't end up
      ;; with a race later by trying to visit the module in a place:
      (call-with-system-wind
       (lambda ()
         (for-each (lambda (lib)
                     (#%$visit-library lib '() #f))
                   '((chezscheme)
                     (rumble)
                     (thread)
                     (io)
                     (regexp)
                     (schemify)
                     (linklet)
                     (expander)))
         ;; Only need to visit once (although multiple time is ok)
         (set-prepare-for-place! void)))))

   (set-place-get-inherit!
    (lambda ()
      (list (current-directory)
            (current-library-collection-paths)
            (current-library-collection-links)
            (current-compiled-file-roots))))

   (set-start-place!
    (lambda (pch mod sym in out err cust plumber inh)
      (io-place-init! in out err cust plumber)
      (regexp-place-init!)
      (expander-place-init!)
      (initialize-place!)
      (current-directory (list-ref inh 0))
      (current-library-collection-paths (list-ref inh 1))
      (current-library-collection-links (list-ref inh 2))
      (current-compiled-file-roots (list-ref inh 3))
      (let loop ([l (reverse embedded-load-in-places)])
        (unless (null? l)
          (let-values ([(path n m bstr) (apply values (car l))])
            (embedded-load n m bstr #t path))
          (loop (cdr l))))
      (lambda ()
        (let ([f (dynamic-require mod sym)])
          (|#%app| f pch)))))
   (set-destroy-place!
    (lambda ()
      (io-place-destroy!)))

   (let ([a (or addon-dir
                (getenv-bytes "PLTADDONDIR"))])
     (when a
       (set-addon-dir! (path->complete-path (->path a)))))
   (unless (eq? host-addon-dir 'inherit)
     (let ([a (or host-addon-dir
                  (getenv-bytes "PLTADDONDIR"))])
       (set-host-addon-dir! (and a (path->complete-path (->path a))))))

   (when (getenv "PLT_STATS_ON_BREAK")
     (keyboard-interrupt-handler
      (let ([orig (keyboard-interrupt-handler)])
        (lambda args
          (dump-memory-stats)
          (apply orig args)))))

   (when (getenv "PLT_MAX_COMPACT_GC")
     (in-place-minimum-generation 254))

   (let ([s (getenv "PLT_INCREMENTAL_GC")])
     (when (and s
                (>= (string-length s) 1)
                (#%memv (string-ref s 0) '(#\0 #\n #\N)))
       (set-incremental-collection-enabled! #f)))

   (when (getenv "PLTDISABLEGC")
     (collect-request-handler void))

   (let ([s (getenv "PLT_THREAD_QUANTUM")])
     ;; Setting the thread quantum is useful in probing for race conditions. The default quantum
     ;; is 100000. If it's made too small (on the order of 100), then a thread will use up its
     ;; quantum just checking for breaks as it is swapped in, and then it won't make any progress.
     (when s
       (let ([n (string->number s)])
         (when (and n (exact-nonnegative-integer? n))
           (set-schedule-quantum! n)))))

   (let ([build-stamp (or (hash-ref config 'build-stamp #f) "")])
     (unless (equal? build-stamp "")
       (set-build-stamp! build-stamp)))

   (when version?
     (#%display (banner)))

   (call/cc ; Chez Scheme's `call/cc`, used here to escape from the Racket-thread engine loop
    (lambda (entry-point-k)
      (call-in-main-thread
       (lambda ()
         (initialize-exit-handler!)
         (initialize-place!)

         (when (and make? (not (null? compiled-file-paths)))
           (|#%app|
            current-load/use-compiled
            (|#%app| (dynamic-require 'compiler/private/cm-minimal
                                      'make-compilation-manager-load/use-compiled-handler))))

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
             (let ([m (get-repl-init-filename config)])
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
           (|#%app| (executable-yield-handler) exit-value))

         (cond
          [embedded-interactive-mode?
           (register-embedded-entry-info!
            (lambda ()
              (entry-point-k exit-value)))]
          [else
           (exit exit-value)]))))))

 (define the-command-line-arguments
   (or (and (top-level-bound? 'bytes-command-line-arguments)
            (top-level-value 'bytes-command-line-arguments))
       (command-line-arguments)))

 (if (null? the-command-line-arguments)
     ;; Assume that we're running as a boot file
     (scheme-start (lambda args (run args)))
     ;; Assume that we're running as a script
     (run the-command-line-arguments)))
