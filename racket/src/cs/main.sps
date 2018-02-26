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
               find-main-config
               executable-yield-handler
               load-on-demand-enabled
               use-user-specific-search-paths
               eval
               read
               load
               dynamic-require
               namespace-require
               module-declared?
               module->language-info
               module-path-index-join
               version
               exit)
         (regexp)
         (io)
         (thread)
         (only (linklet)
               platform-independent-zo-mode?
               linklet-performance-init!
               linklet-performance-report!))

 (linklet-performance-init!)

 (define the-command-line-arguments
   (or (and (top-level-bound? 'bytes-command-line-arguments)
            (map (lambda (s) (bytes->string/locale s #\?))
                 (top-level-value 'bytes-command-line-arguments)))
       (command-line-arguments)))

 (unless (>= (length the-command-line-arguments) 5)
   (error 'racket "expected `self`, `collects`, and `libs` paths plus `segment-offset` and `is-gui?` to start"))
 (set-exec-file! (path->complete-path (car the-command-line-arguments)))
 (define init-collects-dir (let ([s (cadr the-command-line-arguments)])
                             (if (equal? s "") 'disable (string->path s))))
 (define init-config-dir (string->path (or (getenv "PLTCONFIGDIR")
                                           (caddr the-command-line-arguments))))
 (define segment-offset (#%string->number (list-ref the-command-line-arguments 3)))
 (define gracket? (string=? "true" (list-ref the-command-line-arguments 4)))

 (when (foreign-entry? "racket_exit")
   (#%exit-handler (foreign-procedure "racket_exit" (int) void)))

 (|#%app| use-compiled-file-paths
  (list (string->path (string-append "compiled/"
                                     (cond
                                      [(getenv "PLT_ZO_PATH")
                                       => (lambda (s)
                                            (unless (and (not (equal? s ""))
                                                         (relative-path? s))
                                              (error 'racket "PLT_ZO_PATH environment variable is not a valid path"))
                                            s)]
                                      [platform-independent-zo-mode? "cs"]
                                      [else (symbol->string (machine-type))])))))

 (define (see saw . args)
   (let loop ([saw saw] [args args])
     (if (null? args)
         saw
         (loop (hash-set saw (car args) #t) (cdr args)))))
 (define (saw? saw tag)
   (hash-ref saw tag #f))

 (define rx:logging-spec (pregexp "^[\\s]*(none|fatal|error|warning|info|debug)(?:@([^\\s @]+))?(.*)$"))
 (define rx:all-whitespace (pregexp "^[\\s]*$"))
 (define (parse-logging-spec str where exit-on-fail?)
   (define (fail)
     (let ([msg (string-append
                 "stderr <levels> " where " must be one of the following\n"
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
          (hash-ref (call-with-input-file
                     (build-path (find-main-config) "config.rktd")
                     read)
                    (if gracket? 'gui-interactive-file 'interactive-file)
                    #f)
          (if gracket? 'racket/interactive 'racket/gui/interactive)))
    (default-continuation-prompt-tag)
    (lambda args #f)))

 (define init-library (if gracket?
                          '(lib "racket/gui/init")
                          '(lib "racket/init")))
 (define loads '())
 (define repl? #f)
 (define repl-init? #t)
 (define version? #f)
 (define stderr-logging-arg #f)
 (define runtime-for-init? #t)
 (define exit-value 0)
 (define host-collects-dir init-collects-dir)
 (define host-config-dir init-config-dir)

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

 (define-syntax string-case
   ;; Assumes that `arg` is a variable
   (syntax-rules ()
     [(_ arg [else body ...])
      (let () body ...)]
     [(_ arg [(str ...) body ...] rest ...)
      (if (or (string=? arg str) ...)
          (let () body ...)
          (string-case arg rest ...))]))

 (let flags-loop ([args (list-tail the-command-line-arguments 5)]
                  [saw (hasheq)])
   ;; An element of `args` can become `(cons _arg _within-arg)`
   ;; due to splitting multiple flags with a single "-"
   (define (loop args) (flags-loop args saw))
   ;; Called to handle remaining non-switch arguments:
   (define (finish args saw)
     (cond
      [(and (pair? args)
            (not (saw? saw 'non-config)))
       (loop (cons "-u" args))]
      [else
       (|#%app| current-command-line-arguments (list->vector args))
       (when (and (null? args) (not (saw? saw 'non-config)))
         (set! repl? #t)
         (unless gracket?
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
             (set! loads
                   (cons
                    (lambda ()
                      (namespace-require+ `(lib ,lib-name)))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-t" "--require")
           (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (namespace-require+ `(file ,file-name)))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-u" "--script")
           (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (namespace-require+ `(file ,file-name)))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-f" "--load")
           (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (load file-name))
                    loads))
             (flags-loop rest-args (see saw 'non-config)))]
          [("-e" "--eval")
           (let-values ([(expr rest-args) (next-arg "expression" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (eval (read (open-input-string expr))))
                    loads))
             (flags-loop rest-args (see saw 'non-config)))]
          [("-i" "--repl") 
           (set! repl? #t)
           (set! version? #t)
           (flags-loop (cdr args) (see saw 'non-config 'top))]
          [("-n" "--no-lib")
           (set! init-library #f)
           (flags-loop (cdr args) (see saw 'non-config))]
          [("-v" "--version") 
           (set! version? #t)
           (flags-loop (cddr args) (see saw 'non-config))]
          [("-c" "--no-compiled")
           (|#%app| use-compiled-file-paths '())
           (loop (cdr args))]
          [("-I")
           (let-values ([(lib-name rest-args) (next-arg "library name" arg within-arg args)])
             (when init-library
               (set! init-library `(lib ,lib-name)))
             (loop rest-args))]
          [("-X" "--collects")
           (let-values ([(collects-path rest-args) (next-arg "collects path" arg within-arg args)])
             (cond
              [(equal? collects-path "")
               (set! init-collects-dir 'disable)]
              [else 
               (check-path-arg "collects path" arg within-arg)
               (set! init-collects-dir (path->complete-path (string->path collects-path)))])
             (loop rest-args))]
          [("-G" "--config")
           (let-values ([(config-path rest-args) (next-arg "config path" arg within-arg args)])
             (check-path-arg "config path" arg within-arg)
             (set! init-config-dir (path->complete-path (string->path config-path)))
             (loop rest-args))]
          [("-C" "--cross")
           (set! host-config-dir init-config-dir)
           (set! host-collects-dir init-collects-dir)
           (loop (cdr args))]
          [("-U" "--no-user-path")
           (|#%app| use-user-specific-search-paths #f)
           (loop (cdr args))]
          [("-d")
           (|#%app| load-on-demand-enabled #f)
           (loop (cdr args))]
          [("-q" "--no-init-file")
           (set! repl-init? #f)
           (loop (cdr args))]
          [("-W" "--stderr")
           (let-values ([(spec rest-args) (next-arg "stderr level" arg within-arg args)])
             (set! stderr-logging-arg (parse-logging-spec spec (format "after ~a switch" (or within-arg arg)) #t))
             (loop rest-args))]
          [("-N" "--name")
           (let-values ([(name rest-args) (next-arg "name" arg within-arg args)])
             (set-run-file! (string->path name))
             (loop rest-args))]
          [("--")
           (cond
            [(or (null? (cdr args)) (not (pair? (cadr args))))
             (finish (cdr args) saw)]
            [else
             ;; Need to handle more switches from a combined flag
             (loop (cons (cadr args) (cons (car args) (cddr args))))])]
          [else
           (cond
            [(eqv? (string-ref arg 0) #\-)
             (cond
              [(and (> (string-length arg) 2)
                    (not (eqv? (string-ref arg 1) #\-)))
               ;; Split flags
               (loop (append (map (lambda (c) (cons (string #\- c) arg))
                                  (cdr (string->list arg)))
                             (cdr args)))]
              [else
               (raise-user-error 'racket "bad switch: ~a~a"
                                 arg
                                 (if within-arg
                                     (format " within: ~a" within-arg)
                                     ""))])]
            [else
             ;; Non-flag argument
             (finish args saw)])]))))

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
        (let ([debug-GC? (log-level? root-logger 'debug 'GC)])
          (when (or debug-GC?
                    (and (not minor?)
                         (log-level? root-logger 'debug 'GC:major)))
            (let ([delta (- pre-allocated post-allocated)])
              (log-message root-logger 'debug (if debug-GC? 'GC 'GC:major)
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
                           #f))))))))
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
      (|#%app| orig v))))

 (define stderr-logging
   (or stderr-logging-arg
       (let ([spec (getenv "PLTSTDERR")])
         (if spec
             (parse-logging-spec spec "in PLTSTDERR environment variable" #f)
             '(error)))))

 (when (getenv "PLT_STATS_ON_BREAK")
   (keyboard-interrupt-handler
    (let ([orig (keyboard-interrupt-handler)])
      (lambda args
        (dump-memory-stats)
        (apply orig args)))))

 (when version?
   (printf "Welcome to Racket v~a [cs]\n" (version)))
 (call-in-main-thread
  (lambda ()
    (boot)
    (when (and stderr-logging
               (not (null? stderr-logging)))
      (apply add-stderr-log-receiver! (|#%app| current-logger) stderr-logging))
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
       (find-library-collection-paths)))

   (when init-library
     (namespace-require+ init-library))

   (for-each (lambda (ld) (ld))
             (reverse loads))

   (when repl?
     (when repl-init?
       (let ([m (get-repl-init-filename)])
         (when m
           (call-with-continuation-prompt
            (lambda () (dynamic-require m 0))
            (default-continuation-prompt-tag)
            (lambda args (set! exit-value 1))))))
     (|#%app| (if gracket?
                  (dynamic-require 'racket/gui/init 'graphical-read-eval-print-loop)
                  (dynamic-require 'racket/base 'read-eval-print-loop)))
     (unless gracket?
       (newline)))

   (|#%app| (|#%app| executable-yield-handler) 0)

   (exit exit-value))))
