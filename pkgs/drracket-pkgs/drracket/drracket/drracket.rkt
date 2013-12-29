#lang racket/base
(require racket/gui/base "private/key.rkt")

(module test racket/base)

(define debugging? (getenv "PLTDRDEBUG"))
(define profiling? (getenv "PLTDRPROFILE"))

(define first-parallel? (getenv "PLTDRPAR"))

(define repl (getenv "PLTDRREPL"))

(define install-cm? (and (not debugging?)
                         (getenv "PLTDRCM")))

(define cm-trace? (or (equal? (getenv "PLTDRCM") "trace")
                      (equal? (getenv "PLTDRDEBUG") "trace")
                      (equal? (getenv "PLTDRPAR") "trace")))

;; the flush is only here to ensure that the output is 
;; appears when running in cygwin under windows.
(define (flprintf fmt . args)
  (apply printf fmt args)
  (flush-output))

(define (run-trace-thread)
  (let ([evt (make-log-receiver (current-logger) 'info)])
    (void
     (thread
      (λ ()
        (let loop ()
          (define vec (sync evt))
          (define str (vector-ref vec 1))
          (when (regexp-match #rx"^cm: *compil(ing|ed)" str)
            (display str)
            (newline))
          (loop)))))))

(cond
  [debugging?
   (flprintf "PLTDRDEBUG: loading CM to load/create errortrace zos\n")
   (let-values ([(zo-compile
                  make-compilation-manager-load/use-compiled-handler)
                 (parameterize ([current-namespace (make-base-empty-namespace)]
                                [use-compiled-file-paths '()])
                   (values
                    (dynamic-require 'errortrace/zo-compile 'zo-compile)
                    (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)))])
     (flprintf "PLTDRDEBUG: installing CM to load/create errortrace zos\n")
     (current-compile zo-compile)
     (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
     (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
     (error-display-handler (dynamic-require 'errortrace/errortrace-lib
                                             'errortrace-error-display-handler))
     (when cm-trace?
       (flprintf "PLTDRDEBUG: enabling CM tracing\n")
       (run-trace-thread)))]
  [install-cm?
   (flprintf "PLTDRCM: loading compilation manager\n")
   (let ([make-compilation-manager-load/use-compiled-handler
          (parameterize ([current-namespace (make-base-empty-namespace)])
            (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler))])
     (flprintf "PLTDRCM: installing compilation manager\n")
     (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
     (when cm-trace?
       (flprintf "PLTDRCM: enabling CM tracing\n")
       (run-trace-thread)))]
  [first-parallel?
   (flprintf "PLTDRPAR: loading compilation manager\n")
   (define tools? (not (getenv "PLTNOTOOLS")))
   (define (files-in-coll coll)
     (define dir (collection-path coll))
     (map (λ (x) (build-path dir x)) 
          (filter
           (λ (x) (regexp-match #rx"rkt$" (path->string x)))
           (directory-list dir))))
   (define (randomize lst)
     (define vec (make-vector (length lst) #f))
     (let loop ([i 0]
                [lst lst])
       (cond
         [(= i (vector-length vec)) (void)]
         [else
          (define index (random (- (vector-length vec) i)))
          (define ele (list-ref lst index))
          (vector-set! vec i ele)
          (loop (+ i 1) (remq ele lst))]))
     (vector->list vec))
   
   (define (tool-files id)
     (apply 
      append
      (map
       (λ (x)
         (define proc (get-info/full x))
         (if proc
             (map (λ (dirs)
                    (apply build-path 
                           x
                           (if (list? dirs)
                               dirs
                               (list dirs))))
                  (proc id (λ () '())))
             '()))
       (find-relevant-directories (list id)))))
   
   (define make-compilation-manager-load/use-compiled-handler
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)))
   (when cm-trace?
     (flprintf "PLTDRPAR: enabling CM tracing\n")
     (run-trace-thread))
   (flprintf "PLTDRPAR: loading setup/parallel-build\n")
   (define-values (parallel-compile-files get-info/full find-relevant-directories)
     (parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
       (values (dynamic-require 'setup/parallel-build 'parallel-compile-files)
               (and tools? (dynamic-require 'setup/getinfo 'get-info/full))
               (and tools? (dynamic-require 'setup/getinfo 'find-relevant-directories)))))
   (if tools?
       (flprintf "PLTDRPAR: parallel compile of framework, drracket, and tools\n")
       (flprintf "PLTDRPAR: parallel compile of framework and drracket\n"))
   
   (parallel-compile-files (randomize (append (files-in-coll "drracket") 
                                              (files-in-coll "framework")
                                              (if tools?
                                                  (append (tool-files 'drracket-tools)
                                                          (tool-files 'tools))
                                                  '())))
                           #:handler
                           (λ (handler-type path msg out err)
                             (case handler-type
                               [(done) 
                                (when cm-trace?
                                  (printf "PLTDRPAR: made ~a\n" path))]
                               [else
                                (printf "~a\n" msg)
                                (printf "stdout from compiling ~a:\n~a\n" path out)
                                (flush-output)
                                (eprintf "stderr from compiling ~a:\n~a\n" path err)])))
   (flprintf "PLTDRPAR: installing compilation manager\n")
   (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))])

(when profiling?
  (flprintf "PLTDRPROFILE: installing profiler\n")
  ;; NOTE that this might not always work.
  ;; it creates a new custodian and installs it, but the
  ;; original eventspace was created on the original custodian
  ;; and this code does not create a new eventspace. 
  (let ([orig-cust (current-custodian)]
        [orig-eventspace (current-eventspace)]
        [new-cust (make-custodian)])
    (current-custodian new-cust)
    ((dynamic-require 'drracket/private/profile-drs 'start-profile) orig-cust)))

(dynamic-require 'drracket/private/drracket-normal #f)

(when repl
  (printf "Welcome to DrRacket, v~a\n" (version))
  (namespace-require 'racket)
  (namespace-require 'drracket/tool-lib)
  (unless (equal? repl "-q")
    (define init-file (find-system-path 'init-file))
    (when (file-exists? init-file)
      (load init-file)))
  (void (thread read-eval-print-loop)))

