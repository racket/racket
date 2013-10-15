#lang racket
(require racket/file
         racket/runtime-path
         "job-queue.rkt"
         "metadata.rkt"
         "run-collect.rkt"
         "cache.rkt"
         "dirstruct.rkt"
         "replay.rkt"
         "notify.rkt"
         "path-utils.rkt"
         "sema.rkt"
         "scm.rkt")

(define current-env (make-parameter (make-immutable-hash empty)))
(define-syntax-rule (with-env ([env-expr val-expr] ...) expr ...)
  (parameterize ([current-env
                  (for/fold ([env (current-env)])
                      ([k (in-list (list env-expr ...))]
                       [v (in-list (list val-expr ...))])
                    (hash-set env k v))])
    expr ...))

(define (build-revision rev)
  (define rev-dir (revision-dir rev))
  (define co-dir (revision-trunk-dir rev))
  (define log-dir (revision-log-dir rev))
  (define trunk-dir (revision-trunk-dir rev))  
  ;; Checkout the repository revision
  (cache/file/timestamp
   (build-path rev-dir "checkout-done")
   (lambda ()
     (notify! "Removing checkout directory: ~a" co-dir)
     (safely-delete-directory co-dir)
     (local [(define repo (plt-repository))
             (define to-dir
               (path->string co-dir))]
            (notify! "Checking out ~a@~a into ~a"
                     repo rev to-dir)
            (scm-export-repo rev repo to-dir))))
  (parameterize ([current-directory co-dir])
    (with-env
     (["PLT_SETUP_OPTIONS" (format "-j ~a" (number-of-cpus))])
     (run/collect/wait/log
      #:timeout (current-make-install-timeout-seconds)
      #:env (current-env)
      (build-path log-dir "pkg-src" "build" "make")
      (make-path)
      (list "-j" (number->string (number-of-cpus))))))
  (run/collect/wait/log
   #:timeout (current-make-install-timeout-seconds)
   #:env (current-env)
   (build-path log-dir "pkg-src" "build" "archive")
   (tar-path)
   (list "-czvf"
         (path->string (revision-trunk.tgz rev))
         "-C" (path->string rev-dir)
         "trunk")))

(define (call-with-temporary-directory thunk)
  (define tempdir (symbol->string (gensym 'tmpdir)))
  (dynamic-wind
      (lambda ()
        (make-directory* tempdir))
      (lambda ()
        (parameterize ([current-directory tempdir])
          (thunk)))
      (lambda ()
        (delete-directory/files tempdir))))
(define-syntax-rule (with-temporary-directory e)
  (call-with-temporary-directory (lambda () e)))

(define-syntax-rule
  (define-with-temporary-planet-directory with-temporary-planet-directory env-str)
  (begin
    (define (call-with-temporary-planet-directory thunk)
      (define tempdir
        (build-path (current-directory)
                    (symbol->string (gensym 'planetdir))))
      (dynamic-wind
          (lambda ()
            (make-directory* tempdir))
          (lambda ()
            (with-env ([env-str (path->string tempdir)])
                      (thunk)))
          (lambda ()
            (delete-directory/files tempdir))))
    (define-syntax-rule (with-temporary-planet-directory e)
      (call-with-temporary-planet-directory (lambda () e)))))
(define-with-temporary-planet-directory with-temporary-planet-directory "PLTPLANETDIR")
(define-with-temporary-planet-directory with-temporary-tmp-directory "TMPDIR")

(define (call-with-temporary-home-directory thunk)
  (define new-dir
    (make-temporary-file
     "home~a"
     'directory
     (current-temporary-directory)))
  (dynamic-wind
      (lambda ()
        (with-handlers ([exn:fail? void])
          (copy-directory/files
           (hash-ref (current-env) "HOME")
           new-dir)))
      (lambda ()
        (with-env (["HOME" (path->string new-dir)])
                  (thunk)))
      (lambda ()
        (delete-directory/files new-dir))))
(define-syntax-rule (with-temporary-home-directory e)
  (call-with-temporary-home-directory (lambda () e)))

(define (with-running-program command args thunk)
  (if command
    (let ()
      (define-values (new-command new-args)
        (command+args+env->command+args
         #:env (current-env)
         command args))
      (define-values
        (the-process _stdout stdin _stderr)
        (parameterize ([subprocess-group-enabled #t])
          (apply subprocess
                 (current-error-port)
                 #f
                 (current-error-port)
                 new-command new-args)))
      ;; Die if this program does
      (define parent
        (current-thread))
      (define waiter
        (thread
         (lambda ()
           (subprocess-wait the-process)
           (eprintf "Killing parent because wrapper (~a) is dead...\n" (list* command args))
           (kill-thread parent))))

      ;; Run without stdin
      (close-output-port stdin)

      (dynamic-wind
          void
          ;; Run the thunk
          thunk
          (λ ()
            ;; Close the output ports
            ;;(close-input-port stdout)
            ;;(close-input-port stderr)

            ;; Kill the guard
            (kill-thread waiter)

            ;; Kill the process
            (subprocess-kill the-process #f)
            (sleep)
            (subprocess-kill the-process #t))))
    (thunk)))

(define-runtime-path package-list "pkgs")
(define (planet-packages)
  (file->value package-list))

(define (test-revision rev)
  (define rev-dir (revision-dir rev))
  (define trunk-dir
    (revision-trunk-dir rev))
  (define log-dir
    (revision-log-dir rev))
  (define trunk->log
    (rebase-path trunk-dir log-dir))
  (define racket-path
    (path->string (build-path trunk-dir "racket" "bin" "racket")))
  (define raco-path
    (path->string (build-path trunk-dir "racket" "bin" "raco")))
  ;; XXX Remove
  (define mzc-path
    (path->string (build-path trunk-dir "racket" "bin" "mzc")))
  (define gracket-path
    (path->string (build-path trunk-dir "racket" "bin" "gracket")))
  (define gui-workers (make-job-queue 1))
  (define test-workers (make-job-queue (number-of-cpus)))

  (define pkgs-pths
    (list (build-path trunk-dir "racket" "collects")
          (build-path trunk-dir "pkgs")))
  (define (test-directory dir-pth upper-sema)
    (define dir-log (build-path (trunk->log dir-pth) ".index.test"))
    (cond
      [(read-cache* dir-log)
       (semaphore-post upper-sema)]
      [else
       (notify! "Testing in ~S" dir-pth)
       (define files/unsorted (directory-list* dir-pth))
       (define dir-sema (make-semaphore 0))
       (define files
         (sort files/unsorted <
               #:key (λ (p)
                       (if (bytes=? #"tests" (path->bytes p))
                         0
                         1))
               #:cache-keys? #t))
       (for ([sub-pth (in-list files)])
         (define pth (build-path dir-pth sub-pth))
         (define directory? (directory-exists? pth))
         (cond
           [directory?
            ;; XXX do this in parallel?
            (test-directory pth dir-sema)]
           [else
            (define log-pth (trunk->log pth))
            (cond
              [(file-exists? log-pth)
               (semaphore-post dir-sema)]
              [else
               (define pth-timeout
                 (or (path-timeout pth)
                     (current-subprocess-timeout-seconds)))
               (define pth-cmd/general
                 (path-command-line pth))
               (define-values
                 (pth-cmd the-queue)
                 (match pth-cmd/general
                   [#f
                    (values #f #f)]
                   [(list-rest (or 'mzscheme 'racket) rst)
                    (values
                     (lambda (k)
                       (k (list* racket-path rst)))
                     test-workers)]
                   [(list-rest 'mzc rst)
                    (values
                     (lambda (k) (k (list* mzc-path rst)))
                     test-workers)]
                   [(list-rest 'raco rst)
                    (values
                     (lambda (k) (k (list* raco-path rst)))
                     test-workers)]
                   [(list-rest (or 'mred 'mred-text
                                   'gracket 'gracket-text)
                               rst)
                    (values
                     (if (on-unix?)
                       (lambda (k)
                         (k
                          (list* gracket-path
                                 "-display"
                                 (format
                                  ":~a"
                                  (cpu->child
                                   (current-worker)))
                                 rst)))
                       #f)
                     gui-workers)]
                   [_
                    (values #f #f)]))
               (cond
                 [pth-cmd
                  (submit-job!
                   the-queue
                   (lambda ()
                     (dynamic-wind
                         void
                         (λ ()
                           (pth-cmd
                            (λ (l)
                              (with-env
                               (["DISPLAY"
                                 (format ":~a"
                                         (cpu->child
                                          (current-worker)))])
                               (with-temporary-tmp-directory
                                (with-temporary-planet-directory
                                 (with-temporary-home-directory
                                  (with-temporary-directory
                                   (run/collect/wait/log
                                    log-pth
                                    #:timeout pth-timeout
                                    #:env (current-env)
                                    (first l)
                                    (rest l))))))))))
                         (λ ()
                           (semaphore-post dir-sema)))))]
                 [else
                  (semaphore-post dir-sema)])])]))
       (thread
        (lambda ()
          (define how-many (length files))
          (semaphore-wait* dir-sema how-many)
          (notify! "Done with dir: ~a" dir-pth)
          (write-cache! dir-log (current-seconds))
          (semaphore-post upper-sema)))]))
  ;; Some setup
  (for ([pp (in-list (planet-packages))])
    (match pp
      [`(,auth ,pkg ,majn ,minn ,ver)
       (define maj (number->string majn))
       (define min (number->string minn))
       (run/collect/wait/log
        ;; XXX Give it its own timeout
        #:timeout (current-make-install-timeout-seconds)
        #:env (current-env)
        (build-path log-dir "planet" auth pkg maj min)
        raco-path
        (list "planet" "install" auth pkg maj min))]))
  (run/collect/wait/log
   #:timeout (current-subprocess-timeout-seconds)
   #:env (current-env)
   (build-path log-dir "pkg-src" "build" "set-browser.rkt")
   racket-path
   (list "-t"
         (path->string*
          (build-path (drdr-directory) "set-browser.rkt"))))
  ;; And go
  (define (test-directories ps upper-sema)
    (define list-sema (make-semaphore 0))
    (define how-many 
      (for/fold ([cnt 0]) ([p (in-list ps)])
        (if (directory-exists? p)
          (begin (test-directory p list-sema)
                 (add1 cnt))
          cnt)))
    (and (not (zero? how-many))
         (thread
          (lambda ()
            (semaphore-wait* list-sema how-many)
            (semaphore-post upper-sema)))))

  (define top-sema (make-semaphore 0))
  (notify! "Starting testing")
  (when (test-directories pkgs-pths top-sema)
    (notify! "All testing scheduled... waiting for completion")
    (sync
     top-sema
     (handle-evt
      (alarm-evt 
       (+ (current-inexact-milliseconds)
          (* 1000 (* 2 (current-make-install-timeout-seconds)))))
      (λ _
        (kill-thread (current-thread))))))
  (notify! "Stopping testing")
  (stop-job-queue! test-workers)
  (stop-job-queue! gui-workers))

(define (recur-many i r f)
  (if (zero? i)
    (f)
    (r (sub1 i) (lambda ()
                  (recur-many (sub1 i) r f)))))

(define XSERVER-OFFSET 20)
(define ROOTX XSERVER-OFFSET)
(define (cpu->child cpu-i)
  ROOTX
  #;
  (+ XSERVER-OFFSET cpu-i 1))

(define (remove-X-locks tmp-dir i)
  (for ([dir (in-list (list "/tmp" tmp-dir))])
    (safely-delete-directory
     (build-path dir (format ".X~a-lock" i)))
    (safely-delete-directory
     (build-path dir ".X11-unix" (format ".X~a-lock" i)))
    (safely-delete-directory
     (build-path dir (format ".tX~a-lock" i)))))

(define (integrate-revision rev)
  (define test-dir
    (build-path (revision-dir rev) "test"))
  (define planet-dir
    (build-path test-dir "planet"))
  (define home-dir
    (build-path test-dir "home"))
  (define tmp-dir
    (build-path test-dir "tmp"))
  (define trunk-dir
    (revision-trunk-dir rev))
  (cache/file/timestamp
   (build-path (revision-dir rev) "integrated")
   (lambda ()
     (make-directory* test-dir)
     (make-directory* planet-dir)
     (make-directory* home-dir)
     (make-directory* tmp-dir)
     ;; We are running inside of a test directory so that random files are stored there
     (parameterize ([current-directory test-dir]
                    [current-temporary-directory tmp-dir]
                    [current-rev rev])
       (with-env (["PLTSTDERR" "error"]
                  ["GIT_DIR" (path->string (plt-repository))]
                  ["TMPDIR" (path->string tmp-dir)]
                  ["PLTDRDR" "yes"]
                  ["PATH"
                   (format "~a:~a"
                           (path->string
                            (build-path trunk-dir "bin"))
                           (getenv "PATH"))]
                  ["PLTPLANETDIR" (path->string planet-dir)]
                  ["HOME" (path->string home-dir)])
                 (unless (read-cache* (revision-commit-msg rev))
                   (write-cache! (revision-commit-msg rev)
                                 (get-scm-commit-msg rev (plt-repository))))
                 (when (build?)
                   (build-revision rev))

                 (define (start-x-server i inner)
                   (notify! "Starting X server #~a" i)
                   (remove-X-locks tmp-dir i)
                   (with-running-program
                    "/usr/bin/Xorg" (list (format ":~a" i))
                    (lambda ()
                      (with-env
                       (["DISPLAY" (format ":~a" i)])
                       (sleep 2)
                       (notify! "Starting WM #~a" i)
                       (with-running-program
                        (fluxbox-path)
                        (list "-d" (format ":~a" i)
                              "--sm-disable"
                              "--no-composite")
                        inner)))))

                 (start-x-server
                  ROOTX
                  (lambda ()
                    (sleep 2)
                    (notify! "Starting test of rev ~a" rev)
                    (test-revision rev)))))
     ;; Remove the test directory
     (safely-delete-directory test-dir))))

(provide/contract
 [integrate-revision (exact-nonnegative-integer? . -> . void)])
