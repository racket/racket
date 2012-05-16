#lang racket
(require racket/file
         racket/runtime-path
         (planet jaymccarthy/job-queue)
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
  (define src-dir (build-path co-dir "src"))
  (define build-dir (build-path src-dir "build"))
  (define futures-build-dir (build-path src-dir "futures-build"))
  (define log-dir (revision-log-dir rev))
  (define trunk-dir
    (revision-trunk-dir rev))
  (define setup-plt-path
    (path->string (build-path trunk-dir "bin" "setup-plt")))
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
  ;; Make the build directory
  (make-directory* build-dir)
  ;; Run Configure, Make, Make Install
  (parameterize ([current-directory build-dir])
    (run/collect/wait/log
     #:timeout (current-subprocess-timeout-seconds)
     #:env (current-env)
     (build-path log-dir "src" "build" "configure")
     (path->string (build-path src-dir "configure"))
     empty)
    (run/collect/wait/log
     #:timeout (current-make-timeout-seconds)
     #:env (current-env)
     (build-path log-dir "src" "build" "make")
     (make-path) 
     (list "-j" (number->string (number-of-cpus))))
    (with-env 
        (["PLT_SETUP_OPTIONS" (format "-j ~a" (number-of-cpus))])
      (run/collect/wait/log
       #:timeout (current-make-install-timeout-seconds)
       #:env (current-env)
       (build-path log-dir "src" "build" "make-install")
       (make-path) 
       (list "-j" (number->string (number-of-cpus)) "install"))))
  (run/collect/wait/log
   #:timeout (current-make-install-timeout-seconds)
   #:env (current-env)
   (build-path log-dir "src" "build" "archive")
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

(define (call-with-temporary-planet-directory thunk)
  (define tempdir 
    (build-path (current-directory)
                (symbol->string (gensym 'planetdir))))
  (dynamic-wind
   (lambda ()
     (make-directory* tempdir))
   (lambda ()
     (with-env (["PLTPLANETDIR" (path->string tempdir)])
               (thunk)))
   (lambda ()
     (delete-directory/files tempdir))))
(define-syntax-rule (with-temporary-planet-directory e)
  (call-with-temporary-planet-directory (lambda () e)))

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
        ; Die if this program does
        (define parent
          (current-thread))
        (define waiter
          (thread
           (lambda ()
             (subprocess-wait the-process)
             (printf "Killing parent because wrapper is dead...\n")
             (kill-thread parent))))
        
        ; Run without stdin
        (close-output-port stdin)
        
        (dynamic-wind
         void
         ; Run the thunk
         thunk
         (λ ()
           ; Close the output ports
           #;(close-input-port stdout)
           #;(close-input-port stderr)
           
           ; Kill the guard
           (kill-thread waiter)
           
           ; Kill the process
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
    (path->string (build-path trunk-dir "bin" "racket")))
  (define raco-path
    (path->string (build-path trunk-dir "bin" "raco")))
  ; XXX Remove
  (define mzc-path
    (path->string (build-path trunk-dir "bin" "mzc")))
  (define gracket-path
    (path->string (build-path trunk-dir "bin" "gracket")))
  (define collects-pth
    (build-path trunk-dir "collects"))
  ; XXX Use a single GUI thread so that other non-GUI apps can run in parallel
  (define gui-lock (make-semaphore 1))
  (define test-workers (make-job-queue (number-of-cpus)))
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
               (define pth-cmd
                 (match pth-cmd/general
                   [#f
                    #f]
                   [(list-rest (or 'mzscheme 'racket) rst)
                    (lambda (k) 
                      (k (list* racket-path rst)))]
                   [(list-rest 'mzc rst)
                    (lambda (k) (k (list* mzc-path rst)))]
                   [(list-rest 'raco rst)
                    (lambda (k) (k (list* raco-path rst)))]
                   [(list-rest (or 'mred 'mred-text
                                   'gracket 'gracket-text)
                               rst)
                    (if (on-unix?)
                        (lambda (k) 
                          (call-with-semaphore 
                           gui-lock
                           (λ ()
                             (k
                              (list* gracket-path 
                                     "-display" 
                                     (format
                                      ":~a"
                                      (cpu->child
                                       (current-worker)))
                                     rst)))))
                        #f)]
                   [_
                    #f]))               
               (cond
                 [pth-cmd
                  (submit-job!
                   test-workers
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
                               (with-temporary-planet-directory
                                (with-temporary-home-directory
                                 (with-temporary-directory
                                  (run/collect/wait/log
                                   log-pth 
                                   #:timeout pth-timeout
                                   #:env (current-env)
                                   (first l)
                                   (rest l)))))))))
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
  ; Some setup
  (for ([pp (in-list (planet-packages))])
    (match pp
      [`(,auth ,pkg ,majn ,minn ,ver)
       (define maj (number->string majn))
       (define min (number->string minn))
       (run/collect/wait/log 
        ; XXX Give it its own timeout
        #:timeout (current-make-install-timeout-seconds)
        #:env (current-env)
        (build-path log-dir "planet" auth pkg maj min)
        raco-path 
        (list "planet" "install" auth pkg maj min))]))
  (run/collect/wait/log 
   #:timeout (current-subprocess-timeout-seconds)
   #:env (current-env)
   (build-path log-dir "src" "build" "set-browser.rkt")
   racket-path 
   (list "-t" 
         (path->string*
          (build-path (drdr-directory) "set-browser.rkt"))))
  ; And go
  (define top-sema (make-semaphore 0))
  (notify! "Starting testing")
  (when (directory-exists? collects-pth)
    (test-directory collects-pth top-sema)
    (notify! "All testing scheduled... waiting for completion")
    (semaphore-wait top-sema))
  (notify! "Stopping testing")
  (stop-job-queue! test-workers))

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
     ; We are running inside of a test directory so that random files are stored there
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
               (sleep 2)
               (notify! "Starting fluxbox #~a" i)
               (with-running-program
                   (fluxbox-path) 
                 (list "-display"
                       (format ":~a" i)
                       "-rc" "/home/pltdrdr/.fluxbox/init")
                 inner))))
         
         (start-x-server 
          ROOTX 
          (lambda ()
            (sleep 2)
            (notify! "Starting test of rev ~a" rev)
            (test-revision rev)))))
     ; Remove the test directory
     (safely-delete-directory test-dir))))

(provide/contract
 [integrate-revision (exact-nonnegative-integer? . -> . void)])
