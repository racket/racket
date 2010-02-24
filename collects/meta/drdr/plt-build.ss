#lang scheme
(require scheme/file
         scheme/runtime-path
         (planet jaymccarthy/job-queue)
         "metadata.ss"
         "run-collect.ss"
         "cache.ss"
         "dirstruct.ss"
         "notify.ss"
         "path-utils.ss"
         "svn.ss")

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
     ; XXX Give it its own timeout
     (parameterize ([current-subprocess-timeout-seconds (current-make-install-timeout-seconds)])
       (svn-checkout
        (plt-repository) rev
        (path->string co-dir)))))
  ;; Make the build directory
  (make-directory* build-dir)
  ;; Run Configure, Make, Make Install
  (parameterize ([current-directory build-dir])
    (run/collect/wait/log
     (build-path log-dir "src" "build" "configure")
     (path->string (build-path src-dir "configure")))
    (parameterize ([current-subprocess-timeout-seconds (current-make-timeout-seconds)])
      (run/collect/wait/log
       (build-path log-dir "src" "build" "make")
       (make-path) "-j" (number->string (number-of-cpus))))
    (parameterize ([current-subprocess-timeout-seconds (current-make-install-timeout-seconds)])
      (run/collect/wait/log
       (build-path log-dir "src" "build" "make-install")
       (make-path) "-j" (number->string (number-of-cpus)) "install"))
    #;(parameterize ([current-subprocess-timeout-seconds (current-make-install-timeout-seconds)])
        (run/collect/wait/log
         (build-path log-dir "src" "build" "setup-plt-no-docs")
         setup-plt-path "--no-docs"))
    #;(parameterize ([current-subprocess-timeout-seconds (current-make-install-timeout-seconds)])
        (run/collect/wait/log
         (build-path log-dir "src" "build" "setup-plt")
         setup-plt-path)))
  ;; Test Futures
  (make-directory* futures-build-dir)
  ;; Run Configure, Make, Test
  (parameterize ([current-directory futures-build-dir])
    (run/collect/wait/log
     (build-path log-dir "src" "futures-build" "configure")
     (path->string (build-path src-dir "configure")) "--enable-futures")
    (parameterize ([current-subprocess-timeout-seconds (current-make-timeout-seconds)]
                   [current-directory (build-path futures-build-dir "mzscheme")])
      (run/collect/wait/log
       (build-path log-dir "src" "futures-build" "mzscheme" "make")
       (make-path) "-j" (number->string (number-of-cpus)))
      (run/collect/wait/log
       (build-path log-dir "src" "futures-build" "mzscheme" "futures-startup-test")
       (path->string (build-path futures-build-dir "mzscheme" "mzscheme3m")) "-e" "(printf \"startedup\n\")")
      )))

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

(define (semaphore-wait* sema how-many)
  (unless (zero? how-many)
    (semaphore-wait sema)
    (semaphore-wait* sema (sub1 how-many))))

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
  (define mzscheme-path
    (path->string (build-path trunk-dir "bin" "mzscheme")))
  (define mzc-path
    (path->string (build-path trunk-dir "bin" "mzc")))
  (define mred-text-path
    (path->string (build-path trunk-dir "bin" "mred-text")))
  (define mred-path
    (path->string (build-path trunk-dir "bin" "mred")))
  (define planet-path
    (path->string (build-path trunk-dir "bin" "planet")))
  (define collects-pth
    (build-path trunk-dir "collects"))
  (define test-workers (make-job-queue (number-of-cpus)))
  (define top-sema (make-semaphore 0))
  (define (test-directory dir-pth upper-sema)
    (define dir-log (build-path (trunk->log dir-pth) ".index.test"))
    (if (read-cache* dir-log)
        (semaphore-post upper-sema)
        (begin        
          (notify! "Testing in ~S" dir-pth)
          (local [(define files (directory-list* dir-pth))
                  (define how-many (length files))
                  (define dir-sema (make-semaphore 0))]     
            (for-each (lambda (sub-pth)
                        (define pth (build-path dir-pth sub-pth))
                        (define directory? (directory-exists? pth))
                        (if directory?
                            (test-directory pth dir-sema)
                            (local [(define log-pth (trunk->log pth))]
                              (if (file-exists? log-pth)
                                  (semaphore-post dir-sema)
                                  (local [(define pth-timeout (path-timeout pth))
                                          (define pth-cmd/general (path-command-line pth))
                                          (define pth-cmd
                                            (match pth-cmd/general
                                              [#f
                                               #f]
                                              [(list-rest "mzscheme" rst)
                                               (lambda () (list* mzscheme-path rst))]
                                              [(list-rest "mzc" rst)
                                               (lambda () (list* mzc-path rst))]
                                              [(list-rest "mred-text" rst)
                                               (lambda () (list* mred-text-path "-display" (format ":~a" (+ XSERVER-OFFSET (current-worker))) rst))]
                                              [(list-rest "mred" rst)
                                               (lambda () (list* mred-path "-display" (format ":~a" (+ XSERVER-OFFSET (current-worker))) rst))]
                                              [_
                                               #f]))]       
                                    (if pth-cmd
                                        (submit-job!
                                         test-workers
                                         (lambda ()
                                           ; XXX Maybe this should destroy the old home and copy in a new one
                                           ;     Otherwise it is a source of randomness
                                           (with-temporary-directory
                                               (parameterize ([current-subprocess-timeout-seconds pth-timeout])
                                                 (apply run/collect/wait/log log-pth 
                                                        "/usr/bin/env"
                                                        (format "DISPLAY=~a"
                                                                (format ":~a" (+ XSERVER-OFFSET (current-worker))))
                                                        (format "HOME=~a"
                                                                (home-dir (current-worker)))
                                                        (pth-cmd))))
                                           (semaphore-post dir-sema)))
                                        (semaphore-post dir-sema)))))))
                      files)
            (thread
             (lambda ()
               (semaphore-wait* dir-sema how-many)
               (notify! "Done with dir: ~a" dir-pth)
               (write-cache! dir-log (current-seconds))
               (semaphore-post upper-sema)))))))
  ; Some setup
  ; XXX Give it its own timeout
  (parameterize ([current-subprocess-timeout-seconds (current-make-install-timeout-seconds)])
    (for ([pp (in-list (planet-packages))])
      (match pp
        [`(,auth ,pkg ,majn ,minn ,ver)
         (define maj (number->string majn))
         (define min (number->string minn))
         (run/collect/wait/log 
          (build-path log-dir "planet" auth pkg maj min)
          planet-path "install" auth pkg maj min)])))  
  (run/collect/wait/log 
   (build-path log-dir "src" "build" "set-browser.ss")
   mzscheme-path "-t" (path->string* (build-path (drdr-directory) "set-browser.ss")))
  ; Make home directories
  (cache/file/timestamp
   (build-path rev-dir "homedir-dup")
   (lambda ()
     (notify! "Copying home directory for each worker")
     (for ([i (in-range (number-of-cpus))])
       (copy-directory/files (getenv "HOME") (home-dir i)))))
  ; And go
  (notify! "Starting testing")
  (test-directory collects-pth top-sema)
  (notify! "All testing scheduled... waiting for completion")
  (semaphore-wait top-sema)
  (notify! "Stopping testing")
  (stop-job-queue! test-workers))

(define (home-dir i)
  (format "~a~a"
          (getenv "HOME")
          i))

(define-syntax (with-env stx)
  (syntax-case stx ()
    [(_ ([env-expr val-expr] ...) expr ...)
     (with-syntax ([(env-val ...) (generate-temporaries #'(env-expr ...))]
                   [(old-env-val ...) (generate-temporaries #'(env-expr ...))]
                   [(new-env-val ...) (generate-temporaries #'(env-expr ...))])
       (syntax/loc stx
         (local [(define env-val env-expr)
                 ...
                 (define old-env-val (getenv env-val))
                 ...
                 (define new-env-val val-expr)
                 ...]
           (dynamic-wind
            (lambda ()
              (putenv env-val new-env-val)
              ...)
            (lambda ()
              expr ...)
            (lambda ()
              (when old-env-val
                (putenv env-val old-env-val))
              ...)))))]))

(define (recur-many i r f)
  (if (zero? i)
      (f)
      (r (sub1 i) (lambda ()
                    (recur-many (sub1 i) r f)))))

(define XSERVER-OFFSET 20)

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
                    [current-rev rev])
       (with-env (["PLTSTDERR" "error"]
                  ["TMPDIR" (path->string tmp-dir)]
                  ["PATH" 
                   (format "~a:~a"
                           (path->string (build-path trunk-dir "bin"))
                           (getenv "PATH"))]
                  ["PLTPLANETDIR" (path->string planet-dir)]
                  ["HOME" (path->string home-dir)])
         (unless (read-cache* (revision-commit-msg rev))
           (write-cache! (revision-commit-msg rev)
                         (svn-revision-log rev (plt-repository))))
         (build-revision rev)
         (recur-many (number-of-cpus)
                     (lambda (j inner)
                       (define i (+ j XSERVER-OFFSET))
                       (notify! "Starting X server #~a" i)
                       (safely-delete-directory (format "/tmp/.X~a-lock" i))
                       (safely-delete-directory (build-path tmp-dir (format ".X~a-lock" i)))
                       (safely-delete-directory (format "/tmp/.tX~a-lock" i))
                       (safely-delete-directory (build-path tmp-dir (format ".tX~a-lock" i)))
                       (with-running-program
                           (Xvfb-path) (list (format ":~a" i) "-screen" "0" "800x600x24")
                         (lambda ()
                           (with-running-program
                               (fluxbox-path) (list "-display" (format ":~a" i) "-rc" "/home/jay/.fluxbox/init")
                             inner))))
                     (lambda ()
                       (test-revision rev)))))
     ; Remove the test directory
     (safely-delete-directory test-dir))))

(provide/contract
 [integrate-revision (exact-nonnegative-integer? . -> . void)])