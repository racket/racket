#lang racket/base
(require rackunit
         racket/file
         racket/format
         web-server/servlet-env
         "util.rkt"
         "shelly.rkt")

(this-test-is-run-by-the-main-test)

(define (set-file path content)
  (call-with-output-file* 
   path
   #:exists 'truncate/replace
   (lambda (o) (displayln content o))))

(pkg-tests
 (define git-exe (find-executable-path
                  (if (eq? 'windows (system-type)) "git.exe" "git")))
 
 (when git-exe
   (define tmp-dir (path->directory-path (make-temporary-file "pkg~a" 'directory)))
   (define http-custodian (make-custodian))
   
   (parameterize ([current-custodian http-custodian])
     (thread
      (lambda ()
        (serve/servlet
         void
         #:command-line? #t
         #:extra-files-paths
         (list tmp-dir)
         #:servlet-regexp #rx"$." ; no servlets
         #:port 9998))))
   
   (shelly-wind
    (sync (system-idle-evt)) ; let web server get going

    (define clone-dir (build-path tmp-dir "clones"))
    (make-directory clone-dir)

    (define a-dir (build-path tmp-dir "a"))

    ;; ----------------------------------------
    ;; Single-package repository

    (make-directory a-dir)    
    $ (~a "cd " a-dir "; git init")
    (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
    (define (commit-changes-cmd [a-dir a-dir])
      (~a "cd " a-dir "; git add .; git commit -m change; git update-server-info"))
    $ (commit-changes-cmd)

    (shelly-case
     "basic --clone installation"
     $ (~a "raco pkg install --clone " (build-path clone-dir "a") " --name a http://localhost:9998/a/.git")
     $ "racket -l a" =stdout> "1\n")
    
    (shelly-case
     "update of --clone installation"
     (set-file (build-path a-dir "main.rkt") "#lang racket/base 2")
     $ (commit-changes-cmd)
     $ (~a "raco pkg update a")
     $ "racket -l a" =stdout> "2\n")
    
    (shelly-case
     "update of --clone installation doesn't overwrite local changes"
     (set-file (build-path a-dir "main.rkt") "#lang racket/base 3")
     $ (commit-changes-cmd)
     (set-file (build-path clone-dir "a" "alt.rkt") "#lang racket/base 'one")
     $ (~a "cd " (build-path clone-dir "a") "; git add .; git commit -m local")
     $ "racket -l a" =stdout> "2\n"
     $ "racket -l a/alt" =stdout> "'one\n"
     $ (~a "raco pkg update a") =exit> 1 =stderr> #rx"fast-forward"
     $ (~a "cd "  (build-path clone-dir "a") "; git pull --rebase")
     $ (~a "raco pkg update a")
     $ "racket -l a" =stdout> "3\n"
     $ "racket -l a/alt" =stdout> "'one\n")
    
    (shelly-case
     "update of --clone installation doesn't proceed past conflicts"
     (set-file (build-path a-dir "main.rkt") "#lang racket/base 4")
     $ (commit-changes-cmd)
     (set-file (build-path clone-dir "a" "main.rkt") "#lang racket/base 3.5")
     $ (~a "raco pkg update a") =exit> 1
     $ "racket -l a" =stdout> "3.5\n")
    
    (shelly-case
     "removal of --clone installation leaves local clone intact"
     $ "raco pkg remove a"
     $ "racket -l a" =exit> 1
     $ (~a "ls " (build-path clone-dir "a")))

    (delete-directory/files (build-path clone-dir "a"))
    (delete-directory/files a-dir)

    ;; ----------------------------------------
    ;; Multi-package repository
    
    (make-directory a-dir)
    $ (~a "cd " a-dir "; git init")
    (make-directory* (build-path a-dir "one"))
    (set-file (build-path a-dir "one" "main.rkt") "#lang racket/base 1")
    (make-directory* (build-path a-dir "two"))
    (set-file (build-path a-dir "two" "main.rkt") "#lang racket/base 2")
    $ (commit-changes-cmd)
    
    (shelly-case
     "--clone installation with path into repository"
     $ (~a "raco pkg install --clone " (build-path clone-dir "a") " --name one http://localhost:9998/a/.git?path=one")
     $ "racket -l one" =stdout> "1\n"
     $ (~a "ls " (build-path clone-dir "a")))
    
    (shelly-case
     "update of --clone installation"
     (set-file (build-path a-dir "one" "main.rkt") "#lang racket/base 1.0")
     $ (commit-changes-cmd)
     $ (~a "raco pkg update one")
     $ "racket -l one" =stdout> "1.0\n")
    
    (shelly-case
     "--clone second installation with path into same repository"
     (set-file (build-path a-dir "one" "main.rkt") "#lang racket/base 'one")
     $ (commit-changes-cmd)
     $ (~a "raco pkg install --clone " (build-path clone-dir "a") " http://localhost:9998/a/.git?path=two")
     $ "racket -l one" =stdout> "'one\n"
     $ "racket -l two" =stdout> "2\n")
    
    (shelly-case
     "no changes => still an update, since previous update was implicit via shared repo"
     $ "raco pkg update one" =stdout> #rx"Re-installing one\n")

    (shelly-case
     "no further changes => no update"
     $ "raco pkg update one two" =stdout> #rx"No updates available\n")
    
    $ "raco pkg remove one two"

    (shelly-case
     "conflicting repositories with the same name"
     (define another-a-dir (build-path tmp-dir "another" "a"))
     (make-directory* another-a-dir)
     $ (~a "cd " another-a-dir "; git init")
     (make-directory* (build-path another-a-dir "two"))
     (set-file (build-path another-a-dir "two" "main.rkt") "#lang racket/base 'two")
     $ (commit-changes-cmd another-a-dir)
     
     ;; A wacky merge of repsitories will happen here, but the checkout should not
     ;; get mangled. The package manager should bail out at the point that it would
     ;; try to rebase the single "a" clone on different commits.
     $ (~a "raco pkg install --clone " (build-path clone-dir "a")
           " http://localhost:9998/a/.git?path=one"
           " http://localhost:9998/another/a/.git?path=two")
     =exit> 1
     =stderr> #rx"different target commits"
     ;; Check that the old repo checkout is not mangled:
     $ (~a "racket " (build-path clone-dir "a" "two" "main.rkt")) =stdout> "2\n")
    
    ;; ----------------------------------------
    
    (finally
     (custodian-shutdown-all http-custodian)
     (delete-directory/files tmp-dir)))))
