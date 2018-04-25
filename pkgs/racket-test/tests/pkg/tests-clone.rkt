#lang racket/base
(require rackunit
         racket/file
         racket/format
         racket/system
         web-server/servlet-env
         "util.rkt"
         "shelly.rkt")

(this-test-is-run-by-the-main-test)

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
   
   (define (current-commit dir)
     (define o (open-output-bytes))
     (parameterize ([current-directory dir]
                    [current-output-port o])
       (system "git log -1 --format=%H"))
     (read-line (open-input-bytes (get-output-bytes o))))
   (initialize-catalogs)
     
   (shelly-wind
    (sync (system-idle-evt)) ; let web server get going

    (define clone-dir (build-path tmp-dir "clones"))
    (make-directory clone-dir)

    (define a-dir (build-path tmp-dir "a"))

    (define (commit-changes-cmd [a-dir a-dir])
      (~a "cd " a-dir "; git add .; git commit -m change; git update-server-info"))

    ;; ----------------------------------------
    ;; Single-package repository

    (make-directory a-dir)    
    $ (~a "cd " a-dir "; git init")
    (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
    $ (commit-changes-cmd)

    (with-fake-root
      (shelly-begin
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
        "failed update can be ignored with `--pull try'"
        (set-file (build-path clone-dir "a" "main.rkt") "#lang racket/base 3.5")
        $ (~a "raco pkg update --pull try a") =exit> 0 =stdout> #rx"anyway"
        $ "racket -l a" =stdout> "3.5\n")
       
       (shelly-case
        "rebase mode fails on conflicts"
        $ (~a "raco pkg update --pull rebase a") =exit> 1
        $ "racket -l a" =stdout> "3.5\n")
       
       (shelly-case
        "rebase succeeds on non-conflifting changes"
        (set-file (build-path clone-dir "a" "main.rkt") "#lang racket/base 3") ; reverts local change
        (set-file (build-path clone-dir "a" "more.rkt") "#lang racket/base 30")
        $ (~a "cd " (build-path clone-dir "a") "; git add .; git commit -m change")
        $ (~a "raco pkg update --pull rebase a")
        $ "racket -l a" =stdout> "4\n"
        $ "racket -l a/more" =stdout> "30\n")
       
       (shelly-case
        "removal of --clone installation leaves local clone intact"
        $ "raco pkg remove a"
        $ "racket -l a" =exit> 1
        $ (~a "ls " (build-path clone-dir "a")))))

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
    
    (with-fake-root
      (shelly-begin
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
        ;; try to fast-forward the single "a" clone on different commits.
        $ (~a "raco pkg install --clone " (build-path clone-dir "a")
              " http://localhost:9998/a/.git?path=one"
              " http://localhost:9998/another/a/.git?path=two")
        =exit> 1
        =stdout> #rx"different target commits"
        ;; Check that the old repo checkout is not mangled:
        $ (~a "racket " (build-path clone-dir "a" "two" "main.rkt")) =stdout> "2\n")))

    (delete-directory/files (build-path clone-dir "a"))
    (delete-directory/files a-dir)
    
    ;; ----------------------------------------
    ;; Single-package repository that becomes multi-package

    (define (check-changing try-bogus?)
      (shelly-case
       "Single-package repository that becomes multi-package"
       (make-directory a-dir)
       $ (~a "cd " a-dir "; git init")
       (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
       $ (commit-changes-cmd)
       
       (with-fake-root
           (shelly-begin
            (shelly-case
             "--clone installation with path into repository"
             $ (~a "raco pkg install --clone " (build-path clone-dir "a") " --name one http://localhost:9998/a/.git")
             $ "racket -l one" =stdout> "1\n"
             $ (~a "ls " (build-path clone-dir "a")))
            
            $ (~a "cd " a-dir "; git rm main.rkt")
            (make-directory* (build-path a-dir "one"))
            (set-file (build-path a-dir "one" "main.rkt") "#lang racket/base 1")
            (set-file (build-path a-dir "one" "info.rkt") "#lang info (define deps '(\"http://localhost:9998/a/.git?path=two\"))")
            (make-directory* (build-path a-dir "two"))
            (set-file (build-path a-dir "two" "main.rkt") "#lang racket/base 2")
            $ (commit-changes-cmd)
            
            (when try-bogus?
              ;; A `raco pkg update one` at this point effectively
              ;; breaks the package installation, because the package
              ;; source will remain pathless. We only try this sometimes,
              ;; so that we check the next step with an without creating
              ;; paths "one" and "two" before that step.
              (shelly-begin
               $ "raco pkg update one"
               $ "racket -l one" =exit> 1))
            
            $ (~a "raco pkg update --clone " (build-path clone-dir "a") " --auto --multi-clone convert http://localhost:9998/a/.git?path=one")
            
            $ "racket -l one" =stdout> "1\n"
            $ "racket -l two" =stdout> "2\n"
            
            (set-file (build-path a-dir "two" "main.rkt") "#lang racket/base 2.0")
            $ (commit-changes-cmd)
            
            $ "racket -l two" =stdout> "2\n"
            $ "raco pkg update two"
            $ "racket -l two" =stdout> "2.0\n"))

       (delete-directory/files (build-path clone-dir "a"))
       (delete-directory/files a-dir)))
    
    (check-changing #f)
    (check-changing #t)

    ;; ----------------------------------------
    ;; Using local changes for metadata

    (make-directory a-dir)
    $ (~a "cd " a-dir "; git init")
    (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
    $ (commit-changes-cmd)

    (with-fake-root
      (shelly-begin
       (shelly-case
        "basic --clone installation"
        $ (~a "raco pkg install --clone " (build-path clone-dir "a") " --name a http://localhost:9998/a/.git")
        $ "racket -l a" =stdout> "1\n")

       (shelly-case
        "update of metadata in clone"
        (set-file (build-path clone-dir "a" "info.rkt") "#lang info\n(define deps '(\"b\"))\n")
        $ (~a "raco pkg update --update-deps --deps fail a")
        =exit> 1
        =stderr> #rx"missing dependencies")))

    (delete-directory/files (build-path clone-dir "a"))
    (delete-directory/files a-dir)

    ;; ----------------------------------------
    ;; Conversion to a clone

    (with-fake-root
      (shelly-begin
       (make-directory a-dir)
       $ (~a "cd " a-dir "; git init")
       (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
       (~a "cd " a-dir "; git add .; git commit -m change; git update-server-info")
       $ (commit-changes-cmd)
       
       (define (update-a-in-catalog!)
         (hash-set! *index-ht-1* "a"
                    (hasheq 'checksum
                            (current-commit a-dir)
                            'source
                            "http://localhost:9998/a/.git")))
       (update-a-in-catalog!)
       $ "raco pkg config --set catalogs http://localhost:9990"      
       
       $ "raco pkg install a"
       $ "racket -l a" =stdout> "1\n"
       
       (set-file (build-path a-dir "main.rkt") "#lang racket/base 2")
       $ (commit-changes-cmd)
       ;; Catalog is not changed, yet:
       $ (~a "raco pkg update a")
       $ "racket -l a" =stdout> "1\n"
       (update-a-in-catalog!)
       $ (~a "raco pkg update a")
       $ "racket -l a" =stdout> "2\n"
       
       (shelly-case
        "convert catalog-based to clone"
        $ (~a "raco pkg update --clone " (build-path clone-dir "a")) ; package name "a" is inferred
        $ "racket -l a" =stdout> "2\n"
        (set-file (build-path a-dir "main.rkt") "#lang racket/base 3")
        $ (commit-changes-cmd)
        $ "racket -l a" =stdout> "2\n"
        $ "raco pkg update a"
        $ "racket -l a" =stdout> "3\n")
       
       (shelly-case
        "using directory name for an update should update the repo"
        (set-file (build-path a-dir "main.rkt") "#lang racket/base 4")
        $ (commit-changes-cmd)
        $ (~a "raco pkg update " (build-path clone-dir "a"))
        $ "racket -l a" =stdout> "4\n")
       
       (delete-directory/files (build-path clone-dir "a"))
       (delete-directory/files a-dir)))

    ;; ----------------------------------------
    ;; Combining --clone and --lookup

    (with-fake-root
      (shelly-begin
       (make-directory a-dir)
       $ (~a "cd " a-dir "; git init")
       (set-file (build-path a-dir "main.rkt") "#lang racket/base 1")
       (~a "cd " a-dir "; git add .; git commit -m change; git update-server-info")
       $ (commit-changes-cmd)
       
       (define (update-a-in-catalog!)
         (hash-set! *index-ht-1* "a"
                    (hasheq 'checksum
                            (current-commit a-dir)
                            'source
                            "http://localhost:9998/a/.git")))
       (update-a-in-catalog!)
       $ "raco pkg config --set catalogs http://localhost:9990"      
       
       $ (~a "raco pkg install " a-dir)
       $ "racket -l a" =stdout> "1\n"

       (set-file (build-path a-dir "main.rkt") "#lang racket/base 2")
       ;; didn't commit, yet
       $ "racket -l a" =stdout> "2\n"

       (shelly-case
        "convert directory-linked to clone via --lookup"
        $ (~a "raco pkg update --clone " (build-path clone-dir "a"))
        =exit> 1
        $ (~a "raco pkg update --lookup --clone " (build-path clone-dir "a"))
        =exit> 0
        $ "racket -l a" =stdout> "1\n"
        
        $ (commit-changes-cmd)
        $ "raco pkg update a"
        $ "racket -l a" =stdout> "2\n")
       
       (delete-directory/files (build-path clone-dir "a"))
       (delete-directory/files a-dir)))

    ;; ----------------------------------------
    ;; Detecting when packages should share a clone

    ;; Checks installing "two" when same-repo "one" is installed as a clone.
    ;; If an extra "three" package is involved, it is in the same repo but
    ;; is initially installed as a non-clone.
    ;; If "two" is installed as a dependency of "b", we check sharing
    ;; that is discovered after the intiallation process starts.
    (define (check-share-mode three? ; involve extra package "three"?
                              mode   ; the `--multi-clone` mode
                              via-b? ; get "two" as a dependency of "b"?
                              #:one-directly-as-clone? [one-directly-as-clone? (not three?)]
                              #:update-one? [update-one? (not via-b?)])
      (with-fake-root
        (shelly-case
         (~a "Share mode"
             (if three? " with extra conflicting" "")
             " and --multi-clone " mode)
         
         (make-directory a-dir)
         $ (~a "cd " a-dir "; git init")
         (make-directory* (build-path a-dir "one"))
         (set-file (build-path a-dir "one" "main.rkt") "#lang racket/base 1")
         (make-directory* (build-path a-dir "two"))
         (set-file (build-path a-dir "two" "main.rkt") "#lang racket/base 2")
         (make-directory* (build-path a-dir "three"))
         (set-file (build-path a-dir "three" "main.rkt") "#lang racket/base 3")
         $ (commit-changes-cmd)

         ;; Used only if `via-b?`:
         (define b-dir (build-path tmp-dir "b"))
         (make-directory* b-dir)
         (set-file (build-path b-dir "main.rkt") "#lang racket/base (require two)")
         (set-file (build-path b-dir "info.rkt") "#lang info (define deps '(\"two\"))")

         (define (update-in-catalog! pkg)
           (hash-set! *index-ht-1* pkg
                      (hasheq 'checksum
                              (current-commit a-dir)
                              'source
                              (~a "http://localhost:9998/a/.git?path=" pkg))))
         (update-in-catalog! "one")
         (update-in-catalog! "two")
         (update-in-catalog! "three")
         $ "raco pkg config --set catalogs http://localhost:9990"      
  
         (if one-directly-as-clone?
             (shelly-begin
              $ (~a "raco pkg install --clone " (build-path clone-dir "a") " one"))
             (shelly-begin
              $ "raco pkg install one"
              $ "racket -l one" =stdout> "1\n"
              $ (~a "raco pkg update --clone " (build-path clone-dir "a") " one")))
         $ "racket -l one" =stdout> "1\n"
         
         (when three?
           ;; Install "three" in conflict with "one"
           (shelly-begin
            $ (~a "raco pkg install --multi-clone force three")
            $ "racket -l three" =stdout> "3\n"))
         
         $ (if via-b?
               (~a "raco pkg install --auto --multi-clone " mode " " (build-path tmp-dir "b"))
               (~a "raco pkg install --multi-clone " mode " two"))
         =exit> (if (eq? mode 'fail) 1 0)

         
         (unless (eq? mode 'fail)
           (shelly-begin
            $ "racket -l two" =stdout> "2\n"
            (when via-b?
              (shelly-begin
               $ "racket -l b" =stdout> "2\n"))

            (set-file (build-path a-dir "two" "main.rkt") "#lang racket/base 'two")
            (set-file (build-path a-dir "three" "main.rkt") "#lang racket/base 'three")
            $ (commit-changes-cmd)
            ;; ... but not updated in the catalog
            
            $ (~a "raco pkg update" (if update-one? " one" "") " two")
            $ (if via-b?
                  "racket -l b"
                  "racket -l two")
            =stdout> (if (eq? mode 'convert)
                         "'two\n"
                         "2\n")))
         
         (when three?
           (shelly-begin
            $ "racket -l three" =stdout> (if (eq? mode 'convert)
                                             "'three\n"
                                             "3\n")))
            
         (when (eq? mode 'convert)
           (shelly-case
            "Converting back to non-clone"
            $ (~a "raco pkg update --multi-clone convert "
                  (if three? "--unclone" "--lookup") ; try either alias, arbitrarily tied to `three?`
                  " one")
            $ "racket -l two" =stdout> "2\n"
            (when three?
              (shelly-begin
               $ "racket -l three" =stdout> "3\n"))))
         
         (delete-directory/files (build-path clone-dir "a"))
         (delete-directory/files a-dir)
         (delete-directory/files b-dir))))

    (for* ([via-b? '(#t #f)]
           [three? '(#f #t)]
           [mode '(fail force convert)])
      (check-share-mode three? mode via-b?))
    
    ;; ----------------------------------------
    
    (finally
     (hash-remove! *index-ht-1* "a")
     (hash-remove! *index-ht-1* "one")
     (hash-remove! *index-ht-1* "two")
     (hash-remove! *index-ht-1* "three")
     (custodian-shutdown-all http-custodian)
     (delete-directory/files tmp-dir)))))
