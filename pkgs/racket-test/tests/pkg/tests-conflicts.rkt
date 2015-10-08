#lang racket/base
(require rackunit
         racket/system
         racket/match
         racket/format
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)
  
  $ "raco pkg create --format plt test-pkgs/pkg-test1/"
  $ "raco pkg create --format plt test-pkgs/pkg-test1-not-conflict/"
  (shelly-install "only modules are considered for conflicts"
                  "test-pkgs/pkg-test1.plt"
                  $ "raco pkg install test-pkgs/pkg-test1-not-conflict.plt")
  
  (shelly-case
   "conflicts"
   (shelly-install "double install fails" "test-pkgs/pkg-test1.zip"
                   $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 1)

   (with-fake-root
    (shelly-case
     "conflicts with racket fail"
     $ "test -f test-pkgs/racket-conflict.tgz"
     $ "raco pkg install test-pkgs/racket-conflict.tgz" =exit> 1))

   (shelly-install "conflicts are caught" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1-conflict.zip"
                   $ "raco pkg install test-pkgs/pkg-test1-conflict.zip" =exit> 1)

   (shelly-install "conflicts are caught across sharing modes" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1-conflict.zip"
                   $ "raco pkg install -s test-pkgs/pkg-test1-conflict.zip" =exit> 1)

   (shelly-install "conflicts are caught for compiled files" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1b.zip"
                   $ "raco pkg install test-pkgs/pkg-test1b.zip" =exit> 1)

   (shelly-install* "conflicts are caught in single-collection" 
                    "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3.zip" "pkg-test1 pkg-test3"
                    $ "test -f test-pkgs/pkg-test3-v2.zip"
                    $ "raco pkg install test-pkgs/pkg-test3-v2.zip" =exit> 1)
   (shelly-install* "conflicts are caught in single-collection against multi-collection"
                    "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3-v2.zip" "pkg-test1 pkg-test3-v2"
                    $ "test -f test-pkgs/pkg-test3.zip"
                    $ "raco pkg install test-pkgs/pkg-test3.zip" =exit> 1)

   (define tmp-dir (path->directory-path (make-temporary-file "pkg~a" 'directory)))
   (shelly-wind
    $ (~a "cp -r test-pkgs/pkg-test1 " tmp-dir"pkg-test1-linking")
    $ (~a "raco pkg install --link " tmp-dir"pkg-test1-linking")
    $ "test -f test-pkgs/pkg-test1-conflict.zip"
    $ "raco pkg install test-pkgs/pkg-test1-conflict.zip" =exit> 1
    $ "raco pkg remove pkg-test1-linking"
    (finally
     (delete-directory/files tmp-dir)))

   (shelly-install "conflicts can be forced" "test-pkgs/pkg-test1.zip"
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 42
                   $ "raco pkg install --force test-pkgs/pkg-test1-conflict.zip" =exit> 0
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 42
                   $ "raco pkg remove pkg-test1-conflict")

   (shelly-install "conflicts can be forced" "test-pkgs/pkg-test1-conflict.zip"
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 43
                   $ "raco pkg install --force test-pkgs/pkg-test1.zip" =exit> 0
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 43
                   $ "raco pkg remove pkg-test1-conflict"))

  (with-fake-root
    (shelly-case
     "update succeeds when module is moved to dependency"
     (define tmp-dir (path->directory-path (make-temporary-file "pkg~a" 'directory)))
     
     (shelly-wind
      (define a-dir (build-path tmp-dir "a"))
      (make-directory a-dir)
      (set-file (build-path a-dir "info.rkt") "#lang info\n(define collection \"a\")\n")
      (set-file (build-path a-dir "apple.rkt") "#lang racket/base\n")

      (define b-dir (build-path tmp-dir "b"))
      (make-directory b-dir)
      (set-file (build-path b-dir "info.rkt") "#lang info\n(define collection \"a\")\n")
      (set-file (build-path b-dir "apple.rkt") "#lang racket/base\n")
      
      $ (~a "raco pkg install --copy " a-dir " " b-dir)
      =exit> 1 
      =stderr> #rx"packages conflict"
      
      $ (~a "raco pkg install --copy " a-dir)
      (set-file (build-path a-dir "info.rkt") 
                (~a "#lang info\n(define collection \"a\")\n"
                    "(define deps '((" (~s (path->string b-dir)) ")))\n"))
      $ (~a "raco pkg update --auto --copy " a-dir)
      =exit> 1 
      =stderr> #rx"packages conflict"
      
      (delete-file (build-path a-dir "apple.rkt"))
      $ (~a "raco pkg update --auto --copy " a-dir)
      
      (finally
       (delete-directory/files tmp-dir)))))

  (shelly-case
   "conflict extra installs"
   (for ([c '("test-pkgs/pkg-add-a"
             "test-pkgs/pkg-add-x"
             "test-pkgs/pkg-add-1")])
    (with-fake-root
     (shelly-begin
      $ (~a "raco pkg install --copy --strict-doc-conflicts test-pkgs/pkg-add-base " c) =exit> 1
      $ (~a "raco pkg install --copy --strict-doc-conflicts " c "test-pkgs/pkg-add-base") =exit> 1))))
  (shelly-case
   "doc conflict allowed in non-strict mode"
   (for ([c '("test-pkgs/pkg-add-a")])
    (with-fake-root
     (shelly-begin
      $ (~a "raco pkg install --copy test-pkgs/pkg-add-base " c) =exit> 0))))
  (putenv "PLT_PKG_NOSETUP" "")
  (with-fake-root
   (shelly-case
    "conflict extra installs with already installed"
    $ (~a "raco pkg install --copy test-pkgs/pkg-add-base") =exit> 0
    (for ([c '("test-pkgs/pkg-add-a"
               "test-pkgs/pkg-add-x"
               "test-pkgs/pkg-add-1")])
      (shelly-begin
       $ (~a "raco pkg install --copy --strict-doc-conflicts " c) =exit> 1)))
   (for ([c '("test-pkgs/pkg-add-a")])
    (with-fake-root
     (shelly-begin
      $ (~a "raco pkg install --copy --no-setup " c) =exit> 0))))
  (putenv "PLT_PKG_NOSETUP" "1")
  (with-fake-root
   (shelly-case
    "no conflict for non-matching platform"
    $ "raco pkg install --copy --strict-doc-conflicts test-pkgs/pkg-add-base test-pkgs/pkg-add-none"))
  (shelly-case
   "no doc conflict for an update"
   (for ([c '("test-pkgs/pkg-add-base"
              "test-pkgs/pkg-add-a"
              "test-pkgs/pkg-add-x"
              "test-pkgs/pkg-add-1")])
    (with-fake-root
     (shelly-begin
      $ "raco pkg install --copy test-pkgs/pkg-add-base"
      $ "raco setup -D --pkgs pkg-add-base"
      $ (~a "raco pkg update --copy --name pkg-add-base " c) =exit> 0))))

  (shelly-case
   "compile-omit-paths is used by `pkg-directory->additional-installs`:"
   $ (~a "racket -e '(require pkg/lib)' -e '"
         (~s '(pkg-directory->additional-installs
               (path-only (collection-file-path "test.rkt" "tests/pkg"))
               "racket-test"))
         "'")
   =stdout> "'()\n")
  
  (with-fake-root
      (shelly-case
       "non-conflicts on .zo files that will be deletced by `raco setup`"

       (define (copy+install-not-conflict)
         (define t1nc-dir (make-temporary-file "~a-t1nc" 'directory))
         (define src-dir "test-pkgs/pkg-test1-not-conflict/")
         (for ([i (directory-list src-dir)])
           (copy-directory/files (build-path src-dir i) (build-path t1nc-dir i)))
         (shelly-begin
          $ (~a "raco pkg install " t1nc-dir))
         t1nc-dir)
       (define (set-conflict-mode t1nc-dir mode)
         (define (maybe-delete-file p) (when (file-exists? p) (delete-file p)))
         (case mode
           [(src)
            (set-file (build-path t1nc-dir "data" "empty-set.rkt") "#lang racket/base 'empty")
            (maybe-delete-file (build-path t1nc-dir "data" "compiled" "empty-set_rkt.zo"))
            (maybe-delete-file (build-path t1nc-dir "data" "info.rkt"))]
           [(both)
            (set-file (build-path t1nc-dir "data" "empty-set.rkt") "#lang racket/base 'empty")
            (set-file (build-path t1nc-dir "data" "compiled" "empty-set_rkt.zo") "not real...")
            (set-file (build-path t1nc-dir "data" "info.rkt") "#lang info\n(define assume-virtual-sources #t)")]
           [(zo-stays)
            (set-file (build-path t1nc-dir "data" "compiled" "empty-set_rkt.zo") "not real...")
            (maybe-delete-file (build-path t1nc-dir "data" "empty-set.rkt"))
            (set-file (build-path t1nc-dir "data" "info.rkt") "#lang info\n(define assume-virtual-sources #t)")]
           [(zo-goes)
            (set-file (build-path t1nc-dir "data" "compiled" "empty-set_rkt.zo") "not real...")
            (maybe-delete-file (build-path t1nc-dir "data" "empty-set.rkt"))
            (maybe-delete-file (build-path t1nc-dir "data" "info.rkt"))]))
       
       (define (install-pkg1-fails)
         (shelly-begin
          $ "raco pkg install test-pkgs/pkg-test1.zip"
          =exit> 1
          =stderr> #rx"packages conflict.*data/empty-set"))
       (define (install-pkg1-succeeds)
         (shelly-begin
          $ "raco pkg install test-pkgs/pkg-test1.zip"
          $ "raco pkg remove pkg-test1"))
       
       (define t1-nc1-dir (copy+install-not-conflict))
       (set-conflict-mode t1-nc1-dir 'src)
       (install-pkg1-fails)

       (set-conflict-mode t1-nc1-dir 'both)
       (install-pkg1-fails)

       (set-conflict-mode t1-nc1-dir 'zo-stays)
       (install-pkg1-fails)
       
       (set-conflict-mode t1-nc1-dir 'zo-goes)
       (install-pkg1-succeeds)
       
       (define t1-nc2-dir (copy+install-not-conflict))

       (for* ([m1 '(src both zo-stays zo-goes)]
              [m2 '(src both zo-stays zo-goes)])
         (when (verbose?)
           (printf "trying ~s ~s\n" m1 m2))
         (set-conflict-mode t1-nc1-dir m1)
         (set-conflict-mode t1-nc2-dir m2)
         (if (and (eq? m1 'zo-goes) (eq? m2 'zo-goes))
             (install-pkg1-succeeds)
             (install-pkg1-fails)))
       
       (delete-directory/files t1-nc1-dir)
       (delete-directory/files t1-nc2-dir)))))
