#lang racket/base
(require "shelly.rkt"
         "util.rkt"
         racket/file
         file/unzip)

(pkg-tests

 $ "raco pkg install --deps fail test-pkgs/pkg-x/" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/pkg-y/" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/pkg-x/ test-pkgs/pkg-z/" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/pkg-y/ test-pkgs/pkg-z/" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/pkg-x/ test-pkgs/pkg-y/" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/pkg-y/ test-pkgs/pkg-x/" =exit> 1

 $ "raco pkg install --deps fail test-pkgs/pkg-z/" =exit> 0

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg install test-pkgs/pkg-x/ test-pkgs/pkg-y/"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -l x -e '(x)'" =stdout> "'x\n"
 $ "racket -l racket/base -l y -e '(y)'" =stdout> "'y\n"

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #t)'"
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #t)'"

 (make-directory* "test-pkgs/src-pkgs")
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-x"
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-y"
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-z"

 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-x"
 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-y"
 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-z"

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg remove pkg-x pkg-y pkg-z"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #f)'"
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #f)'"

 (define tmp-dir (make-temporary-file "unpack-~a" 'directory))
 (make-directory* tmp-dir)
 (define (unpack name)
   (define orig-d (current-directory))
   (let ([z (path->complete-path (format "test-pkgs/src-pkgs/pkg-~a.zip" name))])
     (define d (build-path tmp-dir name))
     (make-directory* d)
     (parameterize ([current-directory d])
       (unzip z)
       (for ([f (in-directory)])
         (define orig-f (build-path orig-d (format "test-pkgs/pkg-~a" name) f))
         (unless (if (file-exists? f)
                     (file-exists? orig-f)
                     (directory-exists? orig-f))
           (error 'diff "extra ~s" f))
         (when (file-exists? f)
           (unless (equal? (file->bytes f)
                           (file->bytes orig-f))
             (error 'diff "file differs ~s" f)))))
     (parameterize ([current-directory (format "test-pkgs/pkg-~a" name)])
       (for ([f (in-directory)])
         (define new-f (build-path d f))
         (unless (if (file-exists? f)
                     (file-exists? new-f)
                     (directory-exists? new-f))
           (unless (regexp-match? #rx#"nosrc" (path->bytes f))
             (error 'diff "missing ~s" new-f)))))))
 (unpack "x")
 (unpack "y")
 (unpack "z")
 (delete-directory/files tmp-dir)

 (make-directory* tmp-dir)
 (define (unpack-bin name)
   (let ([z (path->complete-path (format "test-pkgs/pkg-~a.zip" name))])
     (define d (build-path tmp-dir name))
     (make-directory* d)
     (parameterize ([current-directory d])
       (unzip z)
       (for ([f (in-directory)])
         (when (or (regexp-match? #rx#"[.](?:rkt|scrbl)$" (path->bytes f))
                   (regexp-match? #rx#"nobin" (path->bytes f)))
           (unless (regexp-match? #rx#"(?:info[.]rkt|keep.scrbl)$" (path->bytes f))
             (error 'binary "extra ~s" f)))))))
 (unpack-bin "x")
 (unpack-bin "y")
 (unpack-bin "z")
 (delete-directory/files tmp-dir)

 (shelly-case
  "source-package dependencies like original"
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-x.zip" =exit> 1
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-y.zip" =exit> 1
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-x.zip test-pkgs/src-pkgs/pkg-z.zip" =exit> 1
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-y.zip test-pkgs/src-pkgs/pkg-z.zip" =exit> 1
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-x.zip test-pkgs/src-pkgs/pkg-y.zip" =exit> 1
  $ "raco pkg install --deps fail test-pkgs/src-pkgs/pkg-y.zip test-pkgs/src-pkgs/pkg-x.zip" =exit> 1)

 (shelly-case
  "binary-package dependencies are less"
  $ "raco pkg install --deps fail test-pkgs/pkg-x.zip" =exit> 1
  
  $ "raco pkg install --deps fail test-pkgs/pkg-x.zip test-pkgs/pkg-z.zip"
  $ "raco pkg remove pkg-x pkg-z"
  $ "raco pkg install --deps fail test-pkgs/pkg-y.zip"
  $ "raco pkg remove pkg-y")

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg install --deps fail test-pkgs/pkg-y.zip"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #f)'"
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #t)'"

 (shelly-case
  "check that cleaning doesn't destroy a binary install"
  $ "racket -l y"
  $ "raco setup -c -l y"
  $ "racket -l y")

 $ "raco pkg remove pkg-y"

 )
