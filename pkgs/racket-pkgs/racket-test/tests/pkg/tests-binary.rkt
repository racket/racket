#lang racket/base
(require "shelly.rkt"
         "util.rkt"
         racket/file
         file/unzip)

(this-test-is-run-by-the-main-test)

(pkg-tests

 $ "raco pkg install --deps fail --copy test-pkgs/pkg-x/" =exit> 1
 $ "raco pkg install --deps fail --copy test-pkgs/pkg-y/" =exit> 1
 $ "raco pkg install --deps fail --copt test-pkgs/pkg-x/ test-pkgs/pkg-z/" =exit> 1
 $ "raco pkg install --deps fail --copy test-pkgs/pkg-y/ test-pkgs/pkg-z/" =exit> 1
 $ "raco pkg install --deps fail --copy test-pkgs/pkg-x/ test-pkgs/pkg-y/" =exit> 1
 $ "raco pkg install --deps fail --copy test-pkgs/pkg-y/ test-pkgs/pkg-x/" =exit> 1

 $ "raco pkg install --deps fail --copy test-pkgs/pkg-z/" =exit> 0

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg install --copy test-pkgs/pkg-x/ test-pkgs/pkg-y/"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -l x -e '(x)'" =stdout> "'x\n"
 $ "racket -l racket/base -l y -e '(y)'" =stdout> "'y\n"
 $ "racket -l racket/base -l y/sub/s -e '(s)'" =stdout> "'s\n"

 $ "racket -l racket/base -e '(require (submod x test))'"
 $ "racket -l racket/base -e '(require (submod y/other doc))'"

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #t)'" =stderr> ""
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #t)'" =stderr> ""

 (make-directory* "test-pkgs/src-pkgs")
 (make-directory* "test-pkgs/built-pkgs")
 (make-directory* "test-pkgs/binary-lib-pkgs")
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-x"
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-y"
 $ "raco pkg create --from-install --source --dest test-pkgs/src-pkgs pkg-z"

 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-x"
 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-y"
 $ "raco pkg create --from-install --binary --dest test-pkgs pkg-z"

 $ "raco pkg create --from-install --binary-lib --dest test-pkgs/binary-lib-pkgs pkg-x"
 $ "raco pkg create --from-install --binary-lib --dest test-pkgs/binary-lib-pkgs pkg-y"
 $ "raco pkg create --from-install --binary-lib --dest test-pkgs/binary-lib-pkgs pkg-z"

 $ "raco pkg create --from-install --built --dest test-pkgs/built-pkgs pkg-x"
 $ "raco pkg create --from-install --built --dest test-pkgs/built-pkgs pkg-y"
 $ "raco pkg create --from-install --built --dest test-pkgs/built-pkgs pkg-z"

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg remove pkg-x pkg-y pkg-z"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #f)'" =stderr> ""
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #f)'" =stderr> ""

 (define tmp-dir (make-temporary-file "unpack-~a" 'directory))

 (shelly-case
  "check content of source package"
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
            (unless (or (regexp-match? #rx#"nosrc" f)
                        (and (regexp-match? #rx#"keep2" f)
                             (regexp-match? #rx#"doc" f)))
              (error 'diff "missing ~s" new-f)))))))
  (unpack "x")
  (unpack "y")
  (unpack "z")
  (delete-directory/files tmp-dir))

 (shelly-case
   "check content of binary package"
   (make-directory* tmp-dir)
   (define (unpack-bin name lib?)
     (let ([z (path->complete-path (format "test-pkgs/~apkg-~a.zip"
                                           (if lib? "binary-lib-pkgs/" "")
                                           name))])
       (define d (build-path tmp-dir name))
       (delete-directory/files d #:must-exist? #f)
       (make-directory* d)
       (parameterize ([current-directory d])
         (unzip z)
         (for ([f (in-directory)])
           (when (or (and (regexp-match? #rx#"(?:[.](?:rkt|scrbl|dep)|_scrbl[.]zo)$" f)
                          (not (regexp-match? #rx#"(?:info_rkt[.]dep)$" f)))
                     (and (regexp-match? #rx#"nobin" f)
                          (or lib?
                              (not (regexp-match? #rx#"nobinlib" f)))))
             (unless (regexp-match? #rx#"(?:info[.]rkt|keep.scrbl)$" f)
               (error (if lib? 'binary-lib 'binary) "extra ~s" f)))
           (when lib?
             (when (and (regexp-match? #rx#"doc$" f)
                        (not (regexp-match? #rx#"keep2" f)))
               (error 'binary-lib "extra ~s" f)))
           (when (regexp-match? #rx#"(?:info[.]rkt|info_rkt.zo)$" f)
             (parameterize ([current-namespace (make-base-namespace)])
               (define i (dynamic-require f '#%info-lookup))
               (for ([key '(implies update-implies)])
                 (define implies (i key (lambda () null)))
                 (define deps (map (lambda (p) (if (pair? p) (car p) p))
                                   (i 'deps (lambda () null))))
                 (for ([s (in-list implies)])
                   (unless (member s deps)
                     (error 'binary "bad `~a'" key))))))))))
   (unpack-bin "x" #f)
   (unpack-bin "y" #f)
   (unpack-bin "z" #f)
   (unpack-bin "x" #t)
   (unpack-bin "y" #t)
   (unpack-bin "z" #t)
   (delete-directory/files tmp-dir))

  (shelly-case
  "check content of built package"
  (make-directory* tmp-dir)
  (define (unpack-built name)
    (let ([src (path->complete-path (format "test-pkgs/src-pkgs/pkg-~a.zip" name))]
          [bin (path->complete-path (format "test-pkgs/pkg-~a.zip" name))]
          [blt (path->complete-path (format "test-pkgs/built-pkgs/pkg-~a.zip" name))])
      (define sd (build-path tmp-dir name "src"))
      (define bd (build-path tmp-dir name "bin"))
      (define td (build-path tmp-dir name "blt"))
      (make-directory* sd)
      (make-directory* bd)
      (make-directory* td)
      (parameterize ([current-directory sd])
        (unzip src))
      (parameterize ([current-directory bd])
        (unzip bin))
      (parameterize ([current-directory td])
        (unzip blt)
        (for ([f (in-directory)])
          (when (file-exists? f)
            (unless (or (file-exists? (build-path sd f))
                        (file-exists? (build-path bd f)))
              (unless (regexp-match? #rx#"(?:[.](?:dep)|_scrbl[.]zo)$" f)
                (error 'built "extra ~s" f))))
          (when (regexp-match? #rx#"[.]zo$" f)
            (unless (file-exists? (path-replace-suffix f #".dep"))
              (error 'build "dep missing for ~s" f)))
          (when (regexp-match? #rx#"[.](rkt|scrbl|ss)$" f)
            (let-values ([(base name dir?) (split-path f)])
              (unless (file-exists? (build-path (if (eq? base 'relative) 'same base)
                                                "compiled" 
                                                (path-add-suffix name #".zo")))
                (unless (regexp-match? #rx#"^(?:info.rkt|x/keep.scrbl)$" f)
                  (error 'build "compiled file missing for ~s" f)))))))
      (parameterize ([current-directory sd])
        (for ([f (in-directory)])
          (when (file-exists? f)
            (unless (file-exists? (build-path td f))
              (error 'built "missing source ~s" f)))))
      (parameterize ([current-directory bd])
        (define (extra-info? p)
          ;; A binary package can have extra "info.rkt" files in collections
          ;; to set `assume-virtual-sources` to #t.
          (let-values ([(base name dir?) (split-path p)])
            (define s (path->string name))
            (and (or (equal? "info.rkt" s)
                     (equal? "info_rkt.zo" s)
                     (equal? "info_rkt.dep" s))
                 (or (eq? base 'relative)
                     (and (path? base)
                          (let-values ([(base name dir?) (split-path base)])
                            (or (eq? base 'relative)
                                (and (path? name)
                                     (equal? (path->string name) "compiled")
                                     (let-values ([(base name dir?) (split-path base)])
                                       (eq? base 'relative))))))))))
        (for ([f (in-directory)])
          (when (file-exists? f)
            (unless (file-exists? (build-path td f))
              (unless (extra-info? f)
                (error 'built "missing binary ~s" f))))))))
  (unpack-built "x")
  (unpack-built "y")
  (unpack-built "z")
  (delete-directory/files tmp-dir))

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

 $ "racket -l racket/base -l y -e '(y)'" =stdout> "'y\n"
 $ "racket -l racket/base -l y/sub/s -e '(s)'" =stdout> "'s\n"

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #f)'" =stderr> ""
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #t)'" =stderr> ""

 $ "racket -l racket/base -e '(require (submod x test))'" =exit> 1
 $ "racket -l racket/base -e '(require (submod y/other doc))'" =exit> 1

 (shelly-case
  "check that cleaning doesn't destroy a binary install"
  $ "racket -l y"
  $ "raco setup -c -l y"
  $ "racket -l y")

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg remove pkg-y"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "raco pkg install --deps fail test-pkgs/built-pkgs/pkg-x.zip" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/built-pkgs/pkg-y.zip" =exit> 1
 $ "raco pkg install --deps fail test-pkgs/built-pkgs/pkg-x.zip test-pkgs/built-pkgs/pkg-z.zip" =exit> 1

 (putenv "PLT_PKG_NOSETUP" "")
 $ "raco pkg install --deps fail test-pkgs/built-pkgs/pkg-x.zip test-pkgs/built-pkgs/pkg-y.zip test-pkgs/built-pkgs/pkg-z.zip"
 =stdout> #rx"syncing: [^\n]*x\n[^\n]*syncing: [^\n]*y"
 (putenv "PLT_PKG_NOSETUP" "1")

 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-x #t)'" =stderr> ""
 $ "racket -l racket/base -t test-docs.rkt -e '(test-docs-y #t)'" =stderr> ""
 $ "racket -l racket/base -l x -e '(x)'" =stdout> "'x\n"
 $ "racket -l racket/base -l y -e '(y)'" =stdout> "'y\n"
 $ "racket -l racket/base -l y/sub/s -e '(s)'" =stdout> "'s\n"

 $ "raco pkg remove pkg-x pkg-y pkg-z"

 (shelly-case
  "incompatible conversion request rejected"
  $ "raco pkg install --binary test-pkgs/src-pkgs/pkg-z.zip"
  =exit> 1 =stderr> #rx"not compatible"
  $ "raco pkg install --binary-lib test-pkgs/src-pkgs/pkg-z.zip"
  =exit> 1 =stderr> #rx"not compatible"
  $ "raco pkg install --source test-pkgs/pkg-z.zip"
  =exit> 1 =stderr> #rx"not compatible"
  $ "raco pkg install --source test-pkgs/binary-lib-pkgs/pkg-z.zip"
  =exit> 1 =stderr> #rx"not compatible")

 )
