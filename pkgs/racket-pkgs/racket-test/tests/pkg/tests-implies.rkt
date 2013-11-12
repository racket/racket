#lang racket/base
(require racket/file
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (initialize-catalogs)

  (shelly-case
   "create packages"
   $ "raco pkg create --format zip test-pkgs/pkg-implied-one"
   $ "raco pkg create --format zip test-pkgs/pkg-implied-two"
   $ "raco pkg create --format zip test-pkgs/pkg-implies")

  (define (implied-version! s)
    (hash-set! *index-ht-1* "pkg-implied"
               (hasheq 'checksum
                       (file->string (format "test-pkgs/pkg-implied-~a.zip.CHECKSUM" s))
                       'source
                       (format "http://localhost:9999/pkg-implied-~a.zip" s))))
  (implied-version! "one")
  (hash-set! *index-ht-1* "pkg-implies"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-implies.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-implies.zip"))

  (with-fake-root
   (shelly-begin
    $ "raco pkg config --set catalogs http://localhost:9990")

   (shelly-case
    "install with auto-dependencies"
    $ "raco pkg install --auto pkg-implies"
    $ "racket -l pkg-implied" =stdout> #rx"implied-1")
   (shelly-case
    "update checks implied, but does nothing"
    $ "raco pkg update pkg-implies" =stdout> #rx"pkg-implied.*No updates available")

   (implied-version! "two") ; << UPDATE version
   (shelly-case
    "update does not auto-update implies when disabled"
    $ "raco pkg update --ignore-implies pkg-implies" =stdout> #rx"^(?!pkg-implied).*No updates available"
    $ "racket -l pkg-implied" =stdout> #rx"implied-1")
   (shelly-case
    "update auto-updates implied by default"
    $ "raco pkg update pkg-implies" =stdout> #rx"pkg-implied"
    $ "racket -l pkg-implied" =stdout> #rx"implied-2")

   (implied-version! "one") ; << UPDATE version
   (shelly-case
    "installign a package updates its implied packages"
    $ "raco pkg remove pkg-implies"
    $ "racket -l pkg-implied" =stdout> #rx"implied-2"
    $ "raco pkg install pkg-implies" =stdout> #rx"pkg-implied"
    $ "racket -l pkg-implied" =stdout> #rx"implied-1")

   (implied-version! "two") ; << UPDATE version
   (shelly-case
    "implied packages can be treated as normal dependencies"
    $ "raco pkg update --ignore-implies pkg-implies" =stdout> #rx"^(?!pkg-implied).*No updates available"
    $ "racket -l pkg-implied" =stdout> #rx"implied-1"
    $ "raco pkg update --ignore-implies --auto pkg-implies"
    $ "racket -l pkg-implied" =stdout> #rx"implied-2")

   (implied-version! "one") ; << UPDATE version
   (shelly-case
    "update works ok with --all"
    $ "raco pkg update --all"
    $ "racket -l pkg-implied" =stdout> #rx"implied-1")

   (void))))
