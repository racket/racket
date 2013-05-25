#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (define dir (getenv "PLTADDONDIR"))
 (define pkg-dir (build-path dir "pkgs"))
 (define (try-now)
   (shelly-case
    "Should be able to read information that is read-only"
    (define old-dir-perms (and (directory-exists? dir)
                               (file-or-directory-permissions dir 'bits)))
    (define old-pkg-dir-perms (and (directory-exists? pkg-dir)
                                   (file-or-directory-permissions pkg-dir 'bits)))
    (when old-pkg-dir-perms
      (file-or-directory-permissions pkg-dir 0))
    (when old-dir-perms
      (file-or-directory-permissions dir 0))
    $ "raco pkg show"
    (when old-dir-perms
      (file-or-directory-permissions dir old-dir-perms))
    (when old-pkg-dir-perms
      (file-or-directory-permissions pkg-dir old-pkg-dir-perms))))

 (try-now)

 (if (directory-exists? pkg-dir)
     ;; move the directory and try again:
     (let ([alt-dir (build-path dir "xpkgs")])
       (rename-file-or-directory pkg-dir alt-dir)
       (try-now)
       (rename-file-or-directory alt-dir pkg-dir))
     ;; create the directory and try again:
     (begin
       (make-directory pkg-dir)
       (try-now))))
