#lang racket/load

;; This script uses the "s3-sync" package, even though the enclosing
;; package does not depend on it, and this module is written with
;; `racket/load` to avoid the dependency.

(require racket/cmdline)

(define dry-run? #f)
(define save-temps? #f)

(command-line
 #:once-each
 [("--dry-run") "Don't actually upload"
  (printf "Dry-run mode enabled\n")
  (set! dry-run? #t)]
 [("--save-temps") "Preserve generated files"
  (printf "Saving generated files\n")
  (set! save-temps? #t)])

;; Manually check package install and version:
(define s3-sync-pkg "s3-sync")
(define min-s3-sync-vers "1.1")
(require pkg/lib
         setup/getinfo
         version/utils)
(let ([dir (pkg-directory s3-sync-pkg)])
  (unless dir
    (error 'sync "please install the ~s package" s3-sync-pkg))
  (let ([i (get-info/full dir)])
    (unless (and i
                 (let ([v (i 'version (lambda () #f))])
                   (and v
                        (version<=? min-s3-sync-vers v))))
      (error 'sync
             "please update the ~s package to get version ~a or later"
             s3-sync-pkg min-s3-sync-vers))))

(require s3-sync
         s3-sync/gzip)

(define (step . s)
  (displayln (make-string 72 #\=))
  (for-each displayln s)
  (displayln (make-string 72 #\-)))

(define orig-dir (current-directory))
(define tmp-dir (make-temporary-file "sync~a" 'directory))

(current-directory tmp-dir)

;; ----------------------------------------

(step "Generate web pages")
(parameterize ([current-namespace (make-base-namespace)]
               [current-command-line-arguments (vector "-w"
                                                       "-o" "generated"
                                                       "-f")])
  (dynamic-require 'meta/new-web/all #f)
  (dynamic-require '(submod meta/new-web/all main) #f))

(define-values (gzip-web-in gzip-web-enc) 
  (make-gzip-handlers #rx"[.](css|js)$" #:min-size (* 1 1024)))

(define (upload dir site #:shallow? [shallow? #f])
  (step (format "Uploading ~a" site))
  (s3-sync (build-path "generated" dir)
           site
           #f
           #:dry-run? dry-run?
           #:shallow? shallow?
           #:upload? #t
           #:reduced-redundancy? #t
           #:acl "public-read"
           #:link-mode 'redirect
           #:make-call-with-input-file gzip-web-in
           #:get-content-encoding gzip-web-enc
           #:log displayln))
(upload "www" "racket-lang.org")
(upload "www" "www.racket-lang.org")
(upload "pre" "pre.racket-lang.org")
(upload "con" "con.racket-lang.org")
(upload "drracket" "www.drracket.org")
(upload "download" "download.racket-lang.org" #:shallow? #t)

;; ----------------------------------------

(current-directory orig-dir)

(if save-temps?
    (printf "Files saved in ~a\n" tmp-dir)
    (delete-directory/files tmp-dir))
