#lang racket/load

;; This script uses the "s3-sync" package, even though the enclosing
;; package does not depend on it, and this module is written with
;; `racket/load` to avoid the dependency.
(require s3-sync)

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

(define (upload dir site)
  (step (format "Uploading ~a" site))
  (s3-sync (build-path "generated" dir)
           site
           #f
           #:upload? #t
           #:reduced-redundancy? #t
           #:acl "public-read"
           #:link-mode 'redirect
           #:log displayln))
(upload "www" "racket-lang.org")
(upload "www" "www.racket-lang.org")
(upload "pre" "pre.racket-lang.org")
(upload "con" "con.racket-lang.org")
(upload "drracket" "www.drracket.org")
(upload "download" "download.racket-lang.org")

;; ----------------------------------------

(current-directory orig-dir)
(delete-directory/files tmp-dir)
