#lang racket
(require "path-utils.rkt")

(define number-of-cpus
  (make-parameter 1))

(define current-subprocess-timeout-seconds
  (make-parameter (* 60 10)))

(define plt-directory
  (make-parameter (build-path (current-directory))))

(define (plt-build-directory)
  (build-path (plt-directory) "builds"))

(define (plt-future-build-directory)
  (build-path (plt-directory) "future-builds"))

(define (plt-data-directory)
  (build-path (plt-directory) "data"))

(define drdr-directory
  (make-parameter (build-path (current-directory) "drdr")))

(define make-path
  (make-parameter "/usr/bin/make"))
(define tar-path
  (make-parameter "/bin/tar"))

(define Xvfb-path
  (make-parameter "/usr/bin/Xvfb"))

(define fluxbox-path
  (make-parameter "/usr/bin/fluxbox"))

(define vncviewer-path
  (make-parameter "/usr/bin/vncviewer"))

(define (plt-repository)
  (build-path (plt-directory) "repo"))

(define current-make-timeout-seconds
  (make-parameter (* 60 30)))

(define current-make-install-timeout-seconds
  (make-parameter (* 60 30)))

(define current-rev
  (make-parameter #f))

(define previous-rev
  (make-parameter #f))

(define (revision-dir rev)
  (build-path (plt-build-directory) (number->string rev)))

(define (revision-log-dir rev)
  (build-path (revision-dir rev) "logs"))

(define (revision-analyze-dir rev)
  (build-path (revision-dir rev) "analyze"))

(define (revision-trunk-dir rev)
  (build-path (revision-dir rev) "trunk"))
(define (revision-trunk.tgz rev)
  (build-path (revision-dir rev) "trunk.tgz"))
(define (revision-trunk.tar.7z rev)
  (build-path (revision-dir rev) "trunk.tar.7z"))

(define (revision-commit-msg rev)
  (build-path (revision-dir rev) "commit-msg"))

(define (path->revision pth)
  (define builds (explode-path (plt-build-directory)))
  (define builds-len (length builds))
  (define pths (explode-path pth))
  (string->number (path->string* (list-ref pths builds-len))))

(define (revision-archive rev)
  (build-path (revision-dir rev) "archive.db"))

(define (future-record-path n)
  (build-path (plt-future-build-directory) (number->string n)))

(define (path-timing-log p)
  (path-add-suffix (build-path (plt-data-directory) p) #".timing"))

(define (path-timing-png p)
  (path-add-suffix (path-timing-log p) #".png"))
(define (path-timing-html p)
  (path-add-suffix (path-timing-log p) #".html"))
(define (path-timing-png-prefix p)
  (path-timing-log p))

(define build? (make-parameter #t))

(define (on-unix?)
  (symbol=? 'unix (system-type 'os)))

(provide/contract
 [current-subprocess-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [number-of-cpus (parameter/c exact-nonnegative-integer?)]
 [current-rev (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [previous-rev (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [plt-directory (parameter/c path-string?)]
 [plt-build-directory (-> path?)]
 [plt-data-directory (-> path?)]
 [plt-future-build-directory (-> path?)]
 [drdr-directory (parameter/c path-string?)]
 [tar-path (parameter/c (or/c false/c string?))]
 [make-path (parameter/c (or/c false/c string?))]
 [Xvfb-path (parameter/c (or/c false/c string?))]
 [vncviewer-path (parameter/c (or/c false/c string?))] 
 [fluxbox-path (parameter/c (or/c false/c string?))]
 [build? (parameter/c boolean?)]
 [on-unix? (-> boolean?)]
 [plt-repository (-> path?)]
 [path-timing-log (path-string? . -> . path?)]
 [path-timing-png (path-string? . -> . path?)]
 [path-timing-png-prefix (path-string? . -> . path?)]
 [path-timing-html (path-string? . -> . path?)]
 [future-record-path (exact-nonnegative-integer? . -> . path?)]
 [current-make-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [current-make-install-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [revision-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-commit-msg (exact-nonnegative-integer? . -> . path?)]
 [revision-log-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-analyze-dir (exact-nonnegative-integer? . -> . path-string?)]
 [revision-trunk-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-trunk.tgz (exact-nonnegative-integer? . -> . path?)]
 [revision-trunk.tar.7z (exact-nonnegative-integer? . -> . path?)]
 [revision-archive (exact-nonnegative-integer? . -> . path?)]
 [path->revision (path-string? . -> . exact-nonnegative-integer?)])
