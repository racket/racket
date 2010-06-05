#!/bin/sh
#|
exe="racket";
if [ -x "$PLTHOME/bin/racket" ]; then exe="$PLTHOME/bin/racket"; fi
exec "$exe" "$0" "$@"
|#

#lang racket/base

(require racket/cmdline racket/runtime-path
         racket/string racket/file
         "html/resource.rkt" "common/distribute.rkt"
         "config.rkt" "navbar.rkt")

(define build-mode #f)
(define distribute? #f)
(define warn? #t)

(command-line
 #:once-any
 [("-l" "--local")
  "create content that is viewable in the build directory"
  "  (all links are relative) "
  (set! build-mode 'local)]
 [("-w" "--web")
  "create content that is viewable on the Racket web pages"
  (set! build-mode 'web)]
 #:once-each
 [("-o" "--output") dir
  "output directory"
  "  (defaults to the current directory)"
  (unless (directory-exists? dir)
    (printf "Creating \"~a\"\n" dir) (make-directory dir))
  (current-directory dir)]
 [("-f")
  "avoid warning about directory cleanup"
  (set! warn? #f)]
 [("-d" "--dist")
  "distribute resulting content"
  "  (will only work with the right access to the servers)"
  (set! distribute? #t)])

(unless build-mode (raise-user-error 'build "build mode not specified"))

(define-runtime-path here ".")
(let ([build (file-or-directory-identity (current-directory))])
  (let loop ([dir here])
    (if (equal? build (file-or-directory-identity dir))
      (raise-user-error 'build "might clobber sources, refusing to build")
      (let-values ([(base name dir?) (split-path dir)])
        (when base (loop base))))))

(let ([paths (sort (map path->string (directory-list)) string<?)])
  (when (pair? paths)
    (if (or (not warn?)
            (begin (printf "Directory not empty, these will be deleted: ~a.\n"
                           (string-join paths ", "))
                   (printf "Continue? ") (flush-output)
                   (regexp-match? #rx" *[yY]" (read-line))))
      (for-each delete-directory/files paths)
      (raise-user-error 'build "Aborting."))))

(printf "Building ~a content...\n" build-mode)
(parameterize ([url-roots (and (eq? 'web build-mode) sites)]) (render-all))
(when distribute? (printf "Distributing...\n") (distribute distributions))
(printf "Done.\n")
