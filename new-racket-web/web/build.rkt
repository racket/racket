#!/bin/sh
#| -*- scheme -*-
exe="racket";
if [ -x "$PLTHOME/bin/racket" ]; then exe="$PLTHOME/bin/racket"; fi
exec "$exe" "$0" "$@"
|#

#lang racket/base

(require racket/cmdline racket/runtime-path racket/file scribble/html
         "common/distribute.rkt" "config.rkt" "all.rkt")

(define build-mode #f)
(define output-dir (current-directory))
(define distribute? #f)
(define warn? #t)
(define extra-files '())

(command-line
 #:once-any
 [("-l" "--local")
  "local mode: create content that is viewable in the build directory"
  "  (all links are relative) "
  (set! build-mode 'local)]
 [("-w" "--web")
  "web mode: create content that is viewable on the Racket web pages"
  (set! build-mode 'web)]
 #:once-each
 [("-o" "--output") dir
  "output directory"
  "  (defaults to the current directory)"
  (unless (directory-exists? dir)
    (printf "Creating \"~a\"\n" dir) (make-directory dir))
  (set! output-dir dir)]
 [("-f" "--force")
  "avoid warning about directory cleanup"
  (set! warn? #f)]
 [("-d" "--dist")
  "distribute resulting content"
  "  (will only work with the right access to the servers)"
  (set! distribute? #t)]
 #:multi
 [("-e" "--extra") extra
  "extra file to render more content"
  (set! extra-files (cons extra extra-files))]
 #:help-labels
 " ** Note: set $KNOWN_MIRRORS_FILE to a file if you want to poll mirror"
 "          links (see top comment in \"download/mirror-link.rkt\").")

(unless build-mode (raise-user-error 'build "build mode not specified"))

(define-runtime-path here ".")
(let ([build (file-or-directory-identity output-dir)])
  (let loop ([dir here])
    (if (equal? build (file-or-directory-identity dir))
      (raise-user-error 'build
                        "might clobber sources, refusing to build (use `-o')")
      (let-values ([(base name dir?) (split-path dir)])
        (when base (loop base))))))

(parameterize ([current-directory output-dir])
  (define paths (sort (map path->string (directory-list)) string<?))
  (when (pair? paths)
    (if (or (not warn?)
            (begin (printf "Directory not empty, these will be deleted: ~a.\n"
                           (string-join paths ", "))
                   (printf "Continue? ") (flush-output)
                   (regexp-match? #rx" *[yY]" (read-line))))
      (for-each delete-directory/files paths)
      (raise-user-error 'build "Aborting."))))

(printf "Building ~a content...\n" build-mode)
(parameterize ([url-roots (and (eq? 'web build-mode) sites)])
  (for ([extra (in-list extra-files)])
    (if (file-exists? extra)
      (dynamic-require `(file ,extra) #f)
      (printf "  ignoring missing extra file: ~a\n" extra)))
  (parameterize ([current-directory output-dir])
    (render-all)
    (when distribute?
      (printf "Distributing...\n")
      (distribute (distributions)))))
(printf "Done.\n")
