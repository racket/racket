#lang racket/base

;; Do the same work that Chez Scheme's `configure` performs
;; to set up build directories for Windows.

(provide prep-chez-scheme)

(define (prep-chez-scheme dir machine-name)
  (define (not-git? p)
    (define-values (base name dir?) (split-path p))
    (not (equal? ".git" (path->string name))))

  (define (maybe-make-directory p)
    (unless (directory-exists? p) (make-directory p)))

  (define (copy-one-dir name)
    (define src-dir (build-path dir name))
    (define dest-dir (build-path dir machine-name name))
    (maybe-make-directory dest-dir)
    (define paths (parameterize ([current-directory src-dir])
		    (for/list ([p (in-directory #f not-git?)])
			      p)))
    (for ([p (in-list paths)])
      (define src-p (build-path src-dir p))
      (define dest-p (build-path dest-dir p))
      (cond
       [(directory-exists? src-p)
	(maybe-make-directory dest-p)]
       [else
	(define src-ts (file-or-directory-modify-seconds src-p))
	(define dest-ts (file-or-directory-modify-seconds dest-p #f (lambda () #f)))
	(unless (and dest-ts (dest-ts . >= . src-ts))
	  (printf "copying ~a\n" (build-path name p))
	  (copy-file src-p dest-p #t))])))

  (maybe-make-directory (build-path dir machine-name))
  (maybe-make-directory (build-path dir machine-name "boot"))
  (maybe-make-directory (build-path dir machine-name "bin"))
  (maybe-make-directory (build-path dir machine-name "bin" machine-name))

  (copy-one-dir "c")
  (copy-one-dir "s")
  (copy-one-dir "mats")
  (copy-one-dir "nanopass")
  (copy-one-dir "zlib")
  (copy-one-dir "lz4")
  (copy-one-dir (build-path "boot" machine-name))

  (define config-h (build-path dir machine-name "c" "config.h"))
  (unless (file-exists? config-h)
    (printf "making ~a\n" config-h)
    (call-with-output-file*
     config-h
     (lambda (o)
       (fprintf o "#define SCHEME_SCRIPT \"scheme-script\"")))))
