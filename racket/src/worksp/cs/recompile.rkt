#lang racket/base
(require racket/file
	 racket/system
	 "../../ChezScheme/rktboot/parse-makefile.rkt")

(provide recompile)

(define optimize-level 3)

(define (recompile scheme-dir machine #:system* [system* system*])
  (when (or (same-content? (build-path scheme-dir "boot" machine "petite.boot")
			   (build-path scheme-dir machine "boot" machine "petite.boot"))
	    (same-content? (build-path scheme-dir "boot" machine "scheme.boot")
			   (build-path scheme-dir machine "boot" machine "scheme.boot")))
    (define-values (petite-srcs scheme-srcs) (get-sources-from-makefile scheme-dir))

    (define abs-scheme-dir (path->complete-path scheme-dir))

    (parameterize ([current-directory (build-path scheme-dir machine "nanopass")])
      (define o (open-output-bytes))
      (write-nanopass-config o)
      (remove-files #rx"[.]so$")
      (write '(compile-library "nanopass.ss" "nanopass.so") o)
      (parameterize ([current-input-port (open-input-bytes (get-output-bytes o))])
	(system* (build-path abs-scheme-dir machine "bin" machine "scheme.exe")
		 "-q"
		 "--compile-imported-libraries")))
    
    (parameterize ([current-directory (build-path scheme-dir machine "s")])
      (copy-file (format "~a.def" machine) "machine.def" #t)
      (define o (open-output-bytes))
      (define (src->so src) (regexp-replace #rx"[.]ss$" src ".so"))
      (write-system-config o)
      (for ([f (in-list '("cmacros.ss" "priminfo.ss"))])
        (write `(compile-file ,f) o)
        (write `(load ,(src->so f)) o))
      (for ([f (in-list (append petite-srcs scheme-srcs))])
        (write `(compile-file ,f) o))
      (write `($make-boot-file ,(format "../boot/~a/petite.boot" machine)
                               ',(string->symbol machine) '()
                               ,@(map src->so petite-srcs))
	     o)
      (write `($make-boot-file ,(format "../boot/~a/scheme.boot" machine)
                               ',(string->symbol machine) '("petite")
                               ,@(map src->so scheme-srcs))
	     o)
      ;;(printf "~a\n" (get-output-string o))
      (parameterize ([current-input-port (open-input-bytes (get-output-bytes o))])
	(system* (build-path abs-scheme-dir machine "bin" machine "scheme.exe")
		 "-q"
		 "--libdirs" "../nanopass")))))

(define (same-content? f1 f2)
  (and (equal? (file-size f1)
	       (file-size f2))
       (equal? (file->bytes f1)
	       (file->bytes f2))))

(define (write-config o)
  (write '(reset-handler abort) o)
  (write '(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset))) o)
  (write '(keyboard-interrupt-handler (lambda () (display "interrupted---aborting\n") (reset))) o)
  (write `(optimize-level ,optimize-level) o)
  (write '(debug-level 0) o)
  (write '(generate-inspector-information #f) o))

(define (write-nanopass-config o)
  (write-config o))

(define (write-system-config o)
  (write-config o)
  (write '(subset-mode (quote system)) o))

(define (remove-files rx)
  (for ([f (in-directory)])
    (when (regexp-match? rx f)
      (delete-file f))))
