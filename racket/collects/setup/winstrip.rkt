#lang racket/base
(require racket/cmdline)

(module test racket/base)

;; Remove debugging and CGC files

(define keep-cgc? #f)

(define dir
  (command-line
   #:once-each
   [("--keep-cgc") "Keep CGC/3m executables and libraries"
    (set! keep-cgc? #t)]
   #:args
   (dir)
   dir))

(define (delete-file* p)
  (printf "Deleting ~a\n" p)
  (delete-file p))

(for ([a (directory-list dir)])
  (define f (build-path dir a))
  (define b (path-element->bytes a))
  (when (and (file-exists? f)
	     (or (regexp-match? #rx#"[.](?i:pdb|ilk)$" b)
		 (regexp-match? #rx#"(?i:CGC[.]exe)$" b)))
    (delete-file* f)))

(for ([f (in-directory (build-path dir "lib"))])
  (when (and (file-exists? f)
	     (let ([b (path-element->bytes
		       (let-values ([(base name dir?) (split-path f)])
			 name))])
	       (or (regexp-match? #rx#"[.](?i:pdb|ilk|manifest)$" b)
		   (regexp-match? #rx#"(?i:CGC[.](?:dll|exe))$" b)
		   (and (regexp-match? #rx#"(?i:[.](?:dll|exp|obj|lib))$" b)
			(not (regexp-match? #rx#"3m" b))))))
    (delete-file* f)))


