;; Generate ".def" files for installed DLLs
#lang racket/base
(require racket/system)

(define lib-dir (build-path 'up 'up "lib"))

(define (gen-one name)
  (define f (build-path lib-dir (format "~axxxxxxx.dll" name)))
  (when (file-exists? f)
    (define s (open-output-bytes))
    (parameterize ([current-output-port s])
      (system (format "dumpbin /exports ~a" f)))
    (define i (open-input-bytes (get-output-bytes s)))
    (regexp-match #rx"ordinal +hint +RVA +name" i)
    (call-with-output-file*
     (build-path lib-dir (format "~axxxxxxx.def" name))
     #:exists 'truncate
     (lambda (o)
      (fprintf o "EXPORTS\n")
      (for ([l (in-lines i)])
        (define m (regexp-match
		   #rx"([0-9]+) +(?:[0-9A-Fa-f]+) +(?:[0-9A-Fa-f]+) +([_A-Za-z][_A-Za-z0-9]*) +="
		   l))
	(when m
	  (fprintf o "    ~a @~a\n" (caddr m) (cadr m))))))))

(gen-one "libmzgc")
(gen-one "libracket")
(gen-one "libracket3m")

