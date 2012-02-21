
;; This script attempts to update a program that uses the v20x class
;; system to use the v299 class system: it eliminates `rename' clauses,
;; and changes the corresponding uses of `(super-<id> <arg> ...)' to
;; uses of `(super <id> <arg> ...)'.

;; The script is not particularly clever: it assumes that an S-exp of
;; the form `(rename [<id> <id>] ...)' is a use in `class', unless it
;; appears immediately a `unit/sig' S-exp. Furthermore, uses of the
;; first identifier in the rename pair are converted to use `super'
;; only if that identifier's name starts with `super-'.

;; Provide files to convert on the command line:
;;    mzscheme -qr rename-super-fixup.ss <file> ...
;; Each <file> is copied to <file>.bak before <file> is overwritten
;; with the converted code (unless <file>.bak already exists, in which
;; case it is left as-is).

(require mzlib/list
	 syntax/stx)

;; returns a list of syntax for `rename' clauses
;; the order in the list matches the textual order in the source
(define (find-all-renames s)
  (cond
   [(and (stx-pair? s)
	 (eq? 'unit/sig (syntax-e (stx-car s))))
    ;; Avoid `rename' clause in a unit:
    (let ([l (stx->list s)])
      (if (and l
	       ((length l) . > . 3)
	       (stx-pair? (cadddr l))
	       (eq? 'rename (syntax-e (stx-car (cadddr l)))))
	  (find-all-renames (list-tail l 4))
	  (find-all-renames (stx-cdr s))))]
   [(and (stx-pair? s)
	 (eq? 'rename (syntax-e (stx-car s))))
    (let ([l (or (stx->list (stx-cdr s)) '(#f))])
      (if (andmap (lambda (i)
		    (and (stx-pair? i)
			 (identifier? (stx-car i))
			 (stx-pair? (stx-cdr i))
			 (identifier? (stx-car (stx-cdr i)))
			 (stx-null? (stx-cdr (stx-cdr i)))))
		  l)
	  (list s)
	  null))]
   [(syntax? s) (find-all-renames (syntax-e s))]
   [(pair? s) (append
	       (find-all-renames (car s))
	       (find-all-renames (cdr s)))]
   [else null]))

;; returns a list of identifiers
(define (find-all-uses s ids)
  (cond
   [(and (identifier? s)
	 (memq (syntax-e s) ids))
    (list s)]
   [(syntax? s) (find-all-uses (syntax-e s) ids)]
   [(pair? s) (append
	       (find-all-uses (car s) ids)
	       (find-all-uses (cdr s) ids))]
   [else null]))

(define (fixup f)
  ;; First, search in S-exp form
  (fprintf (current-error-port) "Fixing ~a~n" (if (path? f)
						  (path->string f)
						  f))
  (let ([s (with-input-from-file f read-syntax)])
    (let* ([renames (find-all-renames s)]
	   [ids (filter
		 (lambda (sym)
		   (regexp-match #rx"super-" (symbol->string sym)))
		 (map syntax-e
		      (map stx-car 
			   (apply
			    append
			    (stx->list (map stx-cdr renames))))))]
	   [uses (find-all-uses s ids)])
      ;; Load file content as bytes:
      (let ([str (with-input-from-file f (lambda () (read-string (file-size f))))])
	;; Change `super-' to `super ':
	(for-each (lambda (use)
		    (let ([pos (syntax-position use)])
		      (when pos
			(string-set! str (+ (sub1 pos) 5) #\space))))
		  uses)
	;; Drop `rename' lines:
	(for-each (lambda (rename)
		    (let ([pos (syntax-position rename)]
			  [span (syntax-span rename)])
		      (when (and pos span)
			(let ([line-start
			       (regexp-match-positions
				#rx"[\t ]+$"
				str
				0
				(sub1 pos))]
			      [line-end (regexp-match-positions
					 #rx"^ *(\r|\n|\r\n)"
					 str
					 (+ (sub1 pos) span))])
			  
			  (set! str
				(string-append
				 (substring str 0 (if line-start
						      (caar line-start)
						      (sub1 pos)))
				 (substring str (if line-end
						    (cdar line-end)
						    (+ (sub1 pos) span)))))))))
		  (reverse renames))
	;; Write string out:
	(let ([bak (format "~a.bak" (if (path? f)
					(path->string f)
					f))])
	  (unless (file-exists? bak)
	    (copy-file f bak)))
	(with-output-to-file f (lambda ()
				 (display str))
			     'truncate)))))
      
(map fixup (vector->list (current-command-line-arguments)))
