
(load-relative "loadtest.rktl")

(Section 'filelib)

(require scheme/file
	 mzlib/process
	 mzlib/list)

(define tmp-name "tmp0-filelib")
(when (file-exists? tmp-name) (delete-file tmp-name))
(display-lines-to-file '("a" "b" "c") tmp-name #:separator #"\r\n" #:mode 'binary)
(test '(a b c) file->list tmp-name)
(test '("a\r" "b\r" "c\r") file->list tmp-name read-line)
(test '("a" "b" "c") file->list tmp-name (lambda (p) (read-line p 'any)))
(test "a\r\nb\r\nc\r\n" file->string tmp-name #:mode 'binary)
(test #"a\r\nb\r\nc\r\n" file->bytes tmp-name)
(test '("a" "b" "c") file->lines tmp-name)
(test '(#"a" #"b" #"c") file->bytes-lines tmp-name)
(test '("a" "b" "c") file->lines tmp-name #:line-mode 'any #:mode 'binary)
(test '(#"a" #"b" #"c") file->bytes-lines tmp-name #:line-mode 'any #:mode 'text)
(err/rt-test (display-to-file #"a\nb" tmp-name) exn:fail:filesystem:exists?)
(display-to-file #"a\nb" tmp-name #:exists 'truncate)
(test #"a\nb" file->bytes tmp-name)
(display-to-file "\u03BB" tmp-name #:exists 'truncate)
(test #"\316\273" file->bytes tmp-name)
(write-to-file "\u03BB" tmp-name #:exists 'truncate)
(test #"\"\316\273\"" file->bytes tmp-name)
(test "\u03BB" file->value tmp-name)
(define tmp-name "tmp0-filelib")

(define-syntax-rule (err/rt-chk-test (op arg ...))
  (err/rt-test (op arg ...) (check-msg 'op)))
(define (check-msg op)
  (lambda (exn)
    (regexp-match (format "^~a: " op) (exn-message exn))))
(err/rt-chk-test (file->string 'x))
(err/rt-chk-test (file->bytes 'x))
(err/rt-chk-test (file->string "x" #:mode 'other))
(err/rt-chk-test (file->bytes "x" #:mode 'other))
(err/rt-chk-test (file->value "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file 10 "x"))
(err/rt-chk-test (display-lines-to-file '(10) "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file '(10) "x" #:exists 'other))
(err/rt-chk-test (file->lines "x" #:line-mode 'junk))
(err/rt-chk-test (file->lines "x" #:mode 'other))
(err/rt-chk-test (file->bytes-lines "x" #:line-mode 'junk))
(err/rt-chk-test (file->bytes-lines "x" #:mode 'other))
(err/rt-chk-test (display-to-file "y" "x" #:exists 'other))
(err/rt-chk-test (display-to-file "y" "x" #:mode 'other))
(err/rt-chk-test (write-to-file #"y" "x" #:exists 'other))
(err/rt-chk-test (write-to-file #"y" "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file 'y "x"))
(err/rt-chk-test (display-lines-to-file '(y) "x" #:exists 'other))
(err/rt-chk-test (display-lines-to-file '(y) "x" #:mode 'other))

;; ----------------------------------------

(parameterize ([current-directory (current-load-relative-directory)])
  (let ([rel (find-files values)]
	[abs (find-files values (current-directory))])
    (test #t = (length rel) (sub1 (length abs)))
    (test #f member "filelib.rktl" abs)
    (test #f null? (member "filelib.rktl" rel))
    (test #f null? (member (build-path (current-directory) "filelib.rktl") abs))

    (test (list (string->path "filelib.rktl")) find-files (lambda (f) (regexp-match "^filelib[.]rkt$" (path->string f))))
    (test (list (build-path (current-directory) "filelib.rktl"))
	  find-files (lambda (f) (regexp-match "filelib[.]rkt$" (path->string f)))
	  (current-directory))

    (let ([rel2 (fold-files (lambda (name kind accum)
			      (test kind name (if (file-exists? name)
                                                  'file
                                                  'dir))
			      (cons name accum))
			    null)]
	  [sort (lambda (l)
		  (sort l (lambda (a b)
			    (bytes<? (path->bytes a) (path->bytes b)))))])
      (test #t equal? (sort rel) (sort rel2))

      (unless (eq? (system-type) 'windows)
        (make-file-or-directory-link "filelib.rktl" "filelib-link")
        (make-file-or-directory-link "." "loop-link")

	(test (+ 2 (length rel2))
	      fold-files 
	      (lambda (name kind accum)
		(test kind values (cond
				   [(link-exists? name) 'link]
				   [(file-exists? name) 'file]
				   [(directory-exists? name) 'dir]
				   [else '???]))
		(when (member name '("filelib-link" "loop-link"))
		  (test kind name 'link))
		(add1 accum))
	      0
	      #f
	      #f)

	(test (+ 2 (length rel2))
	      fold-files 
	      (lambda (name kind accum)
		(test kind values (cond
				   [(link-exists? name) 'link]
				   [(file-exists? name) 'file]
				   [(directory-exists? name) 'dir]
				   [else '???]))
		(when (member name '("filelib-link" "loop-link"))
		  (test kind name 'link))
		(values (add1 accum) #t))
	      0
	      #f
	      #f)

        (delete-file "loop-link")

	(test (+ 1 (length rel2))
	      fold-files 
	      (lambda (name kind accum)
		(test kind values (cond
				   [(file-exists? name) 'file]
				   [else 'dir]))
		(when (member name '("filelib-link"))
		  (test kind name 'file))
		(add1 accum))
	      0
	      #f
	      #t)

	(delete-file "filelib-link")

	'done))))

(report-errs)
