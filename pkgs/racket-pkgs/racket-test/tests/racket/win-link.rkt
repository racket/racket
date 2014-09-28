#lang racket/base
(require racket/file
	 racket/system
	 racket/format)

;; Run this test on a Windows machine with a user that is allowed
;; to create symbolic links. Since that's not usually the case,
;; `raco test` will do nothing:
(module test racket/base)

(define count 0)

(define-syntax-rule (test expect get)
  (do-test expect get #'get))
(define (do-test expected got where)
  (set! count (add1 count))
  (unless (equal? expected got)
    (error 'test
	   (string-append "failure\n"
			  "  expected: ~e\n"
			  "  got: ~e\n"
			  "  expression: ~s")
	   expected
	   got
	   where)))

(define temp-dir (find-system-path 'temp-dir))

(define sub-name "link-sub")
(define sub (build-path temp-dir sub-name))
(delete-directory/files sub #:must-exist? #f)
(make-directory* sub)
(define (go build tbuild rbuild
	    #:mklink [make-____-__-directory-link make-file-or-directory-link]
	    #:rmlink [delete-____ delete-file])
  (test #f (link-exists? (build "l1")))

  ;; t1 -> l1
  (make-file-or-directory-link (rbuild "t1") (build "l1"))

  (test #t (link-exists? (build "l1")))
  (test #f (file-exists? (build "l1")))
  (test #f (directory-exists? (build "l1")))

  (make-directory (tbuild "t1"))
  (test #t (link-exists? (build "l1")))
  (test #f (file-exists? (build "l1")))
  (test #t (directory-exists? (build "l1")))

  ;; File via link to enclsoing dir
  (call-with-output-file (build-path (tbuild "t1") "f")
    (lambda (o) (display "t1-f" o)))
  (test (list (string->path "f")) (directory-list (build "l1")))
  (test "t1-f" (file->string (build-path (build "l1") "f")))
  (test #t (file-exists? (build-path (build "l1") "f")))
  (test (file-or-directory-modify-seconds (build-path (tbuild "t1") "f"))
	(file-or-directory-modify-seconds (build-path (build "l1") "f")))

  ;; Link to file in dir
  (make-file-or-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (build-path p "f2")
				     (string-append p "\\f2")))
			       (build "l2"))
  (call-with-output-file (build-path (tbuild "t1") "f2")
    (lambda (o) (display "t1-f2" o)))
  (test "t1-f2" (file->string (build "l2")))
  (delete-file (build-path (tbuild "t1") "f2"))

  ;; Link to dir in dir
  (make-directory (build-path (tbuild "t1") "f2"))
  (call-with-output-file (build-path (tbuild "t1") "f2" "f3")
    (lambda (o) (display "t1-f2-f3" o)))
  (test "t1-f2-f3" (file->string (build-path (build "l2") "f3")))
  (test (list (string->path "f3")) (directory-list (build "l2")))
  (delete-file (build "l2"))

  ;; Link to dir in dir with "." path elements
  (make-____-__-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (build-path p 'same 'same "f2" 'same)
				     (string-append p "\\.\\.\\f2\\.")))
			       (build "l2"))
  (test #t (directory-exists? (build "l2")))
  (test "t1-f2-f3" (file->string (build-path (build "l2") "f3")))
  (test (list (string->path "f3")) (directory-list (build "l2")))
  (delete-____ (build "l2"))

  ;; Link with ".." to cancel first link element
  (make-file-or-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (build-path p 'up "f3")
				     (string-append p "\\..\\f3")))
			       (build "l3"))
  (call-with-output-file (build-path (tbuild "t1") 'up "f3")
    (lambda (o) (display "f3!" o)))
  (test "f3!" (file->string (build "l3")))
  (delete-file (build-path (tbuild "t1") 'up "f3"))
  (delete-file (build "l3"))

  ;; Link with ".." to go up from link's directory
  (make-file-or-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (build-path p "f3")
				     (string-append "..\\" sub-name "\\" p "\\f3")))
			       (build "l3"))
  (call-with-output-file (build-path (tbuild "t1") "f3")
    (lambda (o) (display "f3." o)))
  (test "f3." (file->string (build "l3")))
  (delete-file (build-path (tbuild "t1") "f3"))
  (delete-file (build "l3"))

  ;; Trailing ".."
  (make-____-__-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (build-path p 'up)
				     (string-append p "\\..")))
			       (build "l3"))
  (call-with-output-file (build-path sub "f4")
    (lambda (o) (display "(f4)" o)))
  (test #t (directory-exists? (build "l3")))
  (test "(f4)" (file->string (build-path (build "l3") "f4")))
  (delete-file (build-path sub "f4"))
  (delete-____ (build "l3"))

  ;; Forward slashes (ok for absolute, not ok for relative)
  (define abs? (absolute-path? (rbuild "t1")))
  (define ssq? (and (path? (rbuild "t1"))
		    (equal? "\\\\?\\"
			    (substring (path->string (rbuild "t1")) 0 4))))
  (make-file-or-directory-link (let ([p (rbuild "t1")])
				 (if (path? p)
				     (let ([p (build-path p "f5")])
				       (if ssq?
					   p
					   (bytes->path
					    (regexp-replace #rx"\\" (path->bytes p) "/"))))
				     (string-append p "/f5")))
			       (build "l5"))
  (call-with-output-file (build-path (tbuild "t1") "f5") void)
  (test (or abs? ssq?) (file-exists? (build "l5")))
  (delete-file (build-path (tbuild "t1") "f5"))
  (make-directory (build-path (tbuild "t1") "f5"))
  (test (or abs? ssq?) (directory-exists? (build "l5")))
  (delete-directory (build-path (tbuild "t1") "f5"))
  (delete-file (build "l5"))

  (delete-directory/files (tbuild "t1"))
  (test #f (directory-exists? (build "l1")))
  (test #f (file-exists? (build-path (build "l1") "f")))

  ;; Check some extra functions on files:
  (call-with-output-file (tbuild "t1")
    (lambda (o) (display "t1" o)))
  (test "t1" (file->string (build "l1")))
  (test #t (file-exists? (build "l1")))
  (test (file-or-directory-modify-seconds (tbuild "t1"))
	(file-or-directory-modify-seconds (build "l1")))

  (delete-file (tbuild "t1"))
  (delete-file (build "l1")))

(define (in-sub s) (build-path sub s))
(define (in-sub/unc s)
  (define e (explode-path (in-sub s)))
  (apply build-path
	 (format "\\\\localhost\\~a$\\"
		 (substring (path->string (car e)) 0 1))
	 (cdr e)))
(define (trailing-space/string s)
  (string-append s " "))
(define (trailing-space s)
  (string->path-element (trailing-space/string s)))
(define (trailing-space-in-sub s)
  (in-sub (trailing-space s)))
(define (trailing-space-in-sub/unc s)
  (define-values (base name dir) (split-path (in-sub/unc s)))
  (build-path base (trailing-space s)))

(define (make-junction dest src)
  (unless (system (~a "mklink /j " src " " dest))
    (error)))

(go in-sub in-sub values)
(go in-sub in-sub in-sub)
(go in-sub in-sub in-sub #:mklink make-junction #:rmlink delete-directory)
(parameterize ([current-directory sub])
  (go values values values)
  (go values in-sub in-sub))

(go in-sub/unc in-sub values)
(go in-sub in-sub in-sub/unc)

(parameterize ([current-directory sub])
  (go in-sub trailing-space trailing-space/string)
  (go in-sub/unc trailing-space trailing-space/string))
(go in-sub trailing-space-in-sub trailing-space/string)
(go in-sub trailing-space-in-sub/unc trailing-space/string)

(delete-directory/files sub)

(printf "~a tests passed\n" count)
