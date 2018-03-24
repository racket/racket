#lang racket/base
(require setup/dirs
	 dynext/filename-version)

(provide get-racket-dlls
	 search-dll)

(define (get-racket-dlls types #:extras-only? [extras-only? #f])
  (define (versionize template)
    (let ([f (search-dll (format template filename-version-part))])
      (if (file-exists? f)
	  (format template filename-version-part)
	  (format template "xxxxxxx"))))
  (append
   (list
    "libiconv-2.dll"
    "longdouble.dll")
   (if extras-only?
       '()
       (cond
	[(or (memq 'racketcgc types)
	     (memq 'gracketcgc types))
	 (list
	  (versionize "libracket~a.dll")
	  (versionize "libmzgc~a.dll"))]
	[(or (memq 'racket3m types)
	     (memq 'gracket3m types))
	 (list
	  (versionize "libracket3m~a.dll"))]
	[(or (memq 'racketcs types)
	     (memq 'gracketcs types))
	 (list
	  (versionize "libracketcs~a.dll"))]))))

(define (search-dll dll)
  (define dll-dir (find-cross-dll-dir))
  (if dll-dir
      (build-path dll-dir dll)
      (let* ([exe-dir
	      (let ([exec (path->complete-path 
			   (find-executable-path (find-system-path 'exec-file))
			   (find-system-path 'orig-dir))])
		(let-values ([(base name dir?) (split-path exec)])
		  base))]
	     [paths (cons
		     exe-dir
		     (path-list-string->path-list
		      (or (getenv "PATH") "")
		      (list (find-system-path 'sys-dir))))])
	(or (ormap (lambda (p)
		     (let ([p (build-path p dll)])
		       (and (file-exists? p)
			    p)))
		   paths)
	    ;; Can't find it, so just use executable's dir:
	    (build-path exe-dir dll)))))
