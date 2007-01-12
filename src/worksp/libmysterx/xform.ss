(use-compiled-file-paths null)

(require (lib "restart.ss"))

(define cpp-flags "/D _CRT_SECURE_NO_DEPRECATE /D WIN32")
(define includes 
  (string-append
   "/I ../../mzscheme/include /I . /I .. /I ../../mysterx"
   " /I ../../mzscheme/src" ; for schvers.h
   " /I myssink /I ../../mysterx/myssink"
   " /I myspage /I ../../mysterx/myspage"))

(unless (directory-exists? "xsrc")
  (make-directory "xsrc"))

(define use-precomp "xsrc/precomp.h")

(define (try src dest precomp?)
  (unless (and (file-exists? dest)
	       (let ([t (file-or-directory-modify-seconds dest)])
		 (andmap
		  (lambda (dep)
		    (let ([dep (cond
				[(bytes? dep) (bytes->path dep)]
				[else dep])])
		      (> t (file-or-directory-modify-seconds dep))))
		  (append (if precomp?
			      null
			      (if use-precomp (list use-precomp) null))
			  (list src)
			  (let ([deps (path-replace-suffix dest #".sdep")])
			    (if (file-exists? deps)
				(with-input-from-file deps read)
			      null))))))
    (xform src dest precomp?)))

(define (xform src dest precomp?)
  (parameterize ([use-compiled-file-paths (list "compiled")])
    (restart-mzscheme #() (lambda (x) x)
                      (list->vector 
                       (append
                        (list "-r"
                              "../../mzscheme/gc2/xform.ss"
                              "--setup"
                              "--indirect"
                              "--depends")
			(cond
			 [precomp? (list "--precompile")]
			 [use-precomp (list "--precompiled" use-precomp)]
			 [else null])
                        (list
                         "--cpp"
                         (format "cl.exe /MT /E ~a ~a" 
                                 cpp-flags 
                                 includes)
                         "-o"
                         dest
                         src)))
                      void)))

(with-output-to-file
 "gc_traverse.inc"
 (lambda ()
   (load "../../mysterx/gc-trav.ss"))
 'truncate)

(try "../../mysterx/precomp.cxx"
     use-precomp
     #t)

(for-each (lambda (file)
	    (try (format "../../mysterx/~a.cxx" file)
		 (format "xsrc/~a3m.cxx" file)
		 #f))
	  '("mysterx"
	    "array"
	    "browser"
	    "bstr"
	    "comtypes"
	    "htmlevent"
	    "htmlutil"))
