; Derived from:                 	-*-scheme-*-
; "Template.scm" configuration template of *features* for Scheme
;  Copyright (C) 1991, 1992, 1993 Aubrey Jaffer.

; Compatibility file for MzScheme --
;   http://www.plt-scheme.org/software/mzscheme/
; -- and DrScheme --
;   http://www.plt-scheme.org/software/drscheme/
; -- produced by Shriram Krishnamurthi <sk@cs.brown.edu>,
; Mon Feb 10 12:03:53 CST 1997

(require (lib "pretty.ss"))
(unless (memq (system-type) '(unix beos))
  (namespace-require '(lib "date.ss")))
;; See hack at end:
(require (rename mzscheme mz:require require))

;;; (software-type) should be set to the generic operating system type.
;;; UNIX, VMS, MACOS, AMIGA and MS-DOS are supported.

(define (software-type)
  (case (system-type)
    [(unix macosx) 'UNIX]
    [(windows) 'MS-DOS]
    [(macos) 'MACOS]
    [else (system-type)]))

;;; (scheme-implementation-type) should return the name of the scheme
;;; implementation loading this file.

(define (scheme-implementation-type) '|MzScheme|)

;;; (scheme-implementation-home-page) should return a (string) URL
;;; (Uniform Resource Locator) for this scheme implementation's home
;;; page; or false if there isn't one.

(define (scheme-implementation-home-page)
  "http://www.plt-scheme.org/")

;;; (scheme-implementation-version) should return a string describing
;;; the version the scheme implementation loading this file.

(define scheme-implementation-version version)

;;; (implementation-vicinity) should be defined to be the pathname of
;;; the directory where any auxillary files to your Scheme
;;; implementation reside.

(define implementation-vicinity
  (let ([path
	 (or (getenv "PLTHOME")
	     (with-handlers ([void (lambda (x) #f)])
	       (let ([p (collection-path "mzlib")])
		 (let*-values ([(base name dir?) (split-path p)]
			       [(base name dir?) (split-path base)])
		   (and (string? base) base))))
	     (case (system-type)
	       ((unix macosx) "/usr/local/lib/plt")
	       ((windows) "C:\\Program Files\\PLT")
	       ((macos) "My Disk:plt:")))])
    (lambda () path)))

;;; (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.

(define library-vicinity
  (let ((library-path
	 (or
	  ;; Use this getenv if your implementation supports it.
	  (getenv "SCHEME_LIBRARY_PATH")
	  ;; Use this path if your scheme does not support GETENV
	  (with-handlers ([void
			   (lambda (x)
			     (error 'slib-init
				    "can't find SCHEME_LIBRARY_PATH environment variable or \"slib\" collection"))])
	    (collection-path "slib")))))
    (lambda () library-path)))

;;; (home-vicinity) should return the vicinity of the user's HOME
;;; directory, the directory which typically contains files which
;;; customize a computer environment for a user.

(define (home-vicinity)
  (find-system-path 'home-dir))

;;; *FEATURES* should be set to a list of symbols describing features
;;; of this implementation.  Suggestions for features are:

(define *features*
      '(
	source				;can load scheme source files
					;(slib:load-source "filename")
	compiled			;can load compiled files
					;(slib:load-compiled "filename")
	rev4-report			;conforms to
;	rev3-report			;conforms to
;	ieee-p1178			;conforms to
;	sicp				;runs code from Structure and
					;Interpretation of Computer
					;Programs by Abelson and Sussman.
	rev4-optional-procedures	;LIST-TAIL, STRING->LIST,
					;LIST->STRING, STRING-COPY,
					;STRING-FILL!, LIST->VECTOR,
					;VECTOR->LIST, and VECTOR-FILL!
;	rev2-procedures			;SUBSTRING-MOVE-LEFT!,
					;SUBSTRING-MOVE-RIGHT!,
					;SUBSTRING-FILL!,
					;STRING-NULL?, APPEND!, 1+,
					;-1+, <?, <=?, =?, >?, >=?
	multiarg/and-			;/ and - can take more than 2 args.
	multiarg-apply			;APPLY can take more than 2 args.
	rationalize
	delay				;has DELAY and FORCE
	with-file			;has WITH-INPUT-FROM-FILE and
					;WITH-OUTPUT-FROM-FILE
	string-port			;has CALL-WITH-INPUT-STRING and
					;CALL-WITH-OUTPUT-STRING
;	transcript			;TRANSCRIPT-ON and TRANSCRIPT-OFF
	char-ready?
	macro				;has R4RS high level macros
	defmacro			;has Common Lisp DEFMACRO
	eval				;SLIB:EVAL is single argument eval
;	record				;has user defined data structures
	values				;proposed multiple values
	dynamic-wind			;proposed dynamic-wind
	ieee-floating-point		;conforms to
	full-continuation		;can return multiple times
;	object-hash			;has OBJECT-HASH

;	sort
;	queue				;queues
	pretty-print
;	object->string
;	format
;	trace				;has macros: TRACE and UNTRACE
;	compiler			;has (COMPILER)
;	ed				;(ED) is editor
	system				;posix (system <string>)
	getenv				;posix (getenv <string>)
	program-arguments		;returns list of strings (argv)
;	Xwindows			;X support
;	curses				;screen management package
;	termcap				;terminal description package
;	terminfo			;sysV terminal description
	current-time			;returns time in seconds since 1/1/1970
	))

(define program-arguments
  (lambda ()
    (vector->list (current-command-line-arguments))))

(define current-time
  ;; Gives time since 1/1/1970 ...
  ;;   ... GMT for Unix, Windows, and Mac OS X.
  ;;   ... local time for Mac OS.
  (if (memq (system-type) '(unix macosx windows))
      current-seconds
      (let ([zero (find-seconds 0 0 0 1 1 1970)])
	(lambda ()
	  (- (current-seconds) zero)))))

;;; Remainder is modifications of existing code in Template.scm.

;;; (OUTPUT-PORT-WIDTH <port>)
(define (output-port-width . arg) 79)

;;; (OUTPUT-PORT-HEIGHT <port>)
(define (output-port-height . arg) 24)

;;; (CURRENT-ERROR-PORT)
;; Already in MzScheme

;;; (TMPNAM) makes a temporary file name.
(define tmpnam (let ((cntr 100))
		 (lambda () (set! cntr (+ 1 cntr))
			 (string-append "slib_" (number->string cntr)))))

;;; (FILE-EXISTS? <string>)
;; Already in MzScheme

;;; (DELETE-FILE <string>)
;; Already in MzScheme

;;; FORCE-OUTPUT flushes any pending output on optional arg output port
;;; use this definition if your system doesn't have such a procedure.
(define force-output flush-output)

;;; CALL-WITH-INPUT-STRING and CALL-WITH-OUTPUT-STRING are the string
;;; port versions of CALL-WITH-*PUT-FILE.

(define call-with-input-string
  (lambda (string thunk)
    (parameterize ((current-input-port (open-input-string string)))
      (thunk))))

(define call-with-output-string
  (lambda (receiver)
    (let ((sp (open-output-string)))
      (receiver sp)
      (get-output-string sp))))

;;; "rationalize" adjunct procedures.
(define (find-ratio x e)
  (let ((rat (rationalize x e)))
    (list (numerator rat) (denominator rat))))
(define (find-ratio-between x y)
  (find-ratio (/ (+ x y) 2) (/ (- x y) 2)))

;;; CHAR-CODE-LIMIT is one greater than the largest integer which can
;;; be returned by CHAR->INTEGER.
(define char-code-limit 256)

;;; MOST-POSITIVE-FIXNUM is used in modular.scm
(define most-positive-fixnum #x3FFFFFFF) ; 30 bits on 32-bit machines
; (define most-positive-fixnum #x3FFFFFFFFFFFFFFF) ; 62 bits on 64-bit machines

;;; Return argument
(define (identity x) x)

;;; If your implementation provides eval SLIB:EVAL is single argument
;;; eval using the top-level (user) environment.
(define slib:eval eval)

;;; If your implementation provides R4RS macros:
;(define macro:eval slib:eval)
;(define macro:load load)

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "slib:G" (number->string *gensym-counter*))))))


(define *defmacros*
  (list (cons 'defmacro
	      (lambda (name parms . body)
		`(set! *defmacros* (cons (cons ',name (lambda ,parms ,@body))
					 *defmacros*))))))
(define (defmacro? m) (and (assq m *defmacros*) #t))

(define (macroexpand-1 e)
  (if (pair? e)
      (let ((a (car e)))
	(cond ((symbol? a) (set! a (assq a *defmacros*))
	       (if a (apply (cdr a) (cdr e)) e))
	      (else e)))
      e))

(define (macroexpand e)
  (if (pair? e)
      (let ((a (car e)))
	(cond ((symbol? a)
	       (set! a (assq a *defmacros*))
	       (if a (macroexpand (apply (cdr a) (cdr e))) e))
	      (else e)))
      e))

(define base:eval slib:eval)
(define (defmacro:eval x) (base:eval (defmacro:expand* x)))
(define (defmacro:expand* x)
  (slib:require 'defmacroexpand) 
  (apply defmacro:expand* x '()))

(define (defmacro:load <pathname>)
  (slib:eval-load <pathname> defmacro:eval))

(define (slib:eval-load <pathname> evl)
  (if (not (file-exists? <pathname>))
      (set! <pathname> (string-append <pathname> (scheme-file-suffix))))
  (call-with-input-file <pathname>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <pathname>)
	(do ((o (read port) (read port)))
	    ((eof-object? o))
	  (evl o))
	(set! *load-pathname* old-load-pathname)))))

;;; define an error procedure for the library
(define slib:error
  (lambda args
    (let ((cep (current-error-port)))
      (if (provided? 'trace) (print-call-stack cep))
      (apply error "Error:" args))))

;;; define these as appropriate for your system.
(define slib:tab (integer->char 9))
(define slib:form-feed (integer->char 12))

;;; Support for older versions of Scheme.  Not enough code for its own file.
(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l))
(define t #t)
(define nil #f)

;;; Define these if your implementation's syntax can support it and if
;;; they are not already defined.

(define 1+ add1)
(define -1+ sub1)
(define 1- -1+)

(define in-vicinity 
  (lambda args
    (let loop ([args args])
      (cond
       [(null? (cdr args)) (car args)]
       [(string=? "" (car args)) (loop (cdr args))]
       [else (let ([v (loop (cdr args))])
	       (build-path (car args) v))]))))

;;; Define SLIB:EXIT to be the implementation procedure to exit or
;;; return if exitting not supported.
(define slib:exit exit)

;;; Here for backward compatability
(define scheme-file-suffix
  (let ((suffix (case (software-type)
		  ((NOSVE) "_scm")
		  (else ".scm"))))
    (lambda () suffix)))

;;; (SLIB:LOAD-SOURCE "foo") should load "foo.scm" or with whatever
;;; suffix all the module files in SLIB have.  See feature 'SOURCE.

(define (slib:load-source f) (load (string-append f ".scm")))

;;; (SLIB:LOAD-COMPILED "foo") should load the file that was produced
;;; by compiling "foo.scm" if this implementation can compile files.
;;; See feature 'COMPILED.

(define (slib:load-compiled f) (load (string-append f ".zo")))

;;; At this point SLIB:LOAD must be able to load SLIB files.

(define slib:load slib:load-source)

(define slib:warn
  (lambda args
    (let ((cep (current-error-port)))
      (if (provided? 'trace) (print-call-stack cep))
      (display "Warn: " cep)
      (for-each (lambda (x) (display x cep)) args)
      (newline cep))))

(slib:load (in-vicinity (library-vicinity) "require"))

;;; Hack `require' to try to work with both SLIB
;;; and MzScheme:

(define slib:require require)
(define-syntax (require stx)
  (syntax-case stx (quote)
    [_
     (identifier? stx)
     #'slib:require]
    [(_ (quote something))
     #'(slib:require (quote something))]
    [(_ req ...)
     (if (eq? 'top-level (syntax-local-context))
	 #'(mz:require req ...)
	 #'(slib:require req ...))]))

;; The rest are from:

;;;"DrScheme.init" Initialization for SLIB for DrScheme	-*-scheme-*-
;;  Friedrich Dominicus <frido@q-software-solutions.com>
;;  Newsgroups: comp.lang.scheme
;;  Date: 02 Oct 2000 09:24:57 +0200

(define (make-exchanger obj)
  (lambda (rep) (let ((old obj)) (set! obj rep) old)))
(define (open-file filename modes)
  (case modes
    ((r rb) (open-input-file filename))
    ((w wb) (open-output-file filename))
    (else (slib:error 'open-file 'mode? modes))))
(define (port? port) (or (input-port? port) (output-port? port)))
(define (call-with-open-ports . ports)
  (define proc (car ports))
  (cond ((procedure? proc) (set! ports (cdr ports)))
	(else (set! ports (reverse ports))
	      (set! proc (car ports))
	      (set! ports (reverse (cdr ports)))))
  (let ((ans (apply proc ports)))
    (for-each close-port ports)
    ans))
(define (close-port port)
  (cond ((input-port? port)
	 (close-input-port port)
	 (if (output-port? port) (close-output-port port)))
	((output-port? port) (close-output-port port))
	(else (slib:error 'close-port 'port? port))))

