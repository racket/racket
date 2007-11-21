; Module header is generated automatically
#cs(module libmisc mzscheme
(require (rename (lib "pretty.ss") pp pretty-print))
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tmp/ssax"))

;; Portable Library of Miscellaneous Functions
;; $Id: libmisc.scm,v 1.7 2002/10/08 15:47:21 kl Exp kl $ 

;==============================================================================
; Miscellaneous

; Identity function
(define (self x) x)

;==============================================================================
; Lists 

; Returns #f if given list is empty and the list itself otherwise
; It is intended for emulation of MIT-style empty list treatment
; (not-null? <list>) may be considered as a counterpart to MIT-style <list>
(define (not-null? l)
  (if (null? l)
    #f
    l))

;------------------------------------------------------------------------------
; Converters

; Transform a list of characters to a symbol
(define (list->symbol lst) 
  (string->symbol (apply string lst)))

; Elements if given list <l>, which are supposed to be strings,
; are returned as a string separated by sep-str 
; or space separated if <sep-str> is omitted
(define (list-to-string l . sep-str)
  (let ((sp-st (if (null? sep-str) " " (car sep-str))))
    (if (not (null? l))
  (let rpt ((x l) (rez ""))
  (if (null? (cdr x))
    (string-append rez (car x))
    (rpt (cdr x) (string-append rez (car x) sp-st))))
  ""
  )))

; Convert a string separated by (car rest) to a list of lines
; If the rest is omitted, then #\space is used
(define (string-to-list str . rest)
  (let ((lngth (string-length str))
	(sep-char (if (null? rest)
		    #\space
		    (car rest)))) 
  (let rpt ((indx 0) (rzt '()))
    (let seek ((i 0))
    (cond 
      ((= lngth (+ i indx))
	  (reverse (cons (substring str indx lngth) rzt))
       )
      ((char=? (string-ref str (+ i indx)) sep-char) 
      (rpt (+ indx i 1) 
	   (cons (substring str indx (+ indx i)) rzt)))
      (else (seek (+ i 1))))))))

;==============================================================================
; Strings

; Return a string where every line of given <text> is commented out
; using <comment-string>
(define (comment-out text comment-string)
   (let rpt ((txt (reverse (string-to-list text #\newline))) (rzt ""))
      (if (null? txt) 
	rzt
	(rpt (cdr txt) (string-append comment-string (car txt) "\n" rzt)))))
      
; Reads all the characters up to the end of the line and put
; them in a string.
; Returns a string containing all the characters read, including
; the end-of-line character
; If the line read is eof-object terminated, then it is returned 
; with eof-object replaced by #\newline
; If the eof-object is the only one character read,
; then it is returned as is
(define (read-whole-line . port)
  (let ((p (if (null? port)
	     (current-input-port)
	     (car port))))
  (let rpt ((l '())
	    (c (read-char p)))
    (cond
      ((and (eof-object? c) (null? l)) c)
      ((or (eof-object? c) (char=? c #\newline)) 
      (list->string (reverse (cons #\newline l))))
      (else
      (rpt (cons c l) (read-char p)))))))

; Skip all the leading characters of a given string <str> which are members
; of <skip-chars> list and return the substring remaining
(define (skip-prefix skip-chars str)
  (let ((strl (string-length str)))
  (do ((i 0 (+ i 1))) 
       ((or (>= i strl) 
	    (not (memq (string-ref str i)
		       skip-chars)))
	(substring str i strl))
       )))

;==============================================================================
; System

; Default operating system 
(define *OPERATING-SYSTEM* 'unix)

;==============================================================================
; IO related

; Newline string
(define (nl-string . op-system)
  (case (if (null? op-system)
	  *OPERATING-SYSTEM*
	  (car op-system))
    ((UNIX) (string (integer->char 10)))
    ((WIN)  (string (integer->char 13) (integer->char 10)))
    ((MAC)  (string (integer->char 13)))
    (else (cerr nl "Unsupported operating system: " op-system nl)
	  (exit))))

; cout redirection to a file with the given "fname"
(define (make-cout fname)
 (let ((o-port
	 (open-output-file fname)))
   (lambda args
  (for-each (lambda (x)
              (if (procedure? x) 
		(display (x) o-port) 
		(display x o-port)))
            args))))

; Like pp, but symbols are quoted
(define (ppw obj . port)
  (let ((port (if (null? port) (current-output-port) (car port))))
    (begin
      (and (symbol? obj)
	   (display "'" port))
      (pp obj port))))

;------------------------------------------------------------------------------
; "Controlled verbosity" messages 

(define (tee tag x)
  (cerr tag x nl)
  x)

(define (tee-1 tag x)
  x)

(define (tee-2 tag x)
  x)

(define (tee-3 tag x)
  x)

(define (tee-4 tag x)
  x)

(define (verb-1 . x)
  #f)

(define (verb-2 . x)
  #f)

(define (verb-3 . x)
  #f)

(define (verb-4 . x)
   #f)

; DL: commented this non-functional acrobatics out
;(define (set-verbosity-4)
;  (set-verbosity-3)
;  (set! verb-4 (lambda mes (apply cerr mes) (cerr nl)))
;  (set! tee-4 (lambda (tag x) (cerr tag x nl) x)))
;
;(define (set-verbosity-3)
;  (set-verbosity-2)
;  (set! verb-3 (lambda mes (apply cerr mes) (cerr nl)))
;  (set! tee-3 (lambda (tag x) (cerr tag x nl) x)))
;
;(define (set-verbosity-2)
;  (set-verbosity-1)
;  (set! verb-2 (lambda mes (apply cerr mes) (cerr nl)))
;  (set! tee-2 (lambda (tag x) (cerr tag x nl) x)))
;
;(define (set-verbosity-1)
;  (set! verb-1 (lambda mes (apply cerr mes) (cerr nl)))
;  (set! tee-1 (lambda (tag x) (cerr tag x nl) x)))

;==============================================================================
; Command line parameters parsing
;@requires util.scm string-prefix? substring?
;@requires myenv.scm cerr ++

; NOTE: This function doesn't require any SXML software, but SXPath is
; a natural way to operate on its result.

; The function accepts a command line as a list, parse it and returns
; SXML element: 
;   (command-line 
;       (arg 'arg-value')*             ; one per argument
;       ('opt-name'                    ; one per option
;          (@ (type { "--" | "-" }))? 
;          'opt-value'?)*
;    )
;
;  The function obtains options and their arguments from a list of 
; parameters that follows the standard POSIX.2 option syntax.
; It recognizes a subset of POSIX.2 options syntax wich may be unambiguously 
; parsed without explicit description.
; Supported types of options are: 
;  Short without arguments:  -o 
;  Short combined:           -abc 
;      which is equal to:    -a -b -c 
;  Long without arguments:   --opt 
;  Long with argument:       --opt=val
; 
; The function may accept an optional second argument - a list of 
; possible options. Each option in this list has to be represented as a string.
; Short options are represented without leading dash, while long option
; are represented with both leading dashes presented. 
; Example '("v" "--update").
; If the list of acceptable options was given, and command line contains
; an option not included in this list, then the function will print an
; "Invalid option" error message and (exit -1).
;
; The function doesn't use any global variables.
(define (argv->sxml argv . options)
  (let* ((vopt (if (null? options) #f (car options)))
	 (test-valid (lambda(opt . fopt)
		       (and vopt
			    (not (member opt vopt))
			    (begin (cerr nl "Invalid option: " opt " " 
					 (if (pair? fopt) fopt "") nl) 
				   (exit -1))))))
    (cons 
      'command-line 
      (let rpt ((cl argv)
		(rez '()))
	(cond
	  ((null? cl)
	   (reverse rez))
	  ((string=? (car cl) "--")
	   (append (reverse rez) (map 
				   (lambda(x)
				     `(arg ,x))
				   (cdr cl))))
	  (else (rpt 
		  (cdr cl)
		  (append
		    (cond
		      ; Long option
		      ((string-prefix? "--" (car cl))
		       (cond 
			 ; with argument
			 ((substring? "=" (car cl))
			  =>(lambda(pos)
				(test-valid 
			       (substring (car cl) 0 pos)
					    (car cl))
				`((,(string->symbol 
				      (substring (car cl) 2 pos) )  ; option
				    (@ (type "--"))
				    ,(substring (car cl) (++ pos)   ; argument
					       (string-length (car cl))))
				  )))
			 ; without argument
			 (else  
			   (test-valid (car cl))
			   `((,(string->symbol 
				 (substring (car cl) 2 
					    (string-length (car cl)))) 
			       (@ (type "--")))
			     ))))
		      ; short option
		      ((string-prefix? "-" (car cl))
		       (map
			 (lambda (x) 
			   (let ((opt (string x)))
			     (test-valid opt (car cl))
			     `(,(string->symbol opt) 
				(@ (type "-")))))
			 (cdr (string->list (car cl)))))
		      ; non-option
		      (else `((argument ,(car cl)))))
		    rez))))
	))))

;==============================================================================
; A minimalistic and pure functional record type.

; A record  constructor, which returns record as a function.
; This returned function may be used as:
;   a field accessor 
;        -- returns value of a specified field
;           if applyed to an only parameter of type symbol (field name)
;        -- returns a list of record fields as a list of (<name> <value>) lists
;           if called without parameters
;   a modifier for some elements of the record
;        -- if its parameters are lists whose CARs are names of record fields
;           (alteration descriptors). This function doesn't modify the original
;           record but returns the record modified.
; Two forms of alteration descriptors are supported:
;   1. (<field-name> <new-value>)
;     Specifies new value for the field <field-name>.
;   2. (<field-name> => <expression>)
;     The <expression> must be a procedure that accepts one argument; 
;     this procedure is then called on the value of the <field-name> field
;     and the value returned by this procedure is the new value of this field.
;     Both <field-name> and => has to be symbols.
; Note: a type of record constructed with "lambda-tuple" is not distinct
; from "procedure" type.
(define (lambda-tuple . elts)
  (lambda param
    (cond 
      ((null? param) elts)
      ((symbol? (car param))
       (cond
	 ((assq (car param) elts)
	  => cadr)
	 ((eq? '*LT-ADD* (car param))
	  (apply lambda-tuple (append elts (cdr param))))
	 (else (verb-4 nl "Lambda-tuple field name not found: " (car param)
	        nl "Valid names are: " (map car elts) nl)
	       '*LT-NOT-FOUND*
	       )))
      (else (apply lambda-tuple
		   (map
		     (lambda(e)
		       (cond 
			 ((assq (car e) param)
			  => (lambda(mut) 
			       (list (car e)
				     (if (eq? '=> (cadr mut))
				       ((caddr mut) (cadr e))
				       (cadr mut)))))
			 (else e)))
		     elts))))))

(provide (all-defined)))
