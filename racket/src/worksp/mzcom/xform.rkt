#lang racket/base
(require (for-label "../../bc/gc2/xform-mod.rkt"))
	 
(define cpp-flags "/D _CRT_SECURE_NO_DEPRECATE /D WIN32 /D _USE_DECLSPECS_FOR_SAL=0 /D _USE_ATTRIBUTES_FOR_SAL=0")
(define includes "/I ../../bc/include /I . /I .. /I ../../mzcom")

(define (xform src dest)
  (parameterize ([current-command-line-arguments
		  (list->vector 
		   (append
		    (list "--indirect"
			  "--depends")
		    (list
		     "--cpp"
		     (format "cl.exe /MT /E ~a ~a" 
			     cpp-flags 
			     includes)
		     "-o"
		     dest
		     src)))])
    (dynamic-require "../../bc/gc2/xform-mod.rkt" #f)))

(xform "../../mzcom/mzobj.cxx" "../../mzcom/mzobj3m.cxx")
