#!r6rs

;; Implementation of implicit-naming layer for R6RS Records

;; Based on the SRFI implementation:
; Copyright (C) Michael Sperber (2005). All Rights Reserved. 
; 
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(library (rnrs records syntactic (6))
  (export define-record-type
	  record-type-descriptor
	  record-constructor-descriptor
          fields mutable immutable parent protocol 
          sealed opaque nongenerative)
  (import (for (rnrs base (6)) run expand)
          (for (rnrs syntax-case (6)) expand)
	  (rename (r6rs private records-explicit)
		  (define-record-type define-record-type/explicit)))

  ;; R5RS part of the implementation of DEFINE-RECORD-TYPE for Records SRFI

  (define-syntax define-record-type
    (syntax-rules ()
      ((define-record-type (?record-name ?constructor-name ?predicate-name)
	 ?clause ...)
       (define-record-type-1 ?record-name (?record-name ?constructor-name ?predicate-name)
	 ()
	 ?clause ...))
      ((define-record-type ?record-name
	 ?clause ...)
       (define-record-type-1 ?record-name ?record-name
	 ()
	 ?clause ...))))

  (define-syntax define-record-type-1
    (syntax-rules (fields)

      ;; find FIELDS clause
      ((define-record-type-1 ?record-name ?record-name-spec
	 (?simple-clause ...)
	 (fields ?field-spec ...)
	 ?clause ...)
       (process-fields-clause (fields ?field-spec ...)
			      ?record-name ?record-name-spec
			      (?simple-clause ...)
			      ?clause ...))

      ;; collect all other clauses
      ((define-record-type-1 ?record-name ?record-name-spec
	 (?simple-clause ...)
	 ?clause0
	 ?clause ...)
       (define-record-type-1 ?record-name ?record-name-spec
	 (?simple-clause ... ?clause0)
	 ?clause ...))

      ;; pass it on
      ((define-record-type-1 ?record-name ?record-name-spec
	 (?simple-clause ...))

       (define-record-type-2 ?record-name ?record-name-spec
	 (?simple-clause ...)))))

  ;; syntax-rules part

  (define-syntax define-record-type-2
    (lambda (form)
      (syntax-case form ()
	((_ ?record-name (?record-name-2 ?constructor-name ?predicate-name)
	    (?simple-clause ...))

	 (syntax
	  (begin
	    (define-record-type/explicit (?record-name ?constructor-name ?predicate-name)
	      ?simple-clause ...))))

	((_ ?record-name ?record-name-2
	    (?simple-clause ...))

	 (with-syntax ((?constructor-name
			(datum->syntax (syntax ?record-name)
					      (string->symbol
					       (string-append "make-"
							      (symbol->string
							       (syntax->datum
								(syntax ?record-name)))))))
		       (?predicate-name
			(datum->syntax (syntax ?record-name)
					      (string->symbol
					       (string-append (symbol->string
							       (syntax->datum
								(syntax ?record-name)))
							      "?")))))
	   
	   
	   (syntax
	    (define-record-type-2 ?record-name (?record-name ?constructor-name ?predicate-name)
	      (?simple-clause ...))))))))

  (define-syntax process-fields-clause
    (lambda (form)
      (syntax-case form (fields mutable immutable)
	((_ (fields ?field-clause ...)
	    ?record-name ?record-name-spec
	    (?simple-clause ...)
	    ?clause ...)

	 (let ((record-name (symbol->string (syntax->datum (syntax ?record-name)))))
	   (with-syntax
	       (((?simple-field ...)
		 (map (lambda (clause)
			(syntax-case clause (mutable immutable)
			  ((immutable ?field-name)
			   (with-syntax ((?accessor-name
					  (datum->syntax
					   (syntax ?field-name)
					   (string->symbol
					    (string-append record-name "-"
							   (symbol->string
							    (syntax->datum
							     (syntax ?field-name))))))))
			     (syntax
			      (immutable ?field-name ?accessor-name))))
			  ((mutable ?field-name)
			   (with-syntax ((?accessor-name
					  (datum->syntax
					   (syntax ?field-name)
					   (string->symbol
					    (string-append record-name "-"
							   (symbol->string
							    (syntax->datum
							     (syntax ?field-name)))))))
					 (?mutator-name
					  (datum->syntax
					   (syntax ?field-name)
					   (string->symbol
					    (string-append record-name "-"
							   (symbol->string
							    (syntax->datum
							     (syntax ?field-name)))
							   "-set!")))))
			     (syntax
			      (mutable ?field-name ?accessor-name ?mutator-name))))
			  (?clause
			   clause)))
		      (syntax (?field-clause ...)))))
	     (syntax
	      (define-record-type-1 
		?record-name ?record-name-spec
		(?simple-clause ... (fields ?simple-field ...))
		?clause ...)))))))))
