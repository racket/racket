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
          fields mutable immutable parent parent-rtd protocol 
          sealed opaque nongenerative)
  (import (for (rnrs base (6)) run expand)
          (for (rnrs syntax-case (6)) expand)
	  (rename (r6rs private records-explicit)
		  (define-record-type define-record-type/explicit)))

  (define-syntax define-record-type
    ;; Just check syntax, then send off to reference implementation
    (lambda (stx)
      (syntax-case stx ()
        [(_ name-spec clause ...)
         (with-syntax ([(name parts)
                        (syntax-case #'name-spec ()
                          [(name cname pname)
                           (begin
                             (if (not (identifier? #'name))
                                 (syntax-violation #f
                                                   "expected an identifier for the record type name"
                                                   stx
                                                   #'name))
                             (if (not (identifier? #'cname))
                                 (syntax-violation #f
                                                   "expected an identifier for the record type constructor name"
                                                   stx
                                                   #'cname))
                             (if (not (identifier? #'pname))
                                 (syntax-violation #f
                                                   "expected an identifier for the record type predicate name"
                                                   stx
                                                   #'pname))
                             #'(name (name cname pname)))]
                          [name
                           (identifier? #'name)
                           #'(name name)]
                          [_
                           (syntax-violation #f
                                             (string-append
                                              "expected either an identifier for the record type name"
                                              " or a form (<name-id> <constructor-id> <predicate-id>)")
                                             stx
                                             #'name-spec)])])
           (let* ([clauses #'(clause ...)]
                  [extract (lambda (id)
                             (let loop ([clauses clauses][found #f])
                               (cond
                                [(null? clauses) found]
                                [(syntax-case (car clauses) ()
                                   [(tag . body)
                                    (free-identifier=? #'tag id)])
                                 (if found
                                     (syntax-violation #f
                                                       (string-append "duplicate "
                                                                      (symbol->string (syntax->datum id))
                                                                      " clause")
                                                       stx
                                                       (car clauses))
                                     (loop (cdr clauses) (car clauses)))]
                                [else
                                 (loop (cdr clauses) found)])))]
                  [kws (with-syntax ([(kw ...)
                                      #'(fields mutable immutable parent 
                                                protocol sealed opaque nongenerative
                                                parent-rtd)])
                         #'(kw ...))])
             (for-each (lambda (clause)
                         (syntax-case clause ()
                           [(id . _)
                            (and (identifier? #'id)
                                 (let loop ([kws kws])
                                   (and (not (null? kws))
                                        (or (free-identifier=? #'id (car kws))
                                            (loop (cdr kws))))))
                            'ok]
                           [_
                            (syntax-violation #f
                                              (string-append
                                               "expected a `mutable', `immutable', `parent',"
                                               " `protocol', `sealed', `opaque', `nongenerative',"
                                               " or `parent-rtd' clause")
                                              stx
                                              clause)]))
                       clauses)
             (if (and (extract #'parent) (extract #'parent-rtd))
                 (syntax-violation #f
                                   "cannot specify both `parent' and `parent-rtd'"
                                   stx))
             (syntax-case (extract #'fields) ()
               [#f 'ok]
               [(_ spec ...)
                (for-each (lambda (spec)
                            (syntax-case spec (immutable mutable)
                              [(immutable id acc-id)
                               (begin
                                 (if (not (identifier? #'id))
                                     (syntax-violation #f
                                                       "expected a field-name identifier"
                                                       spec
                                                       #'id))
                                 (if (not (identifier? #'acc-id))
                                     (syntax-violation #f
                                                       "expected a field-accessor identifier"
                                                       spec
                                                       #'acc-id)))]
                              [(immutable id)
                               (if (not (identifier? #'id))
                                   (syntax-violation #f
                                                     "expected a field-name identifier"
                                                     spec
                                                     #'id))]
                              [(immutable . _)
                               (syntax-violation #f
                                                 "expected one or two identifiers"
                                                 spec)]
                              [(mutable id acc-id set-id)
                               (begin
                                 (if (not (identifier? #'id))
                                     (syntax-violation #f
                                                       "expected a field-name identifier"
                                                       spec
                                                       #'id))
                                 (if (not (identifier? #'acc-id))
                                     (syntax-violation #f
                                                       "expected a field-accessor identifier"
                                                       spec
                                                       #'acc-id))
                                 (if (not (identifier? #'set-id))
                                     (syntax-violation #f
                                                       "expected a field-mutator identifier"
                                                       spec
                                                       #'set-id)))]
                              [(mutable id)
                               (if (not (identifier? #'id))
                                   (syntax-violation #f
                                                     "expected a field-name identifier"
                                                     spec
                                                     #'id))]
                              [(mutable . _)
                               (syntax-violation #f
                                                 "expected one or three identifiers"
                                                 spec)]
                              [id (identifier? #'id) 'ok]
                              [_ (syntax-violation #f
                                                   "expected an identifier, `immutable' form, or `mutable' form for field"
                                                   stx
                                                   spec)]))
                          #'(spec ...))])

             (let ([p (extract #'parent)])
               (syntax-case p ()
                 [#f 'ok]
                 [(_ id) (identifier? #'id) 'ok]
                 [_ (syntax-violation #f
                                      "expected a single identifier for the parent record type"
                                      p)]))

             (let ([p (extract #'parent-rtd)])
               (syntax-case p ()
                 [#f 'ok]
                 [(_ id cons-id) 'ok]
                 [_ (syntax-violation #f
                                      "expected two expressions for the parent record type and constructor"
                                      p)]))

             (syntax-case (extract #'protocol) ()
               [#f 'ok]
               [(_ expr) 'ok])

             (syntax-case (extract #'sealed) ()
               [#f 'ok]
               [(_ #f) 'ok]
               [(_ #t) 'ok])

             (syntax-case (extract #'opaque) ()
               [#f 'ok]
               [(_ #f) 'ok]
               [(_ #t) 'ok])

             (syntax-case (extract #'nongenerative) ()
               [#f 'ok]
               [(_) 'ok]
               [(_ id) (identifier? #'id) 'ok])

             #'(define-record-type-1 name parts
                 () clause ...)))])))

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
                          (?field-name
                           (identifier? (syntax ?field-name))
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
			  (?clause
			   clause)))
		      (syntax (?field-clause ...)))))
	     (syntax
	      (define-record-type-1 
		?record-name ?record-name-spec
		(?simple-clause ... (fields ?simple-field ...))
		?clause ...)))))))))
