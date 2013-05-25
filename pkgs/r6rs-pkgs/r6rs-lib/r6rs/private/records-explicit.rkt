#!r6rs

;; Implementation of explicit-naming layer for R6RS Records

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

(library (r6rs private records-explicit)
  (export define-record-type
	  record-type-descriptor
	  record-constructor-descriptor
          fields mutable immutable parent parent-rtd protocol 
          sealed opaque nongenerative)
  (import (for (rnrs base) run expand)
	  (rnrs records procedural)
          (for (rnrs syntax-case) expand))

  (define-syntax define-aux
    (syntax-rules ()
      [(_ id) (define-syntax id (syntax-rules ()))]
      [(_ id ...) (begin (define-aux id) ...)]))

  (define-aux
    fields mutable immutable parent parent-rtd protocol sealed opaque nongenerative)

  ;; ASSQ at the syntax level
  (define-syntax define-alist-extractor
    (syntax-rules ()
      ((define-alist-extractor ?name ?name/cps ?tag ?default)
       (begin

	 (define-syntax ?name/cps
	   (syntax-rules (?tag)
	     ((?name/cps () ?k . ?rands)
	      (?k ?default . ?rands))
	     ((?name/cps ((?tag ?val) . ?rest) ?k . ?rands)
	      (?k ?val . ?rands))
	     ((?name/cps ((?another-tag . ?vals) . ?rest) ?k . ?rands)
	      (?name/cps ?rest ?k . ?rands))))
	 (define-syntax ?name
	   (syntax-rules (?tag)
	     ((?name ())
	      ?default)
	     ((?name ((?tag ?val) . ?rest))
	      ?val)
	     ((?name ((?another-tag . ?vals) . ?rest))
	      (?name ?rest))))))))

  (define-syntax extract-parent/sel 
    (syntax-rules (parent parent-rtd)
      ((_ () ?sel) #f)
      ((_ ((parent ?val) . ?rest) ?sel)
       (?sel (record-type-descriptor ?val)
             (record-constructor-descriptor ?val)))
      ((_ ((parent-rtd ?rtd ?cons) . ?rest) ?sel)
       (?sel ?rtd ?cons))
      ((_ ((?another-tag . ?vals) . ?rest) ?sel)
       (extract-parent/sel ?rest ?sel))))
  (define-syntax sel-record-type-descriptor
    (syntax-rules ()
      [(_ ?rtd ?cons) ?rtd]))
  (define-syntax sel-record-constructor-descriptor
    (syntax-rules ()
      [(_ ?rtd ?cons) ?cons]))

  (define-alist-extractor extract-sealed extract-sealed/cps sealed #f)
  (define-alist-extractor extract-opaque extract-opaque/cps opaque #f)
  (define-alist-extractor extract-protocol extract-protocol/cps
    protocol #f)
  (define-alist-extractor extract-nongenerative extract-nongenerative/cps nongenerative #f)

  (define-alist-extractor extract-record-name extract-record-name/cps record-name cant-happen)
  (define-alist-extractor extract-constructor-name extract-constructor-name/cps
    constructor-name cant-happen)
  (define-alist-extractor extract-predicate-name extract-predicate-name/cps
    predicate-name cant-happen)


  (define-syntax define-record-type
    (syntax-rules ()
      ((define-record-type (?record-name ?constructor-name ?predicate-name)
         ?clause ...)
       (define-record-type-1
         ((record-name ?record-name)            ; prop alist
          (constructor-name ?constructor-name)
          (predicate-name ?predicate-name))
         ()                             ; fields
         ?clause ...))))

  (define-syntax define-record-type-1
    (syntax-rules (parent parent-rtd protocol sealed nongenerative opaque fields mutable immutable)
      ;; find PARENT clause
      ((define-record-type-1 ?props
	 ?field-specs
	 (parent ?parent)
	 ?clause ...)
       (define-record-type-1 ((parent ?parent) . ?props)
	 ?field-specs
	 ?clause ...))
      ((define-record-type-1 ?props
	 ?field-specs
	 (parent-rtd ?parent ?cons)
	 ?clause ...)
       (define-record-type-1 ((parent-rtd ?parent ?cons) . ?props)
	 ?field-specs
	 ?clause ...))

      ;; find PROTOCOL clause
      ((define-record-type-1 ?props
	 ?field-specs
	 (protocol ?protocol)
	 ?clause ...)
       (define-record-type-1 ((protocol ?protocol) . ?props)
	 ?field-specs
	 ?clause ...))

      ;; find SEALED clause
      ((define-record-type-1 ?props
	 ?field-specs
	 (sealed #t)
	 ?clause ...)
       (define-record-type-1 ((sealed #t) . ?props)
	 ?field-specs
	 ?clause ...))
      ((define-record-type-1 ?props
	 ?field-specs
	 (sealed #f)
	 ?clause ...)
       (define-record-type-1 ((sealed #f) . ?props)
	 ?field-specs
	 ?clause ...))

      ;; find OPAQUE clause
      ((define-record-type-1 ?props
	 ?field-specs
	 (opaque #t)
	 ?clause ...)
       (define-record-type-1 ((opaque #t) . ?props)
	 ?field-specs
	 ?clause ...))
      ((define-record-type-1 ?props
	 ?field-specs
	 (opaque #f)
	 ?clause ...)
       (define-record-type-1 ((opaque #f) . ?props)
	 ?field-specs
	 ?clause ...))

      ;; parse FIELDS clause

      ;; base case
      ((define-record-type-1 ?props
	 (?field-spec ...)
	 (fields)
	 ?clause ...)
       (define-record-type-1 ?props
	 (?field-spec ...)
	 ?clause ...))

      ;; complete spec
      ((define-record-type-1 ?props
	 (?field-spec ...)
	 (fields (immutable ?field-name ?accessor) ?rest ...)
	 ?clause ...)
       (define-record-type-1 ?props
	 (?field-spec ... (immutable ?field-name (?accessor))) 
	 (fields ?rest ...)
	 ?clause ...))

      ((define-record-type-1 ?props
	 (?field-spec ...)
	 (fields (mutable ?field-name ?accessor ?mutator) ?rest ...)
	 ?clause ...)
       (define-record-type-1 ?props
	 (?field-spec ... (mutable ?field-name (?accessor ?mutator)))
	 (fields ?rest ...)
	 ?clause ...))

      ;; find NONGENERATIVE clause
      ((define-record-type-1 ?props
	 ?field-specs
	 (nongenerative ?uid)
	 ?clause ...)
       (define-record-type-1 ((nongenerative '?uid) . ?props)
	 ?field-specs
	 ?clause ...))

      ;; generate code
      ((define-record-type-1 ?props
	 ((?mutability ?field-name ?procs) ...))

       (begin
	 ;; where we need LETREC* semantics if this is to work internally

	 (define $rtd
	   (make-record-type-descriptor (extract-record-name/cps ?props quote)
					(extract-parent/sel ?props sel-record-type-descriptor)
					(extract-nongenerative ?props)
					(extract-sealed ?props)
					(extract-opaque ?props)
					'#((?mutability ?field-name) ...)))

	 (define $constructor-descriptor
	   (make-record-constructor-descriptor
	    $rtd
	    (extract-parent/sel ?props sel-record-constructor-descriptor)
	    (extract-protocol ?props)))

	 (extract-record-name/cps
	  ?props
	  define-record-type-name $rtd $constructor-descriptor)

	 (extract-constructor-name/cps
	  ?props
	  define
	  (record-constructor $constructor-descriptor))
	 
	 (extract-predicate-name/cps ?props
				     define (record-predicate $rtd))

	 (define-record-fields $rtd
	   0 (?field-name ?procs) ...)))))

  (define-syntax define-record-type-name
    (syntax-rules ()
      ((define-record-type-name ?name ?rtd ?constructor-descriptor)
       (define-syntax ?name
         (make-variable-transformer
          (lambda (stx)
            (syntax-case stx (descriptor constructor-descriptor set!)
              (?name (identifier? #'?name) #'?rtd)
              ((set! ?name . rest) (syntax-violation #f "cannot mutate record-type descriptor binding" stx #'name))
              ((?name descriptor) #'?rtd)
              ((?name constructor-descriptor) #'?constructor-descriptor))))))))

  (define-syntax no-record-type
    (syntax-rules (descriptor constructor-descriptor)
      ((?name descriptor) #f)
      ((?name constructor-descriptor) #f)))

  (define-syntax record-type-descriptor
    (syntax-rules ()
      ((record-type-descriptor ?record-type)
       (?record-type descriptor))))

  (define-syntax record-constructor-descriptor
    (syntax-rules ()
      ((record-constructor-descriptor ?record-type)
       (?record-type constructor-descriptor))))

  (define-syntax define-record-fields
    (syntax-rules ()
      ((define-record-fields ?rtd ?index)
       (begin))
      ((define-record-fields ?rtd ?index (?field-name ?procs) . ?rest)
       (begin
	 (define-record-field ?rtd ?field-name ?index ?procs)
	 (define-record-fields ?rtd (+ 1 ?index) . ?rest)))))

  (define-syntax define-record-field
    (syntax-rules ()
      ((define-record-field ?rtd
	 ?field-name ?index (?accessor-name))
       (define ?accessor-name
	 (record-accessor ?rtd ?index)))
      ((define-record-field ?rtd
	 ?field-name ?index (?accessor-name ?mutator-name))
       (begin
	 (define ?accessor-name
	   (record-accessor ?rtd ?index))
	 (define ?mutator-name
	   (record-mutator ?rtd ?index)))))))
