#lang scheme/base

; PLT module definition for vector types for R6RS Records

; Copyright (C) Matthew Flatt (2006). All Rights Reserved. 
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

(require scheme/mpair)

(provide (rename-out [make-a-vector-type make-vector-type])
         vector-type?
         vector-type-data
         vector-type-predicate
         typed-vector-constructor
         typed-vector-accessor typed-vector-mutator
         typed-vector?
         typed-vector-type)

(define-struct vector-type (data field-count supertype
                                 struct-type constructor predicate accessor mutator))

(define-values (prop:typed-vector typed-vector? typed-vector-ref)
  (make-struct-type-property 'typed-vector))

(define (make-a-vector-type name supertype data field-mutability opaque?)
  (let* ([super-field-count (if supertype
                                (vector-type-field-count supertype)
                                0)]
         [field-mutability (list-tail (mlist->list field-mutability) super-field-count)]
         [bx (box #f)])
    (let-values ([(struct: make-s s? s-ref s-set!)
                  (make-struct-type name 
                                    (and supertype
                                         (vector-type-struct-type supertype))
                                    (length field-mutability) 0 #f
                                    (append (list (cons prop:typed-vector bx))
                                            (if opaque?
                                                null
                                                ;; `equal?' shouldn't work on transparent structs:
                                                (list
                                                 (cons prop:equal+hash
                                                       (list 
                                                        (lambda (a b equal?) (eqv? a b))
                                                        (lambda (a hash-code) (hash-code a))
                                                        (lambda (a hash-code) (hash-code a)))))))
                                    (and opaque? (current-inspector))
                                    #f ; not a procedure
                                    (let loop ([field-mutability field-mutability]
                                               [index 0])
                                      (cond
                                       [(null? field-mutability) null]
                                       [(not (car field-mutability)) (cons index
                                                                           (loop (cdr field-mutability)
                                                                                 (add1 index)))]
                                       [else (loop (cdr field-mutability) (add1 index))])))])
      (let ([vt (make-vector-type data 
                                  (+ (length field-mutability) super-field-count)
                                  supertype
                                  struct: make-s s? 
                                  s-ref s-set!)])
        (set-box! bx vt)
        vt))))

(define (vector-type-index t pos)
  (let* ([supertype (vector-type-supertype t)]
         [super-field-count (if supertype
                                (vector-type-field-count supertype)
                                0)])
    (if (pos . < . super-field-count)
        (vector-type-index supertype pos)
        (- pos super-field-count))))

(define (typed-vector-constructor t)
  (vector-type-constructor t))

(define (typed-vector-type v)
  (unbox (typed-vector-ref v)))

(define (typed-vector-accessor t pos)
  (make-struct-field-accessor (vector-type-accessor t) (vector-type-index t pos)))

(define (typed-vector-mutator t pos)
  (make-struct-field-mutator (vector-type-mutator t) (vector-type-index t pos)))
