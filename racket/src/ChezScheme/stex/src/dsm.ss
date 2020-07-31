;;; dsm.ss
;;;
;;; Copyright (c) 1998-2016 R. Kent Dybvig and Oscar Waddell
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; authors: R. Kent Dybvig and Oscar Waddell

(library (dsm) (export define-syntactic-monad) (import (chezscheme))
  (define-syntax define-syntactic-monad
    (syntax-rules ()
      [(_ name formal ...)
       (andmap identifier? #'(name formal ...))
       (define-syntax name
         (lambda (x)
           (syntax-case x (lambda case-lambda)
             [(key lambda more-formals . body)
              (with-implicit (key formal ...)
                #'(lambda (formal ... . more-formals) . body))]
             [(key case-lambda (more-formals . body) (... ...))
              (with-implicit (key formal ...)
                #'(case-lambda ((formal ... . more-formals) . body) (... ...)))]
             [(key proc ((x e) (... ...)) arg (... ...))
              (andmap identifier? #'(x (... ...)))
              (with-implicit (key formal ...)
                (for-each
                  (lambda (x)
                    (unless (let mem ((ls #'(formal ...)))
                              (and (not (null? ls))
                                   (or (free-identifier=? x (car ls))
                                       (mem (cdr ls)))))
                      (syntax-error x (format "in syntactic monad ~s, unrecognized identifier" 'name))))
                  #'(x (... ...)))
                (with-syntax ([(t (... ...)) (generate-temporaries #'(arg (... ...)))])
                  #'(let ((p proc) (x e) (... ...) (t arg) (... ...))
                      (p formal ... t (... ...)))))]
             [(key proc) #'(key proc ())])))]))
  )
