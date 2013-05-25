;; The versions from `racket/base' don't support rest args.

#|

Modified to use `syntax-parse' and multiple macros by Sam
Tobin-Hochstadt, 2011.

The original:

Copyright (C) Lars T Hansen (1999). All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax let-values
  (syntax-parser
    ((let-values (?binding ...) ?body0 ?body1 ...)
     #'(let-values/bind (?binding ...) () (begin ?body0 ?body1 ...)))))

(define-syntax let-values/bind
  (syntax-parser
    ((let-values/bind () ?tmps ?body)
     #'(let ?tmps ?body))
    ((let-values/bind ((?b0 ?e0) ?binding ...) ?tmps ?body)
     #'(let-values/mktmp ?b0 ?e0 () (?binding ...) ?tmps ?body))))

(define-syntax let-values/mktmp
  (syntax-parser
    ((let-values/mktmp () ?e0 ?args ?bindings ?tmps ?body)
     #'(call-with-values
           (lambda () ?e0)
         (lambda ?args
           (let-values/bind ?bindings ?tmps ?body))))

    ((let-values/mktmp (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     #'(let-values/mktmp ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))

    ((let-values/mktmp ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     #'(call-with-values (lambda () ?e0)
         (lambda (?arg ... . x)
           (let-values/bind ?bindings (?tmp ... (?a x)) ?body))))))

(define-syntax let*-values
  (syntax-parser
    ((let*-values () ?body0 ?body1 ...)
     #'(begin ?body0 ?body1 ...))

    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     #'(let-values (?binding0)
         (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

(provide let-values let*-values)
