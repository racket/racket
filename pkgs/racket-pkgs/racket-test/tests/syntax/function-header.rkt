#lang racket

(require syntax/parse
         syntax/parse/lib/function-header
         rackunit)

(define-binary-check (syntax-check-equal? actual expected)
  (check-equal? (syntax->datum actual)
                (syntax->datum expected)))

(syntax-check-equal?
 (syntax-parse #'(b)
   [(a:arg) #'t])
 #'t)

(syntax-check-equal?
 (syntax-parse #'(b)
   [(a:arg) #'(a.name a.kw a.default)])
 #'(b #f #f))

(syntax-check-equal?
 (syntax-parse #'(#:keyword argument)
   [(a:arg) #'(a.name a.kw a.default)])
 #'(argument #:keyword #f))

(syntax-check-equal?
 (syntax-parse #'(#:keyword [optional argument])
   [(a:arg) #'(a.name a.kw a.default)])
 #'(optional #:keyword argument))

(syntax-check-equal?
 (syntax-parse #'([optional argument])
   [(a:arg) #'(a.name a.kw a.default)])
 #'(optional #f argument))

(syntax-check-equal?
 (syntax-parse #'(a b)
   [(a:arg) #'f]
   [_       #'t])
 #'t)

(syntax-check-equal?
 (syntax-parse #'(#:keyword)
   [(a:arg) #'f]
   [_       #'t])
 #'t)

(syntax-check-equal?
 (syntax-parse #'(a b c)
   [a:args #'(a a.params)])
 #'((a b c) (a b c)))

(syntax-check-equal?
 (syntax-parse #'(a #:keyword arg #:optional [keyword arg] . rest)
   [a:args #'(a a.params)])
 #'((a #:keyword arg #:optional [keyword arg] . rest)
    (a arg keyword rest)))

(syntax-check-equal?
 (syntax-parse #'([optional before] required)
   [a:args #'f]
   [_      #'t])
 #'t)

(syntax-check-equal?
 (syntax-parse #'(a [optional arg] . rest)
   [a:args #'(a a.params)])
 #'((a [optional arg] . rest)
    (a optional rest)))

(syntax-check-equal?
 (syntax-parse #'(f a b c)
   [a:function-header #'(a a.params)])
 #'((f a b c) (a b c)))

(syntax-check-equal?
 (syntax-parse #'((f doing) currying)
   [a:function-header #'(a a.params)])
 #'(((f doing) currying)
    (doing currying)))
