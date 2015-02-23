#lang racket/base

(require syntax/parse
         syntax/parse/lib/function-header
         rackunit
         "setup.rkt")

(test-case "basic"
 (syntax-parse #'(b)
   [(a:formal)
    (s= t 't)]))

(test-case "formal: id"
 (syntax-parse #'(b)
   [(a:formal)
    (s= a.name 'b)
    (a= a.kw #f)
    (a= a.default #f)]))

(test-case "formal: kw arg"
 (syntax-parse #'(#:keyword argument)
   [(a:formal)
    (s= a.name 'argument)
    (s= a.kw '#:keyword)
    (a= a.default #f)]))

(test-case "formal: kw arg w/ default"
 (syntax-parse #'(#:keyword [optional argument])
   [(a:formal)
    (s= a.name 'optional)
    (s= a.kw '#:keyword)
    (s= a.default 'argument)]))

(test-case "formal: plain arg w/ default"
 (syntax-parse #'([optional argument])
   [(a:formal)
    (s= a.name 'optional)
    (a= a.kw #f)
    (s= a.default 'argument)]))

(test-case "bad formal: two args"
 (check-equal?
  (syntax-parse #'(a b)
    [(a:formal) 'ok]
    [_          'bad])
  'bad))

(test-case "bad formal: keyword"
 (check-equal?
  (syntax-parse #'(#:keyword)
    [(a:formal) 'ok]
    [_          'bad])
  'bad))

(test-case "formals: simple"
 (syntax-parse #'(a b c)
   [a:formals
    (s= a '(a b c))
    (s= a.params '(a b c))]))

(test-case "formals: mixed"
  (syntax-parse #'(a #:keyword arg #:optional [keyword arg] . rest)
    [a:formals
     (s= a '(a #:keyword arg #:optional [keyword arg] . rest))
     (s= a.params '(a arg keyword rest))]))

(test-case "bad formals: mandatory after optional"
 (check-equal?
  (syntax-parse #'([optional before] required)
    [a:formals 'ok]
    [_         'bad])
  'bad))

(test-case "formals: optional before rest"
 (syntax-parse #'(a [optional arg] . rest)
   [a:formals
    (s= a '(a [optional arg] . rest))
    (s= a.params '(a optional rest))]))

(test-case "function header: simple"
 (syntax-parse #'(f a b c)
   [a:function-header
    (s= a '(f a b c ))
    (s= a.params '(a b c))]))

(test-case "function header: curried"
 (syntax-parse #'((f doing) currying)
   [a:function-header
    (s= a '((f doing) currying))
    (s= a.params '(doing currying))]))
