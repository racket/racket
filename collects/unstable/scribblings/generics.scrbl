#lang scribble/manual
@(require "utils.rkt" (for-label racket/base unstable/generics))

@title{Generics}
@unstable["Eli Barzilay"
          @author+email["Jay McCarthy" "jay@racket-lang.org"]]

@defmodule[unstable/generics]

@defform/subs[(define-generics (name prop:name name?)
                [method . kw-formals*]
                ...)
              ([kw-formals* (arg* ...)
                            (arg* ...+ . rest-id)
                            rest-id]
               [arg* id
                     [id]
                     (code:line keyword id)
                     (code:line keyword [id])])
              #:contracts
              ([name identifier?]
               [prop:name identifier?]
               [name? identifier?]
               [method identifier?])]{

Defines @racket[name] as a transformer binding for the static
information about a new generic group.

Defines @racket[prop:name] as a structure type property.  Structure
types implementing this generic group should have this property where
the value is a vector with one element per @racket[method] where each
value is either @racket[#f] or a procedure with the same arity as
specified by @racket[kw-formals*].  (@racket[kw-formals*] is similar to
the @racket[kw-formals] used by @racket[lambda], except no expression is
given for optional arguments.)  The arity of each method is checked by
the guard on the structure type property.

Defines @racket[name?] as a predicate identifying instances of structure
types that implement this generic group.

Defines each @racket[method] as a generic procedure that calls the
corresponding method on values where @racket[name?] is true. Each method
must have a required by-position argument that is
@racket[free-identifier=?] to @racket[name]. This argument is used in
the generic definition to locate the specialization.

}

@defform[(generics name 
                   [method . kw-formals*]
                   ...)
         #:contracts
         ([name identifier?]
          [method identifier?])]{

Expands to

@racketblock[(define-generics (name _prop:name _name?)
               [method . kw-formals*]
               ...)]

where @racket[_prop:name] and @racket[_name?] are created with the lexical
context of @racket[name].

}

@defform[(define-methods name definition ...)
         #:contracts
         ([name identifier?])]{

@racket[name] must be a transformer binding for the static information
about a new generic group.

Expands to a value usable as the property value for the structure type
property of the @racket[name] generic group.

If the @racket[definition]s define the methods of @racket[name], then
they are used in the property value.

If any method of @racket[name] is not defined, then @racket[#f] is used
to signify that the structure type does not implement the particular
method.

Allows @racket[define/generic] to appear in @racket[definition ...].

}

@defform[(define/generic local-name method-name)
         #:contracts
         ([local-name identifier?]
          [method-name identifier?])]{

When used inside @racket[define-methods], binds @racket[local-name] to
the generic for @racket[method-name]. This is useful for method
specializations to use the generic methods on other values.

Syntactically an error when used outside @racket[define-methods].

}

@; Examples
@(require scribble/eval)
@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  unstable/generics))
     e))

@(define evaluator (new-evaluator))

@examples[#:eval evaluator
(define-generics (printable prop:printable printable?)
  (gen-print printable [port])
  (gen-port-print port printable)
  (gen-print* printable [port] #:width width #:height [height]))

(define-struct num (v)
  #:property prop:printable
  (define-methods printable
    (define/generic super-print gen-print)
    (define (gen-print n [port (current-output-port)])
      (fprintf port "Num: ~a" (num-v n)))
    (define (gen-port-print port n)
      (super-print n port))
    (define (gen-print* n [port (current-output-port)]
                        #:width w #:height [h 0])
      (fprintf port "Num (~ax~a): ~a" w h (num-v n)))))
         
(define-struct bool (v)
  #:property prop:printable
  (define-methods printable
    (define/generic super-print gen-print)
    (define (gen-print b [port (current-output-port)])
      (fprintf port "Bool: ~a" 
               (if (bool-v b) "Yes" "No")))
    (define (gen-port-print port b)
      (super-print b port))
    (define (gen-print* b [port (current-output-port)]
                        #:width w #:height [h 0])
      (fprintf port "Bool (~ax~a): ~a" w h 
               (if (bool-v b) "Yes" "No")))))

(define x (make-num 10))
(gen-print x)
(gen-port-print (current-output-port) x)
(gen-print* x #:width 100 #:height 90)

(define y (make-bool #t))
(gen-print y)
(gen-port-print (current-output-port) y)
(gen-print* y #:width 100 #:height 90)
]

@close-eval[evaluator]
