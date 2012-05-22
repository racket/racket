#lang scribble/manual
@(require (for-label racket/base racket/generics))

@title[#:tag "struct-generics"]{Generics}
@; @author[@author+email["Eli Barzilay" "eli@racket-lang.org"]
@;         @author+email["Jay McCarthy" "jay@racket-lang.org"]
@; 	@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
@; 	@author+email["Asumu Takikawa" "asumu@racket-lang.org"]]

@defmodule[racket/generics]

A @deftech{generic interface} allows per-type methods to be
associated with generic functions. Generic functions are defined
using a @racket[define-generics] form. Method implementations for
a structure type are defined using the @racket[#:methods] keyword
(see @secref["define-struct"]).

@defform/subs[(define-generics (gen:name prop:name name?
                                [#:defined-table defined-table])
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
              ([gen:name identifier?]
               [prop:name identifier?]
               [name? identifier?]
               [method identifier?])]{

Defines @racket[gen:name] as a transformer binding for the static
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
@racket[free-identifier=?] to @racket[gen:name]. This argument is used in
the generic definition to locate the specialization.

The optional @racket[defined-table] argument should be an identifier.
@racket[define-generics] will bind it to a procedure that takes an instance of
the generics and returns an immutable hash-table that maps symbols
corresponding to method names to booleans representing whether or not that
method is implemented by that instance. The intended use case for this table is
to allow higher-level APIs to adapt their behavior depending on method
availability.

}

@defform[(define/generic local-name method-name)
         #:contracts
         ([local-name identifier?]
          [method-name identifier?])]{

When used inside the method definitions associated with the
@racket[#:methods] keyword, binds @racket[local-name] to
the generic for @racket[method-name]. This is useful for method
specializations to use the generic methods on other values.

Syntactically an error when used outside the definitions associated
with @racket[#:methods].

}

@; Examples
@(require scribble/eval)
@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  racket/generics))
     e))

@(define evaluator (new-evaluator))

@examples[#:eval evaluator
(define-generics (printable)
  (gen-print printable [port])
  (gen-port-print port printable)
  (gen-print* printable [port] #:width width #:height [height]))

(define-struct num (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print n [port (current-output-port)])
     (fprintf port "Num: ~a" (num-v n)))
   (define (gen-port-print port n)
     (super-print n port))
   (define (gen-print* n [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Num (~ax~a): ~a" w h (num-v n)))])
         
(define-struct bool (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print b [port (current-output-port)])
     (fprintf port "Bool: ~a" 
              (if (bool-v b) "Yes" "No")))
   (define (gen-port-print port b)
     (super-print b port))
   (define (gen-print* b [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Bool (~ax~a): ~a" w h 
              (if (bool-v b) "Yes" "No")))])

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
