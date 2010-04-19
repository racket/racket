#lang scribble/manual
@(require "utils.ss" (for-label scheme/base unstable/generics))

@title{Generics}

@defmodule[unstable/generics]

@unstable["Eli Barzilay"
          @author+email["Jay McCarthy" "jay@plt-scheme.org"]]

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
                                      
Defines @scheme[name] as a transformer binding for the static information about a new generic group.

Defines @scheme[prop:name] as a structure 
type property. Structure types implementing this generic group should have this property where the value is a vector
with one element per @scheme[method] where each value is
either @scheme[#f] or a procedure with the same arity as specified by @scheme[kw-formals*].
(@scheme[kw-formals*] is similar to the @scheme[kw-formals] used by @scheme[lambda], except no expression is given for optional arguments.)
The arity of each method is checked by the guard on the structure type property.

Defines @scheme[name?] as a predicate identifying instances of structure types that implement this generic group.

Defines each @scheme[method] as a generic procedure that calls the corresponding method on values where @scheme[name?] is true. Each method must have a required by-position argument that is @scheme[free-identifier=?] to @scheme[name]. This argument is used in the generic definition to locate the specialization.

}

@defform[(generics name 
                   [method . kw-formals*]
                   ...)
         #:contracts
         ([name identifier?]
          [method identifier?])]{

Expands to

@schemeblock[(define-generics (name _prop:name _name?)
               [method . kw-formals*]
               ...)]

where @scheme[_prop:name] and @scheme[_name?] are created with the lexical
context of @scheme[name].

}
                                
@defform[(define-methods name definition ...)
         #:contracts
         ([name identifier?])]{

@scheme[name] must be a transformer binding for the static information about a new generic group.
                               
Expands to a value usable as the property value for the structure type property of the @scheme[name] generic group.
                                                                                       
If the @scheme[definition]s define the methods of @scheme[name], then they are used in the property value.

If any method of @scheme[name] is not defined, then @scheme[#f] is used to signify that the structure type does not implement the particular method.

Allows @scheme[define/generic] to appear in @scheme[definition ...].

}
                              
@defform[(define/generic local-name method-name)
         #:contracts
         ([local-name identifier?]
          [method-name identifier?])]{
                                      
When used inside @scheme[define-methods], binds @scheme[local-name] to the generic for @scheme[method-name]. This is useful for method specializations to use the generic methods on other values.
                 
Syntactically an error when used outside @scheme[define-methods].

}
                                     
@; Examples
@(require scribble/eval)               
@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax scheme/base)
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