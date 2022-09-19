#lang scribble/manual
@(require (for-label racket/base racket/generic racket/contract/base))

@title[#:tag "struct-generics"]{Generic Interfaces}

@defmodule[racket/generic]


A @deftech{generic interface} allows per-type methods to be
associated with generic functions. Generic functions are defined
using a @racket[define-generics] form. Method implementations for
a structure type are defined using the @racket[#:methods] keyword
(see @secref["define-struct"]).

@defform/subs[(define-generics id
                generics-opt ...
                [method-id . kw-formals*] ...
                generics-opt ...)
              ([generics-opt
                (code:line #:defaults ([default-pred? default-impl ...] ...))
                (code:line #:fast-defaults ([fast-pred? fast-impl ...] ...))
                (code:line #:fallbacks [fallback-impl ...])
                (code:line #:defined-predicate defined-pred-id)
                (code:line #:defined-table defined-table-id)
                (code:line #:derive-property prop-expr prop-value-expr)
                (code:line #:requires [required-method-id ...])]
               [kw-formals* (arg* ...)
                            (arg* ...+ . rest-id)
                            rest-id]
               [arg* arg-id
                     [arg-id]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id])])]{

Defines the following names, plus any specified by keyword options.

@itemlist[

 @item{@racketidfont{gen:}@racket[id] as a transformer binding for
       the static information about a new generic interface;}

 @item{@racket[id]@racketidfont{?} as a predicate identifying
       instances of structure types that implement this generic group; and}

 @item{each @racket[method-id] as a @deftech{generic method} that calls the
       corresponding method on values where
       @racket[id]@racketidfont{?} is true.
       Each @racket[method-id]'s @racket[kw-formals*] must include a required
       by-position argument that is @racket[free-identifier=?] to
       @racket[id]. That argument is used in the generic definition to
       locate the specialization.}

 @item{@racket[id]@racketidfont{/c} as a contract combinator that
       recognizes instances of structure types which implement the
       @racketidfont{gen:}@racket[id] generic interface. The combinator
       takes pairs of @racket[method-id]s and contracts. The contracts
       will be applied to each of the corresponding method implementations.
       The @racket[id]@racketidfont{/c} combinator is intended to be used to
       contract the range of a constructor procedure for a struct type that
       implements the generic interface.}

]

The @racket[#:defaults] option may be provided at most once.
When it is provided, each generic function
uses @racket[default-pred?]s to dispatch to the given
@deftech{default method} implementations,
@racket[default-impl]s, if dispatching to the generic method table fails.
The syntax of the @racket[default-impl]s is the same as the methods
provided for the @racket[#:methods] keyword for @racket[struct].

The @racket[#:fast-defaults] option may be provided at most once.
It works the same as @racket[#:defaults], except the @racket[fast-pred?]s are
checked before dispatching to the generic method table.  This option is
intended to provide a fast path for dispatching to built-in datatypes, such as
lists and vectors, that do not overlap with structures implementing
@racketidfont{gen:}@racket[id].

The @racket[#:fallbacks] option may be provided at most once.
When it is provided, the @racket[fallback-impl]s define
@deftech{fallback method} implementations
that are used for any instance of the generic interface that does not supply a
specific implementation.  The syntax of the @racket[fallback-impl]s is the same
as the methods provided for the @racket[#:methods] keyword for @racket[struct].

The @racket[#:defined-predicate] option may be provided at most once.
When it is provided, @racket[defined-pred-id] is defined as a
procedure that reports whether a specific instance of the generic interface
implements a given set of methods.
Specifically, @racket[(defined-pred-id v 'name ...)] produces @racket[#t] if
@racket[v] has implementations for each method @racket[name], not counting
@racket[#:fallbacks] implementations, and produces @racket[#f] otherwise.
This procedure is intended for use by
higher-level APIs to adapt their behavior depending on method
availability.

The @racket[#:defined-table] option may be provided at most once.
When it is provided, @racket[defined-table-id] is defined as a
procedure that takes an instance of the generic interface and returns an
immutable @tech{hash table} that maps symbols corresponding to method
names to booleans representing whether or not that method is
implemented by the instance.  This option is deprecated; use
@racket[#:defined-predicate] instead.

The @racket[#:derive-property] option may be provided any number of times.
Each time it is provided, it specifies a @tech{structure type property} via
@racket[prop-expr] and a value for the property via @racket[prop-value-expr].
All structures implementing the generic interface via @racket[#:methods]
automatically implement this structure type property using the provided values.
When @racket[prop-value-expr] is executed, each @racket[method-id] is bound to
its specific implementation for the @tech{structure type}.

The @racket[#:requires] option may be provided at most once.
When it is provided, any instance of the generic interface
@emph{must} supply an implementation of the specified @racket[required-method-id]s.
Otherwise, a compile-time error is raised.

If a value @racket[v] satisfies @racket[id]@racketidfont{?}, then @racket[v] is
a @deftech{generic instance} of @racketidfont{gen:}@racket[id].

If a generic instance @racket[v] has a corresponding implementation for some
@racket[method-id] provided via @racket[#:methods] in @racket[struct] or via
@racket[#:defaults] or @racket[#:fast-defaults] in @racket[define-generics],
then @racket[method-id] is an @deftech{implemented generic method} of
@racket[v].

If @racket[method-id] is not an implemented generic method of a generic
instance @racket[v], and @racket[method-id] has a fallback implementation that
does not raise an @racket[exn:fail:support] exception when given @racket[v],
then @racket[method-id] is a @deftech{supported generic method} of @racket[v].

@history[#:changed "8.7.0.5"
         @elem{Added the @racket[#:requires] option.}]

}

@defproc[(raise-support-error [name symbol?] [v any/c]) none/c]{

Raises an @racket[exn:fail:support] exception for a @techlink{generic
method} called @racket[name] that does not support the @techlink{generic
instance} @racket[v].

@examples[#:eval evaluator
(eval:error (raise-support-error 'some-method-name '("arbitrary" "instance" "value")))
]

}

@defstruct*[(exn:fail:support exn:fail) () #:transparent]{

Raised for @techlink{generic methods} that do not support the given
@techlink{generic instance}.

}

@defform[(define/generic local-id method-id)]{

When used inside the method definitions associated with the @racket[#:methods],
@racket[#:fallbacks], @racket[#:defaults] or @racket[#:fast-defaults] keywords,
binds @racket[local-id] to the generic for @racket[method-id]. This form is
useful for method specializations to use generic methods (as opposed to the
local specialization) on other values.

The @racket[define/generic] form is only allowed inside:
@itemlist[
 @item{a @racket[#:methods] specification in @racket[struct] (or @racket[define-struct])}

 @item{the specification of @racket[#:fallbacks], @racket[#:defaults] or
       @racket[#:fast-defaults] in @racket[define-generics]}
]

Using @racket[define/generic] elsewhere is a syntax error.
}


@; Examples
@(require scribble/eval)
@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  racket/contract
                  racket/generic))
     e))

@(define evaluator (new-evaluator))

@examples[#:eval evaluator
(define-generics printable
  (gen-print printable [port])
  (gen-port-print port printable)
  (gen-print* printable [port] #:width width #:height [height])
  #:defaults ([string?
               (define/generic super-print gen-print)
               (define (gen-print s [port (current-output-port)])
                 (fprintf port "String: ~a" s))
               (define (gen-port-print port s)
                 @code:comment{we can call gen-print alternatively}
                 (super-print s port))
               (define (gen-print* s [port (current-output-port)]
                                   #:width w #:height [h 0])
                 (fprintf port "String (~ax~a): ~a" w h s))]))

(struct num (v)
  #:methods gen:printable
  [(define (gen-print n [port (current-output-port)])
     (fprintf port "Num: ~a" (num-v n)))
   (define (gen-port-print port n)
     (gen-print n port))
   (define (gen-print* n [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Num (~ax~a): ~a" w h (num-v n)))])

(struct string+num (v n)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define/generic super-print* gen-print*)
   (define (gen-print b [port (current-output-port)])
     (super-print (string+num-v b) port)
     (fprintf port " ")
     (super-print (string+num-n b) port))
   (define (gen-port-print port b)
     (gen-print b port))
   (define (gen-print* b [port (current-output-port)]
                       #:width w #:height [h 0])
     (super-print* (string+num-v b) #:width w #:height h)
     (fprintf port " ")
     (super-print* (string+num-n b) #:width w #:height h))])

(define x (num 10))
(gen-print x)
(gen-port-print (current-output-port) x)
(gen-print* x #:width 100 #:height 90)

(define str "Strings are printable too!")
(gen-print str)

(define y (string+num str x))
(gen-print y)
(gen-port-print (current-output-port) y)
(gen-print* y #:width 100 #:height 90)

(define/contract make-num-contracted
  (-> number?
      (printable/c
        [gen-print (->* (printable?) (output-port?) void?)]
        [gen-port-print (-> output-port? printable? void?)]
        [gen-print* (->* (printable? #:width exact-nonnegative-integer?)
                         (output-port? #:height exact-nonnegative-integer?)
                         void?)]))
   num)

(define z (make-num-contracted 10))
(eval:error (gen-print* z #:width "not a number" #:height 5))
]

@defform[(generic-instance/c gen-id [method-id method-ctc] ...)
         #:contracts ([method-ctc contract?])]{

Creates a contract that recognizes structures that implement the @tech{generic
interface} @racket[gen-id], and constrains their implementations of the
specified @racket[method-id]s with the corresponding @racket[method-ctc]s.

}

@defform[(impersonate-generics gen-id val-expr
           [method-id method-proc-expr] ...
           maybe-properties)
         #:grammar ([maybe-properties code:blank
                                      (code:line #:properties props-expr)])
         #:contracts ([method-proc-expr (any/c . -> . any/c)]
                      [props-expr (list/c impersonator-property? any/c ... ...)])]{

Creates an @tech{impersonator} of @racket[val-expr], which must be a structure
that implements the @tech{generic interface} @racket[gen-id].  The impersonator
applies the results of the @racket[method-proc-expr]s to the structure's implementation
of the corresponding @racket[method-id]s, and replaces the method
implementation with the result.

A @racket[props-expr] can provide properties to attach to the
impersonator. The result of @racket[props-expr] must be a list with
an even number of elements, where the first element of the list is an
impersonator property, the second element is its value, and so on.

@history[#:changed "6.1.1.8" @elem{Added @racket[#:properties].}]}


@defform[(chaperone-generics gen-id val-expr
           [method-id method-proc-expr] ...
           maybe-properties)]{

Like @racket[impersonate-generics], but
creates a @tech{chaperone} of @racket[val-expr], which must be a structure
that implements the @tech{generic interface} @racket[gen-id].  The chaperone
applies the specified @racket[method-proc]s to the structure's implementation
of the corresponding @racket[method-id]s, and replaces the method
implementation with the result, which must be a chaperone of the original.

}

@defform[(redirect-generics mode gen-id val-expr
            [method-id method-proc-expr] ...
            maybe-properties)]{

Like @racket[impersonate-generics], but
creates an @tech{impersonator} of @racket[val-expr]
if @racket[mode] evaluates to @racket[#f], or creates
a @tech{chaperone} of @racket[val-expr] otherwise.

}

@defform[(make-struct-type-property/generic
           name-expr
           maybe-guard-expr
           maybe-supers-expr
           maybe-can-impersonate?-expr
           property-option
           ...)
  #:grammar
  ([maybe-guard-expr (code:line) guard-expr]
   [maybe-supers-expr (code:line) supers-expr]
   [maybe-can-impersonate?-expr (code:line) can-impersonate?-expr]
   [property-option (code:line #:property prop-expr val-expr)
                    (code:line #:methods gen:name-id method-defs)]
   [method-defs (definition ...)])
  #:contracts
  ([name-expr symbol?]
   [guard-expr (or/c procedure? #f 'can-impersonate)]
   [supers-expr (listof (cons/c struct-type-property? (-> any/c any/c)))]
   [can-impersonate?-expr any/c]
   [prop-expr struct-type-property?]
   [val-expr any/c])]{
Creates a new structure type property and returns three
values, just like @racket[make-struct-type-property] would:

@itemize[
 @item{a @tech{structure type property descriptor}}
 @item{a @tech{property predicate} procedure}
 @item{a @tech{property accessor} procedure}
]

Any struct that implements this property will also implement
the properties and @tech{generic interfaces} given in the
@racket[#:property] and @racket[#:methods] declarations.
The property @racket[val-expr]s and @racket[method-def]s are
evaluated eagerly when the property is created, not when
it is attached to a structure type.
}

@defform[(make-generic-struct-type-property
            gen:name-id
            method-def
            ...)]{
Creates a new structure type property and returns the
@tech{structure type property descriptor}.

Any struct that implements this property will also implement
the @tech{generic interface} given by @racket[gen:name-id]
with the given @racket[method-def]s. The @racket[method-def]s
are evaluated eagerly when the property is created, not when
it is attached to a structure type.
}

@close-eval[evaluator]
