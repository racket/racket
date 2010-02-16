#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label scheme/base
                     scheme/contract
                     (except-in scheme/foreign ->)
                     "private/objc-doc-unsafe.ss"))

@(define objc-eval (make-base-eval))
@(interaction-eval #:eval objc-eval (define-struct cpointer:id ()))

@(define seeCtype
   @elem{see @secref[#:doc '(lib "scribblings/foreign/foreign.scrbl") "ctype"]})

@title{@bold{Objective-C} FFI}

@defmodule[ffi/objc]{The @schememodname[ffi/objc] library builds on
@schememodname[scheme/foreign] to support interaction with
@link["http://developer.apple.com/documentation/Cocoa/Conceptual/ObjectiveC/"]{Objective-C}.}

The library supports Objective-C interaction in two layers. The upper
layer provides syntactic forms for sending messages and deriving
subclasses. The lower layer is a think wrapper on the
@link["http://developer.apple.com/DOCUMENTATION/Cocoa/Reference/ObjCRuntimeRef/index.html"]{Objective-C
runtime library} functions. Even the upper layer is unsafe and
relatively low-level compared to normal Scheme libraries, because
argument and result types must be declared in terms of FFI C types
(@seeCtype).

@bold{Important:} Most of the bindings documented here are available
only after an @scheme[(objc-unsafe!)] declaration in the importing
module.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Using Unsafe Bindings}

@defform[(objc-unsafe!)]{

Analogous to @scheme[(unsafe!)], makes unsafe bindings of
@schememodname[ffi/objc] available in the importing module.}

@; ----------------------------------------------------------------------

@section{FFI Types and Constants}

@defthing[_id ctype?]{

The type of an Objective-C object, an opaque pointer.}

@defthing[_Class ctype?]{

The type of an Objective-C class, which is also an @scheme[_id].}

@defthing[_Protocol ctype?]{

The type of an Objective-C protocol, which is also an @scheme[_id].}

@defthing[_SEL ctype?]{

The type of an Objective-C selector, an opaque pointer.}

@defthing[_BOOL ctype?]{

The Objective-C boolean type. Scheme values are converted for C in the
usual way: @scheme[#f] is false and any other value is true. C values
are converted to Scheme booleans.}

@defthing[YES boolean?]{

Synonym for @scheme[#t]}

@defthing[NO boolean?]{

Synonym for @scheme[#f]}

@; ----------------------------------------------------------------------

@section{Syntactic Forms and Procedures}

@defform*/subs[[(tell result-type obj-expr method-id)
                (tell result-type obj-expr arg ...)]
               ([result-type code:blank
                             (code:line #:type ctype-expr)]
                [arg (code:line method-id expr)
                     (code:line #:type ctype-expr method-id arg)])]{

Sends a message to the Objective-C object produced by
@scheme[obj-expr]. When a type is omitted for either the result or an
argument, the type is assumed to be @scheme[_id], otherwise it must
be specified as an FFI C type (@seeCtype).

If a single @scheme[method-id] is provided with no arguments, then
@scheme[method-id] must not end with @litchar{:}; otherwise, each
@scheme[method-id] must end with @litchar{:}.

@examples[
#:eval objc-eval
(eval:alts (tell NSString alloc) (make-cpointer:id))
(eval:alts (tell (tell NSString alloc)
                 initWithUTF8String: #:type _string "Hello")
           (make-cpointer:id))
]}

@defform*[[(tellv obj-expr method-id)
           (tellv obj-expr arg ...)]]{

Like @scheme[tell], but with a result type @scheme[_void].}

@defform[(import-class class-id ...)]{

Defines each @scheme[class-id] to the class (a value with FFI type
@scheme[_Class]) that is registered with the string form of
@scheme[class-id]. The registered class is obtained via
@scheme[objc_lookUpClass].

@examples[
#:eval objc-eval
(eval:alts (import-class NSString) (void))
]}

@defform[(import-protocol protocol-id ...)]{

Defines each @scheme[protocol-id] to the protocol (a value with FFI type
@scheme[_Protocol]) that is registered with the string form of
@scheme[protocol-id]. The registered class is obtained via
@scheme[objc_getProtocol].

@examples[
#:eval objc-eval
(eval:alts (import-protocol NSCoding) (void))
]}

@defform/subs[#:literals (+ - +a -a)
              (define-objc-class class-id superclass-expr
                maybe-mixins
                maybe-protocols
                [field-id ...]
                method)
              ([maybe-mixins code:blank
                             (code:line #:mixins (mixin-expr ...))]
               [maybe-protocols code:blank
                                (code:line #:protocols (protocol-expr ...))]
               [method (mode result-ctype-expr (method-id) body ...+)
                       (mode result-ctype-expr (arg ...+) body ...+)]
               [mode + - +a -a]
               [arg (code:line method-id [ctype-expr arg-id])])]{

Defines @scheme[class-id] as a new, registered Objective-C class (of
FFI type @scheme[_Class]). The @scheme[superclass-expr] should produce
an Objective-C class or @scheme[#f] for the superclass. An optional
@scheme[#:mixins] clause can specify mixins defined with
@scheme[define-objc-mixin]. An optional @scheme[#:protocols] clause
can specify Objective-C protocols to be implemented by the class.

Each @scheme[field-id] is an instance field that holds a Scheme value
and that is initialized to @scheme[#f] when the object is
allocated. The @scheme[field-id]s can be referenced and @scheme[set!]
directly when the method @scheme[body]s. Outside the object, they can
be referenced and set with @scheme[get-ivar] and @scheme[set-ivar!].

Each @scheme[method] adds or overrides a method to the class (when
@scheme[mode] is @scheme[-] or @scheme[-a]) to be called on instances,
or it adds a method to the meta-class (when @scheme[mode] is
@scheme[+] or @scheme[+a]) to be called on the class itself. All
result and argument types must be declared using FFI C types
(@seeCtype). When @scheme[mode] is @scheme[+a] or @scheme[-a], the
method is called in atomic mode (see @scheme[_cprocedure]).

If a @scheme[method] is declared with a single @scheme[method-id] and
no arguments, then @scheme[method-id] must not end with
@litchar{:}. Otherwise, each @scheme[method-id] must end with
@litchar{:}.

If the special method @scheme[dealloc] is declared for mode
@scheme[-], it must not call the superclass method, because a
@scheme[(super-tell dealloc)] is added to the end of the method
automatically. In addition, before @scheme[(super-tell dealloc)],
space for each @scheme[field-id] within the instance is deallocated.

@examples[
#:eval objc-eval
(eval:alts
 (define-objc-class MyView NSView
   [bm] (code:comment @#,elem{<- one field})
   (- _scheme (swapBitwmap: [_scheme new-bm])
      (begin0 bm (set! bm new-bm)))
   (- _void (drawRect: [@#,schemeidfont{_NSRect} exposed-rect])
      (super-tell drawRect: exposed-rect)
      (draw-bitmap-region bm exposed-rect))
   (- _void (dealloc)
      (when bm (done-with-bm bm))))
 (void))
]}

@defform[(define-objc-mixin (class-id superclass-id)
           maybe-mixins
           maybe-protocols
           [field-id ...]
           method)]{

Like @scheme[define-objc-class], but defines a mixin to be combined
with other method definitions through either
@scheme[define-objc-class] or @scheme[define-objc-mixin]. The
specified @scheme[field-id]s are not added by the mixin, but must be a
subset of the @scheme[field-id]s declared for the class to which the
methods are added.}


@defidform[self]{

When used within the body of a @scheme[define-objc-class] or
@scheme[define-objc-mixin] method, refers to the object whose method
was called. This form cannot be used outside of a
@scheme[define-objc-class] or @scheme[define-objc-mixin] method.}

@defform*[[(super-tell result-type method-id)
           (super-tell result-type arg ...)]]{

When used within the body of a @scheme[define-objc-class] or
@scheme[define-objc-mixin] method, calls a superclass method. The
@scheme[result-type] and @scheme[arg] sub-forms have the same syntax
as in @scheme[tell]. This form cannot be used outside of a
@scheme[define-objc-class] or @scheme[define-objc-mixin] method.}


@defform[(get-ivar obj-expr field-id)]{

Extracts the Scheme value of a field in a class created with
@scheme[define-objc-class].}

@defform[(set-ivar! obj-expr field-id value-expr)]{

Sets the Scheme value of a field in a class created with
@scheme[define-objc-class].}

@defform[(selector method-id)]{

Returns a selector (of FFI type @scheme[_SEL]) for the string form of
@scheme[method-id].

@examples[
(eval:alts (tellv button setAction: #:type _SEL (selector terminate:)) (void))
]}

@defproc[(objc-is-a? [obj _id] [cls _Class]) boolean?]{

Check whether @scheme[obj] is an instance of the Objective-C class
@scheme[cls].}

@; ----------------------------------------------------------------------

@section{Raw Runtime Functions}

@defproc[(objc_lookUpClass [s string?]) (or/c _Class #f)]{

Finds a registered class by name.}

@defproc[(objc_getProtocol [s string?]) (or/c _Protocol #f)]{

Finds a registered protocol by name.}

@defproc[(sel_registerName [s string?]) _SEL]{

Interns a selector given its name in string form.}

@defproc[(objc_allocateClassPair [cls _Class] [s string?] [extra integer?])
         _Class]{

Allocates a new Objective-C class.}

@defproc[(objc_registerClassPair [cls _Class]) void?]{

Registers an Objective-C class.}

@defproc[(object_getClass [obj _id]) _Class]{

Returns the class of an object (or the meta-class of a class).}

@defproc[(class_addMethod [cls _Class] [sel _SEL] 
                          [imp procedure?]
                          [type ctype?]
                          [type-encoding string?])
         boolean?]{

Adds a method to a class. The @scheme[type] argument must be a FFI C
type (@seeCtype) that matches both @scheme[imp] and the not
Objective-C type string @scheme[type-encoding].}

@defproc[(class_addIvar [cls _Class] [name string?] [size exact-nonnegative-integer?]
                        [log-alignment exact-nonnegative-integer?] [type-encoding string?])
         boolean?]{

Adds an instance variable to an Objective-C class.}

@defproc[(object_getInstanceVariable [obj _id]
                                     [name string?])
         (values _Ivar any/c)]{

Gets the value of an instance variable whose type is @scheme[_pointer].}

@defproc[(object_setInstanceVariable [obj _id]
                                     [name string?]
                                     [val any/c])
         _Ivar]{

Sets the value of an instance variable whose type is @scheme[_pointer].}

@defthing[_Ivar ctype?]{

The type of an Objective-C instance variable, an opaque pointer.}

@defproc[((objc_msgSend/typed [types (vector/c result-ctype arg-ctype ...)])
          [obj _id]
          [sel _SEL]
          [arg any/c])
         any/c]{

Calls the Objective-C method on @scheme[_id] named by @scheme[sel].
The @scheme[types] vector must contain one more than the number of
supplied @scheme[arg]s; the first FFI C type in @scheme[type] is used
as the result type.}

@defproc[((objc_msgSendSuper/typed [types (vector/c result-ctype arg-ctype ...)])
          [super _objc_super]
          [sel _SEL]
          [arg any/c])
         any/c]{

Like @scheme[objc_msgSend/typed], but for a super call.}

@deftogether[(
@defproc[(make-objc_super [id _id] [super _Class]) _objc_super]
@defthing[_objc_super ctype?]
)]{

Constructor and FFI C type use for super calls.}
