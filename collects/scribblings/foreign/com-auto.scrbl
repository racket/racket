#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "com-common.rkt"
          (for-label racket/base
                     (except-in racket/contract ->)
                     ffi/unsafe/com
                     ffi/com-registry))

@title[#:tag "com-auto"]{COM Automation}

@defmodule[ffi/com #:use-sources (ffi/unsafe/com)]{The
@racketmodname[ffi/com] library builds on COM automation to provide a
safe use of COM objects that support the @as-index{@cpp{IDispatch}}
interface.}

@margin-note{The @racketmodname[ffi/com] library is based on the
@deftech{MysterX} library by Paul Steckler. MysterX is included with
Racket but deprecated, and it will be replaced in the next version
with a partial compability library that redirects to this one.}

@; ----------------------------------------

@section{GUIDs, CLSIDs, IIDs, and ProgIDs}

@deftogether[(
@defproc[(guid? [v any/c]) boolean?]
@defproc[(clsid? [v any/c]) boolean?]
@defproc[(iid? [v any/c]) boolean?]
)]{

Returns @racket[#t] if @racket[v] is a structure representing a
@tech{GUID}, @racket[#f] otherwise. The @racket[clsid?] and
@racket[iid?] functions are the same as @racket[guid?].

A @tech{GUID} corresponds an a @racket[_GUID] structure at the unsafe
layer.}

@deftogether[(
@defproc[(string->guid [str string?]) guid?]
@defproc[(string->clsid [str string?]) clsid?]
@defproc[(string->iid [str string?]) iid?]
)]{

Converts a string of the form
@racket["{00000000-0000-0000-0000-0000000000}"], where each @tt{0} can
be a hexadecimal digit, to a @tech{GUID}. If @racket[str] does not
have te expected form, the @racket[exn:fail] exception is raised.

The @racket[string->clsid] and @racket[string->iid] functions are the
same as @racket[string->guid].}

@defproc[(guid->string [g guid?]) string?]{

Converts a @tech{GUID} to its string form.}

@defproc[(guid=? [g1 guid?] [g2 guid?]) boolean?]{

Determines whether @racket[g1] and @racket[g2] represent the same @tech{GUID}.}

@deftogether[(
@defproc[(progid->clsid [progid string?]) clsid?]
@defproc[(clsid->progid [clsid clsid?]) (or/c string? #f)]
)]{

Converts a @tech{ProgID} to a @tech{CLSID} or vice versa. Not evey
@tech{COM class} has a @tech{ProgID}, so the result of
@racket[clsid->progid] can be @racket[#f].

The @racket[progid->clsid] function accepts a versionless
@tech{ProgID}, in which case it produces the @tech{CLSID} of the most
recent available version. The @racket[clsid->progid] function always
produces a @tech{ProgID} with its version.}

@; ----------------------------------------

@section{COM Objects}

@defproc[(com-object? [obj com-object?]) boolean?]{

  Returns @racket[#t] if the argument represents a @tech{COM object}, @racket[#f]
  otherwise.}


@defproc[(com-create-instance [clsid-or-progid (or/c clsid? string?)]
                              [where (or/c (one-of/c 'local 'remote) string?) 'local])
         com-object?]{

  Returns an instance of the @tech{COM class} specified by
  @racket[clsid-or-progid], which is either a @tech{CLSID} or a
  @tech{ProgID}.

  The optional @racket[where] argument indicates a location for
  running the instance, and may be @racket['local], @racket['remote],
  or a string indicating a machine name.  See @secref["remote"] for
  more information.

  An object can be created this way for any COM class, but functions
  such as @racket[com-invoke] work only if the object supports the
  @cpp{IDispatch} COM automation interface.

 The resulting object is registered with the current custodian, which
 retains a reference to the object until it is released with
 @racket[com-release] or the custodian is shut down.}


@defproc[(com-release [obj com-object?]) void?]{

Releases the given @tech{COM object}. The given @racket[obj] is
subsequently unusable, and the underlying COM object is destroyed
unless its reference count has been incremented (via COM methods or
unsafe operations).

If @racket[obj] has already been released, @racket[com-release] has
no effect.}


@defproc[(com-get-active-object [clsid-or-progid (or/c clsid? string?)])
         com-object?]{

  Like @racket[com-create-instance], but gets an existing
  active object (always local) instead of creating a new one.}


@defproc[(com-object-clsid [obj com-object?]) clsid?]{

  Returns the @racket{CLSID} of the COM class instantiated by
  @racket[obj], or raises an error if the COM class is not known.}


@defproc[(com-object-set-clsid! [obj com-object?] [clsid clsid?]) void?]{

  Sets the COM @tech{CLSID} for @racket[obj] to @racket[clsid]. This
  is useful when COM event-handling procedures can obtain only
  ambiguous information about the object's COM class.}


@defproc[(com-object-eq? [obj1 com-object?] [obj2 com-object?])
         boolean?]{

  Returns @racket[#t] if @racket[obj1] and @racket[obj2] refer to the
  same @tech{COM object}, @racket[#f] otherwise.

  If two references to a COM object are the same according to
  @racket[com-object-eq?], then they are also the same according to
  @racket[equal?]. Two @racket[com-object-eq?]  references are not
  necessarily @racket[eq?], however.}


@defproc[(com-type? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents reflective information
about a COM object's type, @racket[#f] otherwise.}


@defproc[(com-object-type [obj com-object?]) com-type?]{

Returns a representation of a COM object's type that is independent of
the object itself.}


@defproc[(com-type=? [t1 com-type?] [t2 com-type?]) boolean?]{

Returns @racket[#t] if @racket[t1] and @racket[t2] represent the same
type information, @racket[#f] otherwise.}


@; ----------------------------------------

@section{COM Methods}

@defproc[(com-methods [obj/type (or/c com-object? com-type?)]) 
         (listof string?)]{

   Returns a list of strings indicating the names of methods on
   @racket[obj/type].}


@defproc[(com-method-type [obj/type (or/c com-object? com-type?)]
                          [method-name string?])
         (list/c '-> (listof type-description?) 
                     type-description?)]{

  Returns a list indicating the type of the specified method in
  @racket[obj/type]. The list after the @racket['->] represents the
  argument types, and the final value represents the result type. See
  @secref["com-types"] for more information.}


@defproc[(com-invoke [obj com-object?] [method-name string?] [v any/c] ...)
         any/c]{

  Invokes @racket[method-name] on @racket[obj] with @racket[v]s as the
  arguments. The special value @racket[com-omit] may be used for
  optional arguments, which useful when values are supplied for
  arguments after the omitted argument(s).

  The types of arguments are determined via @racket[com-method-type],
  if possible, and @racket[type-describe] wrappers in the @racket[v]s
  are simply replaced with the values that they wrap. If the types are
  not available from @racket[com-method-type], then types are inferred
  for each @racket[v] with attention to descriptions in any
  @racket[type-describe] wrappers in @racket[v].}


@defthing[com-omit any/c]{

A constant for use with @racket[com-invoke] in place of an optional
argument.}


@; ----------------------------------------

@section{COM Properties}

@defproc[(com-get-properties [obj/type (or/c com-object? com-type?)])
         (listof string?)]{

  Returns a list of strings indicating the names of readable
  properties in @racket[obj/type].}


@defproc[(com-get-property-type [obj/type (or/c com-object? com-type?)]
                                [property-name string?])
         (list/c '-> '() type-description?)]{

  Returns a type for @racket[property-name] like a result of
  @racket[com-method], where the result type corresponds to the
  property value type. See @secref["com-types"] for information on the
  symbols.}


@defproc[(com-get-property [obj com-object?] [property string?] ...+)
         any/c]{

  Returns the value of the final property by following the indicated
  path of @racket[property]s, where each intermediate property must be a
  COM object.}

@defproc[(com-get-property* [obj com-object?] [property string?] [v any/c] ...)
         any/c]{

  Returns the value of a parameterized property, which behaves like a
  method and accepts the @racket[v]s as arguments (like
  @racket[com-invoke]).  When no @racket[v]s are provided,
  @racket[com-get-property*] is the same as @racket[com-get-property].}

@defproc[(com-set-properties [obj/type (or/c com-object? com-type?)]) 
         (listof string?)]{

  Returns a list of strings indicating the names of writeable
  properties in @racket[obj/type].}


@defproc[(com-set-property-type [obj/type (or/c com-object? com-type?)] 
                                [property-name string?])
         (list/c '-> (list/c type-description?) 'void)]{

  Returns a type for @racket[property-name] like a result of
  @racket[com-method], where the sole argument type corresponds to the
  property value type. See @secref["com-types"] for
  information on the symbols.}


@defproc[(com-set-property! [obj com-object?] 
                            [string? property] ...+
                            [v any/c])
         void?]{

   Sets the value of the final property in @racket[obj] to @racket[v]
   by following the @racket[property]s, where the value of each
   intermediate property must be a COM object.

   The type of the property is determined via
   @racket[com-property-type], if possible, and
   @racket[type-describe] wrappers in @racket[v] are then replaced
   with the values that they wrap. If the type is not available from
   @racket[com-property-type], then a type is inferred for @racket[v]
   with attention to the descriptions in any @racket[type-describe]
   wrappers in @racket[v].}

@; ----------------------------------------

@section{COM Events}

@defproc[(com-events [obj/type (or/c com-object? com-type?)]) 
         (listof string?)]{

   Returns a list of strings indicating the names of events on
   @racket[obj/type].}


@defproc[(com-event-type [obj/type (or/c com-object? com-type?)]
                         [event-name string?])
         (list/c '-> (listof type-description?) 'void)]{

  Returns a list indicating the type of the specified events in
  @racket[obj/type]. The list after the @racket['->] represents the
  argument types. See @secref["com-types"] for more information.}


@defproc[(com-event-executor? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @deftech{COM event executor},
which queues event callbacks. A @tech{COM event executor}
@racket[_com-ev-ex] is a synchronizable event in the sense of
@racket[sync], and @racket[(sync _com-ev-ex)] returns a thunk for a
ready callback.}


@defproc[(com-make-event-executor) com-event-executor?]{

Creates a fresh @tech{COM event executor} for use with
@racket[com-register-event-callback].}


@defproc[(com-register-event-callback [obj com-object?]
                                      [name string?]
                                      [proc procedure?]
                                      [com-ev-ex com-event-executor?])
         void?]{

Registers a callback for the event named by @racket[name] in
@racket[obj]. When the event fires, an invocation of @racket[proc] to
event arguments (which depends on @racket[obj] and @racket[name]) is
queued in @racket[com-ev-ex]. Synchronizing on @racket[com-ev-ex]
produces a thunk that applies @racket[proc] to the event arguments and
returns the result.

Only one callback can be registered for each @racket[obj] and
@racket[name] combination.

Registration of event callbacks relies on prior registration of the
COM class implemented by @filepath{myssink.dll} as distributed with
Racket. (The DLL is the same for all Racket versions.)}


@defproc[(com-unregister-event-callback [obj com-object?]
                                        [name string?])
         void?]{

Removes any existing callback for @racket[name] in @racket[obj].}


@; ----------------------------------------

@section{Interface Pointers}

@deftogether[(
@defproc[(com-object-get-iunknown [obj com-object?]) com-iunkown?]
@defproc[(com-object-get-idispatch [obj com-object?]) com-idispatch?]
)]{

Extracts an @cpp{IUnknown} or @cpp{IDispatch} pointer from
@racket[obj]. The former succeeds for any @tech{COM object} that has
not been relased via @racket[com-release]. The latter succeeds
only when the @tech{COM object} supports @cpp{IDispatch}, otherwise
@racket[exn:fail] is raised.}


@defproc[(com-iunknown? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] corresponds to an unsafe
@racket[_IUnknown-pointer], @racket[#f] otherwise. Every @tech{COM
interface} extends @cpp{IUnknown}, so @racket[com-iunknown?] returns
@racket[#t] for every interface pointers.}


@defproc[(com-idispatch? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] corresponds to an unsafe
@cpp{IDispatch}, @racket[#f] otherwise.}


@; ----------------------------------------

@section[#:tag "remote"]{Remote COM servers (DCOM)}

The optional @racket[_where] argument to @racket[com-create-instance]
can be @racket['remote].  In that case, the server instance is run at
the location given by the Registry key

@centerline{@tt{HKEY_CLASSES_ROOT\AppID\@nonterm{CLSID}\RemoteServerName}}

where @nonterm{CLSID} is the CLSID of the application.  This key may
be set using the @exec{dcomcnfg} utility.  From @exec{dcomcnfg}, pick
the application to be run on the @onscreen{Applications} tab, then
click on the @onscreen{Properties} button.  On the @onscreen{Location}
tab, choose @onscreen{Run application on the following computer}, and
enter the machine name.

To run a COM remote server, the registry on the client machine must
contain an entry at

@centerline{@tt{HKEY_CLASSES_ROOT\CLSID\@nonterm{CLSID}}}

where @nonterm{CLSID} is the CLSID for the server.  The server
application itself need not be installed on the client machine.

There are a number of configuration issues relating to DCOM. See

@centerline{@link["http://www.distribucon.com/dcom95.aspx"]{http://www.distribucon.com/dcom95.html}}

for more information on how to setup client and server machines for DCOM.

@; ----------------------------------------

@section[#:tag "com-types"]{COM Types}

In the result of a function like @racket[com-method-type], symbols are
used to represent various atomic types:

@itemlist[

 @item{@racket['int] --- a 32-bit signed integer}

 @item{@racket['unsigned-int] --- a 32-bit unsigned integer}

 @item{@racket['short] --- a 16-bit signed integer}

 @item{@racket['unsigned-short] --- a 16-bit unsigned integer}

 @item{@racket['char] --- an 8-bit signed integer}

 @item{@racket['unsigned-char] --- an 8-bit unsigned integer}

 @item{@racket['long-long] --- a 64-bit signed integer}

 @item{@racket['unsigned-long-long] --- a 64-bit unsigned integer}

 @item{@racket['float] --- a 32-bit floating-point number}

 @item{@racket['double] --- a 64-bit floating-point number}

 @item{@racket['currency] --- an exact number that, when multiplied by 10,000,
                              is a 64-bit signed integer}

 @item{@racket['boolean] --- a boolean}

 @item{@racket['string] --- a string}

 @item{@racket['date] --- a @racket[date] or @racket[date*]}

 @item{@racket['com-object] --- a @tech{COM object} as in @racket[com-object?]}

 @item{@racket['iunknown] --- like @racket['com-object], but also accepts an @cpp{IUnknown} pointer as in @racket[com-iunknown?]}

 @item{@racket['com-enumeration] --- a 32-bit signed integer}

 @item{@racket['any] --- any of the above, or an array when not nested in an array type}

 @item{@racket['...] --- treated like @racket['any], but when it appears at the end of the sequence of types for
                         arguments, allows the preceding type 0 or more times}

 @item{@racket['void] --- no value}

]

A type symbol wrapped in a list with @racket['box], such as
@racket['(box int)], is a call-by-reference argument. A box supplied
for the argument is updated with a new value when the method returns.

A type wrapped in a list with @racket['opt], such as @racket['(opt
(box int))], is an optional argument. The argument can be omitted or
replaced with @racket[com-omit].

A type wrapped in a list with @racket['array] and a positive exact
integer, such as @racket['(array 7 int)], represents a vector of
values to be used as a COM array. A @racket['?] can be used in place
of the length integer to support a vector of any length.  Array types
with non-@racket['?] lengths can be nested to specify a
multidimensional array as represented by nested vectors.

A type wrapped in a list with @racket['variant], such as
@racket['(variant (array 7 int))], is the same as the wrapped type,
but a @racket['variant] wrapper within an @racket['array] type prevents
construction of another array dimension. For example, @racket['(array 2 (array 3
int))] is a two-dimensional array of integers, but @racket['(array 2
(variant (array 3 int)))] is a one-dimensional array whose elements
are one-dimensional arrays of integers.

When type information is not available, functions like @racket[com-invoke]
infer type descriptions from arguments. Inference chooses @racket['boolean]
for booleans; the first of @racket['int], @racket['unsigned-int], 
@racket['long-long], @racket['unsigned-long-long] that fits for an exact integer;
@racket['double] for inexact real numbers; @racket['string] for a string;
@racket['com-object] and @racket['iunknown] for corresponding COM object references;
and an @racket['array] type for a vector, where the element type is inferred
from vector values, resorting to @racket['any] if any two elements have different
inferred types.


@defproc[(type-description? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a COM argument or result type
description as above, @racket[#f] otherwise.}


@deftogether[(
@defproc[(type-described? [v any/c]) boolean?]
@defproc[(type-describe [v any/c] [desc type-description?])
         type-described?]
@defproc[(type-described-value [td type-described?]) any/c]
@defproc[(type-described-description [td type-described?])
         type-description?]
)]{

The @racket[type-described?] predicate recognizes wrappers produced
with @racket[type-describe], and @racket[type-described-value] and
@racket[type-described-description] extract the value and description
parts of a @racket[type-describe] value.

A @racket[type-describe] wrapper combines a base value with a type
description. The description is used instead of an automatically
inferred COM argument type when no type is available for from COM
automation a method for @racket[com-invoke] or a property for
@racket[com-set-property!]. A wrapper can be placed on an immediate
value, or it can be on a value within a box or vector.}

@; ----------------------------------------

@section{Class Display Names}

@defmodule[ffi/com-registry]{The @racketmodname[ffi/com-registry]
library provides a mapping from @tech{coclass} names to @tech{CLSIDs}
for compatibility with the older @tech{MysterX} interface.}

A @deftech{coclass} name corresponds to the display name of a COM
class; the display name is not uniquely mapped to a COM class, and
some COM classes have no display name.


@defproc[(com-all-coclasses) (listof string?)]{

Returns a list of @tech{coclass} strings for all @tech{COM class}es
registered on a system.}


@defproc[(com-all-controls) (listof string?)]{

Returns a list of @tech{coclass} strings for all COM classes in the
system registry that have the @racket["Control"] subkey.}


@deftogether[(
@defproc[(coclass->clsid [coclass string?]) clsid?]
@defproc[(clsid->coclass [clsid clsid?]) string?]
)]{

Converts a @tech{coclass} string to/from a @tech{CLSID}. This
conversion is implemented by an enumeration an @tech{COM class}es from
the system registry.}
