#lang scribble/doc
@(require scribble/manual
          "com-common.rkt"
          scribble/racket
          (for-syntax racket/base)
          (for-label racket/base
                     (except-in racket/contract ->)
                     ffi/unsafe
                     ffi/unsafe/com
                     ffi/unsafe/alloc
                     ffi/winapi))

@title[#:tag "com-intf"]{COM Classes and Interfaces}

@defmodule[ffi/unsafe/com]{The @racketmodname[ffi/unsafe/com] library
exports all of @racketmodname[ffi/com], and it also supports direct,
FFI-based calls to COM object methods.}

@; ----------------------------------------

@section{Describing COM Interfaces}

@defform/subs[(define-com-interface (_id _super-id)
                ([method-id ctype-expr maybe-alloc-spec] ...))
              ([maybe-alloc-spec code:blank
                                 (code:line #:release-with-function function-id)
                                 (code:line #:release-with-method method-id)
                                 #:releases])]{

Defines @racket[_id] as an interface that extends @racket[_super-id],
where @racket[_super-id] is often @racket[_IUnknown], and that
includes methods named by @racket[method-id]. The @racket[_id] and
@racket[_super-id] identifiers must start with an underscore. A
@racket[@#,racket[_super-id]@#,racketidfont{_vt}] must also be defined
for deriving a virtual-method table type.

The order of the @racket[method-id]s must match the specification of
the @tech{COM interface}, not including methods inherited from
@racket[_super-id]. Each method type produced by @racket[ctype-expr]
that is not @racket[_fpointer] must be a function type whose first
argument is the ``self'' pointer, usually constructed with
@racket[_mfun] or @racket[_hmfun].

The @racket[define-com-interface] form binds @racket[_id],
@racket[@#,racketvarfont{id}?], @racket[@#,racket[_id]-pointer],
@racket[@#,racket[_id]@#,racketidfont{_}vt] (for the virtual-method
table), @racket[@#,racket[_id]@#,racketidfont{_}vt-pointer], and
@racket[method-id] for each method whose @racket[ctype-expr] is not
@racket[_fpointer]. (In other words, use @racket[_fpointer] as a
placeholder for methods of the interface that you do not need to
call.) An instance of the interface will have type
@racket[@#,racket[_id]-pointer]. Each defined @racket[method-id] is
bound to a function-like macro that expects a
@racket[@#,racket[_id]-pointer] as its first argument and the method
arguments as the remaining arguments.

A @racket[maybe-alloc-spec] describes allocation and finalization
information for a method along the lines of
@racketmodname[ffi/unsafe/alloc].  If the @racket[maybe-alloc-spec] is
@racket[#:release-with-function function-id], then
@racket[function-id] is used to deallocate the result produced by the
method, unless the result is explictly deallocated before it becomes
unreachable; for exmaple, @racket[#:release-with-function Release] is
suitable for a method that returns a COM interface reference that must
be eventually released.  The @racket[#:release-with-method method-id]
form is similar, except that the deallocator is a method on the same
object as the allocating method (i.e., one of the other
@racket[method-id]s or an inherited method). A @racket[#:releases]
annotation indicates that a method is a deallocator (so that a value
should not be automatically deallocated if it is explicitly
deallocated using the method).

See @secref["com-intf-example"] for an example using
@racket[define-com-interface].}

@; ----------------------------------------

@section{Obtaining COM Interface References}

@defproc[(QueryInterface [iunknown com-iunknown?] [iid iid?] [intf-pointer-type ctype?]) 
         (or/c cpointer? #f)]{

Attempts to extract a @tech{COM interface} pointer for the given
@tech{COM object}. If the object does not support the requested
interface, the result is @racket[#f], otherwise it is cast to the type
@racket[intf-pointer-type].

Specific @tech{IIDs} and @racket[intf-pointer-type]s go together. For
example, @racket[IID_IUnknown] goes with @racket[_IUnknown-pointer].

For a non-@racket[#f] result, @racket[Release] function is the
automatic deallocator for the resulting pointer. The pointer is
register with a deallocator after the cast to
@racket[intf-pointer-type], which is why @racket[QueryInterface]
accepts the @racket[intf-pointer-type] argument (since a cast
generates a fresh reference).}

@deftogether[(
@defproc[(AddRef [iunknown com-iunknown?]) exact-positive-integer?]
@defproc[(Release [iunknown com-iunknown?]) exact-nonnegative-integer?]
)]{

Increments or decrements the reference count on @racket[iunknown],
returning the new reference count and releasing the interface
reference if the count goes to zero.}


@defproc[(make-com-object [iunknown com-iunknown?] [clsid (or/c clsid? #f)]
                          [#:manage? manage? any/c #t])
         com-object?]{

Converts a @tech{COM object} into an object that can be used with the
COM automation functions, such as @racket[com-invoke].

If @racket[manage?] is true, the resulting object is registered with
the current custodian and a finalizer to call @racket[com-release]
when the custodian is shut down or when the object becomes
inaccessible.}

@; ----------------------------------------

@section{COM FFI Helpers}


@defform[(_wfun fun-option ... maybe-args type-spec ... -> type-spec
            maybe-wrapper)]{

Like @racket[_fun], but adds @racket[#:abi winapi].}


@defform[(_mfun fun-option ... maybe-args type-spec ... -> type-spec
            maybe-wrapper)]{

Like @racket[_wfun], but adds a @racket[_pointer] type (for the
``self'' argument of a method) as the first argument @racket[type-spec].}


@defform[(_hfun fun-option ... type-spec ... -> id maybe-allow output-expr)
         #:grammar
         ([maybe-allow code:blank
                       (code:line #:allow [result-id allow?-expr])])]{

Like @racket[_wfun], but for a function that returns an
@racket[_HRESULT]. The result is bound to @racket[result-id] if
@racket[#:allow] is specified, otherwise the result is not directly
accessible.

The @racket[_hfun] form handles the @racket[_HRESULT] value of the
foreign call as follows:

@itemlist[

 @item{If the result is zero or if @racket[#:allow] is specified and
       @racket[allow?-expr] produces @racket[#t], then
       @racket[output-expr] (as in a @racket[_maybe-wrapper] for
       @racket[_fun]) determines the result.}

 @item{If the result is @cpp{RPC_E_CALL_REJECTED} or
       @cpp{RPC_E_SERVERCALL_RETRYLATER}, the call is autmatically
       retried up to @racket[(current-hfun-retry-count)] times with a
       delay of @racket[(current-hfun-retry-delay)] seconds between
       each attempt.}

 @item{Otherwise, an error is raised using @racket[windows-error] and
       using @racket[id] as the name of the failed function.}

]

@history[#:changed "6.2" @elem{Added @racket[#:allow] and automatic retries.}]}


@defform[(_hmfun fun-option ... type-spec ... -> id output-expr)]{

Like @racket[_hfun], but lke @racket[_mfun] in that @racket[_pointer]
is added for the first argument.}

@deftogether[(
@defparam[current-hfun-retry-count exact-nonnegative-integer? count]
@defparam[current-hfun-retry-delay secs (>=/c 0.0)]
)]{

Parameters that determine the behavior of automatic retries for @racket[_hfun].

@history[#:added "6.2"]}


@defproc[(HRESULT-retry? [r exact-nonnegative-integer?]) boolean?]{

Returns @racket[#t] if @racket[r] is @cpp{RPC_E_CALL_REJECTED}
or @cpp{RPC_E_SERVERCALL_RETRYLATER}, @racket[#f] otherwise.

@history[#:added "6.2"]}


@deftogether[(
@defthing[_GUID ctype?]
@defthing[_GUID-pointer ctype?]
@defthing[_HRESULT ctype?]
@defthing[_LCID ctype?]
)]{

Some @tech{C types} that commonly appear in COM interface
specifications.}


@defthing[LOCALE_SYSTEM_DEFAULT exact-integer?]{

The usual value for a @racket[_LCID] argument.}


@deftogether[(
@defproc[(SysFreeString [str _pointer]) void?]
@defproc[(SysAllocStringLen [content _pointer] [len integer?]) cpointer?]
)]{

COM interfaces often require or return srings that must be allocated
or freed as system strings.

When receiving a string value, @racket[cast] it to
@racket[_string/utf-16] to extract a copy of the string, and then free
the original pointer with @racket[SysFreeString].}


@deftogether[(
@defthing[IID_NULL iid?]
@defthing[IID_IUnknown iid?]
)]{

Commonly used @tech{IIDs}.}

@deftogether[(
@defthing[_IUnknown ctype?]
@defthing[_IUnknown-pointer ctype?]
@defthing[_IUnknown_vt ctype?]
)]{

Types for the @cpp{IUnknown} @tech{COM interface}.}


@defproc[(windows-error [msg string?] [hresult exact-integer?])
         any]{

Raises an exception. The @racket[msg] strign provides the base error
message, but @racket[hresult] and its human-readable interpretation
(if available) are added to the message.}

@; ----------------------------------------

@section[#:tag "com-intf-example"]{COM Interface Example}

Here's an example using the Standard Component Categories Manager to
enumerate installed COM classes that are in the different
system-defined categories. The example illustrates instantiating a
COM class by @tech{CLSID}, describing COM interfaces with
@racket[define-com-interface], and using allocation specifications to
ensure that resources are reclaimed even if an error is encountered or
the program is interrupted.

@(define-syntax-rule (define-literals id ...) (begin (define-literal id) ...))
@(define-syntax-rule (define-literal id)
   (define-syntax id (make-element-id-transformer 
                      (lambda (stx) #'@racketidfont[(symbol->string 'id)]))))
@define-literals[_ULONG _CATID _REFCATID
                 _CATEGORYINFO _CATEGORYINFO-pointer
                 _IEnumGUID _IEnumGUID-pointer
                 _IEnumCATEGORYINFO _IEnumCATEGORYINFO-pointer
                 _ICatInformation _ICatInformation-pointer]

@racketmod[
racket/base
(require ffi/unsafe
         ffi/unsafe/com)

(provide show-all-classes)

(code:comment @#,t{The function that uses COM interfaces defined further below:})

(define (show-all-classes)
  (define ccm 
    (com-create-instance CLSID_StdComponentCategoriesMgr))
  (define icat (QueryInterface (com-object-get-iunknown ccm) 
                               IID_ICatInformation 
                               _ICatInformation-pointer))
  (define eci (EnumCategories icat LOCALE_SYSTEM_DEFAULT))
  (for ([catinfo (in-producer (lambda () (Next/ci eci)) #f)])
    (printf "~a:\n"
            (cast (array-ptr (CATEGORYINFO-szDescription catinfo)) 
                  _pointer 
                  _string/utf-16))
    (define eg 
      (EnumClassesOfCategories icat (CATEGORYINFO-catid catinfo)))
    (for ([guid (in-producer (lambda () (Next/g eg)) #f)])
      (printf " ~a\n" (or (clsid->progid guid)
                          (guid->string guid))))
    (Release eg))
  (Release eci)
  (Release icat))

(code:comment @#,t{The class to instantiate:})

(define CLSID_StdComponentCategoriesMgr
  (string->clsid "{0002E005-0000-0000-C000-000000000046}"))

(code:comment @#,t{Some types and variants to match the specification:})

(define _ULONG _ulong)
(define _CATID _GUID)
(define _REFCATID _GUID-pointer)
(define-cstruct _CATEGORYINFO ([catid _CATID]
                               [lcid _LCID]
                               [szDescription (_array _short 128)]))

(code:comment @#,t{------ IEnumGUID -------})

(define IID_IEnumGUID
  (string->iid "{0002E000-0000-0000-C000-000000000046}"))

(define-com-interface (_IEnumGUID _IUnknown)
  ([Next/g (_mfun (_ULONG = 1) (code:comment @#,t{simplifed to just one})
                  (guid : (_ptr o _GUID))
                  (got : (_ptr o _ULONG))
                  -> (r : _HRESULT)
                  -> (cond
                       [(zero? r) guid]
                       [(= r 1) #f] ; done
                       [else (windows-error "Next/g failed" r)]))]
   [Skip _fpointer]
   [Reset _fpointer]
   [Clone _fpointer]))

(code:comment @#,t{------ IEnumCATEGORYINFO -------})

(define IID_IEnumCATEGORYINFO
  (string->iid "{0002E011-0000-0000-C000-000000000046}"))

(define-com-interface (_IEnumCATEGORYINFO _IUnknown)
  ([Next/ci (_mfun (_ULONG = 1) (code:comment @#,t{simplifed to just one})
                   (catinfo : (_ptr o _CATEGORYINFO))
                   (got : (_ptr o _ULONG))
                   -> (r : _HRESULT)
                   -> (cond
                       [(zero? r) catinfo]
                       [(= r 1) #f] ; done
                       [else (windows-error "Next/ci failed" r)]))]
   [Skip _fpointer]
   [Reset _fpointer]
   [Clone _fpointer]))

(code:comment @#,t{------ ICatInformation -------})

(define IID_ICatInformation
  (string->iid "{0002E013-0000-0000-C000-000000000046}"))

(define-com-interface (_ICatInformation _IUnknown)
  ([EnumCategories (_hmfun _LCID
                           (p : (_ptr o _IEnumCATEGORYINFO-pointer))
                           -> EnumCategories p)]
   [GetCategoryDesc (_hmfun _REFCATID _LCID
                            (p : (_ptr o _pointer))
                            -> GetCategoryDesc 
                            (begin0
                             (cast p _pointer _string/utf-16)
                             (SysFreeString p)))]
   [EnumClassesOfCategories (_hmfun (_ULONG = 1) (code:comment @#,t{simplifed})
                                    _REFCATID
                                    (_ULONG = 0) (code:comment @#,t{simplifed})
                                    (_pointer = #f)
                                    (p : (_ptr o 
                                               _IEnumGUID-pointer))
                                    -> EnumClassesOfCategories p)
                            #:release-with-function Release]
   [IsClassOfCategories _fpointer]
   [EnumImplCategoriesOfClass _fpointer]
   [EnumReqCategoriesOfClass _fpointer]))

]
