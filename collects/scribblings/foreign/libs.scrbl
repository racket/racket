#lang scribble/doc
@(require "utils.ss"
          (for-syntax setup/dirs))

@title{Loading Foreign Libraries}

The FFI is normally used by extracting functions and other objects
from @as-index{shared objects} (a.k.a. @defterm{@as-index{shared
libraries}} or @defterm{@as-index{dynamically loaded libraries}}). The
@scheme[ffi-lib] function loads a shared object.

@defproc[(ffi-lib? [v any/c]) boolean>]{

Returns @scheme[#t] if @scheme[v] is the result of @scheme[ffi-lib],
@scheme[#f] otherwise.}


@defproc[(ffi-lib [path (or/c path-string? #f)]
                  [version (or/c string? (listof (or/c string? #f)) #f) #f]) any]{

Returns an foreign-library value. If @scheme[path] is a path, the
result represents the foreign library, which is opened in an
OS-specific way (using @cpp{LoadLibrary} under Windows, and
@cpp{dlopen} under Unix and Mac OS X).

The path is not expected to contain the library suffix, which is added
according to the current platform.  If adding the suffix fails,
several other filename variations are tried: retrying without an
automatically added suffix, and using a full path of a file if it
exists relative to the current directory (since the OS-level library
function usually searches, unless the library name is an absolute
path). An optional @scheme[version] string can be supplied, which is
appended to the name before or after the suffix, depending on platform
conventions, unless it is @scheme[#f] or @scheme[""]. If
@scheme[version] is a list, @scheme[ffi-lib] will try each of them in
order.

If @scheme[path] is @scheme[#f], then the resulting foreign-library
value represents all libraries loaded in the current process,
including libraries previously opened with @scheme[ffi-lib].  In
particular, use @scheme[#f] to access C-level functionality exported
by the run-time system (as described in @|InsideRacket|).

Note: @scheme[ffi-lib] tries to look for the library file in a few
places, inluding the PLT libraries (see @scheme[get-lib-search-dirs]),
a relative path, or a system search. When @scheme[version] is a list,
different versions are tried through each route before continuing the
search with other routes. However, if @cpp{dlopen} cannot open a
library, there is no reliable way to know why it failed, so if all
path combinations fail, it will raise an error with the result of
@cpp{dlopen} on the unmodified argument name.  For example, if you
have a local @filepath{foo.so} library that cannot be loaded because
of a missing symbol, using @scheme[(ffi-lib "foo.so")] will fail with
all its search options, most because the library is not found, and
once because of the missing symbol, and eventually produce an error
message that comes from @cpp{dlopen("foo.so")} which will look like
the file is not found.  In such cases try to specify a full or
relative path (containing slashes, e.g., @filepath{./foo.so}).}


@defproc[(get-ffi-obj [objname (or/c string? bytes? symbol?)]
                      [lib (or/c ffi-lib? path-string? #f)]
                      [type ctype?]
                      [failure-thunk (or/c (-> any) #f) #f]) 
         any]{

Looks for the given object name @scheme[objname] in the given
@scheme[lib] library.  If @scheme[lib] is not a foreign-library value
produced by @scheme[ffi-lib], it is converted to one by calling
@scheme[ffi-lib]. If @scheme[objname] is found in @scheme[lib], it is
converted to Racket using the given @scheme[type]. Types are described
in @secref["types"]; in particular the @scheme[get-ffi-obj] procedure
is most often used with function types created with @scheme[_fun].

Keep in mind that @scheme[get-ffi-obj] is an unsafe procedure; see
@secref["intro"] for details.

If the object is not found, and @scheme[failure-thunk] is provided, it is
used to produce a return value.  For example, a failure thunk can be
provided to report a specific error if an object is not found:

@schemeblock[
(define foo
  (get-ffi-obj "foo" foolib (_fun _int -> _int)
    (lambda ()
      (error 'foolib
             "installed foolib does not provide \"foo\""))))
]

The default (also when @scheme[failure-thunk] is provided as @scheme[#f]) is to
raise an exception.}


@defproc[(set-ffi-obj! [objname (or/c string? bytes? symbol?)]
                       [lib (or/c ffi-lib? path-string? #f)]
                       [type ctype?]
                       [new any/c])
         void?]{

Looks for @scheme[objname] in @scheme[lib] similarly to
@scheme[get-ffi-obj], but then it stores the given @scheme[new] value
into the library, converting it to a C value.  This can be used for
setting library customization variables that are part of its
interface, including Racket callbacks.}


@defproc[(make-c-parameter [objname (or/c string? bytes? symbol?)]
                           [lib (or/c ffi-lib? path-string? #f)]
                           [type ctype?])
         (and/c (-> any)
                (any/c -> void?))]{

Returns a parameter-like procedure that can either references the
specified foreign value, or set it.  The arguments are handled as in
@scheme[get-ffi-obj].

A parameter-like function is useful in case Racket code and library
code interact through a library value.  Although
@scheme[make-c-parameter] can be used with any time, it is not
recommended to use this for foreign functions, since each reference
through the parameter will construct the low-level interface before the
actual call.}


@defform[(define-c id lib-expr type-expr)]{

Defines @scheme[id] behave like a Racket binding, but @scheme[id] is
actually redirected through a parameter-like procedure created by
@scheme[make-c-parameter]. The @scheme[id] is used both for the Racket
binding and for the foreign object's name.}

@defproc[(ffi-obj-ref [objname (or/c string? bytes? symbol?)]
                      [lib (or/c ffi-lib? path-string? #f)]
                      [failure-thunk (or/c (-> any) #f) #f]) 
         any]{

Returns a pointer object for the specified foreign object.  This
procedure is for rare cases where @scheme[make-c-parameter] is
insufficient, because there is no type to cast the foreign object to
(e.g., a vector of numbers).}
