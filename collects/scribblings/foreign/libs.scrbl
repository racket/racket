#lang scribble/doc
@(require "utils.rkt" (for-label setup/dirs) (for-syntax setup/dirs))

@title{Loading Foreign Libraries}

The FFI is normally used by extracting functions and other objects
from @as-index{shared objects} (a.k.a. @defterm{@as-index{shared
libraries}} or @defterm{@as-index{dynamically loaded libraries}}). The
@scheme[ffi-lib] function loads a shared object.

@defproc[(ffi-lib? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is the result of @scheme[ffi-lib],
@scheme[#f] otherwise.}


@defproc[(ffi-lib [path (or/c path-string? #f)]
                  [version (or/c string? (listof (or/c string? #f)) #f) #f]) any]{

Returns a foreign-library value. Normally, 

@itemlist[

 @item{@racket[path] is a path without a version or suffix (i.e.,
       without @filepath{.dll}, @filepath{.so}, or @filepath{.dylib});
       and}

 @item{@racket[version] is a list of versions to try in order with
      @racket[#f] (i.e., no version) as the last element of the list;
      for example, @racket['("2" #f)] indicates version 2 with a
      fallback to a versionless library.}

]

A string or @racket[#f] @racket[version] is equivalent to a list
containing just the string or @racket[#f], and an empty string (by
itself or in a list) is equivalent to @racket[#f].

Beware of relying on versionless library names. On some platforms,
versionless library names are provided only by development
packages. At the same time, other platforms may require a versionless
fallback. A list of version strings followed by @racket[#f] is
typically best for @racket[version].

Assuming that @scheme[path] is not @racket[#f], the result from
@racket[ffi-lib] represents the library found by the following search
process:

@itemlist[

 @item{If @racket[path] is not an absolute path, look in each
       directory reported by @scheme[get-lib-search-dirs]. In each
       directory, try @racket[path] with the first version in
       @racket[version], adding a suitable suffix if @racket[path]
       does not already end in the suffix, then try the second version
       in @racket[version], etc. (If @racket[version] is an empty list,
       no paths are tried in this step.)}

 @item{Try the same filenames again, but without converting the path
       to an absolute path, which allows the operating system to use
       its own search paths. (If @racket[version] is an empty list, no
       paths are tried in this step.)}

 @item{Try @racket[path] without adding any version or suffix, and
       without converting to an absolute path.}

 @item{Try the version-adjusted filenames again, but relative to the
       current directory. (If @racket[version] is an empty list, no
       paths are tried in this step.)}

 @item{Try @racket[path] without adding any version or suffix, but
      converted to an absolute path relative to the current
      directory.}

]

If none of the paths succeed, the error is reported from trying the
first path from the second bullet above or (if @racket[version] is an
empty list) from the third bullet above. A library file may exist but
fail to load for some reason; the eventual error message will
unfortunately name the fallback from the second or third bullet, since
some operating systems offer no way to determine why a given library
path failed.

If @scheme[path] is @scheme[#f], then the resulting foreign-library
value represents all libraries loaded in the current process,
including libraries previously opened with @scheme[ffi-lib].  In
particular, use @scheme[#f] to access C-level functionality exported
by the run-time system (as described in @|InsideRacket|). The
@racket[version] argument is ignored when @racket[path] is
@racket[#f].

Due to the way the operating system performs dynamic binding, loaded
libraries are associated with Racket (or DrRacket) for the duration of
the process. Re-evaluating @racket[ffi-lib] (or hitting the
@onscreen{Run} button in DrRacket) will not force a re-load of the
corresponding library.}

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
