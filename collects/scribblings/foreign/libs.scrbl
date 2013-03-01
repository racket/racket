#lang scribble/doc
@(require "utils.rkt" (for-label setup/dirs) (for-syntax setup/dirs))

@title{Loading Foreign Libraries}

The FFI is normally used by extracting functions and other objects
from @as-index{shared objects} (a.k.a. @defterm{@as-index{shared
libraries}} or @defterm{@as-index{dynamically loaded libraries}}). The
@racket[ffi-lib] function loads a shared object.

@defproc[(ffi-lib? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @deftech{foreign-library value},
@racket[#f] otherwise.}


@defproc[(ffi-lib [path (or/c path-string? #f)]
                  [version (or/c string? (listof (or/c string? #f)) #f) #f]
		  [#:get-lib-dirs get-lib-dirs (-> (listof path?)) get-lib-search-dirs]
		  [#:fail fail (or/c #f (-> any)) #f]
                  [#:global? global? any/c (eq? 'global (system-type 'so-mode))])
         any]{

Returns a @tech{foreign-library value} or the result of @racket[fail]. 
Normally,

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

Assuming that @racket[path] is not @racket[#f], the result from
@racket[ffi-lib] represents the library found by the following search
process:

@itemlist[

 @item{If @racket[path] is not an absolute path, look in each
       directory reported by @racket[get-lib-dirs]. In each
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

If none of the paths succeed and @racket[fail] is a function, then
@racket[fail] is called in tail position. If @racket[fail] is
@racket[#f], an error is reported from trying the
first path from the second bullet above or (if @racket[version] is an
empty list) from the third bullet above. A library file may exist but
fail to load for some reason; the eventual error message will
unfortunately name the fallback from the second or third bullet, since
some operating systems offer no way to determine why a given library
path failed.

If @racket[path] is @racket[#f], then the resulting foreign-library
value represents all libraries loaded in the current process,
including libraries previously opened with @racket[ffi-lib].  In
particular, use @racket[#f] to access C-level functionality exported
by the run-time system (as described in @|InsideRacket|). The
@racket[version] argument is ignored when @racket[path] is
@racket[#f].

If @racket[path] is not @racket[#f], @racket[global?] is true, and the
operating system supports opening a library in ``global'' mode so that
the library's symbols are used for resolving references from libraries
that are loaded later, then global mode is used to open the
library. Otherwise, the library is opened in ``local'' mode, where the
library's symbols are not made available for future resolution. This
local-versus-global choice does not affect whether the library's
symbols are available via @racket[(ffi-lib #f)].

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

Looks for @racket[objname] in
@racket[lib] library.  If @racket[lib] is not a @tech{foreign-library value}
it is converted to one by calling @racket[ffi-lib]. If @racket[objname] 
is found in @racket[lib], it is
converted to Racket using the given @racket[type]. Types are described
in @secref["types"]; in particular the @racket[get-ffi-obj] procedure
is most often used with function types created with @racket[_fun].

Keep in mind that @racket[get-ffi-obj] is an unsafe procedure; see
@secref["intro"] for details.

If the name is not found, and @racket[failure-thunk] is provided, it is
used to produce a return value.  For example, a failure thunk can be
provided to report a specific error if an name is not found:

@racketblock[
(define foo
  (get-ffi-obj "foo" foolib (_fun _int -> _int)
    (lambda ()
      (error 'foolib
             "installed foolib does not provide \"foo\""))))
]

The default (also when @racket[failure-thunk] is provided as @racket[#f]) is to
raise an exception.}


@defproc[(set-ffi-obj! [objname (or/c string? bytes? symbol?)]
                       [lib (or/c ffi-lib? path-string? #f)]
                       [type ctype?]
                       [new any/c])
         void?]{

Looks for @racket[objname] in @racket[lib] similarly to
@racket[get-ffi-obj], but then it stores the given @racket[new] value
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
@racket[get-ffi-obj].

A parameter-like function is useful in case Racket code and library
code interact through a library value.  Although
@racket[make-c-parameter] can be used with any time, it is not
recommended to use this for foreign functions, since each reference
through the parameter will construct the low-level interface before the
actual call.}


@defform[(define-c id lib-expr type-expr)]{

Defines @racket[id] behave like a Racket binding, but @racket[id] is
actually redirected through a parameter-like procedure created by
@racket[make-c-parameter]. The @racket[id] is used both for the Racket
binding and for the foreign name.}

@defproc[(ffi-obj-ref [objname (or/c string? bytes? symbol?)]
                      [lib (or/c ffi-lib? path-string? #f)]
                      [failure-thunk (or/c (-> any) #f) #f]) 
         any]{

Returns a pointer for the specified foreign name, calls
@racket[failure-thunk] if the name is not found, or raises an
exception if @racket[failure-thunk] is @racket[#f].

Normally, @racket[get-ffi-obj] should be used, instead.}
