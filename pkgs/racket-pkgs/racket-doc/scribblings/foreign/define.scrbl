#lang scribble/doc
@(require "utils.rkt" (for-label ffi/unsafe/define ffi/unsafe/alloc))

@title{Defining Bindings}

@defmodule[ffi/unsafe/define]

@defform/subs[(define-ffi-definer define-id ffi-lib-expr
                option ...)
              ([option (code:line #:provide provide-id)
                       (code:line #:define core-define-id)
                       (code:line #:default-make-fail default-make-fail-expr)])]{

Binds @racket[define-id] as a definition form to extract bindings from
the library produced by @racket[ffi-lib-expr]. The syntax of
@racket[define-id] is

@specform/subs[(define-id id type-expr
                 bind-option ...)
               ([bind-option (code:line #:c-id c-id)
                             (code:line #:wrap wrap-expr)
                             (code:line #:make-fail make-fail-expr)
                             (code:line #:fail fail-expr)])]

A @racket[define-id] form binds @racket[id] by extracting a binding
with the name @racket[c-id] from the library produced by
@racket[ffi-lib-expr], where @racket[c-id] defaults to @racket[id].
The other options support further wrapping and configuration:

@itemize[

 @item{Before the extracted result is bound as @racket[_id], it is
       passed to the result of @racket[_wrap-expr], which defaults to
       @racket[values]. Expressions such as @racket[(allocator
       _delete)] or @racket[(deallocator)] are useful as
       @racket[_wrap-expr]s.}

 @item{The @racket[#:make-fail] and @racket[#:fail] options are
       mutually exclusive; if @racket[_make-fail-expr] is provided, it
       is applied to @racket['@#,racket[_id]] to obtain the last
       argument to @racket[get-ffi-obj]; if @racket[_fail-expr] is
       provided, it is supplied directly as the last argument to
       @racket[get-ffi-obj]. The @racket[make-not-available] function
       is useful as @racket[_make-fail-expr] to cause a use of
       @racket[_id] to report an error when it is applied if
       @racket[_c-id] was not found in the foreign library.}

]

If @racket[provide-id] is provided to @racket[define-ffi-definer], then
@racket[define-id] also provides its binding using
@racket[provide-id]. The @racket[provide-protected] form is usually a
good choice for @racket[provide-id].

If @racket[core-define-id] is provided to @racket[define-ffi-definer],
then @racket[code-define-id] is used in place of @racket[define] in
the expansion of @racket[define-id] for each binding.

If @racket[default-make-fail-expr] is provided to
@racket[define-ffi-definer], it serves as the default
@racket[#:make-fail] value for @racket[define-id].

For example,

@racketblock[
  (define-ffi-definer define-gtk gtk-lib)
]

binds @racket[define-gtk] to extract FFI bindings from
@racket[gtk-lib], so that @racket[gtk_rc_parse] could be bound as

@racketblock[
  (define-gtk gtk_rc_parse (_fun _path -> _void))
]

If @tt{gtk_rc_parse} is not found, then @racket[define-gtk] reports an
error immediately. If @racket[define-gtk] is instead defined with

@racketblock[
  (define-ffi-definer define-gtk gtk-lib
     #:default-make-fail make-not-available)
]

then if @tt{gtk_rc_parse} is not found in @racket[gtk-lib], an error
is reported only when @racket[gtk_rc_parse] is called.}


@defproc[(make-not-available [name symbol?]) (#:rest list? -> any/c)]{

Returns a procedure that takes any number of arguments and reports an
error message from @racket[name]. This function is intended for using
with @racket[#:make-fail] or @racket[#:default-make-fail] in
@racket[define-ffi-definer]}

@defform[(provide-protected provide-spec ...)]{

Equivalent to @racket[(provide (protect-out provide-spec ...))]. The
@racket[provide-protected] identifier is useful with
@racket[#:provide] in @racket[define-ffi-definer].}
