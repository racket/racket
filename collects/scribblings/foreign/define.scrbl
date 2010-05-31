#lang scribble/doc
@(require "utils.ss"
          (for-label ffi/unsafe/define
                     ffi/unsafe/alloc))

@title{Defining Bindings}

@defmodule[ffi/unsafe/define]

@defform/subs[(define-ffi-definer define-id ffi-lib-expr
                option ...)
              ([option (code:line #:provide provide-id)
                       (code:line #:define core-define-id)
                       (code:line #:default-make-fail default-make-fail-expr)])]{

Binds @scheme[define-id] as a definition form to extract bindings from
the library produced by @scheme[ffi-lib-expr]. The syntax of
@scheme[define-id] is

@specform/subs[(define-id id type-expr
                 bind-option ...)
               ([bind-option (code:line #:c-id c-id)
                             (code:line #:wrap wrap-expr)
                             (code:line #:make-fail make-fail-expr)
                             (code:line #:fail fail-expr)])]

A @scheme[define-id] form binds @scheme[id] by extracting a binding
with the name @scheme[c-id] from the library produced by
@scheme[ffi-lib-expr], where @scheme[c-id] defaults to @scheme[id].
The other options support further wrapping and configuration:

@itemize[

 @item{Before the extracted result is bound as @scheme[_id], it is
      passed to the result of @scheme[_wrap-expr], which defaults to
      @scheme[values]. Expressions such as @scheme[(allocator
      _delete)] or @scheme[(deallocator)] are useful as
      @scheme[_wrap-expr]s.}

 @item{The @scheme[#:make-fail] and @scheme[#:fail] options are
       mutually exclusive; if @scheme[_make-fail-expr] is provided, it
       is applied to @scheme['@#,scheme[_id]] to obtain the last
       argument to @scheme[get-ffi-obj]; if @scheme[_fail-expr] is
       provided, it is supplied directly as the last argument to
       @scheme[get-ffi-obj]. The @scheme[make-not-available] function
       is useful as @scheme[_make-fail-expr] to cause a use of
       @scheme[_id] to report an error when it is applied if
       @scheme[_c-id] was not found in the foreign library.}

]

If @scheme[provide-id] is provided to @scheme[define-ffi-definer], then
@scheme[define-id] also provides its binding using
@scheme[provide-id]. The @scheme[provide-protected] form is usually a
good choice for @scheme[provide-id].

If @scheme[core-define-id] is provided to @scheme[define-ffi-definer],
then @scheme[code-define-id] is used in place of @scheme[define] in
the expansion of @scheme[define-id] for each binding.

If @scheme[default-make-fail-expr] is provided to
@scheme[define-ffi-definer], it serves as the default
@scheme[#:make-fail] value for @scheme[define-id].

For example,

@schemeblock[
  (define-ffi-definer define-gtk gtk-lib)
]

binds @scheme[define-gtk] to extract FFI bindings from
@scheme[gtk-lib], so that @scheme[gtk_rc_parse] could be bound as

@schemeblock[
  (define-gtk gtk_rc_parse (_fun _path -> _void))
]

If @tt{gtk_rc_parse} is not found, then @scheme[define-gtk] reports an
error immediately. If @scheme[define-gtk] is instead defined with

@schemeblock[
  (define-ffi-definer define-gtk gtk-lib
     #:default-make-fail make-not-available)
]

then if @tt{gtk_rc_parse} is not found in @scheme[gtk-lib], an error
is reported only when @scheme[gtk_rc_parse] is called.}


@defproc[(make-not-available [name symbol?]) (#:rest list? -> any/c)]{

Returns a procedure that takes any number of arguments and reports an
error message from @scheme[name]. This function is intended for using
with @scheme[#:make-fail] or @scheme[#:default-make-fail] in
@scheme[define-ffi-definer]}

@defform[(provide-protected provide-spec ...)]{

Equivalent to @scheme[(provide (protect-out provide-spec ...))]. The
@scheme[provide-protected] identifier is useful with
@scheme[#:provide] in @scheme[define-ffi-definer].}
