#lang scribble/doc
@(require "utils.rkt" (for-label ffi/unsafe/define ffi/unsafe/alloc))

@title{Defining Bindings}

@defmodule[ffi/unsafe/define]

@defform/subs[(define-ffi-definer define-id ffi-lib-expr
                option ...)
              ([option (code:line #:provide provide-id)
                       (code:line #:define core-define-id)
                       (code:line #:default-make-fail default-make-fail-expr)
                       (code:line #:make-c-id make-c-id)])]{

Binds @racket[define-id] as a definition form to extract bindings from
the library produced by @racket[ffi-lib-expr]. The syntax of
@racket[define-id] is

@specform/subs[#:literals (unquote)
               (define-id id type-expr
                 bind-option ...)
               ([bind-option (code:line #:c-id c-id)
                             (code:line #:c-id (@#,(racket unquote) c-id-expr))
                             (code:line #:wrap wrap-expr)
                             (code:line #:make-fail make-fail-expr)
                             (code:line #:fail fail-expr)
                             (code:line #:variable)])]

A @racket[define-id] form binds @racket[id] by extracting a binding
with the foreign name @racket[_c-id] from the library produced by
@racket[ffi-lib-expr], where @racket[_c-id] defaults to @racket[_id].
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

 @item{If the @racket[#:c-id] option is provided with an identifier
       argument @racket[_c-id], then @racket[_c-id] is used as the
       foreign name. If the argument has the form @racket[(@#,(racket
       unquote) _c-id-expr)], then the foreign name is the result of
       evaluating @racket[_c-id-expr]. In either case, the
       @racket[#:make-c-id] argument is ignored.}

 @item{If the @racket[#:c-id] option is absent, the foreign name is
        based on @racket[_id]. If @racket[define-id] was defined with
        the @racket[#:make-c-id] option, it computes the foreign name
        using an @tech{ffi identifier convention}, such as converting
        hyphens to underscores or camel case.  Several conventions are
        provided by @racketmodname[ffi/unsafe/define/conventions]. If
        @racket[#:make-c-id] was absent, then @racket[_id] is used as
        the foreign name.}

 @item{If the @racket[#:variable] keyword is given, then
       @racket[make-c-parameter] is used instead of
       @racket[get-ffi-obj] to get the foreign value.}
]

If @racket[provide-id] is provided to @racket[define-ffi-definer], then
@racket[define-id] also provides its binding using
@racket[provide-id]. The @racket[provide-protected] form is usually a
good choice for @racket[provide-id].

If @racket[core-define-id] is provided to @racket[define-ffi-definer],
then @racket[core-define-id] is used in place of @racket[define] in
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
is reported only when @racket[gtk_rc_parse] is called.

@history[#:changed "6.9.0.5" @elem{Added @racket[#:make-c-id] parameter.}
         #:changed "8.4.0.5" @elem{Added @racket[#:variable] option.
                                   Added @racket[unquote] variant of
                                   @racket[#:c-id] argument.}]}


@defproc[(make-not-available [name symbol?]) procedure?]{

Returns a procedure that takes any number of arguments, including keyword arguments, and reports an
error message from @racket[name]. This function is intended for using
with @racket[#:make-fail] or @racket[#:default-make-fail] in
@racket[define-ffi-definer]

@history[#:changed "8.3.0.5" @elem{Added support for keyword arguments.}]}

@defform[(provide-protected provide-spec ...)]{

Equivalent to @racket[(provide (protect-out provide-spec ...))]. The
@racket[provide-protected] identifier is useful with
@racket[#:provide] in @racket[define-ffi-definer].}

@section{FFI Identifier Conventions}

@defmodule[ffi/unsafe/define/conventions]

This module provides several
@deftech{FFI identifier conventions} for use with
@racket[#:make-c-id] in @racket[define-ffi-definer]. A
@tech{FFI identifier convention} is any
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax transformer}
that converts one identifier to another.

@history[#:added "6.9.0.5"]

@defidform[convention:hyphen->underscore]{

 A convention that converts hyphens in an identifier to
 underscores. For example, the identifier
 @racket[gtk-rc-parse] will transform to @racket[gkt_rc_parse].

@racketblock[
  (define-ffi-definer define-gtk gtk-lib
    #:make-c-id convention:hyphen->underscore)
 (define-gtk gtk-rc-parse (_fun _path -> _void))]
}

@defidform[convention:hyphen->camelcase]{
                                         
 Similar to @racket[convention:hyphen->underscore], but
 converts the identifier to camel case instead, following the
 @racket[string-titlecase] function. For example, the
 identifier @racket[camelCaseVariable] will transform to
 @racket[came-case-variable].
 
 @racketblock[
 (define-ffi-definer define-calib camel-lib
   #:make-c-id convention:hyphen->camelcase)
 (define-calib camel-case-variable (_fun -> _void))]
}
