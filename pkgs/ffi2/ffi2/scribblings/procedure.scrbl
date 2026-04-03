#lang scribble/manual
@(require "common.rkt")

@title[#:tag "procedure"]{Foreign Procedures}

@defform[#:kind "ffi2 type"
         #:literals (: =)
         (-> arg ...
             maybe-varargs
             result maybe-errno
             option
             ...)
         #:grammar ([arg type
                         [arg-id : type]
                         [type = auto-expr]
                         [arg-id : type = auto-expr]]
                    [maybe-varargs (code:line #:varargs arg ...)
                                   ϵ]
                    [result result-type
                            [result-id : result-type]]
                    [maybe-errno #:errno
                                 #:get-last-error
                                 [errno-id : #:errno]
                                 [errno-id : #:get-last-error]
                                 ϵ]
                    [option (code:line #:result result-expr)
                            (code:line #:abi abi)
                            (code:line #:atomic)
                            (code:line #:collect-safe)
                            (code:line #:allow-callback-exn)
                            (code:line #:in-original)])]{

Describes the type of a procedure. The C representation of a procedure
is an address, while the Racket representation is a Racket procedure.

A @racket[->] type is normally used with @racket[ffi2-procedure] or
@racket[define-ffi2-procedure] to obtain a Racket produce that calls a
C function. That use of @racket[->] creates a @deftech{foreign callout}.

A @racket[->] type can also be used with @racket[ffi2-callback] to
turn a Racket procedure into a function callback by C as represented
by a @tech{pointer} object. That of use of @racket[->] creates a
@deftech{foreign callback}. A callback always runs in @deftech{atomic
mode}, which means that it must not attempt any synchronization
operations and generally must not raise an exception (unless
@racket[#:allow-callback-exn] is used for a callout that reaches the
callback).

Each @racket[arg] describes a type for an argument, each with an
optional name @racket[arg-id] and an optional @racket[auto-expr]. When
an argument has an @racket[auto-expr], no corresponding argument is
provided to a callout. Each @racket[auto-expr] can refer to
@racket[arg-id]s for arguments without @racket[auto-expr]s and for
earlier arguments that have @racket[auto-expr]s. When an arrow type
is used for a foreign callback (instead of a callout), @racket[arg-id]s
and @racket[auto-expr]s have no effect.

A @racket[result] can similarly have a @racket[result-id]. That
identifier can be used (along with the @racket[arg-id]s) in a
@racket[result-expr] supplied with @racket[#:result]. The value of
@racket[#:result] becomes the result for a callout, and it is not
used for a callback.

If @racket[#:errno] or @racket[#:get-last-error] (optionally
with a @racket[errno-id]) is specified after
@racket[result-type], then calling a function produced by
@racket[ffi2-procedure] returns two values (absent a
@racket[#:result] expression): the foreign procedure's
return value and the value of the C library's @tt{errno} or the result
of the @tt{GetLastError} function on Windows. The @tt{errno} or
@tt{GetLastError} result is obtained just after the foreign procedure
call returns and before it can be changed. On platforms other than
Windows, @racket[#:get-last-error] is treated as @racket[#:errno]. A
@racket[#:errno] or @racket[#:get-last-error] specification has no
effect for a callback. If @racket[errno-id] is specified, it
can be referenced in a @racket[result-expr] to determine a
callout result.

Each @racket[option] affects the way a foreign procedure is called or
how a callback is handled:

@itemlist[

 @item{@racket[#:abi]: Uses @racket[abi] as the @tech{ABI} for a
 @tech{foreign callout} or @tech{foreign callback}. See
 @racket[default_abi], @racket[cdecl_abi], and @racket[stdcall_abi].}

 @item{@racket[#:atomic]: Adjusts a @tech{foreign callout} to
 potentially improve performance. The foreign procedure must not
 invoke any callbacks or other reach the Racket run-time system, so it
 can be considered an atomic operation from the perspective of Racket.
 This option has no effect on @tech{foreign callbacks}.}

 @item{@racket[#:collect-safe]: Adjusts a @tech{foreign callout}
 to allow Racket garbage collection concurrent with the call, or
 adjusts a @tech{foreign callback} to re-enable synchronization with
 the garbage collector during the callback (i.e., only collect-safe
 callbacks are allowed to be invoked via a collect-safe procedure
 call; a collect-safe callback can be invoked through a
 non-collect-safe foreign procedure call). Note that a collect-safe
 call makes sense only when arguments to the foreign procedure are not
 managed by the Racket garbage collector or are immobile and reliably
 retained.}

 @item{@racket[#:allow-callback-exn]: Adjusts a @tech{foreign callout}
 so that a foreign callback is allowed to raise an exception that
 escape the foreign-procedure call. This option has no effect on
 @tech{foreign callbacks} themselves. A foreign callback must never
 raise an exception unless it is invoked via foreign procedure call
 using this option.}

 @item{@racket[#:in-original]: Adjusts a @tech{foreign callout} to
 take place in a @tech[#:doc ref-doc]{coroutine thread} within
 Racket's main @tech[#:doc ref-doc]{place} (which is useful if the
 foreign procedure is not thread-safe), or adjusts a @tech{foreign
 callback} invocation so that it takes place in a coroutine thread
 within the current place (which can be useful if the callback might
 otherwise run in a thread not created by Racket). The callout or
 callback happens in the context of an unspecified Racket coroutine
 thread, so it must not raise an exception.}

]

}

@defform[(ffi2-procedure ptr-expr arrow-type)]{

Creates a @tech{foreign callout}. The @racket[arrow-type] must be
either an immediate @racket[->] type or a type name that is defined as
an @racket[->] type.

Using @racket[ffi2-procedure] is equivalent to @racket[(ffi2-cast
ptr-expr #:from ptr_t #:to arrow-type)] to convert a foreign
function pointer to a procedure that can be called from Racket.

}

@defform[(ffi2-callback proc-expr arrow-type)]{

Creates a @tech{foreign callback}. The @racket[arrow-type] must be
either an immediate @racket[->] type or a type name that is defined as
an @racket[->] type.

Using @racket[ffi2-callback] is equivalent to @racket[(ffi2-cast
proc-expr #:from arrow-type #:to ptr_t)] to convert a Racket
procedure to a function pointer to be called in a foreign context.

A foreign callback is managed by the Racket garbage collector. A
callback is always immobile in the sense of using @racket[ffi2-malloc]
in @racket[#:gcable-immobile] mode, but the callback must be retained
somehow as long as it is relevant for calls from a foreign context.
A callback is automatically retained in that sense if it is passed to
a foreign callout that uses the callback only until the callout returns.

}

@defform[(define-ffi2-procedure id arrow-type
           option ...)
         #:grammar ([option (code:line #:lib lib-expr)
                            (code:line #:c-id c-id)
                            (code:line #:fail fail-expr)
                            (code:line #:wrap wrap-expr)])]{

A convenience form for binding @racket[id] as a @tech{foreign
callout}. The @racket[#:lib] option must be provided, and an exported
object with name @racket[c-id] is found in the library produced by
@racket[lib-expr]. If @racket[c-id] is not declared using
@racket[#:c-id], then @racket[id] is used.

If @racket[c-id] (or @racket[id]) is not exported from
@racket[ffi-lib], then if @racket[fail-expr] is called with
@racket['#,(racket id)] as its argument, and @racket[id] is defined
as that result instead of a callout based on @racket[arrow-type].

If @racket[wrap-expr] is provided, then the value that otherwise would
be bound to @racket[id] is passed to the result of @racket[wrap-expr],
and @racket[id] is defined as the result of that call.

}

@defform[(define-ffi2-definer id
           option ...)
         #:grammar ([option (code:line #:lib lib-expr)
                            (code:line #:default-fail fail-expr)
                            (code:line #:default-wrap wrap-expr)
                            (code:line #:provide)])]{

Defines @racket[id] as a definition form with the same syntax as
@racket[define-ffi2-procedure], except that a definition using
@racket[id] cannot have a @racket[#:lib] clause.

If @racket[#:default-fail] or @racket[#:default-wrap] are provided,
they supply expressions that are used as @racket[#:fail] or
@racket[#:wrap] defaults for @racket[id].

If @racket[#:provide] is specified, then a name defined by @racket[id]
is also exported using @racket[provide] with @racket[protect-out].

}

@defproc[(make-not-available [name symbol?]) procedure?]{

Returns a procedure that takes any number of arguments, including
keyword arguments, and reports an error message from @racket[name].
This function is intended for using with @racket[#:fail] in
@racket[define-ffi2-procedure] or @racket[#:default-fail] in
@racket[define-ffi2-definer].

}


@deftogether[(
@defidform[#:kind "ffi2 abi" default_abi]
@defidform[#:kind "ffi2 abi" cdecl_abi]
@defidform[#:kind "ffi2 abi" stdcall_abi]
@defform[(define-ffi2-abi name abi)]
)]{

An @deftech{ABI} specifies a calling convention to use for a foreign
procedure or callback. On most platforms, the only meaningful ABI is
@racket[default_abi], because most platforms have only a single
standard ABI for C procedures. Windows for 32-bit x86 defines multiple
procedure ABIs, and @racket[cdecl_abi] and @racket[stdcall_abi]
specify alternative ABIs for that platform; on other platforms,
@racket[cdecl_abi] and @racket[stdcall_abi] are treated the same as
@racket[default_abi].

Use @racket[define-ffi2-abi] to define an alias for an ABI. The
@racket[system-type-case] form works as an ABI to support a
platform-dependent choice.

}
