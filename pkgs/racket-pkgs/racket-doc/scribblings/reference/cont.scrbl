#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "cont"]{Continuations}

@guideintro["conts"]{continuations}

See @secref["cont-model"] and @secref["prompt-model"] for general
information about continuations. Racket's support for prompts and
composable continuations most closely resembles Dorai Sitaram's
@racket[%] and @racket[fcontrol] operator @cite["Sitaram93"].

Racket installs a @tech{continuation barrier} around evaluation in the
following contexts, preventing full-continuation jumps into the
evaluation context protected by the barrier:

@itemize[

 @item{applying an exception handler, an error escape handler, or an
 error display handler (see @secref["exns"]);}

 @item{applying a macro transformer (see @secref["stxtrans"]),
 evaluating a compile-time expression, or applying a module name
 resolver (see @secref["modnameresolver"]);}

 @item{applying a custom-port procedure (see @secref["customport"]), an
 event guard procedure (see @secref["sync"]), or a parameter guard
 procedure (see @secref["parameters"]);}

 @item{applying a security-guard procedure (see
 @secref["securityguards"]);}

 @item{applying a will procedure (see @secref["willexecutor"]); or}

 @item{evaluating or loading code from the stand-alone Racket
 command line (see @secref["running-sa"]).}

]

In addition, extensions of Racket may install barriers in
additional contexts. Finally,
@racket[call-with-continuation-barrier] applies a thunk barrier
between the application and the current continuation.


@defproc[(call-with-continuation-prompt 
          [proc procedure?]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]
          [handler (or/c procedure? #f) #f]
          [arg any/c] ...)
         any]{

Applies @racket[proc] to the given @racket[arg]s with the current
continuation extended by a prompt. The prompt is tagged by
@racket[prompt-tag], which must be a result from either
@racket[default-continuation-prompt-tag] (the default) or
@racket[make-continuation-prompt-tag]. The result of @racket[proc] is
the result of the @racket[call-with-continuation-prompt] call.

The @racket[handler] argument specifies a handler procedure to be
called in tail position with respect to the
@racket[call-with-continuation-prompt] call when the installed prompt
is the target of an @racket[abort-current-continuation] call with
@racket[prompt-tag]; the remaining arguments of
@racket[abort-current-continuation] are supplied to the handler
procedure. If @racket[handler] is @racket[#f], the default handler
accepts a single @racket[_abort-thunk] argument and calls
@racket[(call-with-continuation-prompt _abort-thunk prompt-tag #f)];
that is, the default handler re-installs the prompt and continues with
a given thunk.}

@defproc[(abort-current-continuation
          [prompt-tag any/c]
          [v any/c] ...)
         any]{

Resets the current continuation to that of the nearest prompt tagged
by @racket[prompt-tag] in the current continuation; if no such prompt exists,
the @exnraise[exn:fail:contract:continuation]. The @racket[v]s are delivered
as arguments to the target prompt's handler procedure.

The protocol for @racket[v]s supplied to an abort is specific to the
@racket[prompt-tag]. When @racket[abort-current-continuation] is used with
@racket[(default-continuation-prompt-tag)], generally, a single thunk
should be supplied that is suitable for use with the default prompt
handler. Similarly, when @racket[call-with-continuation-prompt] is
used with @racket[(default-continuation-prompt-tag)], the associated
handler should generally accept a single thunk argument.

Each @tech{thread}'s continuation starts with a prompt for
@racket[(default-continuation-prompt-tag)] that uses the default
handler, which accepts a single thunk to apply (with the prompt
intact).}

@defproc*[([(make-continuation-prompt-tag) continuation-prompt-tag?]
           [(make-continuation-prompt-tag [sym symbol?]) continuation-prompt-tag?])]{

Creates a prompt tag that is not @racket[equal?] to the result of any
other value (including prior or future results from
@racket[make-continuation-prompt-tag]). The optional @racket[sym]
argument, if supplied, is used when printing the prompt tag.}

@defproc[(default-continuation-prompt-tag) continuation-prompt-tag?]{

Returns a constant prompt tag for which a prompt is installed at the
start of every thread's continuation; the handler for each thread's
initial prompt accepts any number of values and returns. The result of
@racket[default-continuation-prompt-tag] is the default tag for
any procedure that accepts a prompt tag.}

@defproc[(call-with-current-continuation 
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

Captures the current continuation up to the nearest prompt tagged by
@racket[prompt-tag]; if no such prompt exists, the
@exnraise[exn:fail:contract:continuation]. The truncated continuation
includes only continuation marks and @racket[dynamic-wind] frames
installed since the prompt. 

The capture continuation is delivered to @racket[proc], which is
called in tail position with respect to the
@racket[call-with-current-continuation] call.

If the continuation argument to @racket[proc] is ever applied, then it
removes the portion of the current continuation up to the nearest
prompt tagged by @racket[prompt-tag] (not including the prompt; if no
such prompt exists, the @exnraise[exn:fail:contract:continuation]), or
up to the nearest continuation frame (if any) shared by the current
and captured continuations---whichever is first. While removing
continuation frames, @racket[dynamic-wind] @racket[_post-thunk]s are
executed. Finally, the (unshared portion of the) captured continuation
is appended to the remaining continuation, applying
@racket[dynamic-wind] @racket[_pre-thunk]s.

The arguments supplied to an applied procedure become the result
values for the restored continuation. In particular, if multiple
arguments are supplied, then the continuation receives multiple
results.

If, at application time, a @tech{continuation barrier} would be
introduced by replacing the current continuation with the applied one,
then the @exnraise[exn:fail:contract:continuation].

A continuation can be invoked from the thread (see
@secref["threads"]) other than the one where it was captured.}

@defproc[(call/cc
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

The @racket[call/cc] binding is an alias for @racket[call-with-current-continuation].
}

@defproc[(call-with-composable-continuation 
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

Similar to @racket[call-with-current-continuation], but applying
the resulting continuation procedure does not remove any portion of
the current continuation. Instead, application always extends the
current continuation with the captured continuation (without
installing any prompts other than those captured in the
continuation).

When @racket[call-with-composable-continuation] is called, if a
continuation barrier appears in the continuation before the closest
prompt tagged by @racket[prompt-tag], the
@exnraise[exn:fail:contract:continuation] (because attempting to apply
the continuation would always fail).}

@defproc[(call-with-escape-continuation 
          [proc (continuation? . -> . any)]) 
         any]{

Like @racket[call-with-current-continuation], but @racket[proc] is not
called in tail position, and the continuation procedure supplied to
@racket[proc] can only be called during the dynamic extent of the
@racket[call-with-escape-continuation] call.

Due to the limited applicability of its continuation,
@racket[call-with-escape-continuation] can be implemented more efficiently
than @racket[call-with-current-continuation].

A continuation obtained from @racket[call-with-escape-continuation] is
actually a kind of prompt. Escape continuations are provided mainly
for backwards compatibility, since they pre-date general prompts in
Racket, and because @racket[call/ec] is often an easy replacement
for @racket[call/cc] to improve performance.}

@defproc[(call/ec
          [proc (continuation? . -> . any)]) 
         any]{

The @racket[call/ec] binding is an alias for @racket[call-with-escape-continuation].
}

@defform[(let/cc k body ...+)]{
Equivalent to @racket[(call/cc (lambda (k) body ...))].
}

@defform[(let/ec k body ...+)]{
Equivalent to @racket[(call/ec (lambda (k) body ...))].
}

@defproc[(call-with-continuation-barrier [thunk (-> any)]) any]{

Applies @racket[thunk] with a @tech{continuation barrier} between the
application and the current continuation. The results of
@racket[thunk] are the results of the
@racket[call-with-continuation-barrier] call.}


@defproc[(continuation-prompt-available?
          [prompt-tag continuation-prompt-tag?]
          [cont continuation? (call/cc values)]) 
         any]{

Returns @racket[#t] if @racket[cont], which must be a continuation,
includes a prompt tagged by @racket[prompt-tag], @racket[#f]
otherwise.
}

@defproc[(continuation? [v any/c]) boolean?]{ Return @racket[#t] if
@racket[v] is a continuation as produced by
@racket[call-with-current-continuation],
@racket[call-with-composable-continuation], or
@racket[call-with-escape-continuation], @racket[#f] otherwise.}

@defproc[(continuation-prompt-tag? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a continuation prompt tag as produced by
@racket[default-continuation-prompt-tag] or @racket[make-continuation-prompt-tag].}

@defproc[(dynamic-wind [pre-thunk (-> any)]
                       [value-thunk (-> any)]
                       [post-thunk (-> any)]) 
          any]{

Applies its three thunk arguments in order.  The value of a
@racket[dynamic-wind] expression is the value returned by
@racket[value-thunk]. The @racket[pre-thunk] procedure is invoked
before calling @racket[value-thunk] and @racket[post-thunk] is invoked
after @racket[value-thunk] returns. The special properties of
@racket[dynamic-wind] are manifest when control jumps into or out of
the @racket[value-thunk] application (either due to a prompt abort or
a continuation invocation): every time control jumps into the
@racket[value-thunk] application, @racket[pre-thunk] is invoked, and
every time control jumps out of @racket[value-thunk],
@racket[post-thunk] is invoked. (No special handling is performed for
jumps into or out of the @racket[pre-thunk] and @racket[post-thunk]
applications.)

When @racket[dynamic-wind] calls @racket[pre-thunk] for normal
evaluation of @racket[value-thunk], the continuation of the
@racket[pre-thunk] application calls @racket[value-thunk] (with
@racket[dynamic-wind]'s special jump handling) and then
@racket[post-thunk].  Similarly, the continuation of the
@racket[post-thunk] application returns the value of the preceding
@racket[value-thunk] application to the continuation of the entire
@racket[dynamic-wind] application.

When @racket[pre-thunk] is called due to a continuation jump, the
continuation of @racket[pre-thunk]

@itemize[

 @item{jumps to a more deeply nested @racket[pre-thunk], if any, or jumps
       to the destination continuation; then}

 @item{continues with the context of the @racket[pre-thunk]'s
       @racket[dynamic-wind] call.}

]

Normally, the second part of this continuation is never reached, due
to a jump in the first part. However, the second part is relevant
because it enables jumps to escape continuations that are contained in
the context of the @racket[dynamic-wind] call. Furthermore, it means
that the continuation marks (see @secref["contmarks"]) and
parameterization (see @secref["parameters"]) for @racket[pre-thunk]
correspond to those of the @racket[dynamic-wind] call that installed
@racket[pre-thunk]. The @racket[pre-thunk] call, however, is
@racket[parameterize-break]ed to disable breaks (see also
@secref["breakhandler"]).

Similarly, when @racket[post-thunk] is called due to a continuation
jump, the continuation of @racket[post-thunk] jumps to a less deeply
nested @racket[post-thunk], if any, or jumps to a @racket[pre-thunk]
protecting the destination, if any, or jumps to the destination
continuation, then continues from the @racket[post-thunk]'s
@racket[dynamic-wind] application. As for @racket[pre-thunk], the
parameterization of the original @racket[dynamic-wind] call is
restored for the call, and the call is @racket[parameterize-break]ed
to disable breaks.

In both cases, the target for a jump is recomputed after each
@racket[pre-thunk] or @racket[post-thunk] completes. When a
prompt-delimited continuation (see @secref["prompt-model"]) is
captured in a @racket[post-thunk], it might be delimited and
instantiated in such a way that the target of a jump turns out to be
different when the continuation is applied than when the continuation
was captured. There may even be no appropriate target, if a relevant
prompt or escape continuation is not in the continuation after the
restore; in that case, the first step in a @racket[pre-thunk] or
@racket[post-thunk]'s continuation can raise an exception.

@examples[
(let ([v (let/ec out 
           (dynamic-wind
            (lambda () (display "in ")) 
            (lambda () 
              (display "pre ") 
              (display (call/cc out))
              #f) 
            (lambda () (display "out "))))])  
  (when v (v "post "))) 

(let/ec k0
  (let/ec k1
    (dynamic-wind
     void
     (lambda () (k0 'cancel))
     (lambda () (k1 'cancel-canceled)))))

(let* ([x (make-parameter 0)]
       [l null]
       [add (lambda (a b)
              (set! l (append l (list (cons a b)))))])
  (let ([k (parameterize ([x 5])
             (dynamic-wind
                 (lambda () (add 1 (x)))
                 (lambda () (parameterize ([x 6])
                              (let ([k+e (let/cc k (cons k void))])
                                (add 2 (x))
                                ((cdr k+e))
                                (car k+e))))
                 (lambda () (add 3 (x)))))])
    (parameterize ([x 7])
      (let/cc esc
        (k (cons void esc)))))
  l)
]}

@; ----------------------------------------------------------------------

@include-section["control-lib.scrbl"]
