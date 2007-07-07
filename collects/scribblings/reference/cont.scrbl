#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:cont"]{Continuations}

See @secref["mz:cont-model"] and @secref["mz:prompt-model"] for
general information about continuations. PLT Scheme's support for
prompts and composable continuations most closely resembles Dorai
Sitaram's @scheme[\%] and @scheme[fcontrol] operator @cite[#:key
"cite:fcontrol" #:title "Handling Control" #:author "Dorai Sitaram"
#:location "Programming Language Design and Implementation" #:date
1993].

Scheme installs a @defterm{continuation barrier} around evaluation in
the following contexts, preventing full-continuation jumps across the
barrier:

@itemize{

 @item{applying an exception handler, an error escape handler, or an
 error display handler (see @secref["mz:exns"]);}

 @item{applying a macro transformer (see @secref["mz:stxtrans"]),
 evaluating a compile-time expression, or applying a module name
 resolver (see @secref["mz:modnameresolver"]);}

 @item{applying a custom-port procedure (see @secref["mz:customport"]), an
 event guard procedure (see @secref["mz:sync"]), or a parameter guard
 procedure (see @secref["mz:parameters"]);}

 @item{applying a security-guard procedure (see
 @secref["mz:securityguards"]);}

 @item{applying a will procedure (see @secref["mz:willexecutor"]); or}

 @item{evaluating or loading code from the stand-alone MzScheme
 command line (see @secref["mz:running-sa"]).}

}

In addition, extensions of PLT Scheme may install barriers in
additional contexts. In particular, MrEd installs a continuation
barrier around most every callback. Finally,
@scheme[call-with-continuation-barrier] applies a thunk barrier
between the application and the current continuation.


@defproc[(call-with-continuation-prompt 
          [thunk (-> any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]
          [handler (or/c procedure? false/c) #f])
         any]{

Calls @scheme[thunk] with the current continuation extended by a
prompt. The prompt is tagged by @scheme[prompt-tag], which must be a
result from either @scheme[default-continuation-prompt-tag] (the
default) or @scheme[make-continuation-prompt-tag]. The result of
@scheme[thunk] is the result of the
@scheme[call-with-continuation-prompt] call.

The @scheme[handler] argument specifies a handler procedure to be
called in tail position with respect to the
@scheme[call-with-continuation-prompt] call when the installed prompt
is the target of a @scheme[abort-current-continuation] call with
@scheme[prompt-tag]; the remaining arguments of
@scheme[abort-current-continuation] are supplied to the handler
procedure. If @scheme[handler] is @scheme[#f], the default handler
accepts a single @scheme[abort-thunk] argument and calls
@scheme[(call-with-continuation-prompt abort-thunk prompt-tag #f)];
that is, the default handler re-installs the prompt and continues with
a given thunk.}

@defproc[(abort-current-continuation
          [prompt-tag any/c]
          [v any/c] ...+) 
         any]{

Resets the current continuation to that of the nearest prompt tagged
by @scheme[prompt-tag] in the current continuation; if no such prompt exists,
the @exnraise[exn:fail:contract:continuation]. The @scheme[v]s are delivered
as arguments to the target prompt's handler procedure.

The protocol for @scheme[v]s supplied to an abort is specific to the
@scheme[prompt-tag]. When @scheme[abort-current-continuation] is used with
@scheme[(default-continuation-prompt-tag)], generally a single thunk
should be supplied that is suitable for use with the default prompt
handler. Similarly, when @scheme[call-with-continuation-prompt] is
used with @scheme[(default-continuation-prompt-tag)], the associated
handler should generally accept a single thunk argument.}

@defproc*[([(make-continuation-prompt-tag) continuation-prompt-tag?]
           [(make-continuation-prompt-tag [sym symbol?]) continuation-prompt-tag?])]{

Creates a prompt tag that is not @scheme[equal?] to the result of any
other value (including prior or future results from
@scheme[make-continuation-prompt-tag]). The optional @scheme[sym]
argument, if supplied, is used when printing the prompt tag.}

@defproc[(default-continuation-prompt-tag) continuation-prompt-tag?]{

Returns a constant prompt tag for a which a prompt is installed at the
start of every thread's continuation; the handler for each thread's
initial prompt accepts any number of values and returns. The result of
@scheme[default-continuation-prompt-tag] is the default tag for more
any procedure that accepts a prompt tag.}

@defproc[(call-with-current-continuation 
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

Captures the current continuation up to the nearest prompt tagged by
@scheme[prompt-tag]; if no such prompt exists, the
@exnraise[exn:fail:contract:continuation]. The truncated continuation
includes only continuation marks and @scheme[dynamic-wind] frames
installed since the prompt. 

The capture continuation is delivered to @scheme[proc], which is
called in tail position with respect to the
@scheme[call-with-current-continuation] call.

If the continuation argument to @scheme[proc] is ever applied, then it
removes the portion of the current continuation up to the nearest
prompt tagged by @scheme[prompt-tag] (not including the prompt; if no
such prompt exists, the @exnraise[exn:fail:contract:continuation]), or
up to the nearest continuation frame (if any) shared by the current
and captured continuations---whichever is first. While removing
continuation frames, @scheme[dynamic-wind] @scheme[post-thunk]s are
executed. Finally, the (unshared portion of the) captured continuation
is appended to the remaining continuation, applying
@scheme[dynamic-wind] @scheme[pre-thunk]s.

The arguments supplied to an applied procedure become the result
values for the restored continuation. In particular, if multiple
arguments are supplied, then the continuation receives multiple
results.

If, at application time, a continuation barrier appears between the
current continuation and the prompt tagged with @scheme[prompt-tag],
and if the same barrier is not part of the captured continuation, then
the @exnraise[exn:fail:contract:continuation].

A continuation can be invoked from the thread (see
@secref["mz:threads"]) other than the one where it was captured.}

@defproc[(call/cc
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

The @scheme[call/cc] binding is an alias for @scheme[call-with-current-continuation].
}

@defproc[(call-with-composable-continuation 
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

Similar to @scheme[call-with-current-continuation], but applying
the resulting continuation procedure does not remove any portion of
the current continuation. Instead, application always extends the
current continuation with the captured continuation (without
installing any prompts other than those be captured in the
continuation). When @scheme[call-with-composable-continuation] is
called, if a continuation barrier appears in the continuation before
the closest prompt tagged by @scheme[prompt-tag], the
@exnraise[exn:fail:contract:continuation].}

@defproc[(call-with-escape-continuation 
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

Like @scheme[call-with-current-continuation], but @scheme[proc] is not
called in tail position, and the continuation procedure supplied to
@scheme[proc] can only be called during the dynamic extent of the
@scheme[call-with-escape-continuation] call. A continuation barrier,
however, never prevents the application of the continuation.

Due to the limited applicability of its continuation,
@scheme[call-with-escape-continuation] can be implemented more efficiently
than @scheme[call-with-current-continuation].

A continuation obtained from @scheme[call-with-escape-continuation] is
actually a kind of prompt. Escape continuations are provided mainly
for backward compatibility, since they pre-date general prompts in
MzScheme, and because @scheme[call/ec] is often an easy replacement
for @scheme[call/cc] to improve performance.}

@defproc[(call/ec
          [proc (continuation? . -> . any)]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) 
         any]{

The @scheme[call/ec] binding is an alias for @scheme[call-with-escape-continuation].
}

@defform[(let/cc k body ...+)]{
Equivalent to @scheme[(call/cc (lambda (k) body ...))].
}

@defform[(let/ec k body ...+)]{
Equivalent to @scheme[(call/ec (lambda (k) body ...))].
}

@defproc[(call-with-continuation-barrier [thunk (-> any)]) any]{

Applies @scheme[thunk] with a barrier between the application and the
current continuation. The results of @scheme[thunk] are the results of
the @scheme[call-with-continuation-barrier] call.}


@defproc[(continuation-prompt-available?
          [prompt-tag continuation-prompt-tag?]
          [cont continuation? (call/cc values)]) 
         any]{

Returns @scheme[#t] if @scheme[cont], which must be a continuation,
includes a prompt tagged by @scheme[prompt-tag], @scheme[#f]
otherwise.
}

@defproc[(continuation? [v any/c]) boolean?]{ Return @scheme[#t] if
@scheme[v] is a continuation as produced by
@scheme[call-with-current-continuation],
@scheme[call-with-composable-continuation], or
@scheme[call-with-escape-continuation], @scheme[#f] otherwise.}

@defproc[(continuation-prompt-tag? [v any/c]) boolean?]{
Returns @scheme[#t] if @scheme[v] is a continuation prompt tag as produced by
@scheme[default-continuation-prompt-tag] or @scheme[make-continuation-prompt-tag].}

@defproc[(dynamic-wind [pre-thunk (-> any)]
                       [value-thunk (-> any)]
                       [post-thunk (-> any)]) 
          any]{

Applies its three thunk arguments in order.  The value of a
@scheme[dynamic-wind] expression is the value returned by
@scheme[value-thunk]. The @scheme[pre-thunk] procedure is invoked
before calling @scheme[value-thunk] and @scheme[post-thunk] is invoked
after @scheme[value-thunk] returns. The special properties of
@scheme[dynamic-wind] are manifest when control jumps into or out of
the @scheme[value-thunk] application (either due to a prompt abort or
a continuation invocation): every time control jumps into the
@scheme[value-thunk] application, @scheme[pre-thunk] is invoked, and
every time control jumps out of @scheme[value-thunk],
@scheme[post-thunk] is invoked. (No special handling is performed for
jumps into or out of the @scheme[pre-thunk] and @scheme[post-thunk]
applications.)

When @scheme[dynamic-wind] calls @scheme[pre-thunk] for normal
evaluation of @scheme[value-thunk], the continuation of the
@scheme[pre-thunk] application calls @scheme[value-thunk] (with
@scheme[dynamic-wind]'s special jump handling) and then
@scheme[post-thunk].  Similarly, the continuation of the
@scheme[post-thunk] application returns the value of the preceding
@scheme[value-thunk] application to the continuation of the entire
@scheme[dynamic-wind] application.

When @scheme[pre-thunk] is called due to a continuation jump, the
continuation of @scheme[pre-thunk]

@itemize{

 @item{jumps to a more deeply nested @scheme[pre-thunk], if any, or jumps
       to the destination continuation; then}

 @item{continues with the context of the @scheme[pre-thunk]'s
       @scheme[dynamic-wind] call.}

}

Normally, the second part of this continuation is never reached, due
to a jump in the first part. However, the second part is relevant
because it enables jumps to escape continuations that are contained in
the context of the @scheme[dynamic-wind] call. Furthermore, it means
that the continuation marks (see @secref["mz:contmarks"]) and
parameterization (see @secref["mz:parameters"]) for @scheme[pre-thunk]
correspond to those of the @scheme[dynamic-wind] call that installed
@scheme[pre-thunk]. The @scheme[pre-thunk] call, however, is
@scheme[parameterize-break]ed to disable breaks (see also
@secref["mz:breakhandler"]).

Similarly, when @scheme[post-thunk] is called due to a continuation
jump, the continuation of @scheme[post-thunk] jumps to a less deeply
nested @scheme[post-thunk], if any, or jumps to a @scheme[pre-thunk]
protecting the destination, if any, or jumps to the destination
continuation, then continues from the @scheme[post-thunk]'s
@scheme[dynamic-wind] application. As for @scheme[pre-thunk], the
parameterization of the original @scheme[dynamic-wind] call is
restored for the call, and the call is @scheme[parameterize-break]ed
to disable breaks.

In both cases, the target for a jump is recomputed after each
@scheme[pre-thunk] or @scheme[post-thunk] completes. When a
prompt-delimited continuation (see @secref["mz:prompt-model"]) is
captured in a @scheme[post-thunk], it might be delimited and
instantiated in such a way that the target of a jump turns out to be
different when the continuation is applied than when the continuation
was captured. There may even be no appropriate target, if a relevant
prompt or escape continuation is not in the continuation after the
restore; in that case, the first step in a @scheme[pre-thunk] or
@scheme[post-thunk]'s continuation can raise an exception.

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
