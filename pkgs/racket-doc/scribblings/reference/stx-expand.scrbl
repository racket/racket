#lang scribble/doc
@(require "mz.rkt")

@title{Expanding Top-Level Forms}


@defproc[(expand [top-level-form any/c]
                 [insp inspector? (current-code-inspector)])
         syntax?]{

Expands all non-primitive syntax in @racket[top-level-form], and
returns a syntax object for the expanded form that contains only core
forms, matching the grammar specified by @secref["fully-expanded"].

Before @racket[top-level-form] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], just as for
@racket[eval]. Use @racket[syntax->datum] to convert the returned
syntax object into a printable datum.

If @racket[insp] is not the original @tech{code inspector} (i.e., the
value of @racket[(current-code-inspector)] when Racket starts), then
the result syntax object is @tech{tainted}.

Here's an example of using @racket[expand] on a module:

@racketblock[
(parameterize ([current-namespace (make-base-namespace)])
 (expand
  (datum->syntax
   #f
   '(module foo scheme
      (define a 3)
      (+ a 4)))))]

Here's an example of using @racket[expand] on a non-top-level form:

@racketblock[
(define-namespace-anchor anchor)
(parameterize ([current-namespace
                (namespace-anchor->namespace anchor)])
 (expand
  (datum->syntax
   #f
   '(delay (+ 1 2)))))]

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}


@defproc[(expand-syntax [stx syntax?]
                        [insp inspector? (current-code-inspector)])
         syntax?]{

Like @racket[(expand stx insp)], except that the argument must be a
@tech{syntax object}, and its lexical context is not enriched before
expansion.

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}


@defproc[(expand-once [top-level-form any/c]
                      [insp inspector? (current-code-inspector)])
         syntax?]{

Partially expands @racket[top-level-form] and returns a syntax object
for the partially-expanded expression. Due to limitations in the
expansion mechanism, some context information may be lost. In
particular, calling @racket[expand-once] on the result may produce a
result that is different from expansion via @racket[expand].  

Before @racket[top-level-form] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], as for
@racket[eval].

The @racket[insp] argument determines whether the result is
@tech{tainted}, the same as for @racket[expand].

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}


@defproc[(expand-syntax-once [stx syntax?]
                             [insp inspector? (current-code-inspector)])
         syntax?]{

Like @racket[(expand-once stx)], except that the argument
must be a @tech{syntax object}, and its lexical context is not
enriched before expansion.

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}


@defproc[(expand-to-top-form [top-level-form any/c]
                             [insp inspector? (current-code-inspector)])
         syntax?]{

Partially expands @racket[top-level-form] to reveal the outermost
syntactic form. This partial expansion is mainly useful for detecting
top-level uses of @racket[begin]. Unlike the result of
@racket[expand-once], expanding the result of
@racket[expand-to-top-form] with @racket[expand] produces the same
result as using @racket[expand] on the original syntax.

Before @racket[stx-or-sexpr] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], as for
@racket[eval].

The @racket[insp] argument determines whether the result is
@tech{tainted}, the same as for @racket[expand].

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}


@defproc[(expand-syntax-to-top-form [stx syntax?]
                                    [insp inspector? (current-code-inspector)])
         syntax?]{

Like @racket[(expand-to-top-form stx)], except that the argument must
be a @tech{syntax object}, and its lexical context is not enriched
before expansion.

@history[#:changed "8.2.0.4" @elem{Added the @racket[insp] argument and tainting.}]}

@;------------------------------------------------------------------------
@section[#:tag "modinfo"]{Information on Expanded Modules}

Information for an expanded @racket[module] declaration is stored in a
set of @tech{syntax properties} (see @secref["stxprops"]) attached
to the syntax object:

@itemize[

 @item{@indexed-racket['module-body-context] --- a syntax
 object whose @tech{lexical information} corresponds to the inside of
 the module, so it includes the expansion's @tech{outside-edge scope}
 and its @tech{inside-edge scope}; that is, the syntax object
 simulates an identifier that is present in the original module body
 and inaccessible to manipulation by any macro, so that its lexical
 information includes bindings for the module's imports and
 definitions.

 @history[#:added "6.4.0.1"]}

 @item{@indexed-racket['module-body-inside-context] --- a syntax
 object whose @tech{lexical information} corresponds to an identifier
 that starts with no lexical context and is moved into the macro, so
 that it includes only the expansions's @tech{inside-edge scope}.

 @history[#:added "6.4.0.1"]}

 @item{@indexed-racket['module-body-context-simple?] --- a boolean,
 where @racket[#t] indicates that the bindings of the module's body
 (as recorded in the @tech{lexical information} of the value of the
 @racket['module-body-inside-context] property) can be directly
 reconstructed from modules directly imported into the module,
 including imported for-syntax, for-meta, and for-template.

 @history[#:added "6.4.0.1"]}

]

@history[#:changed "7.0" @elem{Removed @racket['module-variable-provides],
                               @racket['module-syntax-provides],
                               @racket['module-indirect-provides],
                               and @racket['module-indirect-for-meta-provides]
                               properties.}]
