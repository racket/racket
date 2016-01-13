#lang scribble/doc
@(require "mz.rkt")

@title{Expanding Top-Level Forms}


@defproc[(expand [top-level-form any/c]) syntax?]{

Expands all non-primitive syntax in @racket[top-level-form], and
returns a syntax object for the expanded form that contains only core
forms, matching the grammar specified by @secref["fully-expanded"].

Before @racket[top-level-form] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], just as for
@racket[eval]. Use @racket[syntax->datum] to convert the returned
syntax object into a printable datum.}

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

@defproc[(expand-syntax [stx syntax?]) syntax?]{

Like @racket[(expand stx)], except that the argument must be a
@tech{syntax object}, and its lexical context is not enriched before
expansion.}


@defproc[(expand-once [top-level-form any/c]) syntax?]{

Partially expands @racket[top-level-form] and returns a syntax object
for the partially-expanded expression. Due to limitations in the
expansion mechanism, some context information may be lost. In
particular, calling @racket[expand-once] on the result may produce a
result that is different from expansion via @racket[expand].  

Before @racket[top-level-form] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], as for
@racket[eval].}


@defproc[(expand-syntax-once [stx syntax?]) syntax?]{

Like @racket[(expand-once stx)], except that the argument
must be a @tech{syntax object}, and its lexical context is not
enriched before expansion.}


@defproc[(expand-to-top-form [top-level-form any/c]) syntax?]{

Partially expands @racket[top-level-form] to reveal the outermost
syntactic form. This partial expansion is mainly useful for detecting
top-level uses of @racket[begin]. Unlike the result of
@racket[expand-once], expanding the result of
@racket[expand-to-top-form] with @racket[expand] produces the same
result as using @racket[expand] on the original syntax.

Before @racket[stx-or-sexpr] is expanded, its lexical context is
enriched with @racket[namespace-syntax-introduce], as for
@racket[eval].}


@defproc[(expand-syntax-to-top-form [stx syntax?]) syntax?]{

Like @racket[(expand-to-top-form stx)], except that the argument must
be a @tech{syntax object}, and its lexical context is not enriched
before expansion.}

@;------------------------------------------------------------------------
@section[#:tag "modinfo"]{Information on Expanded Modules}

Information for an expanded @racket[module] declaration is stored in a
set of @tech{syntax properties} (see @secref["stxprops"]) attached
to the syntax object:

@itemize[

 @item{@indexed-racket['module-direct-requires] --- a list of
 @tech{module path index}es (or symbols) representing the modules
 explicitly imported into the module.}

 @item{@indexed-racket['module-direct-for-syntax-requires] --- a list
 of @tech{module path index}es (or symbols) representing the modules
 explicitly for-syntax imported into the module.}

 @item{@indexed-racket['module-direct-for-template-requires] --- a
 list of @tech{module path index}es (or symbols) representing the
 modules explicitly for-template imported into the module.}

 @item{@indexed-racket['module-direct-for-meta-requires] --- a list of
 lists: each list is an integer or @racket[#f] representing a
 @tech{phase level} followed by a list of @tech{module path index}es
 (or symbols) representing the modules explicitly imported into the
 module at the corresponding phase.

 @history[#:added "6.4.0.1"]}

 @item{@indexed-racket['module-variable-provides] --- a list of
 provided items, where each item is one of the following:
 
  @itemize[

  @item{@racket[symbol] --- represents a locally defined variable that
  is provided with its defined name.}

  @item{@racket[(cons _provided-sym _defined-sym)] --- represents a
  locally defined variable that is provided with renaming; the first
  symbol is the exported name, and the second symbol is the defined
  name.}

  @item{@racket[(list* module-path-index _provided-sym _defined-sym)]
  --- represents a re-exported and possibly re-named variable from the
  specified module; @racket[module-path-index] is either a
  @tech{module path index} or symbol (see @secref["modpathidx"]),
  indicating the source module for the binding. The
  @racket[_provided-sym] is the external name for the re-export, and
  @racket[_defined-sym] is the originally defined name in the module
  specified by @racket[module-path-index].}

  ]}

 @item{@indexed-racket['module-syntax-provides] --- like
 @racket['module-variable-provides], but for syntax exports instead of
 variable exports.}

 @item{@indexed-racket['module-indirect-provides] --- a list of symbols for
 variables that are defined in the module but not exported; they may
 be exported indirectly through macro expansions.  Definitions of
 macro-generated identifiers create uninterned symbols in this list.}

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
 reconstructed from the values of @racket['module-direct-requires],
 @racket['module-direct-for-syntax-requires],
 @racket['module-direct-for-template-requires], and
 @racket['module-direct-for-meta-requires].

 @history[#:added "6.4.0.1"]}


]

