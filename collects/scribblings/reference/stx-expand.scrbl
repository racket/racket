#lang scribble/doc
@(require "mz.ss")

@title{Expanding Top-Level Forms}


@defproc[(expand [top-level-form any/c]) syntax?]{

Expands all non-primitive syntax in @scheme[top-level-form], and
returns a syntax object for the expanded form that contains only core
forms, matching the grammar specified by @secref["fully-expanded"].

Before @scheme[top-level-form] is expanded, its lexical context is
enriched with @scheme[namespace-syntax-introduce], just as for
@scheme[eval]. Use @scheme[syntax->datum] to convert the returned
syntax object into a printable datum.}

Here's an example of using @scheme[expand] on a module:

@schemeblock[
(parameterize ([current-namespace (make-base-namespace)])
 (expand
  (datum->syntax
   #f
   '(module foo scheme
      (define a 3)
      (+ a 4)))))]

Here's an example of using @scheme[expand] on a non-top-level form:

@schemeblock[
(define-namespace-anchor anchor)
(parameterize ([current-namespace
                (namespace-anchor->namespace anchor)])
 (expand
  (datum->syntax
   #f
   '(delay (+ 1 2)))))]

@defproc[(expand-syntax [stx syntax?]) syntax?]{

Like @scheme[(expand stx)], except that the argument must be a
@tech{syntax object}, and its lexical context is not enriched before
expansion.}


@defproc[(expand-once [top-level-form any/c]) syntax?]{

Partially expands @scheme[form-level-form] and returns a syntax object
for the partially-expanded expression. Due to limitations in the
expansion mechanism, some context information may be lost. In
particular, calling @scheme[expand-once] on the result may produce a
result that is different from expansion via @scheme[expand].  

Before @scheme[top-level-form] is expanded, its lexical context is
enriched with @scheme[namespace-syntax-introduce], as for
@scheme[eval].}


@defproc[(expand-syntax-once [stx syntax?]) syntax?]{

Like @scheme[(expand-once stx)], except that the argument
must be a @tech{syntax object}, and its lexical context is not
enriched before expansion.}


@defproc[(expand-to-top-form [top-level-form any/c]) syntax?]{

Partially expands @scheme[top-level-form] to reveal the outermost
syntactic form. This partial expansion is mainly useful for detecting
top-level uses of @scheme[begin]. Unlike the result of
@scheme[expand-once], expanding the result of
@scheme[expand-to-top-form] with @scheme[expand] produces the same
result as using @scheme[expand] on the original syntax.

Before @scheme[stx-or-sexpr] is expanded, its lexical context is
enriched with @scheme[namespace-syntax-introduce], as for
@scheme[eval].}


@defproc[(expand-syntax-to-top-form [stx syntax?]) syntax?]{

Like @scheme[(expand-to-top-form stx)], except that the argument must
be a @tech{syntax object}, and its lexical context is not enriched
before expansion.}

@;------------------------------------------------------------------------
@section[#:tag "modinfo"]{Information on Expanded Modules}

Information for an expanded @scheme[module] declaration is stored in a
set of @tech{syntax properties} (see @secref["stxprops"]) attached
to the syntax object:

@itemize[

 @item{@indexed-scheme['module-direct-requires] --- a list of
 @tech{module path index}es (or symbols) representing the modules
 explicitly imported into the module.}

 @item{@indexed-scheme['module-direct-for-syntax-requires] --- a list
 of @tech{module path index}es (or symbols) representing the modules
 explicitly for-syntax imported into the module.}

 @item{@indexed-scheme['module-direct-for-template-requires] --- a
 list of @tech{module path index}es (or symbols) representing the
 modules explicitly for-template imported into the module.}

 @item{@indexed-scheme['module-variable-provides] --- a list of
 provided items, where each item is one of the following:
 
  @itemize[

  @item{@scheme[symbol] --- represents a locally defined variable that
  is provided with its defined name.}

  @item{@scheme[(cons _provided-sym _defined-sym)] --- represents a
  locally defined variable that is provided with renaming; the first
  symbol is the exported name, and the second symbol is the defined
  name.}

  @item{@scheme[(list* module-path-index _provided-sym _defined-sym)]
  --- represents a re-exported and possibly re-named variable from the
  specified module; @scheme[module-path-index] is either a
  @tech{module path index} or symbol (see @secref["modpathidx"]),
  indicating the source module for the binding. The
  @scheme[_provided-sym] is the external name for the re-export, and
  @scheme[_defined-sym] is the originally defined name in the module
  specified by @scheme[module-path-index].}

  ]}

 @item{@indexed-scheme['module-syntax-provides] --- like
 @scheme['module-variable-provides], but for syntax exports instead of
 variable exports.}

 @item{@indexed-scheme['module-indirect-provides] --- a list of symbols for
 variables that are defined in the module but not exported; they may
 be exported indirectly through macro expansions.  Definitions of
 macro-generated identifiers create uninterned symbols in this list.}

]

