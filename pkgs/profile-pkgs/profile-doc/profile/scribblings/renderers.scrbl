#lang scribble/doc

@(require scribble/manual
          (for-label racket/base
                     racket/contract
                     profile/analyzer
                     (prefix-in text: profile/render-text)
                     (prefix-in graphviz: profile/render-graphviz)))

@title[#:tag "renderers"]{Profile Renderers}

After collecting the profile samples and analyzing the data, the last
step of the profiling process is to render the results.  The profile
collection provides several renderers, each providing a rendering
function that consumes a @racket[profile] instance.  See the
@seclink["analyzer"]{analyzer} section for a description of the
@racket[profile] struct if you want to implement a new renderer.

@;--------------------------------------------------------------------
@section{Textual Rendering}

@defmodule[profile/render-text]

@defproc[(render
          [profile-data profile?]
          [#:truncate-source truncate-source exact-nonnegative-integer? 50]
          [#:hide-self hide-self% (between/c 0 1) 1/100]
          [#:hide-subs hide-subs% (between/c 0 1) 2/100])
         void?]{

Prints the given @racket[profile] results as a textual table.

The printout begins with general information about the profile,
followed by a table with an entry for each node in the call graph.
The entries are displayed in a topological order (roughly, since the
graph can have cycles).  This means that it is usually easy to find
the callers and callees of a function in its close environment.

Each row in the table has the following format:
@verbatim[#:indent 2]{
                          B [M1] M2%
  [N1] N2(N3%) N4(N5%)  A ...path/to/source.rkt:12:34
                          C [M3] M4%}
with the following meaning of the numbers and labels:

@itemize[
@item{@tt{A} --- the name of the function (or a stub of the expression) that
  this node represents, followed by the source location for the function if it
  is known.  The name can be ``???'' for anonymous functions, which will be
  identified with their source location.}
@item{@tt{N1} --- an integer label associated with this node in the
  printout.  This label is used to mark references to this function/expression,
  since symbolic names are not unique (and they can be missing or very
  long).  The labels are assigned from the top.}
@item{@tt{N2} --- the time (in milliseconds) that this function/expression has
  been anywhere in a stack snapshot.  This is the total time that the
  execution was somewhere in this function/expression or in its callees.
  (Corresponds to the @racket[node-total] field.)}
@item{@tt{N3} --- this is the percentage of the node's total time
  (@tt{N2}) from the total observed time of the profile.  An entry
  with a 100% refers to a function/expression that was active throughout
  the whole execution.}
@item{@tt{N4} --- the time (in milliseconds) that this function/expression has
  been at the top of the stack snapshot.  This is the time that this
  function/expression was itself doing work rather than calling other
  functions/expressions. (Corresponds to the @racket[node-self] field.)}
@item{@tt{N5} --- this is the percentage of @tt{N4} out of the total
  observed time of the profile.  Functions/expressions with high values here can
  be good candidates for optimization, But, of course, they can
  represent doing real work for a caller that needs to be optimized.}
@item{@tt{B} and @tt{C} --- these are labels for the callers and
  callees of the function/expression.  Any number of callers and callees can
  appear here (including 0).  The function/expression itself can also appear in
  both places if it is (non-tail) recursive.}
@item{@tt{M1} and @tt{M3} --- the index numbers for @tt{B} and
  @tt{C}.  They can be used to disambiguate functions with the same
  name, as well as a quick way to find the corresponding entry in the
  table.}
@item{@tt{M2} and @tt{M4} --- the percentages of the time @tt{A} spent
  being called by @tt{B} and calling @tt{C}.  These percentages
  represent the time that this edge was found on a stack snapshot,
  divided by the number of occurrences of @tt{A} on the same snapshot.
  The number is the percentage of these times out of @tt{N2}, the
  total time @tt{A} has been active.

  The total percentages for the all caller and for all callees should
  be close to 100% minus the time @tt{A} was the leaf or the root.

  These values correspond to the @racket[edge-caller-time] and
  @racket[edge-callee-time] fields; see the documentation for further
  details.}
]

The function has a few keyword arguments to customize its output:
@itemize[

@item{The @racket[truncate-source] argument determines the length that
  the source string should take (together with its label).}

@item{@racket[hide-self%] and @racket[hide-subs%] control hiding some
  of the nodes.  A node is hidden if its self time (@tt{N3} in the
  above example) is smaller than @racket[hide-self%] @emph{and} if all
  places where it occurs as a caller or a callee have percentages that
  are smaller than @racket[hide-subs%].  The reason for requiring both
  conditions is to avoid having ``dangling references'' to hidden
  nodes.}

]}


@;--------------------------------------------------------------------
@section{Graph Rendering}

@defmodule[profile/render-graphviz]

@defproc[(render
          [profile-data profile?]
          [#:hide-self hide-self% (between/c 0 1) 1/100]
          [#:hide-subs hide-subs% (between/c 0 1) 2/100])
         void?]{

Prints the given @racket[profile] results as a Graphviz directed
graph.

This is an experimental module, provided mostly as a proof-of-concept.
It renders the profile's call-graph as a graph representation for one
of the Graphviz tools to render.  Nodes are colored according to their
`self' percentages, and edges.

The keyword arguments control hiding nodes in the same way as with the
textual renderer.}
