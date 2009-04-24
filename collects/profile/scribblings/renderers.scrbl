#lang scribble/doc

@(require scribble/manual
          (for-label scheme
                     profile/analyzer
                     (prefix-in text: profile/render-text)
                     (prefix-in graphviz: profile/render-graphviz)))

@title[#:tag "renderers"]{Profile Renderers}

After collecting the profile samples and analyzing the data, the last
aspect of profiling is to render the results.  The profile collection
provides several renderers, each providing a rendering function that
consumes a @scheme[profile] instance.  See the
@seclink["analyzer"]{analyzer} section for a description of the
@scheme[profile] struct if you want to implement your own renderer.

@;--------------------------------------------------------------------
@section{Textual Rendering}

@defmodule[profile/render-text]

@defproc[(render
          [profile-data profile?]
          [#:truncate-source truncate-source exact-nonnegative-integer? 50]
          [#:hide-self hide-self% (between/c 0 1) 1/100]
          [#:hide-subs hide-subs% (between/c 0 1) 2/100])
         void?]{

Prints the given @scheme[profile] results as a textual table.

The printout begins with some general facts about the profile, and
then a table that represents the call-graph is printed.  Each row in
this table looks like:

@verbatim[#:indent 2]{
                          B [M1] M2%
  [N1] N2(N3%) N4(N5%)  A ...path/to/source.ss:12:34
                          C [M3] M4%}

Where actual numbers appear in the printout.  The meaning of the
numbers and labels is as follows:
@itemize[
@item{@tt{A} --- the name of the function that this node represents,
  followed by the source location for the function if it is known.
  The name can be ``???'' for functions with no identifier, but in
  this case the source location will identify them.}
@item{@tt{N1} --- an index number associated with this node.  This is
  important in references to this function, since the symbolic names
  are not unique (and some can be missing).  The number itself has no
  significance, it simply goes from 1 up.}
@item{@tt{N2} --- the time (in milliseconds) that this function has
  been anywhere in a stack snapshot.  This is the total time that the
  execution was somewhere in this function or in its callees.
  (Corresponds to the @scheme[node-total] field.)}
@item{@tt{N3} --- this is the percentage of the node's total time
  (@tt{N2}) from the total observed time of the profile.  An entry
  with a 100% refers to a function that was active throughout the
  whole execution.}
@item{@tt{N4} --- the time (in milliseconds) that this function has
  been at the top of the stack snapshot.  This is the time that this
  function consumed doing work itself rather than calling other
  functions.  (Corresponds to the @scheme[node-self] field.)}
@item{@tt{N5} --- this is the percentage of @tt{N4} out of the total
  observed time of the profile.  Functions with high values here can
  be good candidates for optimization, But, of course, they can
  represent doing real work due to one of its callers that need to be
  optimized.}
@item{@tt{B} and @tt{C} --- these are labels for the callers and
  callees of the function.  Any number of callers and callees can
  appear here (including 0).  The function itself can also appear in
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

  These values correspond to the @scheme[edge-caller-time] and
  @scheme[edge-callee-time] fields; see the documentation for further
  details.}
]

The function has a few keyword arguments to customize its output:
@itemize[

@item{The @scheme[truncate-source] argument determines the length that
  the source string should take (together with its label).}

@item{@scheme[hide-self%] and @scheme[hide-subs%] control hiding some
  of the nodes.  A node is hidden if its self time (@tt{N3} in the
  above example) is smaller than @scheme[hide-self%] @emph{and} if all
  places where it occurs as a caller or a callee have percentages that
  are smaller than @scheme[hide-subs%].  The reason for requiring both
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

Prints the given @scheme[profile] results as a Graphviz directed
graph.

This is an experimental module, provided mostly as a proof-of-concept.
It renders the profile's call-graph as a graph representation for one
of the Graphviz tools to render.  Nodes are colored according to their
`self' percentages, and edges.

The keyword arguments control hiding nodes in the same way as with the
textual renderer.}
