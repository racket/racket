#lang scribble/doc

@(require scribble/manual
          (for-label scheme profile/analyzer))

@title[#:tag "analyzer"]{Analyzing Profile Data}

@defmodule[profile/analyzer]

Once a profile run is done, and the results are collected, the next
step is to analyze the data.  In this step the sample time are
computed and summed, a call-graph representing the observed function
calls is built, and per-node and per-edge information is created.
This is the job of the main function provided by
@scheme[profile/analyzer].

@defproc[(analyze-samples [raw-sample-data any/c])
         profile?]{

This function consumes the raw result of the
@seclink["sampler"]{sampler} (which is given in an undocumented form),
analyzes it, and returns a @scheme[profile] value holding the analyzed
results.  Without this function, the results of the sampler are
meaningless.}


@defstruct[profile ([total-time exact-nonnegative-integer?]
                    [cpu-time   exact-nonnegative-integer?]
                    [sample-number exact-nonnegative-integer?]
                    [thread-times  (listof (cons exact-nonnegative-integer?
                                                 exact-nonnegative-integer?))]
                    [nodes  (listof node?)]
                    [*-node node?])]{

Represents the analyzed profile result.

@itemize[

@item{@scheme[total-time] is the total observed time (in milliseconds)
  included in the profile.  This is different than the actual time the
  profiling took, due to unaccounted-for time spent in untracked
  threads.  (E.g., the sampler thread itself.)}

@item{@scheme[cpu-time] is the actual cpu time consumed by the process
  during the profiler's work.}

@item{@scheme[sample-number] holds the number of samples taken during
  the profile.  This can be used to compute the average time frame
  each of the input samples represented.}

@item{@scheme[thread-times] holds an association list mapping thread
  identifiers to cpu time for the corresponding threads.  As samples
  are collected, each thread that is observed is assigned a small
  integer identifier.  These identifers are listed for each function
  call, and the total time spent in each thread is in this field.}

@item{@scheme[nodes] is a list of nodes representing all observed
  functions.  These nodes are the components of the call-graph that
  the analyzer assembles (see the @scheme[edge] field).  The nodes are
  sorted by a topological top-to-bottom sort, and by decreasing total
  amount of time (time spent either in the function or in its callees)
  as a secondary key.}

@item{@scheme[*-node] holds a ``special'' node value that is
  constructed for every graph.  This node is used as the caller for
  all top-level function nodes and as the callee for all leaf nodes.
  It can therefore be used to start a scan of the call graph.  In
  addition, the times associated with it's "callers and callees"
  actually represent the time these functions spent being the root of
  the computation or its leaf.  (This can be different from a node's
  ``self'' time, since it is divided by the number of instances a
  function had on the stack for every sample --- so for recursive
  functions this value is different from.)}

]}


@defstruct[node ([id      (or/c #f symbol?)]
                 [src     (or/c #f srcloc?)]
                 [thread-ids (listof exact-nonnegative-integer?)]
                 [total   exact-nonnegative-integer?]
                 [self    exact-nonnegative-integer?]
                 [callers (listof edge?)]
                 [callees (listof edge?)])]{

Represents a function call node in the call graph of an analyzed
profile result.

@itemize[

@item{The @scheme[id] and @scheme[src] field hold a symbol naming the
  function and/or its source location as a @scheme[srcloc] value.
  This is the same as the results of
  @scheme[continuation-mark-set->context], so at most of of these can
  be @scheme[#f], except for the special @scheme[*-node] (see the
  @scheme[profile] struct) that can be identified by both of these
  being @scheme[#f].}

@item{@scheme[thread-ids] holds a list of thread identifiers that were
  observed executing this function.}

@item{@scheme[total] holds the total time (in milliseconds) where this
  function was anywhere on the stack.  It is common to see a few
  toplevel functions that have close to a 100% total time, but
  otherwise small @scheme[self] times --- these functions are the ones
  that derive the work that was done, but they don't do any hard work
  directly.}

@item{@scheme[self] holds the total time (in milliseconds) where this
  function was observed as the leaf of the stack.  It represents the
  actual work done by this function, rather than @scheme[total] that
  represents the work done by both the function and its callees.}

@item{@scheme[callers] and @scheme[callees] hold the list of caller
  and callee nodes.  The nodes are not actually held in these lists,
  instead, @scheme[edge] values are used --- and provide information
  specific to an edge in the call-graph.}

]}


@defstruct[edge ([total       exact-nonnegative-integer?]
                 [caller      node?]
                 [caller-time exact-nonnegative-integer?]
                 [callee      node?]
                 [callee-time exact-nonnegative-integer?])]{

Represents an edge between two function call nodes in the call graph
of an analyzed profile result.

@itemize[

@item{@scheme[total] is analogous to the @scheme[total] field of a
  @scheme[node] value: the total time that this edge was anywhere on
  the stack.}

@item{@scheme[caller] and @scheme[callee] hold the two nodes that are
  connected by this edge.}

@item{@scheme[caller-time] and @scheme[callee-time] hold the time
  spent on this edge from the caller's or the callee's perspective.
  These times are different from each other (as well as from the total
  time) because the sums that make them are each divided by the number
  of times the caller or the callee was on the stack.

  To understand this difference, consider a stack snapshot holding
  @tt{A @'rarr B @'rarr B @'rarr B @'rarr A}, and representing a
  second of observed cpu time.  For this sample, the @tt{A @'rarr B}
  edge is charged by a whole second for its total time (the same goes
  for the @tt{A @'rarr A} edge, for example).  Its caller time is
  charged 1/2 second because @tt{A} appears twice in this stack
  snapshot (in the other half, @tt{A} is chared for being a leaf ---
  the caller of the special @scheme[*-node]), and its callee time is
  charged 1/3 respectively.}

]}
