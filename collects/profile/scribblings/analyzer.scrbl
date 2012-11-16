#lang scribble/doc

@(require scribble/manual
          (for-label racket/base racket/contract profile/analyzer))

@title[#:tag "analyzer"]{Analyzing Profile Data}

@defmodule[profile/analyzer]

Once a profile run is done and the results are collected, the next
step is to analyze the data.  In this step sample times are computed
and summed, a call-graph representing observed function calls is
built, and per-node and per-edge information is created.  This is the
job of the main function provided by @racket[profile/analyzer].

@defproc[(analyze-samples [raw-sample-data any/c])
         profile?]{

This function consumes the raw result of the
@seclink["sampler"]{sampler} (given in an undocumented form), analyzes
it, and returns a @racket[profile] value holding the analyzed results.
Without this function, the results of the sampler should be considered
meaningless.}


@defstruct*[profile ([total-time exact-nonnegative-integer?]
                     [cpu-time   exact-nonnegative-integer?]
                     [sample-number exact-nonnegative-integer?]
                     [thread-times  (listof (cons exact-nonnegative-integer?
                                                  exact-nonnegative-integer?))]
                     [nodes  (listof node?)]
                     [*-node node?])]{

Represents an analyzed profile result.

@itemize[

@item{@racket[total-time] is the total observed time (in milliseconds)
  included in the profile run.  This can be different from the actual
  time the profiling took, due to unaccounted-for time spent in
  untracked threads.  (E.g., time spent in the sampler thread
  itself.)}

@item{@racket[cpu-time] is the actual cpu time consumed by the process
  during the profiler's work.}

@item{@racket[sample-number] holds the number of samples taken during
  the profile.  This can be used to compute the average time frame
  each of the input samples represented.}

@item{@racket[thread-times] holds an association list mapping thread
  identifiers to cpu time for the corresponding threads.  As samples
  are collected, each thread that is observed is assigned a small
  integer identifier.  These identifiers are listed for each function
  call, and the total time spent in each thread is in this field.}

@item{@racket[nodes] is a list of nodes representing all observed
  functions.  These nodes are the components of the call-graph that
  the analyzer assembles (see the @racket[edge] field).  The nodes are
  sorted by a topological top-to-bottom sort, and by decreasing total
  amount of time (time spent either in the function or in its callees)
  as a secondary key.  It does not include the special
  @racket[*-node].}

@item{@racket[*-node] holds a ``special'' root node value that is
  constructed for every call graph.  This node is used as the caller
  for all top-level function nodes and as the callee for all leaf
  nodes.  It can therefore be used to start a recursive scan of the
  call graph.  In addition, the times associated with its ``callers''
  and ``callees'' actually represent the time these functions spent
  being the root of the computation or its leaf.  (This can be
  different from a node's ``self'' time, since it is divided by the
  number of instances a function had on the stack in each sample---so
  for recursive functions this value is always different from the
  ``self'' time.)}

]}


@defstruct*[node ([id      (or/c #f symbol?)]
                  [src     (or/c #f srcloc?)]
                  [thread-ids (listof exact-nonnegative-integer?)]
                  [total   exact-nonnegative-integer?]
                  [self    exact-nonnegative-integer?]
                  [callers (listof edge?)]
                  [callees (listof edge?)])]{

Represents a function call node in the call graph of an analyzed
profile result.

@itemize[

@item{The @racket[id] and @racket[src] fields hold a symbol naming the
  function and/or its source location as a @racket[srcloc] value.
  This is the same as the results of
  @racket[continuation-mark-set->context], so at most one of these can
  be @racket[#f], except for the special @racket[*-node] (see the
  @racket[profile] struct) that can be identified by both being
  @racket[#f].}

@item{@racket[thread-ids] holds a list of thread identifiers that were
  observed executing this function.}

@item{@racket[total] holds the total time (in milliseconds) that this
  function was anywhere on the stack.  It is common to see a few
  toplevel functions that have close to a 100% total time, but
  otherwise small @racket[self] times---these functions are the ones
  that initiate the actual work, but they don't do any hard work
  directly.}

@item{@racket[self] holds the total time (in milliseconds) that this
  function was observed as the leaf of the stack.  It represents the
  actual work done by this function, rather than the @racket[total]
  time spent by both the function and its callees.}

@item{@racket[callers] and @racket[callees] hold the list of callers
  and callees.  The nodes are not actually held in these lists,
  instead, @racket[edge] values are used---and provide information
  specific to each edge in the call-graph.}

]}


@defstruct*[edge ([total       exact-nonnegative-integer?]
                  [caller      node?]
                  [caller-time exact-nonnegative-integer?]
                  [callee      node?]
                  [callee-time exact-nonnegative-integer?])]{

Represents an edge between two function call nodes in the call graph
of an analyzed profile result.

@itemize[

@item{@racket[total] is analogous to the @racket[total] field of a
  @racket[node] value: the total time that this edge was anywhere on
  the stack.}

@item{@racket[caller] and @racket[callee] hold the two nodes that are
  connected by this edge.}

@item{@racket[caller-time] and @racket[callee-time] hold the time
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
  snapshot (in the other half, @tt{A} is charged for being a leaf ---
  the caller of the special @racket[*-node]), and its callee time is
  charged 1/3 respectively.}

]}
