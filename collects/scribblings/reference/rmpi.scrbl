#lang scribble/manual
@(require scribble/eval
          scribble/struct
          scribble/decode
          racket/contract
          racket/place/distributed
          racket/place/distributed/rmpi
          racket/sandbox
          racket/class)
@(require (for-label (except-in racket/base log-message)
                     racket/contract
                     racket/class
                     racket/place
                     racket/place/distributed
                     racket/place/distributed/rmpi
                     racket/place/private/async-bi-channel))


@(define evaler (make-base-eval))
@(interaction-eval #:eval evaler (require racket/place/distributed
                                          racket/class))


@title[#:tag "distributed-places-MPI"]{Distributed Places MPI}

@defmodule[racket/place/distributed/rmpi]

@defstruct*[rmpi-comm ([id exact-nonnegative-integer?] [cnt exact-positive-integer?] [channels vector?])]{
The communicator struct @racket[rmpi-comm] contains the rmpi process
rank @racket[id], the quantity of processes in the communicator group,
@racket[cnt], and a vector of place-channels, one for each process in
the group.
}

@defproc[(rmpi-id [comm rmpi-comm?]) exact-nonnegative-integer? ]{
  Takes a rmpi communicator structure, @racket[comm], and returns the node id of the RMPI
  process.
}

@defproc[(rmpi-cnt [comm rmpi-comm?]) exact-positive-integer? ]{
  Takes a rmpi communicator structure, @racket[comm], and returns the count of the RMPI
  processes in the communicator group.
}

@defproc[(rmpi-send [comm rmpi-comm?] [dest exact-nonnegative-integer?] [val any]) void?]{
  Sends @racket[val] to destination rmpi process number @racket[dest]
  using the RMPI communicator structure @racket[comm].
}

@defproc[(rmpi-recv [comm rmpi-comm?] [src exact-nonnegative-integer?]) any]{
  Receives a message from source rmpi process number @racket[src]
  using the RMPI communicator structure @racket[comm].
}

@defproc[(rmpi-init [ch place-channel?]) (values rmpi-comm? (listof any) (is-a?/c named-place-typed-channel%))]{
  Creates the @racket[rmpi-comm] structure instance using the named
  place's original place-channel @racket[ch].  In addition to the
  communicator structure, @racket[rmpi-init] returns a list of initial
  arguments and the original place-channel @racket[ch] wrapped in a
  @racket[named-place-typed-channel%].  The
  @racket[named-place-typed-channel%] wrapper allows for the reception
  of list messages typed by an initial symbol.
}

@defproc*[([(rmpi-broadcast [comm rmpi-comm?] [src exact-nonnegative-integer?]) any]
           [(rmpi-broadcast [comm rmpi-comm?] [src exact-nonnegative-integer?] [val any]) any])]{
  Broadcasts @racket[val] from @racket[src] to all rmpi processes in
  the communication group using a hypercube algorithm.  Receiving
  processes call @racket[(rmpi-broadcast comm src)].
}

@defproc[(rmpi-reduce [comm rmpi-comm?] [dest exact-nonnegative-integer?] [op procedure?] [val any]) any]{
  Reduces @racket[val] using the @racket[op] operator to @racket[dest]
  rmpi node using a hypercube algorithm.
}

@defproc[(rmpi-barrier [comm rmpi-comm?]) void?]{
  Introduces a synchronization barrier for all rmpi processes in the
  communcication group @racket[comm].
}

@defproc[(rmpi-allreduce [comm rmpi-comm?] [op procedure?] [val any]) any]{
  Reduces @racket[val] using the @racket[op] operator to rmpi node
  @racket[0] and then broadcasts the reduced value to all nodes in the
  communication group.
}

@defproc[(rmpi-partition [comm rmpi-comm?] [num exact-nonnegative-integer?]) (values exact-nonnegative-integer? exact-nonnegative-integer?)]{
  Partitions @racket[num] into @racket[rmpi-cnt] equal pieces and
  returns the offset and length for the @racket[rmpi-id]th piece.
}

@defproc[(rmpi-build-default-config
          [#:racket-path racket-path string?]
          [#:distributed-launch-path distributed-launch-path string?]
          [#:mpi-module mpi-module string?]
          [#:mpi-func mpi-func symbol?]
          [#:mpi-args mpi-args (listof any)]) hash?]{

  Builds a hash from keywords to keyword arguments for use with the
  @racket[rmpi-launch function].
}

@defproc[(rmpi-launch [default-node-config hash?] 
                      [config (listof (list/c string? port-no? symbol? 
                                              exact-nonnegative-integer?))]) 
         void?]{
  Launches distributed places nodes running @racket[#:mpi-func] in
  @racket[#:mpi-module] with @racket[#:mpi-args].  The config is a
  list of node configs, where each node config consists of a hostname,
  port, named place symbol and rmpi id number, followed by and
  optional hash of keyword @racket[#:racket-path],
  @racket[#:distributed-launch-path], @racket[#:mpi-module],
  @racket[#:mpi-func], and @racket[#:mpi-args] to keyword arguments.
  Missing optional keyword arguments will be taken from the
  @racket[default-node-config] hash of keyword arguments.
}

@defproc[(rmpi-finish [comm rmpi-comm?] [tc (is-a?/c named-place-typed-channel%)]) void?]{
  Rendezvous with the @racket[rmpi-launch], using the @racket[tc]
  returned by @racket[rmpi-launch], to indicate that the RMPI module
  is done executing and that @racket[rmpi-launch] can return control
  to its caller.
}


@examples[ #:eval evaler
(eval:alts
(rmpi-launch
    (rmpi-build-default-config
      #:racket-path "/tmp/mplt/bin/racket"
      #:distributed-launch-path (build-distributed-launch-path 
                                 "/tmp/mplt/collects")
      #:mpi-module "/tmp/mplt/kmeans.rkt"
      #:mpi-func   'kmeans-place
      #:mpi-args    (list "/tmp/mplt/color100.bin" #t 100 
                          9 10 0.0000001))

    (list (list "nodea.example.com" 6340 'kmeans_0 0)
          (list "nodeb.example.com" 6340 'kmeans_1 1)
          (list "nodec.example.com" 6340 'kmeans_2 2)
          (list "noded.example.com" 6340 'kmeans_3 3)))
(void))
]

@(close-eval evaler)
