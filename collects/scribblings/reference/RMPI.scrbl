#lang scribble/manual
@(require scribble/eval
          scribble/struct
          scribble/decode
          racket/contract
          racket/place/distributed
          racket/place/distributed/RMPI
          racket/sandbox
          racket/class)
@(require (for-label racket/place/distributed/RMPI racket/class))


@(define evaler (make-base-eval))
@(interaction-eval #:eval evaler (require racket/place/distributed
                                          racket/class
                                          racket/place/define-remote-server))

@(define output-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket/base)))

@(define-syntax interaction-output
  (syntax-rules ()
    [(_ #:eval evaluator e)
        (begin
          (interaction #:eval evaluator e)
          (printf "K ~a\n" (get-output evaluator)))]))

@title[#:tag "distributed-places-MPI"]{Racket Distributed Places MPI}

@defmodule[racket/place/distributed/RMPI]

@defproc[(RMPI-id [comm RMPI-COMM?]) non-negative-integer? ]{
  Takes a RMPI communicator structure, @racket[comm], and returns the node id of the RMPI
  process.
}

@defproc[(RMPI-cnt [comm RMPI-COMM?]) positive-integer? ]{
  Takes a RMPI communicator structure, @racket[comm], and returns the count of the RMPI
  processes in the communicator group.
}

@defproc[(RMPI-send [comm RMPI-COMM?] [dest non-negative-integer?] [val any?]) void?]{
  Sends @racket[val] to destination RMPI process number @racket[dest] using the RMPI communicator structure @racket[comm].
}

@defproc[(RMPI-recv [comm RMPI-COMM?] [src non-negative-integer?]) any?]{
  Receives a message from source RMPI process number @racket[src] using the RMPI communicator structure @racket[comm].
}

@defproc[(RMPI-init [ch place-channel?]) (values RMPI-COMM? (listof any?) named-place-type-channel%?)]{
  Creates the @racket[RMPI-COMM] structure instance using the named place's original place-channel @racket[ch].
  In addition to the communicator structure, @racket[RMPI-init] returns a list of initial arguments and the original place-channel
  @racket[ch] wrapped in a @racket[named-place-type-channel%].  The @racket[named-place-type-channel%] wrapper allows for
  the reception of list messages typed by an initial symbol.
}

@defproc*[([(RMPI-BCast [comm RMPI-COMM?] [src non-negative-integer?]) any?]
           [(RMPI-BCast [comm RMPI-COMM?] [src non-negative-integer?] [val any?]) any?])]{
  Broadcasts @racket[val] from @racket[src] to all RMPI processes in the communication group using a hypercube algorithm.
  Receiving processes call @racket[(RMPI-BCast comm src)].
}

@defproc[(RMPI-Reduce [comm RMPI-COMM?] [dest non-negative-integer?] [op procedure?] [val any?]) any?]{
  Reduces @racket[val] using the @racket[op] operator to @racket[dest] RMPI node using a hypercube algorithm.
}

@defproc[(RMPI-Barrier [comm RMPI-COMM?]) void?]{
  Introduces a synchronization barrier for all RMPI processes in the communcication group @racket[comm].
}

@defproc[(RMPI-AllReduce [comm RMPI-COMM?] [op procedure?] [val any?]) any?]{
  Reduces @racket[val] using the @racket[op] operator to RMPI node @racket[0] and then broadcasts the reduced value to all nodes in the  communication group.
}

@defproc[(RMPI-partition [comm RMPI-COMM?] [num positive-integer?]) (values positive-integer? positive-integer?)]{
  Partitions @racket[num] into @racket[RMPI-cnt] equal pieces and returns the offset and length for the @racket[RMPI-id]th
  piece.
}

@defproc[(RMPI-build-default-config 
          [#:racket-path racket-path string?]
          [#:distributed-launch-path distributed-launch-path string?]
          [#:mpi-module mpi-module string?]
          [#:mpi-func mpi-func symbol?]
          [#:mpi-args mpi-args (listof any?)]) hash?]{

  Builds a hash from keywords to keyword arguments for use with the @racket[RMPI-launch function].
}

@defproc[(RMPI-launch [default-node-config hash?] [config (listof (listof string? port-no? symbol? non-negative-integer?))]) void?]{
  Launches distributed places nodes running @racket[#:mpi-func] in @racket[#:mpi-module] with @racket[#:mpi-args].
  The config is a list of node configs, where each node config consists of a hostname, port, named place symbol and RMPI id number, followed by optional keyword arguments @racket[#:racket-path], @racket[#:distributed-launch-path], @racket[#:mpi-module], @racket[#:mpi-func], and @racket[#:mpi-args].  Missing optional keyword arguments will be taken from the @racket[default-node-config] hash of keyword arguments.
}

@defproc[(RMPI-finish [comm RMPI-COMM?] [tc named-place-type-channel%?]) void?]{
  Rendezvous with the @racket[RMPI-launch], using the @racket[tc] returned by @racket[RMPI-launch], to indicate that the RMPI module is done executing and that @racket[RMPI-launch] can return control to its caller.
}


@examples[ #:eval evaler
(RMPI-launch                                                                                              
    (RMPI-build-default-config                                                                                
      #:racket-path "/tmp/mplt/bin/racket"                                                                    
      #:distributed-launch-path (build-distributed-launch-path "/tmp/mplt/collects")                          
      #:mpi-module "/tmp/mplt/kmeans.rkt"                                                                     
      #:mpi-func   'kmeans-place                                                                              
      #:mpi-args    (list "/tmp/mplt/color100.bin" #t 100 9 10 0.0000001))                                    
                                                                                                              
    (list (list "nodea.example.com" 6340 'kmeans_0 0)                                                        
          (list "nodeb.example.com" 6340 'kmeans_1 1)                                                        
          (list "nodec.example.com" 6340 'kmeans_2 2)                                                       
          (list "noded.example.com" 6340 'kmeans_3 3)))
]

@(close-eval evaler)
