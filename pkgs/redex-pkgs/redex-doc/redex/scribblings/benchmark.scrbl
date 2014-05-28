#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval
          scribble/racket
          scribblings/guide/modfile
          redex/benchmark
          racket/include
          racket/runtime-path
          (for-syntax racket/include)
          (for-label racket/base
                     racket/include
                     redex/benchmark
                     racket/date))

@title[#:tag "benchmark"]{Automated Testing Benchmark}

@defmodule*/no-declare[(redex/benchmark)]
@declare-exporting[redex/benchmark]

Redex's automated testing benchmark provides a collection of buggy models and
utilities to test how efficiently methods of automatic test case 
generation are able to find counterexamples for each bug. 

The benchmark is organized by pairs of @emph{generate} and @emph{check} functions,
as described in @secref{run}. Usually these are defined on a per-module basis,
using pattern based rewrites applied to existing module definitions, as
described in @secref{manage}. More specifically, generate and check
functions are written for an existing (non-buggy) model, and then bugs
are individually added to the model; for each bug, the benchmark measures
how long, on average, different generate and check pairs take to find
a counterexample.

@section[#:tag "manage"]{Managing Benchmark Modules}

This section describes utilities for making changes to existing modules to 
create new ones, intended to assist in adding bugs to models and keeping 
buggy models in sync with changes to the original model.

@defform[#:literals(==>)
         (define-rewrite id from ==> to 
           [#:context (context-id ...)
            #:variables (variable-id ...)
            #:once-only
            #:exactly-once])]

Defines a syntax transformer bound to @racket[id], the effect of which is
to rewrite syntax matching the pattern @racket[from] to the result
expression @racket[to]. The @racket[from] argument should follow the 
grammar of a @racket[syntax-case] pattern, and @racket[to] acts
as the corresponding result expression. The behavior of the match is the
same as @racket[syntax-case], except that all identifiers in
@racket[from] are treated as literals with the exception of an identifier
that has the same binding as a @racket[variable-id] appearing in the
@racket[#:variables] keyword argument, which is treated
as a pattern variable. (The reverse of the situation for
@racket[syntax-case], where literals must be specified instead.)
The rewrite will only be applied in the context of a @racket[module] form,
but it will be applied wherever possible within the module body,
subject to a few constraints.

The rest of the keyword arguments control where and how often the
rewrite may be applied. The @racket[#:once-only] option specifies that the
rewrite can be applied no more than once, and the @racket[#:exactly-once]
option asserts that the rewrite must be applied once (and no more). In both
cases a syntax error is raised if the condition is not met. The
@racket[#:context] option searches for syntax of the form
@racket[(some-id . rest)], where the binding of @racket[some-id] 
matches that of the first @racket[context-id] in the @racket[#:context] list, 
at which point it recurs on @racket[rest] but drops the first id from the
list. Once every @racket[context-id] has been matched, the
rewrite can be applied.

@defform[(define-rewrite/compose id rw-id ...)]
Defines a syntax transformer bound to @racket[id], assuming that
every @racket[rw-id] also binds a syntax transformer, such that @racket[id] 
has the effect of applying all of the @racket[rw-id]s.

@defform[(include/rewrite path-spec mod-id rw-id ...)]
If the syntax designated by @racket[path-spec] is a module, the module syntax
is inlined as a submodule with the identifier @racket[mod-id]. Assumes each
@racket[rw-id] binds a syntax transformer, and applies them to the resulting
module syntax. The syntax of @racket[path-spec] must be same as for
@racket[include].

@(define-runtime-path here ".")
@(define bmark-eval (parameterize ([current-load-relative-directory here])
                      (make-base-eval)))
@(interaction-eval #:eval bmark-eval (require redex/benchmark))


For example, if the contents of the file mod-fx.rkt are:
@(parameterize ([current-load-relative-directory here])
   (racketmodfile "mod-fx.rkt"))
Then:
@interaction[#:eval bmark-eval
  (define-rewrite xy-rw 
    x ==> y
    #:context (f)
    #:once-only)
  (require "mod-fx.rkt")
  (f 3)
  (include/rewrite "mod-fx.rkt" submod-fx xy-rw)
  (require (prefix-in s: 'submod-fx))
  (s:f 3)]

@section[#:tag "run"]{Running Benchmark Models}

@defstruct*[run-results ([tries natural-number/c]
                        [time natural-number/c]
                        [cexps natural-number/c])]{
Returned by @racket[run-gen-and-check]. Minimal results for one run of a generate and check pair.
}

@defproc[(run-gen-and-check [get-gen (-> (-> any/c))]
                            [check (-> any/c boolean?)]
                            [seconds natural-number/c]
                            [#:name name string? "unknown"]
                            [#:type type symbol? 'unknown]) 
                            run-results?]{
Repeatedly generates random terms and checks if they are counterexamples
to some property defined by @racket[check] (a term is considered a counterexample
if @racket[check] returns @racket[#f] for that term).
The thunk @racket[get-gen] is assumed to return a fresh @emph{generator}, which
can then be called repeatedly to generate random terms. (The outer thunk is
assumed to reset the generator, for generators that have internal state. The 
generator is reset each time the property is found to be false.)
Each term is passed to @racket[check] to see if it is a counterexample.
The interval in milliseconds between counterexamples is
tracked, and the process is repeated either until the time specified by
@racket[seconds] has elapsed or the standard error in the average interval
between counterexamples is less than 10% of the average.

Returns an instance of @racket[run-results] containing the total number of
terms generated, the total elapsed time, and the number of counterexamples found.
More detailed information can be obtained using the benchmark logging facilities,
for which @racket[name] is refers to the name of the model, and @racket[type]
is a symbol indicating the generation type used.
}

@defproc[(run-gen-and-check/mods [gen-mod-path module-path?]
                                 [check-mod-path module-path?]
                                 [seconds natural-number/c]
                                 [#:name name string? "unknown"]) 
         run-results?]{
Just like @racket[run-gen-and-check], except that @racket[gen-mod-path] and
@racket[check-mod-path] are module paths to a @emph{generator module} and a
@emph{check module}, which are assumed to have the following characteristics:
@itemlist[
          @item{A @emph{generator module} provides the function @racket[get-generator],
                  which meets the specification for the @racket[get-gen] argument to
                  @racket[run-gen-and-check], and @racket[type], which is a symbol
                  designating the type of the generator.}
          @item{A @emph{check module} provides the function @racket[check], which meets
                  the specification for the @racket[check] argument to 
                  @racket[run-gen-and-check].}
          ]
}

@section[#:tag "log"]{Logging}

@defstruct*[bmark-log-data ([data any/c])]{
Contains data logged by the benchmark, as described below.
}

Detailed information gathered during a benchmark run is logged to the @racket[current-logger], 
at the @racket['info] level, with the message @racket["BENCHMARK-LOGGING"]. The 
@racket[data] field of the log message contains a @racket[bmark-log-data] struct, which
wraps data of the form:

@racketgrammar*[#:literals (list)
                [log-data (list event timestamp data-list)]]

Where @racket[event] is a symbol that designates the type of event, and 
@racket[timestamp] is symbol that contains the @racket[current-date] of the event in
ISO-8601 format.
The information in @racket[data-list] depends on the event, but must be in the form
of a list alternating between a keyword and a datum, where the keyword is a short description
of the datum. 

The following events are logged (the symbol designating the event is in parentheses, and
the form of the data logged for each event is shown):
@itemlist[
          @item{Run starts (@racket['start]), logged when beginning a run with a new
                            generate/check pair.
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:model model '#:type gen)]}
          @item{Run completions (@racket['finished]), logged at the end of a run.
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:model model '#:type gen '#:time-ms time 
                                               '#:attempts tries 
                                               '#:num-counterexamples countxmps 
                                               '#:rate-terms/s rate '#:attempts/cexp atts)]}
          @item{Every counterexample found (@racket['counterexample]).
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:model model '#:type gen 
                                               '#:counterexample term '#:iterations tries 
                                               '#:time time)]}
          @item{New average intervals between counterexamples (@racket['new-average]), which
                    are recalculated whenever a counterexample is found.
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:model model '#:type gen 
                                               '#:average avg '#:stderr err)]}                                           
          @item{Major garbage collections (@racket['gc-major]).
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:amount amount '#:time time)]}
          @item{Heartbeats (@racket['hearbeat]) are logged every 10 seconds by the benchmark
                            as a liveness check.
                @racketgrammar[#:literals (list quote)
                               data-list (list '#:model model '#:type gen)]}
          @item{Timeouts (@racket['timeout]), which occur when generating or checking a single
                         takes term longer than 5 minutes.
                @racketgrammar[#:literals (list check generation quote)
                               data-list (list '#:during 'check '#:term term '#:model model
                                               '#:type gen)
                                         (list '#:during 'generation '#:model model '#:type gen)]}                                            
          ]

@defproc[(benchmark-logging-to [filename string?]
                               [thunk (-> any/c)]) 
         any/c]{Intercepts events logged by the benchmark and writes the data specified by
                the @racket[log-data] production above to @racket[filename].
}

@defparam[bmark-log-directory directory (or/c path-string? path-for-some-system? 'up 'same)
                              #:value (current-directory)]{
Controls the directory where @racket[filename] in @racket[benchmark-logging-to] is located.}

@section{Plotting}

Plotting and analysis tools consume data of the form produced by the benchmark
logging facilities (see @secref{log}).

@emph{TODO!}

@section{Benchmark Models}

The models included in the distribution of the benchmark are in the
@filepath{redex/benchmark/models} subdirectory of the @racket[redex-benchmark]
package. Each such subdirectory contains an info file named according to the
pattern @filepath{<name>-info.rkt}, defining a module that provides the function:

@defproc[(all-mods)
         (listof (list/c string? module-path? module-path?))]{Returns a list of 
generate and check pairs for a given model or set of models, such that for each 
pair the first element is the name of the model, the second is a module defining a 
generator, and the third is a module defining a check function.}

The file @filepath{redex/benchmark/models/all-info.rkt} provides an @racket[all-mods]
function listing all of the generate and check pairs included in the benchmark.

A command line interface is provided by the file 
@filepath{redex/benchmark/run-benchmark.rkt}, 
which takes an ``info'' file as described above as its primary argument and provides
options for running the listed tests. It automatically writes results from each run to
a separate log file, all of which are located in a temporary directory.
(The directory path is printed to standard out at the beginning of the run).


