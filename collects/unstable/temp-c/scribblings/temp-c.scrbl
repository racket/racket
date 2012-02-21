#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/eval
          unstable/scribblings/utils
          (for-label racket/base
                     racket/contract
                     racket/match
                     racket/list))

@(define our-eval (make-base-eval))

@title[#:tag "temp-c"]{Temporal Contracts: Explicit Contract Monitors}
@unstable[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

@defmodule[unstable/temp-c]

The contract system implies the presence of a "monitoring system" that ensures that contracts are not violated. The @racketmodname[racket/contract] system compiles this monitoring system into checks on values that cross a contracted boundary. This module provides a facility to pass contract boundary crossing information to an explicit monitor for approval. This monitor may, for example, use state to enforce temporal constraints, such as a resource is locked before it is accessed.

@section{Warning! Experimental!}

This library is truly experimental and the interface is likely to
drastically change as we get more experience making use of temporal
contracts. In particular, the library comes with no advice about
designing temporal contracts, which are much more subtle than standard
contracts. This subtlety is compounded because, while temporal
contract violations have accurate blame information, we cannot yet
connect violations to sub-pieces of the temporal formula.

For example, applying @racket[f] to @racket["three"] when it is
contracted to only accept numbers will error by blaming the caller and
providing the explanation "expected a <number?>, received: "three"".
In contrast, applying @racket[g] to @racket["even"] and then to
@racket["odd"] when @racket[g] is contracted to accept strings on
every odd invocation, but numbers on every even invocation, will error
by blaming the second (odd) call, but will not provide any explanation
except "the monitor disallowed the call with arguments: "odd"".
Translating non-acceptance of an event trace by an automata into a
palatable user explanation is an open problem.

@section[#:tag "monitor"]{Monitors}

@defmodule[unstable/temp-c/monitor]
@(require (for-label unstable/temp-c/monitor))

@deftogether[[
@defstruct*[monitor ([label symbol?]) #:transparent]
@defstruct*[(monitor:proj monitor)
            ([label symbol?] [proj-label symbol?] [v any/c])
             #:transparent]
@defstruct*[(monitor:call monitor)
            ([label symbol?] [proj-label symbol?] [f procedure?] 
             [app-label symbol?] [kws (listof keyword?)] [kw-args list?] [args list?])
            #:transparent]
@defstruct*[(monitor:return monitor)
            ([label symbol?] [proj-label symbol?] [f procedure?] 
             [app-label symbol?] [kws (listof keyword?)] [kw-args list?] [args list?]
             [rets list?])
             #:transparent]
@defproc[(monitor/c [monitor-allows? (-> monitor? boolean?)]
                    [label symbol?]
                    [c contract?])
         contract?]
]]{
  
  @racket[monitor/c] creates a new contract around @racket[c] that uses @racket[monitor-allows?] to approve
  contract boundary crossings. (@racket[c] approves positive crossings first.)
  
  Whenever a value @racket[v] is projected by the result of @racket[monitor/c], @racket[monitor-allows?]
  must approve a @racket[(monitor:proj label proj-label v)] structure, where @racket[proj-label] is a unique
  symbol for this projection.
  
  If @racket[monitor-allows?] approves and the value is not a function, then the value is returned.
  
  If the value is a function, then a projection is returned, whenever it is called, @racket[monitor-allows?]
  must approve a @racket[(monitor:call label proj-label v app-label kws kw-args args)] structure,
  where @racket[app-label] is a unique symbol for this application and @racket[kws], @racket[kw-args], @racket[args]
  are the arguments passed to the function.
  
  Whenever it returns, @racket[monitor-allows?]
  must approve a @racket[(monitor:return label proj-label v app-label kws kw-args args rets)] structure,
  where @racket[ret] are the return values of the application.
  
  The unique projection label allows explicitly monitored contracts to be useful when used in a first-class way 
  at different boundaries.
  
  The unique application label allows explicitly monitored contracts to pair calls and returns when functions
  return multiple times or never through the use of continuations.
  
}
  
Here is a short example that uses an explicit monitor to ensure that @racket[_malloc] and @racket[_free] are
used correctly.
@racketblock[
 (define allocated (make-weak-hasheq))
 (define memmon
   (match-lambda
     [(monitor:return 'malloc _ _ _ _ _ _ (list addr))
      (hash-set! allocated addr #t)
      #t]
     [(monitor:call 'free _ _  _ _ _ (list addr))
      (hash-has-key? allocated addr)]
     [(monitor:return 'free _ _ _ _ _ (list addr) _)
      (hash-remove! allocated addr)
      #t]
     [_
      #t]))
 (provide/contract
  [malloc (monitor/c memmon 'malloc (-> number?))]
  [free (monitor/c memmon 'free (-> number? void))])
]
           
@section[#:tag "dsl"]{Domain Specific Language}

@defmodule[unstable/temp-c/dsl]
@(require (for-label racket/match
                     racket/contract
                     unstable/temp-c/dsl
                     unstable/automata/re
                     unstable/automata/re-ext))

Constructing explicit monitors using only @racket[monitor/c] can be a bit onerous. This module provides some helpful tools for making the definition easier. It provides everything from @racketmodname[unstable/temp-c/monitor], as well as all bindings from @racketmodname[unstable/automata/re] and @racketmodname[unstable/automata/re-ext]. The latter provide a DSL for writing "dependent" regular expression machines over arbitrary @racketmodname[racket/match] patterns.

First, a few @racket[match] patterns are available to avoid specify all the details of monitored events (since most of the time the detailed options are unnecessary.)

@defform[(call n a ...)]{ A @racket[match] expander for call events to the labeled function @racket[n] with arguments @racket[a]. }
@defform[(ret n a ...)]{ A @racket[match] expander for return events to the labeled function @racket[n] with return values @racket[a]. }

@defform[(with-monitor contract-expr re-pat)]{ Defines a monitored contract where the structural portion of the contract is the @racket[contract-expr] (which may included embedded @racket[label] expressions) and where the temporal portion of the contract is the regular expression given by @racket[re-pat]. (Note: @racket[re-pat] is not a Racket expression that evaluates to a regular expression. It is a literal regular expression.)  An optional @racket[#:concurrent] may be added between the contract and the regular expression to ensure that the machine is safe against race-conditions.}

@defform[(label id contract-expr)]{ Labels a portion of a structural contract inside of @racket[with-monitor] with the label @racket[id]. }

Here is a short example for @racket[_malloc] and @racket[_free]:
@racketblock[
(with-monitor 
    (cons/c (label 'malloc (-> addr?))
            (label 'free (-> addr? void?)))
  (complement 
   (seq (star _)
        (dseq (call 'free addr)
              (seq
               (star (not (ret 'malloc (== addr))))
               (call 'free (== addr)))))))
]
