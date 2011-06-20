#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "contracts-utils.rkt"
          (for-label racket/contract racket/gui))

@title[#:tag "contracts-examples"]{Additional Examples}

 This section illustrates the current state of Racket's contract
 implementation with a series of examples from @italic{Design by
 Contract, by Example} @cite["Mitchell02"].

Mitchell and McKim's principles for design by contract DbC are derived
  from the 1970s style algebraic specifications. The overall goal of DbC is
  to specify the constructors of an algebra in terms of its
  observers. While we reformulate Mitchell and McKim's terminology and 
  we use a mostly applicative approach, we
  retain their terminology of ``classes'' and ``objects'':

@itemize[
@item{@bold{Separate queries from commands.}

    A @italic{query} returns a result but does not change the observable
    properties of an object. A @italic{command} changes the visible
    properties of an object, but does not return a result. In applicative
    implementation a command typically returns an new object of the same
    class.}

@item{@bold{Separate basic queries from derived queries.}

    A @italic{derived query} returns a result that is computable in
    terms of basic queries.}

@item{@bold{For each derived query, write a post-condition contract that
    specifies the result in terms of the basic queries.}}

@item{@bold{For each command, write a post-condition contract that specifies the
    changes to the observable properties in terms of the basic queries.}}

@item{@bold{For each query and command, decide on a suitable
pre-condition contract.}}]

Each of the following sections corresponds to a chapter in
 Mitchell and McKim's book (but not all chapters show up
 here). We recommend that you read the contracts first (near
 the end of the first modules), then the implementation (in
 the first modules), and then the test module (at the end of
 each section).

Mitchell and McKim use Eiffel as the underlying programming language and
 employ a conventional imperative programming style. Our long-term goal is
 to transliterate their examples into applicative Racket,
 structure-oriented imperative Racket, and Racket's class system. 

Note: To mimic Mitchell and McKim's informal notion of parametericity
 (parametric polymorphism), we use first-class contracts. At several
 places, this use of first-class contracts improves on Mitchell and McKim's
 design (see comments in interfaces).

@section{A Customer-Manager Component}

This first module contains some struct definitions in a
separate module in order to better track bugs.

@external-file[1]

This module contains the program that uses the above.

@external-file[1b]

The tests:

@external-file[1-test]

@section{A Parameteric (Simple) Stack}
@external-file[2]

The tests:

@external-file[2-test]

@section{A Dictionary}
@external-file[3]

The tests:

@external-file[3-test]

@section{A Queue}
@external-file[5]

The tests:

@external-file[5-test]
