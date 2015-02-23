#lang scribble/base

@(require "shared.rkt")

@title[#:tag "correct-maintain-speed"]{Basic Facts of Life}

@nested[#:style 'inset]{ @italic{Favor readers over writers.}
 --- Yaron Minsky, JaneStreet, 2010 at NEU/CCS}

@margin-note*{This ordering is occasionally wrong. For example, we could
 avoid IEEE floating point numbers nearly all of the time. To make this
 precise, the Racket @scheme[sqrt] function could return a rational number
 close to the IEEE float result.  We don't do such silly things, however,
 because we have decided to value speed over precision in this context.}
Strive to write code that is correct; maintainable; and fast. The ordering
 of these adjectives is critical: correct is more important than
 maintainable; maintainable is more important than fast; and fast is
 important to include, because nobody wants to live with slow programs.

This section explains these three points as far as the Racket code base is
 concerned. The rest of this guide is to spell out suggestions that should
 help you make correct, maintainable, and fast contributions to the Racket
 code base.

@; -----------------------------------------------------------------------------
@section[#:tag "correctness"]{Correctness and Testing}

@nested[#:style 'inset]{@italic{I have bug reports, therefore I exist.} -- Matthias,
watching Matthew, Robby, Shriram and others create the original code base}

@nested[#:style 'inset]{@italic{It is the way we choose to fight our bugs that
 determines our character, not their presence or absence.} -- Robby, in response}

PLT aims to release good code and to eliminate mistakes as quickly as
 possible.  All software has mistakes; complete correctness is a
 perfectionist goal.  If mistakes are unknown, the software isn't being
 used. The goal is, however, to ensure some basic level of correctness
 before a feature is released and to ensure that the same mistake isn't
 introduced again.

We ensure this basic level of correctness with large test suites. Our test
 suites contain tests at all levels. In addition to unit tests, you will
 find test suites that use a ``random testing'' strategy and tools, others
 use fuzz testing, yet others are end-to-end ``systems level'' tests, and
 DrRacket comes with an automatic GUI player that explores its
 functionality.

For details on testing in the context of the Racket code base, see
 @secref{testing}.

@; -----------------------------------------------------------------------------
@section{Maintenance}

If we wish to create maintainable code, we must ensure that our code is
 comprehensible. Code is comprehensible when you can understand its
 external purpose; when you can guess from its external purpose at its
 organization; when the organization and the code live up to consistent
 criteria of style; and when the occasional complex part comes with
 internal documentation.

Released code must have documentation. Conversely a change to the external
 behavior of code must induce a simultaneous change to its documentation.
 Here ``simultaneous'' means that the two changes are in the same 'push'
 to the code base, not necessarily in the same 'commit'. Also see
 @secref{branch-and-commit} for more on Git actions.

For style rules on documenting code, refer to the
 @hyperlink["http://docs.racket-lang.org/scribble/how-to-doc.html#%28part._reference-style%29"]{style
 guide in the Scribble manual}.  Ideally documentation comes in two parts,
 possibly located in the same document: a ``Guide'' section, which explains
 the purpose and suggests use cases, and a traditional ``Reference''
 section, which presents the minutiae. The documentation for HtDP/2e
 teachpacks is an example where the two parts are collocated. Also consider
 adding examples for each function and construct in your ``Reference''
 section.  Finally, ensure you have all the correct @tt{for-label}
 @tt{require}s and make use of other useful cross-references.

Having said that, the production of a system like Racket occasionally
 requires experimentation. Once we understand these new pieces of
 functionality, though, it is imperative to discard the ``failure
 branches'' of an experiment and to turn the successful part into a
 maintainable package.  You may even consider converting your code to Typed
 Racket eventually.

Without adherence to basic elements of style, code comprehension becomes
 impossible. The rest of this document is mostly about these elements of
 style, including some suggestions on minimal internal documentation.

@; -----------------------------------------------------------------------------
@section{Speed}

Making code fast is an endless task. Making code @emph{reasonably fast} is the goal.

As with correctness, performance demands some ``testing.'' At a minimum,
 exercise your code on some reasonably realistic inputs and some larger
 ones. Add a file to the test suite that runs large inputs regularly. For
 example, a regular test suite for a Universe display deals with a 50 x 50
 display window; one of its stress tests checks whether Universe event
 handlers and drawing routines can cope with laptop size displays or even a
 30in display. Or, if you were to write a library for a queue data
 structure, a regular test suite ensures that it deals correctly with
 enqueue and dequeue for small queues, including empty ones; a stress test
 suite for the same library would run the queue operations on a variety of
 queue sizes, including very large queues of say tens of thousands
 elements.

Stress tests don't normally have an expected output, so they never
 pass. The practice of writing stress tests exposes implementation flaws or
 provides comparative data to be used when choosing between two APIs. Just
 writing them and keeping them around reminds us that things can go bad and
 we can detect when performance degrades through some other door. Most
 importantly, a stress test may reveal that your code isn't implementing an
 algorithm with the expected @math{O(.)} running time. Finding out that
 much alone is useful. If you can't think of an improvement, just document
 the weakness in the external library and move on.

And as you read on, keep in mind that we are not perfectionists. We produce
 reasonable software.
