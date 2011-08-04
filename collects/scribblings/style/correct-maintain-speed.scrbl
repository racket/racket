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
@section[#:tag "correctness"]{Correctness}

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

Most tests suites live in @tt{collects/tests/} in the PLT
 repository. @margin-note*{Due to historical reasons, a few collections
 come with their own local test suites.}  These test suites suggest a
 culture of testing per tool or language. If you add a new collection,
 create a new test suite in the @tt{tests} collection.

Run the test suites before you commit.  After you commit, watch for and
 read(!)  @hyperlink["http://drdr.racket-lang.org/"]{DrDr}'s emails. Do
 @emph{not} ignore them. If you have a tests that regularly fails, consider
 splitting your test directory into two parts: @tt{success} and
 @tt{failure}. The former is for tests that should succeed now, and the
 latter is for tests that are currently intended to fail. See the
 @hyperlink["https://github.com/plt/racket/tree/master/collects/tests/typed-scheme"]{Typed
 Racket testing arrangement} for an example.

When you debug an existing piece of code, formulate a test case first. Put
 it into the test suite for the component so the mistake will never be
 accidentally re-introduced and add a note that points to the problem
 report.  Second, modify the code to fix the mistake. Do this second to be
 sure you didn't introduce a mistake in your tests; it is all too easy to
 think you have fixed a mistake when in reality your new test just doesn't
 properly reveal the old mistake.  Third, re-run the test suite to ensure
 that the mistake is fixed and no existing tests fail.

If there is no test suite and you aren't sure how to build one, then ask on
 the developer mailing list. Perhaps people will explain why there isn't
 one or they will sketch how to create one. Please don't ignore the
 problem. If you cannot build a test suite, you have a few options:

@itemlist[#:style 'ordered

  @item{Add functionality to the library to enable testing. Of course,
  adding functionality means adding external documentation.  Robby and
  Matthew have done so for the GUI library, and there is now a large
  automated test suite for DrRacket. So even GUI programs can come with
  extended test suites.}

  @item{Add an end-to-end test that may have to be verified by a human.
  For example, it might be hard to test Slideshow, so you could create a
  slide set and describe what it should look like so future maintainers to
  verify when @emph{they} make changes. Consider this the @emph{last and
  least desirable} option, however.}
]
@;
 The lack of tests for some collection will not disappear overnight. But if
 we all contribute a little bit, we will eventually expand the test suites
 to cover the entire code base, and future generations of maintainers will
  be grateful.

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
 requires experimentation. Once we understand this new pieces of
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
