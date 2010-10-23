#lang scribble/base

@title[#:tag "correct-maintain-speed"]{Basic Facts of Life}

@nested[#:style 'inset]{ Favor readers over writers.
 --- Yaron Minsky, JaneStreet, 2010 at Northeastern}

Write code that is correct; maintainable; and fast. The ordering of this
adjectives is critical: correct is more important than maintainable;
maintainable is more important than fast; and fast is important to include,
because nobody wants to live with slow programs.

Code is correct.

Code is maintainable.

Code is fast.

The purpose of this guide is to spell out suggestions that help with these
three points. Specifically, it spells out suggestions @emph{that you can
check} and that everyone else can check.

@; -----------------------------------------------------------------------------
@section{Correctness}

@nested[#:style 'inset]{I have bug reports, therefore I exist. -- Matthias,
watching Matthew, Robby, Shriram and others create the original code base}

@nested[#:style 'inset]{It is the way we choose to fight our bugs that
 determines our character, not their presence or absence. -- Robby, in response}

Correctness is a perfectionist goal beyond the reach of PLT.  All software
 has mistakes. If they are unknown, the software isn't being used. The goal
 is, however, not to ensure some basic level of correctness before a
 feature is released and to ensure that the same mistake isn't introduced
 again.

Formulate test suites. Use unit testing. Use random testing. Use fuzz
 testing. Test!

Run the test suites before you commit. Read drdr's emails; don't ignore
 them.

Add tests to test suites during debugging. That is, first, write an
 automated test that exposes the mistake in the existing
 implementation. Put this in the software's test suite so it will never be
 accidentally introduced again.  Second, modify the code to fix the
 mistake. Do this second to be sure you didn't introduce a mistake in your
 tests; it is all too easy to think you've fixed a mistake when in reality
 your new test just doesn't properly reveal the old mistake.  Third, re-run
 the test suite to ensure that the mistake is fixed and no existing tests
 fail.

Create test suites. Editing code without an existing test suite is like
 flying blind.  If there is no existing test suite, you have no idea
 whether changes are introducing any new mistakes or breaking intended
 functionality.  Make a reasonable effort to put in a test case or two for
 the specific functionality that you're adding or modifying.  If there is
 no test suite and you aren't sure how to build one, then ask, see what
 responses you get, and go from there.  In the special case that you found
 a mistake and are fixing it, there should be a way to observe that mistake
 and you should be able to turn that into a test case. If you cannot, you
 have a few options:

 @itemlist[#:style 'ordered

  @item{Add an end-to-end test that may have to be verified by a human.  For
   example, it might be hard to test Slideshow, so you could create a slide
   set and describe what it should look like so future maintainers to
   verify when @emph{they} make changes.}

  @item{Add functionality to the library to expose properties that reveal the
    bug. For example, you may be able to add a few accessors to Slideshow
    slides to enable an automated test suite.}
]
 As we slowly increase the number of tests across the system, this problem
 will go away for future generations.

@; -----------------------------------------------------------------------------
@section{Maintenance}

Comprehensible code is maintainable.

Code is comprehensible when you can understand its external purpose. To
 this end, code must come with external documentation. Released code must
 have documentation. A change to the external behavior of code must induce
 an immediate change to its documentation.

In order to document code, refer to the
 @hyperlink["http://docs.racket-lang.org/scribble/how-to-doc.html#%28part._reference-style%29"]{style
 guide} in the Scribble manual.  Ideally documentation comes in two parts:
 a "Guide" section, which explains the purpose and suggests use cases, and
 a traditional "Reference" section, which presents the minutae.  Also
 consider adding examples to each function in your "Reference" section.
 Finally, ensure you have all the correct @tt{for-label} @tt{require}s
 and make use of other useful cross-references.

Code comprehension also requires adherence to basic elements of style and
 some internal documentation. The rest of this document is about these
 elements of style, including some suggestions on internal documentation.

@; -----------------------------------------------------------------------------
@section{Speed}

Making code fast is an endless task.

Making code @emph{reasonably} fast is the goal.

It is especially the goal for all pieces of the code base that are reused
elsewhere.

Just as for correctness, strive for basic tests, that is, tests that
 exercise your code on reasonably large inputs. While a regular test suite
 for a universe display deal with a 50 x 50 display window, the stress test
 suite should check whether universe event handlers and drawing routines
 can cope with laptop size displays or even a 30in display. Or, if you were
 to write a library for a queue data structure, a regular test suite
 ensures that it deals correctly with enqueue and dequeue for small
 queues, including empty ones;  a stress test suite for the same library
 would run the queue operations on a variety of queue sizes, including very
 large queues of say 10,000 elements.

Stress tests don't normally have an expected output, so they never
 "pass". The practice of writing stress tests exposes implementation flaws
 or provides comparative data to be used when choosing between two
 APIs. Just writing them and keeping them around reminds us that things can
 go bad and we can detect when performance degrades through some other
 door. Most importantly, a stress test suite may reveal that your code
 isn't implementing an algorithm with the expected O(.) running
 time. Finding out that much alone is useful. If you can't think of an
 improvement, just document the weakness in the external library and move
 on.

We are not perfectionist. We produce reasonable software.
