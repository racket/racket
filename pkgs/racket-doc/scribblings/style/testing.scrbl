#lang scribble/base

@(require "shared.rkt")

@title[#:tag "testing"]{Testing}

@; -----------------------------------------------------------------------------
@section[#:tag "test-suite"]{Test Suites}

Most of our collections come with test suites. These tests suites tend to
 live in @tt{collects/tests/} in the PLT repository, though due to
 historical reasons, a few collections come with their own local test
 suites.  If you add a new collection, create a new test suite in the
 @tt{tests} collection.

Run the test suites before you commit. To facilitate testing, we urge you
 to add a @tt{TESTME.txt} file to your collections. Ideally, you may also
 wish to have a file in this directory that runs the basic tests.  See the
 @hyperlink["https://github.com/racket/racket/tree/master/collects/2htdp/"]{2htdp},
 which is one of the collections with its own testing style.  The file should
 describe where the tests are located, how to run these tests, and what to
 look for in terms of successes and failures. These files are necessary
 because different collections have different needs for testing, and
 testing evolved in many different ways in our history.

After you commit, watch for and read(!)
 @hyperlink["http://drdr.racket-lang.org/"]{DrDr}'s emails. Do @emph{not}
 ignore them. If you have tests that are known to fail and fixing this
 requires a lot of work, consider splitting your test directory into two
 parts: @tt{success} and @tt{failure}. The former is for tests that should
 succeed now, and the latter is for tests that are currently expected to
 fail. See the
 @hyperlink["https://github.com/racket/racket/tree/master/collects/tests/typed-scheme"]{Typed
 Racket testing arrangement} for an example. When you create such
 @tt{failure} tests, you may wish to disable DrDr's checking like this:
@verbatim[#:indent 2]{
  git prop set drdr:command-line "" <file> ...
}
 This is a Racket-specific @tt{git} command.

@; -----------------------------------------------------------------------------
@section[#:tag "test-bang"]{Always Test!}

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
