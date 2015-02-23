#lang scribble/base

@(require "shared.rkt")

@title{How to Program Racket}
@author{Matthias Felleisen, Matthew Flatt, Robby Findler, Jay McCarthy}

@section-index{Style Guide}

@; -----------------------------------------------------------------------------

Since 1995 the number of ``repository contributors'' has grown from a small handful to
 three dozen and more. This growth implies a lot of learning 
 and the introduction of inconsistencies of programming styles. This document
 is an attempt leverage the former and to start reducing the latter. Doing so will
 help us, the developers, and our users, who use the open source code in
 our repository as an implicit guide to Racket programming.

To help manage the growth our code and showcase good Racket style, we need
 guidelines that shape the contributions to the code base. These guidelines should
 achieve some level of consistency across the different portions of the
 code base so that everyone who opens files can easily find their way
 around.

This document spells out the guidelines. They cover a range of topics, from
 basic work (commit) habits to small syntactic ideas like indentation and
 naming.

Many pieces of the code base don't live up to the guidelines yet.  Here is how
 we get started. When you start a new file, stick to the guidelines. If you need
 to edit a file, you will need to spend some time understanding its
 workings. If doing so takes quite a while due to inconsistencies with the
 guidelines, please take the time to fix (portions of) the file. After all, if
 the inconsistencies throw you off for that much time, others are likely to
 have the same problems. If you help fixing it, you reduce future
 maintenance time. Whoever touches the file next will be grateful to you.
 @emph{Do} run the test suites, and do @emph{not} change the behavior of
 the file.

Also, look over the commit messages. If you see problems with the code
 deltas, let the contributor know. If you see a bug fix without docs and
 tests, let the contributor know. Code should be viewed by more than one
 person because a second person is likely to catch logical mistakes,
 performance problems, and unintended effects.

@bold{Request} This document isn't complete and it isn't perfect. Consider
 it a call for improvements and suggestions.  If you have ideas, contact
 the first author via email. If your request gets ignored, appeal to all
 four authors.

@bold{Note} The recommendations in this style guide may not jibe with what
 you grew up with. (They conflict with some of the ideas that the primary
 author had about style.) But if you do write code that ends up in the
 Racket code base, please follow the recommendations here. If/when someone
 else works on your code, this person may ``fix'' your code if it isn't in
 compliance with the style guide.

@; -----------------------------------------------------------------------------

@include-section{correct-maintain-speed.scrbl}
@include-section{testing.scrbl}
@include-section{unit.scrbl}
@include-section{constructs.scrbl}
@include-section{textual.scrbl}
@include-section{some-performance.scrbl}
@include-section{branch-and-commit.scrbl}
@include-section{acknowledgment.scrbl}

@include-section{todo.scrbl}
