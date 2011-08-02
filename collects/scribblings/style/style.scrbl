#lang scribble/base

@(require "shared.rkt")

@title{How to Program Racket}
@author{Matthias Felleisen, Matthew Flatt, Robby Findler, Jay McCarthy}

@; -----------------------------------------------------------------------------

Since 1995 PLT has grown from a handful of ``repository contributors'' to
 three dozen and more. This growth implies a lot of learning on our side
 and the introduction of inconsistencies of programming styles. It is time
 to leverage the former and to start reducing the latter. Doing so will
 help us, the developers, and our users, who use the open source code in
 our repository as an implicit guide to Racket programming.

To manage the growth of PLT and to showcase good Racket coding, we need
 rules that govern the contributions to the code base. These rules should
 achieve some level of consistency across the different portions of the
 code base so that everyone who opens files should easily find his way
 around.

This document spells out the rules. They cover a range of topics, from
 basic work (commit) habits to small syntactic ideas like indentations and
 naming.

Many pieces of the code base don't live up to the rules yet.  Here is how
 we get started. When you start a new file, stick to the rules. If you need
 to edit a file, you will need to spend some time understanding its
 workings. If doing so takes quite a while due to inconsistencies with the
 rules, please take the time to fix (portions of) the file. After all, if
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

@bold{Note} If the style guide doesn't suit your personal style because you
 grew up on something different, fine. Use your @emph{personal style} for
 your @emph{personal programs}, but do use this style guide when you create
 code for the PLT code base.

@; -----------------------------------------------------------------------------

@include-section{correct-maintain-speed.scrbl}
@include-section{some-performance.scrbl}
@include-section{unit.scrbl}
@include-section{constructs.scrbl}
@include-section{textual.scrbl}
@include-section{branch-and-commit.scrbl}
@include-section{acknowledgment.scrbl}

@include-section{todo.scrbl}
