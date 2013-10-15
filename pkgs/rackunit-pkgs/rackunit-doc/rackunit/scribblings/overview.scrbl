#lang scribble/doc
@(require "base.rkt")

@title{Overview of RackUnit}

There are three basic concepts in RackUnit:

@itemize[

@item{A @italic{check} is the basic unit of a test.  As the name
suggests, it checks some condition is true.}

@item{A @italic{test case} is a group of checks that form one
conceptual unit.  If any check within the case fails, the entire case
fails.}

@item{A @italic{test suite} is a group of test cases and test suites
that has a name.}
]
