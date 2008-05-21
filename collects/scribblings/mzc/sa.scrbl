#lang scribble/doc
@(require scribble/manual
          "common.ss")

@title[#:tag "sa" #:style 'toc]{Creating and Distributing Stand-Alone Executables}

Whether bytecode or native code, the compiled code produced by @|mzc|
relies on PLT Scheme executables to provide run-time support to the
compiled code. However, @|mzc| can also package code together with its
run-time support to form a complete executable, and then the
executable can be packaged into a distribution that works on other
machines.

@local-table-of-contents[]

@include-section["exe.scrbl"]
@include-section["dist.scrbl"]
@include-section["launcher.scrbl"]
