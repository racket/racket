#lang scribble/doc

@begin[(require (lib "manual.ss" "scribble"))
       (require-for-label (lib "lang.ss" "big"))]

@title{Teachpacks}

Teaching languages are small subsets of a full programming language. While
such restrictions simplify error diagnosis and the construction of tools,
they also make it impossible (or at least difficult) to write some
interesting programs. To circumvent this restriction, it is possible to
import teachpacks into programs written in a teaching language. 

In principle, a teachpack is just a library. Like any other Scheme library,
it may export values, functions, and even new constructs. In contrast to an
ordinary library, however, a teachpack must enforce the contracts of the
"lowest" teaching language into which it is exported and signal errors in a
way with which students are familiar. 

All but the last section describe the teachpacks that are available for
"How to Design Programs" and that will remain around for the second
edition.  The last section describes how to create your own
teachpacks.

@include-section["htdp/Docs/testing.scrbl"]
@include-section["htdp/Docs/image.scrbl"]
@include-section["htdp/Docs/world.scrbl"]
@include-section["htdp/Docs/questions.scrbl"]
