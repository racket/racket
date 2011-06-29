#lang scribble/doc
@(require scribble/manual)

@title{Turtle Graphics}

Turtle graphics are available in two forms: traditional imperative
turtle operations that draw into a fixed window, and functional turtle
operations that consume and produce a turtle picture.

@table-of-contents[]

@include-section["traditional-turtles.scrbl"]
@include-section["value-turtles.scrbl"]
