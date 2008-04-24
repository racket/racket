#lang scribble/doc

@(require scribble/manual)

@title[#:tag "geometry"]{Geometry: geometry.*}

Add 
@verbatim[#:indent 3]{
  import geometry.*
}
at the top of your Definitions Window to import this library. 

This package provides a class for representing positions in a Cartesian world:
@verbatim[#:indent 3]{
                      +----------+
                      | Posn     |
                      +----------+
                      | int x    |
                      | int y    |
                      +----------+
}

@deftech{Posn} is a class with two fields, one per coordinate. The
constructor consumes two integers. 
