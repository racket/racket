#lang scribble/doc
@(require "base.rkt")

@title{Acknowlegements}

The following people have contributed to RackUnit:

@itemize[
  @item{Robby Findler pushed me to release version 3}

  @item{Matt Jadud and his students at Olin College
  suggested renaming @racket[test/text-ui]}

  @item{Dave Gurnell reported a bug in check-not-exn and
  suggested improvements to RackUnit}

  @item{Danny Yoo reported a bug in and provided a fix for
    trim-current-directory}

  @item{Jacob Matthews and Guillaume Marceau for bug reports
    and fixes}

  @item{Eric Hanchrow suggested test/text-ui return a useful
    result}

  @item{Ray Racine and Richard Cobbe provided require/expose}

  @item{John Clements suggested several new checks}

  @item{Jose A. Ortega Ruiz alerted me a problem in the
    packaging system and helped fix it.}

  @item{Sebastian H. Seidel provided help packaging RackUnit
    into a .plt}

  @item{Don Blaheta provided the method for grabbing line number
    and file name in checks}

  @item{Patrick Logan ported example.rkt to version 1.3}

  @item{The PLT team made Racket}

  @item{The Extreme Programming community started the whole
    testing framework thing}
]
