#lang scribble/doc
@(require scribble/manual
	  scribble/eval
          (for-label scheme/base
                     scheme/contract
                     scheme/class
                     scheme/gui/base
                     lang/posn
                     lang/imageeq
                     lang/prim))

@(define DMdA @italic{Die Macht der Abstraktion})
@(define (DMdA-ref s) @secref[#:doc '(lib "deinprogramm/scribblings/deinprogramm.scrbl") s])

Note: This is documentation for the language levels that go with the
German textbook @italic{@link["http://www.deinprogramm.de/dmda/"]{Die
Macht der Abstraktion}}.

@title{@bold{DMdA}: Sprachen als Libraries}

@; ------------------------------------------------------------
@section{@italic{Die Macht der Abstraktion} - Anfänger}

@defmodule[deinprogramm/DMdA-beginner]

Das Modul @racketmodname[deinprogramm/DMdA-beginner] implementiert die
Anfängersprache für @|DMdA|; siehe @DMdA-ref["DMdA-beginner"].

@; ------------------------------------------------------------
@section{@italic{Die Macht der Abstraktion}}

@defmodule[deinprogramm/DMdA-vanilla]

Das Modul @racketmodname[deinprogramm/DMdA-vanilla] implementiert die
Standardsprache für @|DMdA|; siehe @DMdA-ref["DMdA-vanilla"].

@; ------------------------------------------------------------
@section{@italic{Die Macht der Abstraktion} mit Zuweisungen}

@defmodule[deinprogramm/DMdA-assignments]

Das Modul @racketmodname[deinprogramm/DMdA-assignments] implementiert
die Sprachebene für @|DMdA| mit Zuweisungen und Mutationen; siehe
@DMdA-ref["DMdA-assignments"].

@; ------------------------------------------------------------
@section{@italic{Die Macht der Abstraktion} - fortgeschritten}

@defmodule[deinprogramm/DMdA-advanced]

Das Modul @racketmodname[deinprogramm/DMdA-advanced] implementiert
die fortgeschrittene Sprachebene für @|DMdA|; siehe
@DMdA-ref["DMdA-advanced"].
