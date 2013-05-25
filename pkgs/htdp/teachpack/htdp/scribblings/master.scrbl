#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/master))

@teachpack["master"]{MasterMinding}

@;declare-exporting[teachpack/htdp/master]
@defmodule[#:require-form beginner-require htdp/master]

The teachpack implements GUI for playing a simple master mind-like game,
based on a function designed by a student. The player clicks on two colors
and the program responds with an answer that indicates how many colors and
places were correct.

@defproc[(master [check-guess (-> symbol? symbol? symbol? symbol? boolean?)]) symbol?]{
Chooses two ``secret'' colors and then opens a graphical user interface for
playing @emph{MasterMind}. The player is prompted to choose two colors, via
a choice tablet and mouse clicks. Once chosen, @racket[master] uses
@racket[check-guess] to compare them. 

If the two guesses completely match the two secret colors,
@racket[check-guess] must return @racket['PerfectGuess]; otherwise it must
return a different, informative symbol.}
