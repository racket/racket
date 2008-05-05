#lang scribble/doc

@(require scribble/manual
          (for-label scheme 
	  	     teachpack/htdp/master))

@title[#:tag "master"]{MasterMinding : master.ss}

@declare-exporting[teachpack/htdp/master]

The teachpack implements GUI for playing a simple master mind-like game,
based on a function designed by a student. The player clicks on two colors
and the program responds with an answer that indicates how many colors and
places were correct.

@defproc[(master [check-guess (-> symbol? symbol? symbol? symbol? boolean?)]) symbol?]{
Chooses two ``secret'' colors and then opens a graphical user interface for
playing @emph{MasterMind}. The player is prompted to choose two colors, via
a choice tablet and mouse clicks. Once chosen, @scheme[master] uses
@scheme[check-guess] to compare them. 

If the two guesses completely match the two secret colors,
@scheme[check-guess] must return @scheme['PerfectGuess]; otherwise it must
return a different, informative symbol.}
