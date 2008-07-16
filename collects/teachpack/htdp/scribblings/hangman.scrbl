#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/htdp/hangman))

@teachpack["hangman"]{Hangman}

@declare-exporting[teachpack/htdp/hangman]

The teachpack implements the callback functions for playing a
@emph{Hangman} game, based on a function designed by a student. The player
guesses a letter and the program responds with an answer that indicates
how many times, if at all, the letter occurs in the secret word.

The teachpack provides all the drawing operations from @secref{draw} for
managing a canvas into which the ``hangman'' is drawn.

@defproc[(hangman [make-word (-> symbol? symbol? symbol? word?)][reveal (-> word? word? word?)][draw-next-part (-> symbol? true)]) true]{
Chooses a ``secret'' three-letter word and uses the given functions to
manage the @emph{Hangman} game.}

@defproc[(hangman-list
	   [reveal-for-list (-> symbol? (list-of symbol?) (list-of symbol?)
			        (list-of symbol?))]
	   [draw-next-part (-> symbol? true)]) true]{
Chooses a ``secret'' word---a list of symbolic letters---and uses the given
functions to manage the @emph{Hangman} game: 
@scheme[reveal-for-list] determines how many times the chosen letter occurs
in the secret word; 
@scheme[draw-next-part] is given the symbolic name of a body part and draws
it on a separately managed canvas.
}
