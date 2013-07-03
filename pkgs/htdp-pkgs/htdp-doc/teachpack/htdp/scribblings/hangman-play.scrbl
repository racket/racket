#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/hangman))

@teachpack["hangman-play"]{Playing Hangman}

@defmodule[#:require-form beginner-require htdp/hangman-play]

The teachpack implements the Hangman game so that students can play the
game and get an understanding of what we expect from them. 

@defproc[(go [name symbol?]) true]{
chooses a ``secret'' three-letter word, opens a canvas and a menu, 
and asks the player to guess the word.}
