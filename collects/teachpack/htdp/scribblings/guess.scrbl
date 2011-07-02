#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/guess))

@teachpack["guess"]{Guessing Numbers}

@;declare-exporting[teachpack/htdp/guess]
@defmodule[#:require-form beginner-require htdp/guess]

The teachpack provides functions to play a guess-the-number game. Each
function display a GUI in which a player can choose specific values for
some number of digits and then check the guess. The more advanced
functions ask students to implement more of the game.

@defproc[(guess-with-gui [check-guess (-> number? number? symbol?)]) true]{
The @racket[check-guess] function consumes two numbers: @racket[guess], which
is the user's guess, and @racket[target], which is the randomly
chosen number-to-be-guessed. The result is a symbol that reflects the
relationship of the player's guess to the target.}

@defproc[(guess-with-gui-3 [check-guess (-> digit? digit? digit? number? symbol?)]) true]{
The @racket[check-guess] function consumes three digits (@racket[digit0],
@racket[digit1], @racket[digit2]) and one number (@racket[target]). The
latter is the randomly chosen number-to-be-guessed; the three digits are
the current guess. The result is a symbol that reflects the relationship of
the player's guess (the digits converted to a number) to the target.

Note: @racket[digit0] is the @emph{least} significant digit that
the user chose and @racket[digit2] is the @emph{most} significant
one.}

@defproc[(guess-with-gui-list [check-guess (-> (list-of digit?) number? symbol?)]) true]{
The @racket[check-guess] function consumes a list of digits
(@racket[digits]) and a number (@racket[target]). The former is  a list
that makes up the user's guess, and the latter is the randomly chosen
number-to-be-guessed. The result is a symbol that reflects the
relationship of the player's guess (the digits converted to a number) to
the target.

Note: the first item on @racket[digits] is the @emph{least}
significant digit that the user chose, and the last one is the
@emph{most} significant digit.}
