#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/htdp/guess))

@teachpack["guess"]{Guessing Numbers}

@declare-exporting[teachpack/htdp/guess]

The teachpack provides operations to play a guess-the-number game. Each
operation display a GUI in which a player can choose specific values for
some number of digits and then check the guess. The more advanced
operations ask students to implement more of the game.

@defproc[(guess-with-gui [check-guess (-> number? number? symbol?)]) true]{
The @scheme[check-guess] function consumes two numbers: @scheme[guess], which
is the user's guess, and @scheme[target], which is the randomly
chosen number-to-be-guessed. The result is a symbol that reflects the
relationship of the player's guess to the target.}

@defproc[(guess-with-gui-3 [check-guess (-> digit? digit? digit? number? symbol?)]) true]{
The @scheme[check-guess] function consumes three digits (@scheme[digit0],
@scheme[digit1], @scheme[digit2]) and one number (@scheme[target]). The
latter is the randomly chosen number-to-be-guessed; the three digits are
the current guess. The result is a symbol that reflects the relationship of
the player's guess (the digits converted to a number) to the target.

Note: @scheme[digit0] is the @emph{least} significant digit that
the user chose and @scheme[digit2] is the @emph{most} significant
one.}

@defproc[(guess-with-gui-list [check-guess (-> (list-of digit?) number? symbol?)]) true]{
The @scheme[check-guess] function consumes a list of digits
(@scheme[digits]) and a number (@scheme[target]). The former is  a list
that makes up the user's guess, and the latter is the randomly chosen
number-to-be-guessed. The result is a symbol that reflects the
relationship of the player's guess (the digits converted to a number) to
the target.

Note: the first item on @scheme[digits] is the @emph{least}
significant digit that the user chose, and the last one is the
@emph{most} significant digit.}
