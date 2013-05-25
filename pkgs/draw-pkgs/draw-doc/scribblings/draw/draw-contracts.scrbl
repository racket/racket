#lang scribble/doc
@(require "common.rkt")

@title{Drawing Contracts}

@local-table-of-contents[]

This page documents the contracts that are used to describe
the specification of @racketmodname[racket/draw] objects
and functions.

@defthing[font-family/c flat-contract?]{
  Recognizes font designations. Corresponds to the @racket[_family]
  initialization argument of the @racket[font%] class.

  Equivalent to the following definition:
  @racketblock[
    (or/c 'default 'decorative 'roman 'script 'swiss
          'modern 'symbol 'system)]
}

@defthing[font-style/c flat-contract?]{
  Recognizes font styles. Corresponds to the @racket[_style]
  initialization argument of the @racket[font%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'normal 'italic 'slant)]
}

@defthing[font-weight/c flat-contract?]{
  Recognizes font weights. Corresponds to the @racket[_weight]
  initialization argument of the @racket[font%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'normal 'bold 'light)]
}

@defthing[font-smoothing/c flat-contract?]{
  Recognizes a font smoothing amount.
  Corresponds to the @racket[_smoothing]
  initialization argument of the @racket[font%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'default 'partly-smoothed
                     'smoothed 'unsmoothed)]
}

@defthing[font-hinting/c flat-contract?]{
  Recognizes font hinting modes. Corresponds to the @racket[_hinting]
  initialization argument of the @racket[font%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'aligned 'unaligned)]
}

@defthing[pen-style/c flat-contract?]{
  Recognizes pen styles. Corresponds
  to the @racket[_style] initialization argument of the
  @racket[pen%] class.

  Equivalent to the following definition:
  @racketblock[
  (or/c 'transparent 'solid 'xor 'hilite
        'dot 'long-dash 'short-dash 'dot-dash
        'xor-dot 'xor-long-dash 'xor-short-dash
        'xor-dot-dash)]
}

@defthing[pen-cap-style/c flat-contract?]{
  Recognizes pen cap styles. Corresponds
  to the @racket[_cap] initialization argument of the
  @racket[pen%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'round 'projecting 'butt)]
}

@defthing[pen-join-style/c flat-contract?]{
  Recognizes pen join styles. Corresponds
  to the @racket[_join] initialization argument of the
  @racket[pen%] class.

  Equivalent to the following definition:
  @racketblock[(or/c 'round 'bevel 'miter)]
}

@defthing[brush-style/c flat-contract?]{
  Recognizes brush styles. Corresponds
  to the @racket[_style] initialization argument of the
  @racket[brush%] class.

  Equivalent to the following definition:
  @racketblock[
  (or/c 'transparent 'solid 'opaque
        'xor 'hilite 'panel
        'bdiagonal-hatch 'crossdiag-hatch
        'fdiagonal-hatch 'cross-hatch
        'horizontal-hatch 'vertical-hatch)]
}

