#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label scheme/match))

@title[#:tag "match"]{Pattern Matching}

The @scheme[match] form supports pattern matching on arbitrary Scheme
values, as opposed to functions like @scheme[regexp-match] that
compare regular expressions to byte and character sequences (see
@secref["regexp"]).
