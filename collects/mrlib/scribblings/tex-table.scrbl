#lang scribble/doc
@(require "common.rkt" (for-label mrlib/switchable-button))

@title{TeX Table}

@defmodule[mrlib/tex-table]

@defthing[tex-shortcut-table
          (listof
            (list/c string? 
                    (lambda (x) 
                      (and (string? x)
                           (= (string-length x)
                              1)))))]{

  This is an association list mapping the shortcut strings that
  DrRacket uses with its @tt{control-\} (or @tt{command-\}) strings to
  their corresponding unicode characters. For example, it contains
  this mapping:
  @racketblock[
    ("alpha" "α")
  ]
  as well as many more.

}
