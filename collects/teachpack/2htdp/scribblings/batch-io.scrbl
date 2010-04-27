#lang scribble/doc

@(require (for-label scheme teachpack/2htdp/batch-io))
@(require scheme/sandbox scribble/manual scribble/eval scribble/core)
@(require "shared.ss")

@(require 2htdp/batch-io)
@(require scheme/runtime-path)
@(define-runtime-path here ".")
@(define (file-is f)
  (define x (parameterize ([current-directory here]) (read-file f)))
  (centered
    (tabular #:style "searchbox"
      (list (list (verbatim x))))))

@(define-syntax examples-batch-io
  (syntax-rules ()
    [(_ d ...)
     (let ()
       (define me (make-base-eval))
       (begin
         (interaction-eval #:eval me (require 2htdp/batch-io))
         (interaction-eval #:eval me d)
         ...)
       (current-directory here)
       (interaction-eval #:eval me (require lang/htdp-intermediate-lambda))
       me)]))

@; -----------------------------------------------------------------------------

@(define-syntax-rule (reading name ctc s)
   @defproc[(@name [f (and/c string? file-exists?)]) @ctc ]{
 reads the content of file @scheme[f] and produces it as @s .} )

@teachpack["batch-io"]{Batch Input/Output}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/batch-io]

The batch-io teachpack introduces several functions and a form for reading
 content from files and one function for writing to a file.

All functions that read a file consume the name of a file and possibly
 additional arguments. They assume that the specified file exists in the
 same folder as the program; if not they signal an error:
@itemlist[

@item{@reading[read-file string?]{a string, including newlines}

@examples[#:eval (examples-batch-io)
(read-file "data.txt")
]
assuming the file named @scheme["data.txt"] has this shape: 
@(file-is "data.txt")
Note how the leading space in the second line translates into the space
between the newline indicator and the word @scheme["good"] in the result.}

@item{@reading[read-1strings (listof 1string?)]{a list of one-char strings, one per character}

@examples[#:eval (examples-batch-io)
(read-1strings "data.txt")
]
Note how this function reproduces all parts of the file faithfully,
including spaces and newlines.}

@item{@reading[read-lines (listof string?)]{a list of strings, one per line}
@examples[#:eval (examples-batch-io)
(read-lines "data.txt")
]
when @scheme["data.txt"] is the name of the same file as in the preceding
item. And again, the leading space of the second line shows up in the
second string in the list.}

@item{@reading[read-words (listof string?)]{a list of strings, one per white-space separated token in the file}

@examples[#:eval (examples-batch-io)
(read-words "data.txt")
]
This time, however, the extra leading space of the second line of
@scheme["data.txt"] has disappeared in the result. The space is considered
a part of the separator that surrounds the word @scheme["good"].
}

@item{@reading[read-words/line (listof string?)]{a list of lists, one per line; each line is represented as a list of white-space separated tokens}

@examples[#:eval (examples-batch-io)
(read-words/line "data.txt")
]
The results is similar to the one that @scheme[read-words] produces,
except that the organization of the file into lines is preserved. 
}

@item{@reading[read-csv-file (listof (listof any/c))]{a list of lists of comma-separated values}

@examples[#:eval (examples-batch-io)
(read-csv-file "data.csv")
]
where the file named @scheme["data.csv"] has this shape: 
@(file-is "data.csv")
It is important to understand that the rows don't have to have the same
length. Here the third line of the file turns into a row of three
elements. 
}

@item{@defproc[(@read-csv-file/rows [f (and/c string? exists?)][s
 (-> (listof any/c) X?)]) (listof X?)]{reads the content of file @scheme[f] and
 produces it as list of rows, each constructed via @scheme[s]}

@examples[#:eval (examples-batch-io)
(read-csv-file/rows "data.csv" (lambda (x) x))
(read-csv-file/rows "data.csv" length)
]
 The first example shows how @scheme[read-csv-file] is just a short form
 for @scheme[read-csv-file/rows]; the second one simply counts the
 number of separated tokens and the result is just a list of numbers. 
 In many cases, the function argument is used to construct a structure from
 a row.}
]

There is only one writer function at the moment: 
@itemlist[

@item{@defproc[(write-file [f string?] [cntnt string?]) boolean?]{
 turns @scheme[cntnt] into the content of file @scheme[f], located in the
 same folder (directory) as the program. If the file exists when the
 function is called, the function produces @scheme[true]; otherwise it
 produces @scheme[false].}

@examples[#:eval (examples-batch-io)
(if (write-file "output.txt" "good bye")
    (write-file "output.txt" "cruel world")
    (write-file "output.txt" "cruel world"))
]
 After evaluating this examples, the file named @scheme["output.txt"]
 looks like this: 
 @(file-is "output.txt")
 Explain why.
}
]

@(delete-file "output.txt")

