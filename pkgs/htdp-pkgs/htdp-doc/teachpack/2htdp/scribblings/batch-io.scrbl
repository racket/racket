#lang scribble/doc

@(require scheme/sandbox scribble/manual scribble/eval scribble/core
          scribble/html-properties scribble/latex-properties
          2htdp/batch-io
          "shared.rkt"
	  (for-syntax racket)
          (for-label scheme teachpack/2htdp/batch-io))

@(require scheme/runtime-path)
@(define-runtime-path here ".")
@(define io-style-extras
   (list (make-css-addition (build-path here "io.css"))
         (make-tex-addition (build-path here "io.tex"))))
@(define (file-is f)
  (define x (parameterize ([current-directory here]) (read-file f)))
  (nested
    (tabular #:style (make-style "FileBox" io-style-extras)
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
       (me `(,current-directory ,here))
       (interaction-eval #:eval me (require lang/htdp-intermediate-lambda))
       me)]))

@; -----------------------------------------------------------------------------

@(define-syntax (reading stx)
   (syntax-case stx ()
     [(reading name ctc s)
     #`@defproc[(@name [f (or/c 'standard-in 'stdin (and/c string? file-exists?))]) @ctc ]{
      reads the standard input device (until closed) or the content of file
      @racket[f] and produces it as @list[s].}] 
     [(reading name ctc [x ctc2] s ...)
      #`@defproc[(@name [f (or/c 'standard-in 'stdin (and/c string? file-exists?))] [@x @ctc2]) @ctc ]{
      reads the standard input device (until closed) or the content of file
      @racket[f] and produces it as @list[s ...].}]))

@teachpack["batch-io"]{Batch Input/Output}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/batch-io]

The batch-io teachpack introduces several functions and a form for reading
 content from files and one function for writing to a file.

@; -----------------------------------------------------------------------------
@section{IO Functions}

All functions that read a file consume the name of a file and possibly
 additional arguments. They assume that the specified file exists in the
 same folder as the program; if not they signal an error:
@itemlist[

@item{@reading[read-file string?]{a string, including newlines}

@examples[#:eval (examples-batch-io)
(read-file "data.txt")
]
assuming the file named @racket["data.txt"] has this shape: 
@(file-is "data.txt")
Note how the leading space in the second line translates into the space
between the newline indicator and the word @racket["good"] in the result.}

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
when @racket["data.txt"] is the name of the same file as in the preceding
item. And again, the leading space of the second line shows up in the
second string in the list.}

@item{@reading[read-words (listof string?)]{a list of strings, one per white-space separated token in the file}

@examples[#:eval (examples-batch-io)
(read-words "data.txt")
]
This time, however, the extra leading space of the second line of
@racket["data.txt"] has disappeared in the result. The space is considered
a part of the separator that surrounds the word @racket["good"].
}

@item{@reading[read-words/line (listof string?)]{a list of lists, one per line; each line is represented as a list of strings}

@examples[#:eval (examples-batch-io)
(read-words/line "data.txt")
]
The results is similar to the one that @racket[read-words] produces,
except that the organization of the file into lines is preserved. 
In particular, the empty third line is represented as an empty list of words. 
}

@item{@reading[read-words-and-numbers/line (listof (or number? string?))]{a list of lists, one per line; each line is represented as a list of strings and numbers}

@examples[#:eval (examples-batch-io)
(read-words-and-numbers/line "data.txt")
]
The results is like the one that @racket[read-words/line] produces,
except strings that can be parsed as numbers are represented as numbers.}

@item{@reading[read-csv-file (listof (listof any/c))]{a list of lists of comma-separated values}

@examples[#:eval (examples-batch-io)
(read-csv-file "data.csv")
]
where the file named @racket["data.csv"] has this shape: 
@(file-is "data.csv")
It is important to understand that the rows don't have to have the same
length. Here the third line of the file turns into a row of three
elements. 
}

@item{@reading[read-csv-file/rows (listof X?) [s (-> (listof any/c) X?)]]{reads the content of file @racket[f] and
 produces it as list of rows, each constructed via @racket[s]}

@examples[#:eval (examples-batch-io)
(read-csv-file/rows "data.csv" (lambda (x) x))
(read-csv-file/rows "data.csv" length)
]
 The first example shows how @racket[read-csv-file] is just a short form
 for @racket[read-csv-file/rows]; the second one simply counts the
 number of separated tokens and the result is just a list of numbers. 
 In many cases, the function argument is used to construct a structure from
 a row.}

@; -----------------------------------------------------------------------------
@item{@reading[read-xexpr xexpr?]{an X-expression, including whitespace such as tabs and newlines}

Assumption: the file @racket[f] or the selected input device contains an
XML element. It assumes the file contains HTML-like text and reads it as XML. 

@examples[#:eval (examples-batch-io)
(read-xexpr "data.xml")
]
assuming the file named @racket["data.xml"] has this shape: 
@(file-is "data.xml")
Note how the result includes @racket["\\n"] for the newlines.}

@item{@reading[read-plain-xexpr xexpr?]{an X-expression, without whitespace}

Assumption: the file @racket[f] or the selected input device contains an
XML element and the content of this element are other XML elements and
whitespace. In particular, the XML element does not contain any strings as
elements other than whitespace. 

@examples[#:eval (examples-batch-io)
(read-plain-xexpr "data-plain.xml")
]
assuming the file named @racket["data-plain.xml"] has this shape: 
@(file-is "data-plain.xml")
Compare this result with the one for @racket[read-xexpr].}
]

There is only one writer function at the moment: 
@itemlist[

@item{@defproc[(write-file [f (or/c 'standard-out 'stdout string?)] [cntnt string?]) string?]{
 sends @racket[cntnt] to the standard output device or 
 turns @racket[cntnt] into the content of file @racket[f], located in the
 same folder (directory) as the program. If the write succeeds, the
 function produces the name of the file (@racket[f]); otherwise it signals
 an error.}

@examples[#:eval (examples-batch-io)
(if (string=? (write-file "output.txt" "good bye") "output.txt")
    (write-file "output.txt" "cruel world")
    (write-file "output.txt" "cruel world"))
]
 After evaluating this examples, the file named @racket["output.txt"]
 looks like this: 
cruel world
 Explain why.
}
]

@(parameterize ([current-directory here])
   (with-handlers ([exn:fail:filesystem? void])
     (delete-file "output.txt")))

@bold{Warning}: The file IO functions in this teachpack are platform
 dependent. That is, as long as your programs and your files live on the
 same platform, you should not have any problems reading the files that
 programs wrote and vice versa. If, however, one of your programs writes a
 file on a Windows operating system and if you then copy this output file
 to a Mac, reading the copied text file may produce extraneous ``return''
 characters. Note that this describes only one example of possible
 malfunction; there are other cases when trans-platform actions may cause
 this teachpack to fail. 

@; -----------------------------------------------------------------------------
@(define-syntax (reading/web stx)
   (syntax-case stx ()
     [(reading name ctc s)
     #`@defproc[(@name [u string?]) @ctc]{
      reads the content of URL @racket[u] and produces the first XML
      element as an @racket[xexpr?] @list[s]. If possible, the function interprets the HTML at
      the specified URL as XML. The function returns #@racket[f] if the web page
      does not exist (404)}]
     [(reading name ctc [x ctc2] s ...)
      #`@defproc[(@name [f (or/c 'standard-in 'stdin (and/c string? file-exists?))] [@x @ctc2]) @ctc ]{
      reads the content of URL @racket[u] and produces the first XML
      element as an @racket[xexpr?] @list[s ...] If possible, the function interprets the HTML at
      the specified URL as XML. The function returns #@racket[f] if the web page
      does not exist (404)}]))

@section{Web Functions}

All functions that read a web-based XML consume a URL and possibly
 additional arguments. They assume that the computer is connected to
 specified part of the web, though they tolerate non-existent web pages
 (404 errors) 

@itemlist[
@; -----------------------------------------------------------------------------
@item{
@reading/web[read-xexpr/web xexpr?]{including whitespace such as tabs and newlines}}

@item{
@reading/web[read-plain-xexpr/web xexpr?]{without whitespace}}

@item{
@defproc[(url-exists? [u string?]) boolean?]{ensures that the specified URL
@racket[u] does not produce a 404 error.}}

@item{
@defproc[(xexpr? [u any?]) boolean?]{checks that the given value is an
X-expression in the following sense: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
 ;   @deftech{Xexpr} is one of: 
 ;   -- @racket[symbol?] 
 ;   -- @racket[string?] 
 ;   -- @racket[number?] 
 ;   -- @racket[(cons symbol? (cons [List-of #, @tech{Attribute}] [List-of #, @tech{Xexpr}]))]
 ;   -- @racket[(cons symbol? [List-of #, @tech{Xexpr}])]
 ;
 ;   @deftech{Attribute} is:
 ;      @racket[(list symbol? string?)]
 ;   @racket[(list 'a "some text")] is called an a-Attribute 
 ;   and "some text" is a's value.
))
@;%
 Note that full Racket uses a wider notion of X-expression. 
 }}

@item{
@defproc[(xexpr-as-string [x xexpr?]) string?]{renders the given
X-expression as a string.}}

@item{
@defproc[(url-html-neighbors [u string?]) (listof string?)]{retrieves the
content of URL @racket[u] and produces the list of all URLs that refer to
.html pages via an @tt{<a>} tag.}}

]




@; -----------------------------------------------------------------------------
@section{Testing}

@defform[(simulate-file process str ...)]{
 simulates a file system for the function @racket[process], which reads a
 file and may produce one. Note: this form is under development and will be
 documented in a precise manner after it is finalized and useful for a wide
 audience.} 
