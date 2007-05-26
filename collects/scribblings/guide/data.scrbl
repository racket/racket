#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "datatypes" #:style 'toc]{Built-In and Programmer-Defined Datatypes}

The @seclink["to-scheme"]{little Scheme section} introduced some of
Scheme's built-in datatype: numbers, booleans, strings, lists, and
procedures. This section provides a more complete coverage of the
built-in datatypes, and also introduces the @scheme[define-struct]
form for creating your own datatypes. We defer a discussion of the
class-based object system to @secref["classes"].

@local-table-of-contents[]

@section{Booleans}

Scheme has two distinguished constants to represent boolean values:
@scheme[#t] for true and @scheme[#f] for false. Uppercase
@schemevalfont{#T} and @schemevalfont{#F} are parsed as the same
values, but the lowercase forms are preferred.

The @scheme[boolean?] procedure recognizes the two boolean
constants. In the result of a test expression for @scheme[if],
@scheme[cond], @scheme[and], @scheme[or], etc., however, any value
other than @scheme[#f] counts as true.

@examples[
(= 2 (+ 1 1))
(boolean? #t)
(boolean? #f)
(boolean? "no")
(if "no" 1 0)
]

@include-section["numbers.scrbl"]
@include-section["chars.scrbl"]
@include-section["char-strings.scrbl"]
@include-section["byte-strings.scrbl"]
@include-section["vectors.scrbl"]
