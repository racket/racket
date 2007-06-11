#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:characters"]{Characters}

@guideintro["guide:characters"]{characters}

A @pidefterm{character} corresponds to a Unicode scalar value (i.e., a
Unicode code point that is not a surrogate).

@; ----------------------------------------
@section{Classifications}

@defproc[(char-alphabetic? [char character?]) boolean?]{
Returns @scheme[#t] if @scheme[char] is alphabetic...
}

@defproc[(char-whitespace? [char character?]) boolean?]{
Returns @scheme[#t] if @scheme[char] is whitespace...
}