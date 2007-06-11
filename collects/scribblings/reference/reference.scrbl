#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{PLT Scheme Reference Manual}

This manual defines the core PLT Scheme language and describes its
most prominent libraries. The companion manual
@italic{@link["../guide/index.html"]{A Guide to PLT Scheme}} provides
a friendlier (though less precise and less complete) overview of the
language.

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["read.scrbl"]
@include-section["macros.scrbl"]
@include-section["syntax.scrbl"]
@include-section["derived.scrbl"]
@include-section["data.scrbl"]

@;------------------------------------------------------------------------

@section["Input and Output"]

@subsection[#:tag "mz:char-input"]{Form Bytes to Characters}

@;------------------------------------------------------------------------

@section{Platform-Specific Path Conventions}

@subsection[#:tag "mz:unix-path"]{Unix and Mac OS X Paths}

@subsection[#:tag "mz:windows-path"]{Windows Paths}

@;------------------------------------------------------------------------

@index-section["mzscheme-index"]
