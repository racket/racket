#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag-prefix '(lib "reference.scrbl" "scribblings" "reference")
       #:tag "top"]{PLT Scheme Reference}

@declare-exporting[big little]

This manual defines the core PLT Scheme language and describes its
most prominent libraries. The companion manual @|Guide| provides a
friendlier (though less precise and less complete) overview of the
language.

@bold{This reference describes a potential future version of PLT Scheme.
      It does not match the current implementation.}

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["syntax-model.scrbl"]
@include-section["syntax.scrbl"]
@include-section["data.scrbl"]
@include-section["struct.scrbl"]
@include-section["class.scrbl"]
@include-section["units.scrbl"]
@include-section["contracts.scrbl"]
@include-section["control.scrbl"]
@include-section["concurrency.scrbl"]
@include-section["macros.scrbl"]
@include-section["io.scrbl"]
@include-section["security.scrbl"]
@include-section["os.scrbl"]
@include-section["memory.scrbl"]
@include-section["running.scrbl"]

@;------------------------------------------------------------------------

@section{To Do}

This chapter provides some temporary hyper-link targets.

@subsection[#:tag "inside-mzscheme"]{Inside MzScheme}
@subsection[#:tag "running-sa"]{Running MzScheme}
@subsection[#:tag "async-channel"]{Asynchronous Channels}
@subsection[#:tag "honu"]{Honu}

@;------------------------------------------------------------------------

@index-section["mzscheme-index"]
