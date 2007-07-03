#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{PLT Scheme Reference}

This manual defines the core PLT Scheme language and describes its
most prominent libraries. The companion manual
@italic{@link["../guide/index.html"]{A Guide to PLT Scheme}} provides
a friendlier (though less precise and less complete) overview of the
language.

@bold{This reference describes a potential future version of PLT Scheme.
      It does not match the current implementation.}

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["syntax-model.scrbl"]
@include-section["syntax.scrbl"]
@include-section["data.scrbl"]
@include-section["struct.scrbl"]
@include-section["control.scrbl"]
@include-section["concurrency.scrbl"]
@include-section["security.scrbl"]
@include-section["io.scrbl"]
@include-section["os.scrbl"]

@;------------------------------------------------------------------------

@index-section["mzscheme-index"]
