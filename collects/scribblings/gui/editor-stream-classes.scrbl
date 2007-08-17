#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]

@title[#:style '(toc quiet)]{Editor Stream Class Reference}

@local-table-of-contents[]

@require["editor-data-class.scrbl"]
@require["editor-data-class-class.scrbl"]
@require["editor-data-class-list-intf.scrbl"]
@require["editor-stream-in-class.scrbl"]
@require["editor-stream-in-base-class.scrbl"]
@require["editor-stream-in-bytes-base-class.scrbl"]
@require["editor-stream-out-class.scrbl"]
@require["editor-stream-out-base-class.scrbl"]
@require["editor-stream-out-bytes-base-class.scrbl"]
@require["snip-class-class.scrbl"]
@require["snip-class-list-intf.scrbl"]

@include-class[editor-data%]
@include-class[editor-data-class%]
@include-class[editor-data-class-list<%>]
@include-class[editor-stream-in%]
@include-class[editor-stream-in-base%]
@include-class[editor-stream-in-bytes-base%]
@include-class[editor-stream-out%]
@include-class[editor-stream-out-base%]
@include-class[editor-stream-out-bytes-base%]
@include-class[snip-class%]
@include-class[snip-class-list<%>]
