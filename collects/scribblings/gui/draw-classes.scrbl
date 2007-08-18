#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]
@require["mred-classes.ss"]

@title[#:style '(toc quiet)]{Drawing Classes}

@local-table-of-contents[]

@include-class[bitmap%]
@include-class[bitmap-dc%]
@include-class[brush%]
@include-class[brush-list%]
@include-class[color%]
@include-class[color-database<%>]
@include-class[dc<%>]
@include-class[dc-path%]
@include-class[font%]
@include-class[font-list%]
@include-class[font-name-directory<%>]
@include-class[gl-config%]
@include-class[gl-context<%>]
@include-class[pen%]
@include-class[pen-list%]
@include-class[point%]
@include-class[post-script-dc%]
@include-class[printer-dc%]
@include-class[ps-setup%]
@include-class[region%]
