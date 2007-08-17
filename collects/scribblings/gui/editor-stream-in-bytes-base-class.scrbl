#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[editor-stream-in-bytes-base% editor-stream-in-base% ()]{

An @scheme[editor-stream-in-bytes-base%] object can be used to
read editor data from a byte string.


@defconstructor/make[([s bytes?])]{

Creates a stream base that reads from @scheme[s].

}}
