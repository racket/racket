#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "mz:concurrency" #:style 'toc]{Concurrency}

PLT Scheme supports multiple threads of control within a
program. Threads run concurrently, in the sense that one thread can
preempt another without its cooperation, but threads currently all run
on the same processor (i.e., the same underlying OS process and
thread).

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["threads.scrbl"]
@include-section["sync.scrbl"]
@include-section["thread-local.scrbl"]


