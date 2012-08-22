#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "threadgroups"]{Thread Groups}

A @deftech{thread group} is a collection of threads and other thread
groups that have equal claim to the CPU. By nesting thread groups and
by creating certain threads within certain groups, a programmer can
control the amount of CPU allocated to a set of threads. Every thread
belongs to a thread group, which is determined by the
@racket[current-thread-group] parameter when the thread is
created. Thread groups and custodians (see @secref["custodians"])
are independent.

The root thread group receives all of the CPU that the operating
system gives Racket. Every thread or nested group in a particular
thread group receives equal allocation of the CPU (a portion of the
group's access), although a thread may relinquish part of its
allocation by sleeping or synchronizing with other processes.

@defproc[(make-thread-group [group thread-group? (current-thread-group)]) 
         thread-group?]{

Creates a new thread group that belongs to @racket[group].}


@defproc[(thread-group? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a thread group value, @racket[#f]
otherwise.}


@defparam[current-thread-group group thread-group?]{

A @tech{parameter} that determines the thread group for newly created
threads.}
