#lang scribble/doc
@(require "utils.rkt"
          (for-label racket/system))

@title{Subprocesses}

On Unix and Mac OS X, subprocess handling involves
@as-index[@cpp{fork}], @as-index[@cpp{waitpid}], and
@as-index[@cpp{SIGCHLD}], which creates a variety of issues within an
embedding application. On Windows, subprocess handling is more
straightforward, since no @cpp{fork} is required, and since Windows
provides an abstraction that is a close fit to Racket's subprocess
values.

After Racket creates a subprocess via @racket[subprocess] (or
@racket[system], @racket[process], etc.), it periodically polls the
process status using @cpp{waitpid}. If the process is created as its
own group, then the call to @cpp{waitpid} uses the created
subprocess's process ID; for all other subprocesses, polling uses a
single call to @cpp{waitpid} with the first argument as @cpp{0}. Using
@cpp{0}, in particular, can interfere with other libraries in an
embedding context, so Racket refrains from calling @cpp{waitpid} if no
subprocesses are pending.

Racket may or may not rely on a @cpp{SIGCHLD} handler, and it may or
may not block @cpp{SIGCHLD}. Currently, when Racket is compiled to
support @|tech-place|s, Racket blocks @cpp{SIGCHLD} on start up with
the expectation that all created threads have @cpp{SIGCHLD} blocked.
When Racket is not compiled to support @|tech-place|s, then a
@cpp{SIGCHLD} handler is installed.

Using @cpp{fork} in an application that embeds Racket is problematic
for several reasons: Racket may install a @cpp{SIGALRM} handler and
schedule alarms to implement context switches, it may have file
descriptors open that should be closed in a child process, and it may
have changed the disposition of signals such as
@cpp{SIGCHLD}. Consequently, embedding Racket in a process that
@cpp{fork}s is technically not supported; in the future, Racket may
provide better support for such applications.
