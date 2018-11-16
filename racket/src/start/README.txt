This directory constaint source programs and fragments for wrapper
executables used to start/embed Racket. The programs and fragments are
used both for the traditional Racket virtual machine and Racket on
Chez Scheme.

========================================================================
 Embedded Paths in the Executables
========================================================================

On all platforms, the Racket and GRacket executables embed a path to
the main "collects" directory of library collections. This path can be
relative to the executable. Multiple paths can be provided, in which
case the first path is the main "collects" path, and additional paths
are placed before the main path (but after a user-specific "collects"
path) in the default collection path list.

The paths are embedded in the executable immediately after a special
"coLLECTs dIRECTORy:" tag. Each path must be NUL terminated, the
entire list of paths must end with an additional NUL terminator, and
the overall list must be less than 1024 bytes long.

As an alternative to editing an executable directly, the
`create-embedding-executable` procedure from `compiler/embed` can be
used to change the embedded path. For example, the following program
clones the Racket executable to "/tmp/mz" and changes the embedded
path in the clone to "/tmp/collects":

 (require compiler/embed)
 (create-embedding-executable "/tmp/mz" #:collects-path "/tmp/collects")

Similarly, `raco exe` mode accepts a `--collects` flag to set the
collection path in the generated executable.

Under Windows, executables also embed a path to DLLs. For more
information, see "..\worksp\README.txt".

Paths to all other installation directories are found through the
"config.rkt" library of the "config" collection. Search the
documentation for "config search paths" for more information.
