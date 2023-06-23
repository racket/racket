This directory contains source programs and fragments for wrapper
executables used to start/embed Racket. The programs and fragments are
used both for the Racket CS and BC implementations.

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

Under Windows, executables also embed a path to DLLs. For more
information, see "..\worksp\README.txt".

Paths to all other installation directories are found through a
"config.rktd" file in a "configuration directory". Most paths that are
specified in "config.rktd" have default values that are relative to
the main collection directory. The paths of the configuration
directory and main collection directory thus work together to
determine a Racket configuration. A configuration directory may be
embedded in an executable using the "coNFIg dIRECTORy:" tag similarly
to "coLLECTs dIRECTORy:".

As an alternative to editing an executable directly, the
`create-embedding-executable` procedure from `compiler/embed` can be
used to change the embedded path. For example, the following program
clones the Racket executable to "/tmp/mz" and changes the embedded
path in the clone to "/tmp/collects":

 (require compiler/embed)
 (create-embedding-executable "/tmp/mz" #:collects-path "/tmp/collects")

Similarly, `raco exe` mode accepts flags like `--collects-path` and
`--config-path` to set these paths in the generated executable.

See the "Installation Configuration and Search Paths" section in the
`raco` documentation for more details.
