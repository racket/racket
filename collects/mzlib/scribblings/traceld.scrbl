#lang scribble/doc
@(require "common.rkt")

@mzlib[#:mode title traceld]

The @schememodname[mzlib/traceld] library does not provide any
bindings. Instead, @schememodname[mzlib/traceld] is @scheme[require]d
for its side-effects. 

The @schememodname[mzlib/traceld] library installs a new load handler
(see @scheme[current-load]) and load-extension handler (see
@scheme[current-load-extension]) to print information about the files
that are loaded. These handlers chain to the current handlers to
perform the actual loads. Trace output is printed to the port that is
the current error port (see @scheme[current-error-port]) when the
library is instantiated.

Before a file is loaded, the tracer prints the file name and ``time''
(as reported by the procedure @scheme[current-process-milliseconds])
when the load starts. Trace information for nested loads is printed
with indentation. After the file is loaded, the file name is printed
with the ``time'' that the load completed.
