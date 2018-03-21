This directory implements the port, path, encoding, printing, and
formatting layer. It can be run in a host Racket with `make demo`,
which is useful for development and debugging, but it's meant to be
compiled for use in Racket on Chez Scheme; see "../cs/README.txt".

Core error support must be provided as a more primitive layer,
including the exception structures and error functions that do not
involve formatting, such as `raise-argument-error`. The more primitive
layer should provide a `error-value->string-handler` paramemeter, but
this layer sets that parameter (so the primitive error function slike
`raise-argument-error` won't work right until this layer is loaded).

Thread and event support is similarly provided as a more primitive
layer. Running `make demo` doesn't rely on that, while running `make
demo-thread` uses the thread implementation in "../thread" to
demonstrate cooperation between the layers.
