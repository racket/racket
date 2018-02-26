This thread implementation can be run in a host Racket with `make
demo`, but it's meant to be compiled for use in Racket on Chez Scheme;
see "../cs/README.txt".

Core engine support must be provided by a more primitive layer. The
more primitive layer must also provide `break-enabled-key` and special
handling for looking up a mark with that key so that an egine-specific
default thread cell is produced.
