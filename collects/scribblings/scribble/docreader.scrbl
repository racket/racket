#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "bnf.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "docreader"]{Document Reader}

The @file{docreader.ss} module is suitable for use with
@schemefont{#reader} at the beginning of a file. It reads the entire
file with @scheme[read-inside-syntax] from Scribble's
@file{reader.ss}, and then wraps the result with @scheme[(module #,
@nonterm{name} (lib "doclang.ss" "scribble") ...)], where
@nonterm{name} is derived from the enclosing file's name.
