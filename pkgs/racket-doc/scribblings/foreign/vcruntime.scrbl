#lang scribble/doc
@(require "utils.rkt" (for-label ffi/winapi))

@title[#:tag "vcruntime"]{Windows VCRuntime Librraies}

@defmodule[ffi/vcruntime]{ The @racketmodname[ffi/vcruntime] module
 has no exports, but @racket[require]ing it has the side effect on Windows of
 loading @filepath{vcruntime140.dll}, loading @filepath{vcruntime140_1.dll},
 and declaring those DLLs to be carried along by @exec{raco exe} and
 @exec{raco dist}.}

To avoid conflicts, use @racketmodname[ffi/vcruntime] instead of
including your own copies of @filepath{vcruntime140.dll} and
@filepath{vcruntime140_1.dll} in a racket package.

@history[#:added "9.3.0.2"]
