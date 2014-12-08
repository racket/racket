#lang scribble/doc
@(require "utils.rkt" (for-label ffi/winapi))

@title[#:tag "winapi"]{Windows API Helpers}

@defmodule[ffi/winapi]

@defthing[win64? boolean?]{

Indicates whether the current platform is 64-bit Windows: @racket[#t]
if so, @racket[#f] otherwise.}


@defthing[winapi (or/c 'stdcall 'default)]{

Suitable for use as an ABI specification for a Windows API function:
@racket['stdcall] on 32-bit Windows, @racket['default] on 64-bit
Windows or any other platform.}
