#lang scribble/doc
@(require "common.rkt" (for-label file/resource))

@(define-syntax-rule (compat file section indexed-racket what)
   @elem{For backward compatibility, the
 result is @racket[#f] for platforms other than Windows. The registry
 is @|what| when
 @racket[file] is @racket[#f] and when @racket[section] is 
 @indexed-racket["HKEY_CLASSES_ROOT"],
 @indexed-racket["HKEY_CURRENT_CONFIG"],
 @indexed-racket["HKEY_CURRENT_USER"],
 @indexed-racket["HKEY_LOCAL_MACHINE"], or @indexed-racket["HKEY_USERS"].
 When @racket[file] is @racket[#f] and @racket[section] is not one of
 the special registry strings, then 
 @racket[(build-path (find-system-path 'home-dir) "mred.ini")] 
 is @|what|.})

@title[#:tag "resource"]{Windows Registry}

@defmodule[file/resource]

@defproc[(get-resource [section string?]
                       [entry string?]
                       [value-box (or/c #f (box/c (or/c string? bytes? exact-integer?))) #f]
                       [file (or/c #f path-string?) #f]
                       [#:type type (or/c 'string 'string/utf-16 'bytes 'bytes* 'integer)
                               _derived-from-value-box])
         (or/c #f string? bytes? exact-integer? #t)]{

Gets a value from the Windows registry or an @filepath{.ini}
 file. @compat[file section indexed-racket "read"]

The resource value is keyed on the combination of @racket[section] and
 @racket[entry].  The result is @racket[#f] if no value is found for
 the specified @racket[section] and @racket[entry]. If @racket[value-box]
 is a box, then the result is @racket[#t] if a value is found, and the
 box is filled with the value; when @racket[value-box] is @racket[#f], the result is the found
 value.


Registry values of any format can be extracted. A combination of the
 @racket[type] argument and the type of the resource determines how
 the resource is initially converted to a Racket value:

@itemlist[

 @item{A @tt{REG_SZ} registry value's bytes are first converted to a
       string by a nul-terminated UTF-16 interpretation (not including
       the terminator in the string)---unless @racket[type] is
       @racket['bytes*], in which case the bytes are kept as-is in a
       byte string.}

 @item{A @tt{REG_DWORD} registry value's bytes are first interpreted
       as a 32-bit signed integer, and then the integer is converted
       to a string with @racket[number->string].}

 @item{Any other kind of register value's bytes are kept as a byte
       string.}

]

That initial conversion produces either a string or a byte string. The
requested @racket[type] might then trigger an additional
transformation:

@itemlist[

 @item{@racket['string]: a string is kept as-is, but a byte string are
       converted to a string using @racket[bytes->string/utf-8]. Note
       that a UTF-8 conversion is @emph{not} appropriate for some
       resource types, such as @tt{REG_EXPAND_SZ}; use
       @racket['string/utf-16], instead.}

 @item{@racket['string/utf-16]: a string is kept as-is, but a byte
       string is converted to a string by a nul-terminated UTF-16
       interpretation (omitting the nul terminator from the string).}

 @item{@racket['bytes]: a byte string is kept as-is, but a string is
       converted using @racket[string->bytes/utf-8]. Note that this
       conversion does not produce the original bytes for a
       @tt{REG_SZ} resource; use @racket['bytes*], instead, since that
       avoids the initial conversion to a string.}

 @item{@racket['bytes*]: the same as @racket['bytes], but
       @racket['bytes*] affects the initial conversion for a
       @tt{REG_SZ} resource.}

 @item{@racket['integer]: a string is converted to a number using
       @racket[string->number], and a byte string is converted by
       composing @racket[bytes->string/utf-8] with
       @racket[string->number].}

]

If @racket[value-box] is a box, then the default @racket[type] is
 derived from the initial box content: @racket['string],
 @racket['bytes], or @racket['integer]. Otherwise, the default
 @racket[type] is @racket['string].

Resources from @filepath{.ini} files are always strings, and are
converted like @tt{REG_SZ} registry values.

To get the ``default'' value for a registry entry, use a trailing
backslash. For example, the following expression gets a command line
for starting a browser:

@racketblock[
  (get-resource "HKEY_CLASSES_ROOT" 
                "htmlfile\\shell\\open\\command\\")
]

@history[#:changed "8.0.0.10" @elem{Added @racket['sting/utf-16]
         and @racket['bytes*] options for @racket[type].}]}

@defproc[(write-resource [section string?]
                         [entry string?]
                         [value (or/c string? bytes? exact-integer?)]
                         [file (or/c path-string? #f) #f]
                         [#:type type (or/c 'string 'expand-string 'bytes 'dword
                                            'bytes/string 'bytes/expand-string)
                                 'string]
                         [#:create-key? create-key? any/c #f])
         boolean?]{

Write a value to the Windows registry or an @filepath{.ini}
 file. @compat[file section racket "written"]

The resource value is keyed on the combination of @racket[section] and
 @racket[entry]. If @racket[create-key?] is false when writing to the
 registry, the resource entry must already exist, otherwise the write
 fails. If writing to the registry fails (due to a permissions issue or
 when the entry does not exist and @racket[create-key?] is false), then
 @racket[(build-path (find-system-path 'home-dir) "mred.ini")] is written
 to instead. The result is @racket[#f] if the @filepath{.ini} write fails
 or @racket[#t] if either the registry write or the @filepath{.ini} write
 succeeds.

The @racket[type] argument determines both the format of the value
 written to the registry and its conversion of the to bytes:

@itemlist[

 @item{@racket['string]: writes as @tt{REG_SZ}, where a string
       @racket[value] is converted to UTF-16 bytes adding a nul
       terminator. A byte string @racket[value] is converted first
       with @racket[bytes->string/utf-8], and an integer
       @racket[value] is first converted with @racket[number->string],
       and then the result in each case is treated like a string. Note
       that @racket['string] is unlikely to be a useful conversion for
       a byte string @racket[value]; use @racket['bytes/string],
       instead.}

 @item{@racket['expand-string]: like @racket['string], but written as
       @tt{REG_EXPAND_SZ}. Note that @racket['expand-string] is
       unlikely to be a useful conversion for a byte string
       @racket[value]; use @racket['bytes/expand-string], instead.}

 @item{@racket['bytes]: @tt{REG_BINARY}, where a byte string
       @racket[value] is written as-is, a string @racket[value] is
       converted to bytes by @racket[string->bytes/utf-8], and an
       integer @racket[value] is converted to bytes by composing
       @racket[number->string] with @racket[string->bytes/utf-8].}

 @item{@racket['bytes/string]: writes as @tt{REG_SZ}, where a byte
       string @racket[value] is written as-is (unlike
       @racket['string], so the byte string must be a UTF-16 encoding
       with a nul terminator), a string @racket[value]
       is converted to UTF-16 bytes adding a nul terminator, and an
       integer @racket[value] is converted to a string with
       @racket[number->string] and then to UTF-16 bytes adding a nul
       terminator.}

 @item{@racket['bytes/expand-string]: like @racket['bytes/string], but
       writes as @tt{REG_EXPAND_SZ}.}

 @item{@racket['dword]: writes as @tt{REG_DWORD}, where an integer
       @racket[value] is converted to 32-bit signed integer bytes, a
       string @racket[value] is converted with @racket[string->number]
       and then the same as an integer, and a byte string
       @racket[value] is converted by composing
       @racket[bytes->string/utf-8] with @racket[string->number] and
       then the same as an integer.}

]

When writing to an @filepath{.ini} file, the format is always a
 string, independent of @racket[type].

@history[#:changed "8.0.0.10" @elem{Added @racket['expand-string],
         @racket['bytes/string], and @racket['bytes/expand-string]
         options for @racket[type].}]}
