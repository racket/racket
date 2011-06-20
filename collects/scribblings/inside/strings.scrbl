#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "im:encodings"]{String Encodings}

The @cpp{scheme_utf8_decode} function decodes a @cpp{char} array as
UTF-8 into either a UCS-4 @cpp{mzchar} array or a UTF-16 @cpp{short}
array. The @cpp{scheme_utf8_encode} function encodes either a UCS-4
@cpp{mzchar} array or a UTF-16 @cpp{short} array into a UTF-8
@cpp{char} array.

These functions can be used to check or measure an encoding or
decoding without actually producing the result decoding or encoding,
and variations of the function provide control over the handling of
decoding errors.

@function[(int scheme_utf8_decode
           [const-unsigned-char* s]
           [int start]
           [int end]
           [mzchar* us]
           [int dstart]
           [int dend]
           [intptr_t* ipos]
           [char utf16]
           [int permissive])]{

Decodes a byte array as UTF-8 to produce either Unicode code points
 into @var{us} (when @var{utf16} is zero) or UTF-16 code units into
 @var{us} cast to @cpp{short*} (when @var{utf16} is non-zero). No nul
 terminator is added to @var{us}.

The result is non-negative when all of the given bytes are decoded,
 and the result is the length of the decoding (in @cpp{mzchar}s or
 @cpp{short}s). A @cpp{-2} result indicates an invalid encoding
 sequence in the given bytes (possibly because the range to decode
 ended mid-encoding), and a @cpp{-3} result indicates that decoding
 stopped because not enough room was available in the result string.

The @var{start} and @var{end} arguments specify a range of @var{s} to
 be decoded. If @var{end} is negative, @cpp{strlen(@var{s})} is used
 as the end.

If @var{us} is @cpp{NULL}, then decoded bytes are not produced, but
 the result is valid as if decoded bytes were written. The
 @var{dstart} and @var{dend} arguments specify a target range in
 @var{us} (in @cpp{mzchar} or @cpp{short} units) for the decoding; a
 negative value for @var{dend} indicates that any number of bytes can
 be written to @var{us}, which is normally sensible only when @var{us}
 is @cpp{NULL} for measuring the length of the decoding.

If @var{ipos} is non-@cpp{NULL}, it is filled with the first undecoded
 index within @var{s}. If the function result is non-negative, then
 @cpp{*@var{ipos}} is set to the ending index (with is @var{end} if
 non-negative, @cpp{strlen(@var{s})} otherwise). If the result is
 @cpp{-1} or @cpp{-2}, then @cpp{*@var{ipos}} effectively indicates
 how many bytes were decoded before decoding stopped.

If @var{permissive} is non-zero, it is used as the decoding of bytes
 that are not part of a valid UTF-8 encoding. Thus, the function
 result can be @cpp{-2} only if @var{permissive} is @cpp{0}.

This function does not allocate or trigger garbage collection.}

@function[(int scheme_utf8_decode_as_prefix
           [const-unsigned-char* s]
           [int start]
           [int end]
           [mzchar* us]
           [int dstart]
           [int dend]
           [intptr_t* ipos]
           [char utf16]
           [int permissive])]{

Like @cpp{scheme_utf8_decode}, but the result is always the number
 of the decoded @cpp{mzchar}s or @cpp{short}s. If a decoding error is
 encountered, the result is still the size of the decoding up until
 the error.}

@function[(int scheme_utf8_decode_all
           [const-unsigned-char* s]
           [int len]
           [mzchar* us]
           [int permissive])]{

Like @cpp{scheme_utf8_decode}, but with fewer arguments. The
 decoding produces UCS-4 @cpp{mzchar}s. If the buffer @var{us} is
 non-@cpp{NULL}, it is assumed to be long enough to hold the decoding
 (which cannot be longer than the length of the input, though it may
 be shorter). If @var{len} is negative, @cpp{strlen(@var{s})} is used
 as the input length.}


@function[(int scheme_utf8_decode_prefix
           [const-unsigned-char* s]
           [int len]
           [mzchar* us]
           [int permissive])]{

Like @cpp{scheme_utf8_decode}, but with fewer arguments. The
 decoding produces UCS-4 @cpp{mzchar}s. If the buffer @var{us}
 @bold{must} be non-@cpp{NULL}, and it is assumed to be long enough to hold the
 decoding (which cannot be longer than the length of the input, though
 it may be shorter). If @var{len} is negative, @cpp{strlen(@var{s})}
 is used as the input length.

In addition to the result of @cpp{scheme_utf8_decode}, the result
 can be @cpp{-1} to indicate that the input ended with a partial
 (valid) encoding. A @cpp{-1} result is possible even when
 @var{permissive} is non-zero.}

@function[(mzchar* scheme_utf8_decode_to_buffer
           [const-unsigned-char* s]
           [int len]
           [mzchar* buf]
           [int blen])]{

Like @cpp{scheme_utf8_decode_all} with @var{permissive} as @cpp{0},
 but if @var{buf} is not large enough (as indicated by @var{blen}) to
 hold the result, a new buffer is allocated. Unlike other functions,
 this one adds a nul terminator to the decoding result. The function
 result is either @var{buf} (if it was big enough) or a buffer
 allocated with @cpp{scheme_malloc_atomic}.}

@function[(mzchar* scheme_utf8_decode_to_buffer_len
           [const-unsigned-char* s]
           [int len]
           [mzchar* buf]
           [int blen]
           [intptr_t* ulen])]{

Like @cpp{scheme_utf8_decode_to_buffer}, but the length of the
 result (not including the terminator) is placed into @var{ulen} if
 @var{ulen} is non-@cpp{NULL}.}

@function[(int scheme_utf8_decode_count
           [const-unsigned-char* s]
           [int start]
           [int end]
           [int* state]
           [int might_continue]
           [int permissive])]{

Like @cpp{scheme_utf8_decode}, but without producing the decoded
 @cpp{mzchar}s, and always returning the number of decoded
 @cpp{mzchar}s up until a decoding error (if any). If
 @var{might_continue} is non-zero, the a partial valid encoding at
 the end of the input is not decoded when @var{permissive} is also
 non-zero.

If @var{state} is non-@cpp{NULL}, it holds information about partial
 encodings; it should be set to zero for an initial call, and then
 passed back to @cpp{scheme_utf8_decode} along with bytes that
 extend the given input (i.e., without any unused partial
 encodings). Typically, this mode makes sense only when
 @var{might_continue} and @var{permissive} are non-zero.}


@function[(int scheme_utf8_encode
           [const-mzchar* us]
           [int start]
           [int end]
           [unsigned-char* s]
           [int dstart]
           [char utf16])]{

Encodes the given UCS-4 array of @cpp{mzchar}s (if @var{utf16} is
 zero) or UTF-16 array of @cpp{short}s (if @var{utf16} is non-zero)
 into @var{s}. The @var{end} argument must be no less than
 @var{start}.

The array @var{s} is assumed to be long enough to contain the
 encoding, but no encoding is written if @var{s} is @cpp{NULL}. The
 @var{dstart} argument indicates a starting place in @var{s} to hold
 the encoding. No nul terminator is added to @var{s}.

The result is the number of bytes produced for the encoding (or that
 would be produced if @var{s} was non-@cpp{NULL}). Encoding never
 fails.

This function does not allocate or trigger garbage collection.}

@function[(int scheme_utf8_encode_all
           [const-mzchar* us]
           [int len]
           [unsigned-char* s])]{

Like @cpp{scheme_utf8_encode} with @cpp{0} for @var{start},
 @var{len} for @var{end}, @cpp{0} for @var{dstart} and @cpp{0} for
 @var{utf16}.}
 

@function[(char* scheme_utf8_encode_to_buffer
           [const-mzchar* s]
           [int len]
           [char* buf]
           [int blen])]{

Like @cpp{scheme_utf8_encode_all}, but the length of @var{buf} is
 given, and if it is not long enough to hold the encoding, a buffer is
 allocated. A nul terminator is added to the encoded array. The result
 is either @var{buf} or an array allocated with
 @cpp{scheme_malloc_atomic}.}

@function[(char* scheme_utf8_encode_to_buffer_len
           [const-mzchar* s]
           [int len]
           [char* buf]
           [int blen]
           [intptr_t* rlen])]{

Like @cpp{scheme_utf8_encode_to_buffer}, but the length of the
 resulting encoding (not including a nul terminator) is reported in
 @var{rlen} if it is non-@cpp{NULL}.}


@function[(unsigned-short* scheme_ucs4_to_utf16
           [const-mzchar* text]
           [int start]
           [int end]
           [unsigned-short* buf]
           [int bufsize]
           [intptr_t* ulen]
           [int term_size])]{

Converts a UCS-4 encoding (the indicated range of @var{text}) to a
 UTF-16 encoding. The @var{end} argument must be no less than
 @var{start}.

A result buffer is allocated if @var{buf} is not long enough (as
 indicated by @var{bufsize}). If @var{ulen} is non-@cpp{NULL}, it is
 filled with the length of the UTF-16 encoding. The @var{term_size}
 argument indicates a number of @cpp{short}s to reserve at the end of
 the result buffer for a terminator (but no terminator is actually
 written).}

@function[(mzchar* scheme_utf16_to_ucs4
           [const-unsigned-short* text]
           [int start]
           [int end]
           [mzchar* buf]
           [int bufsize]
           [intptr_t* ulen]
           [int term_size])]{

Converts a UTF-16 encoding (the indicated range of @var{text}) to a
 UCS-4 encoding. The @var{end} argument must be no less than
 @var{start}.

A result buffer is allocated if @var{buf} is not long enough (as
 indicated by @var{bufsize}). If @var{ulen} is non-@cpp{NULL}, it is
 filled with the length of the UCS-4 encoding. The @var{term_size}
 argument indicates a number of @cpp{mzchar}s to reserve at the end of
 the result buffer for a terminator (but no terminator is actually
 written).}
