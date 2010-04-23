#lang scribble/doc
@(require "mz.ss"
          (for-label racket/port))

@title[#:tag "encodings"]{Encodings and Locales}

When a port is provided to a character-based operation, such as
@scheme[read-char] or @scheme[read], the port's bytes are read and
interpreted as a UTF-8 encoding of characters. Thus, reading a single
character may require reading multiple bytes, and a procedure like
@scheme[char-ready?] may need to peek several bytes into the stream to
determine whether a character is available. In the case of a byte
stream that does not correspond to a valid UTF-8 encoding, functions
such as @scheme[read-char] may need to peek one byte ahead in the
stream to discover that the stream is not a valid encoding.

When an input port produces a sequence of bytes that is not a valid
UTF-8 encoding in a character-reading context, then bytes that
constitute an invalid sequence are converted to the character
@schemevalfont{#\uFFFD}. Specifically, bytes 255 and 254 are always converted
to @schemevalfont{#\uFFFD}, bytes in the range 192 to 253 produce
@schemevalfont{#\uFFFD} when they are not followed by bytes that form a valid
UTF-8 encoding, and bytes in the range 128 to 191 are converted to
@schemevalfont{#\uFFFD} when they are not part of a valid encoding that was
started by a preceding byte in the range 192 to 253. To put it another
way, when reading a sequence of bytes as characters, a minimal set of
bytes are changed to the encoding of @schemevalfont{#\uFFFD} so that the
entire sequence of bytes is a valid UTF-8 encoding.

See @secref["bytestrings"] for procedures that facilitate
conversions using UTF-8 or other encodings. See also
@scheme[reencode-input-port] and @scheme[reencode-output-port] for
obtaining a UTF-8-based port from one that uses a different encoding
of characters.


A @deftech{locale} captures information about a user's
culture-specific interpretation of character sequences. In particular,
a locale determines how strings are ``alphabetized,'' how a lowercase
character is converted to an uppercase character, and how strings are
compared without regard to case. String operations such as
@scheme[string-ci=?] are @italic{not} sensitive to the current locale,
but operations such as @scheme[string-locale-ci=?] (see
@secref["strings"]) produce results consistent with the current
locale.

A locale also designates a particular encoding of code-point sequences
into byte sequences. Scheme generally ignores this aspect of the
locale, with a few notable exceptions: command-line arguments passed
to Scheme as byte strings are converted to character strings using the
locale's encoding; command-line strings passed as byte strings to
other processes (through @scheme[subprocess]) are converted to byte
strings using the locale's encoding; environment variables are
converted to and from strings using the locale's encoding; filesystem
paths are converted to and from strings (for display purposes) using
the locale's encoding; and, finally, Scheme provides functions such as
@scheme[string->bytes/locale] to specifically invoke a locale-specific
encoding.

A Unix user selects a locale by setting environment variables, such as
@envvar{LC_ALL}. Under Windows and Mac OS X, the operating system
provides other mechanisms for setting the locale. Within Scheme, the
current locale can be changed by setting the @scheme[current-locale]
parameter. The locale name within Scheme is a string, and the
available locale names depend on the platform and its configuration,
but the @scheme[""] locale means the current user's default locale;
under Windows and Mac OS X, the encoding for @scheme[""] is always
UTF-8, and locale-sensitive operations use the operating system's
native interface. (In particular, setting the @envvar{LC_ALL} and
@envvar{LC_CTYPE} environment variables do not affect the locale
@scheme[""] under Mac OS X. Use @scheme[getenv] and
@scheme[current-locale] to explicitly install the
environment-specified locale, if desired.) Setting the current locale
to @scheme[#f] makes locale-sensitive operations locale-insensitive,
which means using the Unicode mapping for case operations and using
UTF-8 for encoding.

@defparam[current-locale locale (or/c string? #f)]{

A parameter that determines the current @tech{locale} for
procedures such as @scheme[string-locale-ci=?].

When locale sensitivity is disabled by setting the parameter to
@scheme[#f], strings are compared (etc.) in a fully portable manner,
which is the same as the standard procedures. Otherwise, strings are
interpreted according to a locale setting (in the sense of the C
library's @tt{setlocale}). The @scheme[""] locale is always a synonym
for the current machine's default locale, and it is the default.  The
@scheme["C"] locale is also always available; setting the locale to
@scheme["C"] is the same as disabling locale sensitivity with
@scheme[#f] only when string operations are restricted to the first
128 characters. Other locale names are platform-specific.

String or character printing with @scheme[write] is not affected by
the parameter, and neither are symbol case or regular expressions (see
@secref["regexp"]).}
