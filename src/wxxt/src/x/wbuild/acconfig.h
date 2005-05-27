/* Special definitions, processed by autoheader.
   Copyright (C) 1995 Free Software Foundation.
   Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1995.  */

/* Default value for alignment of strings in .mo file.  */
#define DEFAULT_ALIGNMENT 1

#ifndef __P
# if __STDC__
#  define __P(args) args
# else
#  define __P(args) ()
# endif
#endif

@TOP@

/* Define to the name of the distribution.  */
#undef PACKAGE

/* Define to the version of the distribution.  */
#undef VERSION

/* Define if you have obstacks.  */
#undef HAVE_OBSTACK

/* Define if <stddef.h> defines ptrdiff_t.  */
#undef HAVE_PTRDIFF_T

/* Define if your locale.h file contains LC_MESSAGES.  */
#undef HAVE_LC_MESSAGES

/* Define if you have the parse_printf_format function.  */
#undef HAVE_PARSE_PRINTF_FORMAT

/* Define to 1 if NLS is requested.  */
#undef ENABLE_NLS

/* Define as 1 if you have catgets and don't want to use GNU gettext.  */
#undef HAVE_CATGETS

/* Define as 1 if you have gettext and don't want to use GNU gettext.  */
#undef HAVE_GETTEXT

/* Define as 1 if you have the stpcpy function.  */
#undef HAVE_STPCPY

@BOTTOM@

/* A file name cannot consist of any character possible.  INVALID_PATH_CHAR
   contains the characters not allowed.  */
#ifndef MSDOS
#define	INVALID_PATH_CHAR "\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\20\21\22\23\24\25\26\27\30\31\32\33\34\35\36\37 \177/"
#else
/* Something like this for MSDOG.  */
#define	INVALID_PATH_CHAR "\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\20\21\22\23\24\25\26\27\30\31\32\33\34\35\36\37 \177\\:."
#endif

/* Length from which starting on warnings about too long strings are given.
   Several systems have limits for strings itself, more have problems with
   strings in their tools (important here: gencat).  1024 bytes is a
   conservative limit.  Because many translation let the message size grow
   (German translations are always bigger) choose a length < 1024.  */
#define WARN_ID_LEN 900

/* This is the page width for the message_print function.  It should not
   be set to more than 80 characters.  It is used to wrap the msgid and
   msgstr strings, and also to wrap the file position (#:) comments.  */
#define PAGE_WIDTH 999	/* For now!  */

/* enable dmalloc debugging */
#undef WITH_DMALLOC
