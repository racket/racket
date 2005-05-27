// bstr.cxx -- BSTR utility functions

#include <windows.h>

#include "escheme.h"

// fwd ref
BSTR schemeToBSTR (Scheme_Object * obj);

static
LPWSTR schemeUCS4ToUTF16 (const mzchar * buffer, int nchars, long * result_length)
{
  return scheme_ucs4_to_utf16 (buffer, 0, nchars, NULL, 0, result_length, 0);
}

static
LPWSTR schemeUTF8ToUTF16 (const unsigned char * buffer, int buflen, long * result_length)
{
  long nchars;
  mzchar * ucs4 = scheme_utf8_decode_to_buffer_len (buffer, buflen, NULL, 0, &nchars);
  return schemeUCS4ToUTF16 (ucs4, nchars, result_length);
}

static
LPWSTR schemeByteStringToWideChar (Scheme_Object * obj, long * result_length)
{
  return
      schemeUTF8ToUTF16 ((unsigned char *)SCHEME_BYTE_STR_VAL (obj),
			 SCHEME_BYTE_STRLEN_VAL (obj),
			 result_length);
}

static
LPWSTR schemeCharStringToWideChar (Scheme_Object * obj, long * result_length)
{
  return
      scheme_ucs4_to_utf16 (SCHEME_CHAR_STR_VAL (obj),
			    0, SCHEME_CHAR_STRLEN_VAL (obj),
			    NULL, 0,
			    result_length, 0);
}

static
LPWSTR schemeSymbolToWideChar (Scheme_Object * obj, long * result_length)
{
  return
      schemeUTF8ToUTF16 ((unsigned char *)SCHEME_SYM_VAL (obj),
			 SCHEME_SYM_LEN (obj),
			 result_length);
}

static
BSTR schemeByteStringToBSTR (Scheme_Object * obj)
{
  long nchars;
  LPCWSTR widestring = schemeByteStringToWideChar (obj, &nchars);
  return SysAllocStringLen (widestring, nchars);
}

static
BSTR schemeCharStringToBSTR (Scheme_Object * obj)
{
  long nchars;
  LPCWSTR widestring = schemeCharStringToWideChar (obj, &nchars);
  return SysAllocStringLen (widestring, nchars);
}

static
BSTR schemeSymbolToBSTR (Scheme_Object * obj)
{
  return
      schemeToBSTR (scheme_make_sized_offset_utf8_string (SCHEME_SYM_VAL(obj),
							  0,
							  SCHEME_SYM_LEN(obj)));
}

static
LPSTR schemeWideStringToMultiByte (LPCWSTR string, long nchars)
{
  int chars_needed = WideCharToMultiByte (CP_ACP, 0, string, nchars, NULL, 0, NULL, NULL);
  LPSTR result = (LPSTR) scheme_malloc_atomic (chars_needed + 1);
  WideCharToMultiByte (CP_ACP, 0, string, nchars, result, chars_needed, NULL, NULL);
  result [chars_needed] = '\0';
  return result;
}

static
LPSTR schemeByteStringToMultiByte (Scheme_Object * obj)
{
  long nchars;
  LPCWSTR unicode = schemeByteStringToWideChar (obj, &nchars);
  return schemeWideStringToMultiByte (unicode, nchars);
}

static
LPSTR schemeCharStringToMultiByte (Scheme_Object * obj)
{
  long nchars;
  LPCWSTR unicode = schemeCharStringToWideChar (obj, &nchars);
  return schemeWideStringToMultiByte (unicode, nchars);
}

static
LPSTR schemeSymbolToMultiByte (Scheme_Object * obj)
{
  long nchars;
  LPCWSTR unicode = schemeSymbolToWideChar (obj, &nchars);
  return schemeWideStringToMultiByte (unicode, nchars);
}

Scheme_Object * multiByteToSchemeCharString (const char * mbstr)
{
  int len = (int) strlen (mbstr);
  WCHAR * wide = (WCHAR *)scheme_malloc_atomic (len * sizeof (WCHAR));
  HRESULT hr = MultiByteToWideChar (CP_ACP, (DWORD)0, mbstr, len, wide, len);

  if (hr == 0 && len > 0)
      scheme_signal_error("Error translating string parameter to Unicode");

  long nchars;
  mzchar * ucs4 = scheme_utf16_to_ucs4 (wide, 0, len, NULL, 0, &nchars, 0);
  return scheme_make_sized_char_string (ucs4, nchars, 0);
}

static
BSTR multiByteToBSTR (LPCSTR text, UINT len)
{
  BSTR bstr = SysAllocStringLen (NULL, len);
  if (bstr == NULL)
      scheme_signal_error ("Error allocating string parameter");

  if (MultiByteToWideChar (CP_ACP, (DWORD)0,
                           text, len,
                           bstr, len) == 0
      && len > 0)
      scheme_signal_error ("Error translating string parameter to WideChar");
  return bstr;
}

BSTR textToBSTR (LPCTSTR text, UINT length)
{
#ifdef UNICODE
  return SysAllocStringLen (text, length);
#else
  return multiByteToBSTR (text, length);
#endif
}

Scheme_Object * BSTRToSchemeString (BSTR bstr)
{
  UINT length = SysStringLen (bstr);
  long nchars;
  mzchar * string = scheme_utf16_to_ucs4 (bstr, 0, length,
					  NULL, 0,
					  &nchars, 0);
  return scheme_make_sized_char_string (string, nchars, 0);
}

Scheme_Object * BSTRToSchemeSymbol (BSTR bstr)
{
  UINT length = SysStringLen (bstr);
  long nchars;
  mzchar * string =  scheme_utf16_to_ucs4 (bstr, 0, length,
					   NULL, 0,
					   &nchars, 0);
  return scheme_intern_exact_char_symbol (string, nchars);
}

// This parameter controls whether strings returned by
// COM are converted to scheme symbols or to scheme strings.
Scheme_Object * mx_unmarshal_strings_as_symbols;

Scheme_Object * unmarshalBSTR (BSTR bstr)
{
  return
      scheme_apply (mx_unmarshal_strings_as_symbols, 0, NULL) == scheme_false
      ? BSTRToSchemeString (bstr)
      : BSTRToSchemeSymbol (bstr);
}

static
void updateSchemeByteStringFromBSTR (Scheme_Object * obj, BSTR bstr)
{
  UINT len = SysStringLen (bstr);
  long nchars;
  mzchar * string = scheme_utf16_to_ucs4 (bstr, 0, len,
					  NULL, 0,
					  &nchars, 0);
  if (nchars > SCHEME_BYTE_STRLEN_VAL(obj))
      scheme_signal_error ("String updated with longer string");

  long ncodes;
  scheme_utf8_encode_to_buffer_len (string, nchars, NULL, 0, &ncodes);
  SCHEME_BYTE_STRLEN_VAL(obj) = ncodes;
}

static
void updateSchemeCharStringFromBSTR (Scheme_Object * obj, BSTR bstr)
{
  UINT len = SysStringLen (bstr);
  long ulen;

  if (len > (unsigned int)SCHEME_CHAR_STRLEN_VAL(obj))
      scheme_signal_error("String updated with longer string");

  scheme_utf16_to_ucs4 (bstr, 0, len,
			SCHEME_CHAR_STR_VAL(obj), SCHEME_CHAR_STRLEN_VAL(obj),
			&ulen, 0);

  SCHEME_CHAR_STRLEN_VAL(obj) = ulen;
}

static
void updateSchemeSymbolFromBSTR (Scheme_Object *, BSTR)
{
  scheme_signal_error ("Symbol cannot be updated from BSTR.");
}

void updateSchemeFromBSTR (Scheme_Object *obj, BSTR bstr)
{
  return
      SCHEME_SYMBOLP (obj)        ? updateSchemeSymbolFromBSTR (obj, bstr)
      : SCHEME_CHAR_STRINGP (obj) ? updateSchemeCharStringFromBSTR (obj, bstr)
      : SCHEME_BYTE_STRINGP (obj) ? updateSchemeByteStringFromBSTR (obj, bstr)
      : SCHEME_PATHP (obj)        ? updateSchemeByteStringFromBSTR (obj, bstr)
      : (scheme_signal_error ("updateSchemeFromBSTR: argument is not a symbol, char string, or byte string"),
	 (LPSTR) 0);
}


BSTR stringToBSTR (LPCSTR s, UINT len)
{
  BSTR bstr = SysAllocStringLen (NULL, len);
  if (bstr == NULL)
    scheme_signal_error ("Error allocating string parameter");

  if (MultiByteToWideChar (CP_ACP, (DWORD)0,
                           s, len,
                           bstr, len) == 0
      && len > 0)
    scheme_signal_error ("Error translating string parameter to WideChar");
  return bstr;
}

LPTSTR schemeCharStringToText (Scheme_Object * obj)
{
#if UNICODE
  return schemeCharStringToWideChar (obj);
#else
  return schemeCharStringToMultiByte (obj);
#endif
}

LPTSTR schemeSymbolToText (Scheme_Object * obj)
{
#if UNICODE
  return schemeSymbolToWideChar (obj);
#else
  return schemeSymbolToMultiByte (obj);
#endif
}

// Returns a pointer to a Microsoft encoded MultiByte string.
LPSTR schemeToMultiByte (Scheme_Object * obj)
{
  return
      SCHEME_SYMBOLP (obj)        ? schemeSymbolToMultiByte (obj)
      : SCHEME_CHAR_STRINGP (obj) ? schemeCharStringToMultiByte (obj)
      : SCHEME_BYTE_STRINGP (obj) ? schemeByteStringToMultiByte (obj)
      : SCHEME_PATHP (obj)        ? schemeByteStringToMultiByte (obj)
      : (scheme_signal_error ("schemeToMultiByte: argument is not a symbol, char string, or byte string"),
	 (LPSTR) 0);
}

// Returns a pointer to a Microsoft encoded string suitable for
// passing to OLE and COM.
BSTR schemeToBSTR (Scheme_Object * obj)
{
  return
      SCHEME_SYMBOLP (obj)        ? schemeSymbolToBSTR (obj)
      : SCHEME_CHAR_STRINGP (obj) ? schemeCharStringToBSTR (obj)
      : SCHEME_BYTE_STRINGP (obj) ? schemeByteStringToBSTR (obj)
      : SCHEME_PATHP (obj)        ? schemeByteStringToBSTR (obj)
      : (scheme_signal_error ("schemeToBSTR: argument is not a symbol, char string, or byte string"),
	 (BSTR) 0);
}

// Returns a pointer to a Microsoft encoded string.  String will be
// either WideChar or MultiByte depending on compilation flag.
LPTSTR schemeToText (Scheme_Object * obj)
{
#ifdef UNICODE
  return schemeToWideChar (obj);
#else
  return schemeToMultiByte (obj);
#endif
}

// Returns a pointer to a Microsoft WideChar-encoded string.
LPWSTR schemeToWideChar (Scheme_Object * obj)
{
  long result_length;

  return
      SCHEME_SYMBOLP (obj)        ? schemeSymbolToWideChar (obj, &result_length)
      : SCHEME_CHAR_STRINGP (obj) ? schemeCharStringToWideChar (obj, &result_length)
      : SCHEME_BYTE_STRINGP (obj) ? schemeByteStringToWideChar (obj, &result_length)
      : SCHEME_PATHP        (obj) ? schemeByteStringToWideChar (obj, &result_length)
      : (scheme_signal_error ("schemeToWideChar: argument is not a symbol, char string, or byte string"),
	 (LPWSTR) 0);
}
