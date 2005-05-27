// bstr.h

BSTR textToBSTR (LPCTSTR, size_t);

// conversions to and from Windows types.
BSTR   schemeToBSTR       (Scheme_Object *);
LPSTR  schemeToMultiByte  (Scheme_Object *);
LPTSTR schemeToText       (Scheme_Object *);
LPWSTR schemeToWideChar   (Scheme_Object *);

LPTSTR schemeCharStringToText (Scheme_Object *);
LPTSTR schemeSymbolToText     (Scheme_Object *);

Scheme_Object * multiByteToSchemeCharString (LPCSTR);
Scheme_Object * textToSchemeCharString (LPCTSTR);
Scheme_Object * textToSchemeSymbol (LPCTSTR);

extern Scheme_Object * mx_unmarshal_strings_as_symbols;
Scheme_Object *BSTRToSchemeString(BSTR);
void updateSchemeFromBSTR(Scheme_Object *,BSTR);
BSTR stringToBSTR(const char *,size_t);
Scheme_Object * unmarshalBSTR (BSTR bstr);

