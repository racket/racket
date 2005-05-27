
typedef BOOL (*wxPDF)(void *data, HWND parent);

BOOL wxPrimitiveDialog(wxPDF f, void *data, int strict);
