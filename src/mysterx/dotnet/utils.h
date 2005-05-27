void Failure (char *message = NULL);
void LaunchDebugger (const char *szMsg, const char *szFile, int iLine);
int show_traceln (char * str, ...);
int show_trace (char * str, ...);

#define TEXT_OUTLN(message) printf ("%s\n", message)
#define _THROW_EXCEPTION(message) do { show_traceln (message); LaunchDebugger (message, __FILE__, __LINE__); throw new SimpleException (message);} while (0)

//
// assert on false
//
#define _ASSERT_( expression ) do \
{ \
        if ( !(expression) ) \
        LaunchDebugger( #expression, __FILE__, __LINE__ );      \
} while (0)



mdTypeRef get_type_token (IMetaDataEmit * pMetaDataEmit,
                          LPCWSTR assembly_name,
                          unsigned short major_version, unsigned short minor_version,
                          unsigned short release, unsigned short revision,
                          BYTE k0, BYTE k1, BYTE k2, BYTE k3,
                          BYTE k4, BYTE k5, BYTE k6, BYTE k7,
                          LPCWSTR type_name);

mdMemberRef signature_no_args (IMetaDataEmit * pMetaDataEmit,
                               mdTypeRef type_ref,
                               LPCWSTR member_name,
                               CorCallingConvention callconv,
                               CorElementType return_type);

mdMemberRef signature_one_arg (IMetaDataEmit * pMetaDataEmit,
                               mdTypeRef type_ref,
                               LPCWSTR member_name,
                               CorCallingConvention callconv,
                               CorElementType return_type,
                               CorElementType arg_type);

mdSignature signature_unmanaged_one_arg (IMetaDataEmit * pMetaDataEmit,
                                         CorUnmanagedCallingConvention callconv,
                                         CorElementType return_type,
                                         CorElementType arg_type);

