#include "stdafx.h"
#include <stdio.h>
#include "corhdr.h"
#include "cor.h"
#include "corhlpr.h"
#include "corprof.h"
#include "cordebug.h"
#include "SimpleException.h"
#include "utils.h"

#define STRING_BUFFER_LEN 1024

int die (char const * szError, HRESULT hr)
{
    fprintf (stderr, "\nDIE: %s\n", szError);
    fflush (stderr);

    if (hr != S_OK) {
        IErrorInfo * pIErr = NULL;
        BSTR bstrDesc = NULL;

        fprintf (stderr, "HRESULT = 0x%08x\n", hr);

        if (GetErrorInfo (0, &pIErr) == S_OK &&
            pIErr->GetDescription (&bstrDesc) == S_OK) {
            fprintf (stderr, "%ls", bstrDesc);
            fflush (stderr);
            SysFreeString (bstrDesc);
            }
        if (pIErr) pIErr->Release();
        }
    CoUninitialize();
    exit (hr);
}

int show_trace (char * str, va_list marker)
{
    CQuickBytes buffer;
    int count = -1;
    int i = 1;
    HRESULT hr;

    while (count < 0) {
        if (FAILED (hr = buffer.ReSize (STRING_BUFFER_LEN * i)))
            die ("Resize failed.", hr);
        count = _vsnprintf ((char *) buffer.Ptr(), STRING_BUFFER_LEN * i, str, marker);
        i *= 2;
        }
    fprintf (stderr, "%s", (char *) buffer.Ptr());
    return count;
}

int show_trace (char * str, ...)
{
    va_list marker;
    int     count;

    va_start (marker, str);
    count = show_trace (str, marker);
    va_end (marker);
    fflush (stderr);
    return count;
}

int show_traceln (char * str, ...)
{
    va_list marker;
    int     count;

    va_start (marker, str);
    count = show_trace (str, marker);
    va_end (marker);
    fprintf (stderr, "\n");
    fflush (stderr);
    return count;
}

#define DEBUG_ENVIRONMENT               "DBG_PRF"

#define MAX_LENGTH 256

#define _DbgBreak() __asm { int 3 }

void Failure (char *message)
{
    if (message == NULL)
        message = "**** SEVERE FAILURE: TURNING OFF APPLICABLE PROFILING EVENTS ****";

        //
        // Display the error message and turn off the profiler events except the
        // IMMUTABLE ones. Turning off the IMMUTABLE events can cause crashes. The only
        // place that we can safely enable or disable immutable events is the Initialize
        // callback.
        //
    TEXT_OUTLN (message);
} // PrfInfo::Failure

int String2Number (char * number)
{
    WCHAR ch;
    int base;
    int iIndex = 1;
    BOOL errorOccurred = FALSE;


    // check to see if this is a valid number
    // if the first digit is '0', then this is
    // a hex or octal number
    if (number[0] == '0') {
        //
        // hex
        //
        if ((number[1] == 'x') || (number[1] == 'X')) {
            iIndex++;
            base = 16;
            while ((errorOccurred == FALSE)
                   && ((ch = number[iIndex++]) != '\0')) {
                if (((ch >= '0') && (ch <= '9'))
                    || ((ch >= 'a') && (ch <= 'f'))
                    || ((ch >= 'A') && (ch <= 'F')))
                    continue;
                else
                    errorOccurred = TRUE;
                }
            }
        //
        // octal
        //
        else {
            base = 8;
            while ((errorOccurred == FALSE) &&
                   ((ch = number[iIndex++]) != '\0')) {
                if ((ch >= '0') && (ch <= '7'))
                    continue;
                else
                    errorOccurred = TRUE;
                }
            }
        }
    //
    // decimal
    //
    else {
        base = 10;
        while  ((errorOccurred == FALSE)
                && ((ch = number[iIndex++]) != '\0')) {
            if ((ch >= '0') && (ch <= '9'))
                continue;
            else
                errorOccurred = TRUE;
            }
        }

    return ((errorOccurred == TRUE) ? -1 : base);
}

DWORD GetEnvVarValue (char *value)
{
    int base = String2Number (value);

    return (base != -1)
        ? (DWORD) strtoul (value, NULL, base)
        : -1;
}

DWORD FetchEnvironment (const char *environment)
{
    DWORD retVal = -1;
    char buffer[MAX_LENGTH];

    return (GetEnvironmentVariableA (environment, buffer, MAX_LENGTH) > 0 )
        ? GetEnvVarValue (buffer)
        : -1;
}

void LaunchDebugger (const char *szMsg, const char *szFile, int iLine)
{
    static DWORD launchDebugger = FetchEnvironment (DEBUG_ENVIRONMENT);

    if ((launchDebugger >= 1) && (launchDebugger != 0xFFFFFFFF)) {
        char title[MAX_LENGTH];
        char message[MAX_LENGTH];


        sprintf (message,
                 "%s\n\n"     \
                 "File: %s\n" \
                 "Line: %d\n",
                 ((szMsg == NULL) ? "FAILURE" : szMsg),
                 szFile,
                 iLine);

        sprintf (title,
                 "Test Failure (PID: %d/0x%08x, Thread: %d/0x%08x)      ",
                 GetCurrentProcessId(),
                 GetCurrentProcessId(),
                 GetCurrentThreadId(),
                 GetCurrentThreadId());

        switch (MessageBoxA (NULL,
                             message,
                             title,
                             (MB_ABORTRETRYIGNORE | MB_ICONEXCLAMATION))) {
          case IDABORT:
              TerminateProcess (GetCurrentProcess(), 999 /* bad exit code */ );
              break;

          case IDRETRY:
              _DbgBreak();


          case IDIGNORE:
              break;
            } // switch
        }

} // BASEHELPER::LaunchDebugger

mdTypeRef get_type_token (IMetaDataEmit * pMetaDataEmit,
                          LPCWSTR assembly_name,
                          unsigned short major_version, unsigned short minor_version,
                          unsigned short release, unsigned short revision,
                          BYTE k0, BYTE k1, BYTE k2, BYTE k3,
                          BYTE k4, BYTE k5, BYTE k6, BYTE k7,
                          LPCWSTR type_name)
{
  IMetaDataAssemblyEmit* pMetaDataAssemblyEmit;

  if (FAILED (pMetaDataEmit->QueryInterface (IID_IMetaDataAssemblyEmit,
                                             (void **)&pMetaDataAssemblyEmit)))
      _THROW_EXCEPTION ("QueryInterface IID_IMetaDataAssemblyEmit failed.");

  ASSEMBLYMETADATA amd;
  ZeroMemory (&amd, sizeof (amd));
  amd.usMajorVersion = major_version;
  amd.usMinorVersion = minor_version;
  amd.usBuildNumber = release;
  amd.usRevisionNumber = revision;

  BYTE key [] = {k0, k1, k2, k3, k4, k5, k6, k7};

  mdModuleRef module_token;
  if (FAILED (pMetaDataAssemblyEmit->DefineAssemblyRef (key, sizeof (key),
                                                        assembly_name, &amd, NULL,
                                                        0, 0, &module_token)))
      _THROW_EXCEPTION ("DefineAssemblyRef failed.");

  mdTypeRef type_token;
  if (FAILED (pMetaDataEmit->DefineTypeRefByName (module_token, type_name, &type_token)))
      _THROW_EXCEPTION ("DefineTypeRefByName failed.");

  pMetaDataAssemblyEmit->Release();
  return type_token;
}

mdMemberRef signature_no_args (IMetaDataEmit * pMetaDataEmit,
                               mdTypeRef type_ref,
                               LPCWSTR member_name,
                               CorCallingConvention callconv,
                               CorElementType return_type)
{
  BYTE sigblob [] = {callconv,
                     0x00, //argcount
                     return_type};

  mdMemberRef token;

  if (FAILED (pMetaDataEmit->DefineMemberRef (type_ref,
                                              member_name,
                                              sigblob,
                                              sizeof (sigblob),
                                              &token)))
      _THROW_EXCEPTION ("signature_no_args failed.");

  return token;
}

mdMemberRef signature_one_arg (IMetaDataEmit * pMetaDataEmit,
                               mdTypeRef type_ref,
                               LPCWSTR member_name,
                               CorCallingConvention callconv,
                               CorElementType return_type,
                               CorElementType arg_type)
{
  BYTE sigblob [] = {callconv,
                     0x01, //argcount
                     return_type,
                     arg_type};

  mdMemberRef token;

  if (FAILED (pMetaDataEmit->DefineMemberRef (type_ref,
                                              member_name,
                                              sigblob,
                                              sizeof (sigblob),
                                              &token)))
      _THROW_EXCEPTION ("signature_one_arg failed.");

  return token;
}

mdSignature signature_unmanaged_one_arg (IMetaDataEmit * pMetaDataEmit,
                                         CorUnmanagedCallingConvention callconv,
                                         CorElementType return_type,
                                         CorElementType arg_type)
{
  char sigblob [] = {callconv,
                     0x01, //argcount
                     return_type,
                     arg_type};
  mdSignature msig;

  pMetaDataEmit->GetTokenFromSig ((PCCOR_SIGNATURE)sigblob, sizeof (sigblob), &msig);
  return msig;
}
