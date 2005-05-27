#include "stdafx.h"
#include <stdio.h>
#include "corhdr.h"
#include "cor.h"
#include "corhlpr.h"
#include "corprof.h"
#include "cordebug.h"
#include "BaseInfo.h"
#include "SimpleException.h"
#include "SList.h"
#include "Synchronize.h"
#include "ThreadInfo.h"
#include "utils.h"

SList<ThreadInfo *, ThreadID> * thread_table;
ICorProfilerInfo * profiler_info;
CRITICAL_SECTION critical_section;

void InitializePatch (IUnknown * pICorProfilerInfoUnk)
{


    HRESULT hr = pICorProfilerInfoUnk->QueryInterface (IID_ICorProfilerInfo, (void **)(&profiler_info));

    if (SUCCEEDED (hr)) {

        InitializeCriticalSection (&critical_section);
        thread_table = new SList<ThreadInfo *, ThreadID>();
        profiler_info->SetEventMask (COR_PRF_ENABLE_INPROC_DEBUGGING
                                     | COR_PRF_MONITOR_THREADS
                                     | COR_PRF_MONITOR_CODE_TRANSITIONS
                                     // | COR_PRF_MONITOR_EXCEPTIONS
                                     // | COR_PRF_MONITOR_CLR_EXCEPTIONS
                                     | COR_PRF_MONITOR_JIT_COMPILATION
                                     | COR_PRF_ENABLE_JIT_MAPS);

#if 0
        hr = profiler_info->SetEnterLeaveFunctionHooks ((FunctionEnter*)&EnterNaked,
                                                        (FunctionLeave*)&LeaveNaked,
                                                        (FunctionTailcall*)&TailcallNaked);
#endif
        show_traceln ("Initializing DotNet synchronization patch.");
        show_traceln ("Built at " __TIME__ " on " __DATE__);
        }
    else
        show_traceln ("DotNet synchronization patch failed to initialize.");
}

void ShutdownPatch (void)
{
  profiler_info->Release();
  if (thread_table != NULL) delete thread_table;
  DeleteCriticalSection (&critical_section);
}

static void _GetThreadInfoHelper (ThreadInfo **ppThreadInfo)
{
    HRESULT hr;

    if (profiler_info != NULL) {
        //
        // Assume that the thread starts running managed code
        // otherwise we would not get notified for its creation
        //
        (*ppThreadInfo)->m_status = MANAGED;

        //
        // Get the win32 threadID for the specific Runtime thread
        //
        hr = profiler_info->GetThreadInfo ((*ppThreadInfo)->m_id,
                                           &((*ppThreadInfo)->m_win32ThreadID));
        if (SUCCEEDED(hr)) {
            //
            // Get the thread handle
            //
            hr = profiler_info->GetHandleFromThread ((*ppThreadInfo)->m_id,
                                                     &((*ppThreadInfo)->m_hThread));
            if (FAILED(hr))
                _THROW_EXCEPTION( "ICorProfilerInfo::GetHandleFromThread() FAILED" );
            }
        else
            _THROW_EXCEPTION( "ICorProfilerInfo::GetThreadInfo() FAILED" );
        }
    else
        _THROW_EXCEPTION( "ICorProfilerInfo Interface has NOT been Initialized" );
}

void AddThread (ThreadID threadID)
{
    Synchronize guard (critical_section);

    HRESULT hr;
    ThreadID myThreadID;

    hr = profiler_info->GetCurrentThreadID (&myThreadID);
    if (SUCCEEDED (hr)) {
        if (threadID == myThreadID) {
            ThreadInfo * pThreadInfo;
            pThreadInfo = new ThreadInfo (threadID);
            if (pThreadInfo != NULL) {
                try {
                    _GetThreadInfoHelper (&pThreadInfo);
                    thread_table->AddEntry (pThreadInfo, threadID);
                    pThreadInfo->Dump();
                    }
                catch (SimpleException *){
                    delete pThreadInfo;
                    throw;
                    }
                }
            else
                _THROW_EXCEPTION( "Allocation for ThreadInfo Object FAILED" );

            }
        else
            _THROW_EXCEPTION( "Thread ID's do not match FAILED" );

        }
    else
        _THROW_EXCEPTION( "ICorProfilerInfo::GetCurrentThreadID() FAILED" );
}

static void RemoveThread1 (ThreadID threadID)
{
    Synchronize guard (critical_section);

    if (threadID != NULL) {
        ThreadInfo *pThreadInfo;

        pThreadInfo = thread_table->Lookup (threadID);
        if (pThreadInfo != NULL) {
            _ASSERT_ (pThreadInfo->m_isValid == TRUE);
            pThreadInfo->m_isValid = FALSE;
            pThreadInfo->m_id = 0xbadf00d;
            }
        else
            _THROW_EXCEPTION( "Thread was not found in the Thread Table" );
        }
    else
        _THROW_EXCEPTION( "ThreadID is NULL" );
}

void RemoveThread (ThreadID threadID)
{
    try {
        RemoveThread1 (threadID);
        show_traceln (".NET thread 0x%x Destroyed", threadID);
        }
    catch (SimpleException * exception) {
        exception->ReportFailure();
        delete exception;

        Failure();
        }

}

static ThreadInfo * win32ThreadID_to_ThreadInfo (DWORD win32ThreadId)
{
    if (thread_table->IsEmpty()) {
        show_traceln ("win32ThreadId_to_ThreadInfo:  table is empty");
        return NULL;
        }

    thread_table->Reset();

    do {
        ThreadInfo * this_info = thread_table->Entry();
        if (this_info == NULL) {
            show_traceln ("win32ThreadId_to_ThreadInfo:  thread not found");
            return NULL;
            }

        else if (this_info->m_win32ThreadID == win32ThreadId) {
            _ASSERT_ (this_info->m_isValid == TRUE);
            return this_info;
            }

        else
            thread_table->Next();
        } while (true);
}

static ThreadInfo * current_thread_info()
{
    ThreadInfo * cti = win32ThreadID_to_ThreadInfo (GetCurrentThreadId());
    if (cti == NULL)
        show_traceln ("Current thread info not found.");
    return cti;
}

// YOU MUST
//   Synchronize guard (critical_section);
// BEFORE CALLING THIS
static ThreadInfo * profiler_info_current_thread_id()
{
  ThreadID threadID;
  if (FAILED (profiler_info->GetCurrentThreadID (&threadID)))
      _THROW_EXCEPTION ( "ICorProfilerInfo::GetCurrentThreadID() FAILED");

  ThreadInfo *pThreadInfo = thread_table->Lookup (threadID);
  if (pThreadInfo == NULL)
      _THROW_EXCEPTION ("Thread Structure was not found in the thread list");

  _ASSERT_(pThreadInfo->m_isValid == TRUE);

  return pThreadInfo;
}

void UpdateOSThreadID (ThreadID managedThreadID, DWORD win32ThreadID)
{
    try {
        Synchronize guard (critical_section);
        profiler_info_current_thread_id()->m_win32ThreadID = win32ThreadID;
        show_traceln ("Managed thread 0x%x assigned to OSThread 0x%x");
        }
    catch (SimpleException * exception) {
        exception->ReportFailure();
        delete exception;

        Failure();
        }
}

static void UpdateTransitionState (TransitionState state)
{
    try {
        Synchronize guard (critical_section);
        profiler_info_current_thread_id()->m_status = state;
        }
    catch (SimpleException * exception) {
        exception->ReportFailure();
        delete exception;
        Failure();
        }
}

void NoteThreadManaged (void)
{
  UpdateTransitionState (MANAGED);
}

void NoteThreadUnmanaged (void)
{
  UpdateTransitionState (UNMANAGED);
}

static BOOL scan_frame1 (ICorDebugProcess * pProcess,
                  ULONG32 pnOffset, CorDebugMappingResult mappingResult,
                  CORDB_ADDRESS code_start, ULONG32 code_size)
{
  bool result = false;

  if (mappingResult == MAPPING_PROLOG)
      result = false;

  else if (mappingResult == MAPPING_EPILOG) {
      show_trace (" MAPPING_EPILOG");
      result = true;
      }
  else if (mappingResult == MAPPING_NO_INFO) {
      show_trace (" MAPPING_NO_INFO");
      result = false;
      }
  else if (mappingResult == MAPPING_UNMAPPED_ADDRESS) {
      show_trace (" MAPPING_UNMAPPED_ADDRESS");
      result = false;
      }
  else {
      BYTE buffer[1024];
      DWORD realbufsiz;
      DWORD bufptr = 0;

      code_start += code_size;

      // align to 4-byte boundary
      while ((code_start & 0x3) != 0) code_start += 1;

      if (FAILED (pProcess->ReadMemory (code_start, 1024, buffer, &realbufsiz)))
          _THROW_EXCEPTION ("Couldn't get code.");

      if (buffer[0] == 0x01) {
          // show_traceln ("EH info follows.");
          unsigned char dsize = buffer[1];
          DWORD nclauses = (dsize - 4)/12;
          // show_traceln ("%d %d clauses", dsize, nclauses);
          unsigned int i;
          for (i = 0; i < nclauses; i++) {
              DWORD flags      = (buffer[(i * 12) + 4 + 1] * 256) + buffer[(i * 12) + 4 + 0];
              DWORD try_offset = (buffer[(i * 12) + 4 + 3] * 256) + buffer[(i * 12) + 4 + 2];
              DWORD try_length = buffer[(i * 12) + 4 + 4];
              DWORD handler_offset = (buffer[(i * 12) + 4 + 6] * 256) + buffer[(i * 12) + 4 + 5];
              DWORD handler_length = buffer[(i * 12) + 4 + 7];
              DWORD class_token = (buffer[(i * 12) + 4 + 9] * 256) + buffer[(i * 12) + 4 + 8];
              DWORD filter_offset = (buffer[(i * 12) + 4 + 11] * 256) + buffer[(i * 12) + 4 + 10];
//                 show_traceln ("\tflags (%x)", flags);
//                 show_traceln ("\ttry_offset (%x)", try_offset);
//                 show_traceln ("\ttry_length (%x)", try_length);
//                 show_traceln ("\thandler_offset (%x)", handler_offset);
//                 show_traceln ("\thandler_length (%x)", handler_length);
//                 show_traceln ("\tclass_token (%x)", class_token);
//                 show_traceln ("\tfilter_offset (%x)", filter_offset);

              if (flags == COR_ILEXCEPTION_CLAUSE_FINALLY
                  && pnOffset >= handler_offset
                  && pnOffset < (handler_offset + handler_length)) {
                  show_traceln ("Deferring abort during cleanup.");
                  result = false;
                  }
              }
          }
      else if (buffer [0] == 0x41) {
          show_traceln ("FAT EH info follows.");
          unsigned char dsize = (((buffer[3] * 256) + buffer[2]) * 256) + buffer[1];
          DWORD nclauses = (dsize - 4)/24;
          show_trace ("%d %d clauses", dsize, nclauses);
          }
      else {
          // show_traceln ("No EH info.");
          result = true;
          }
      }
  return result;
}

static BOOL GetMethodNameFromTokenAndMetaData (mdToken dwToken, IMetaDataImport * pIMetaDataImport,
                                               LPWSTR wszClass, LPWSTR wszMethod)
{
  wchar_t _wszMethod[512];
  DWORD cchMethod = sizeof (_wszMethod)/sizeof (_wszMethod[0]);
  mdTypeDef mdClass;
  COR_SIGNATURE const * method_signature;
  ULONG sig_size;
  DWORD dwAttr;

  HRESULT hr = pIMetaDataImport->GetMethodProps (dwToken, &mdClass, _wszMethod,
                                                 cchMethod, &cchMethod,
                                                 &dwAttr,
                                                 &method_signature, &sig_size,
                                                 0, 0);

  if (FAILED (hr)) {
      show_traceln ("GetMethodProps failed.");
      return false;
      }

  lstrcpyW (wszMethod, _wszMethod);

  wchar_t wszTypeDef[512];
  DWORD cchTypeDef = sizeof(wszTypeDef)/sizeof(wszTypeDef[0]);

  if (mdClass == 0x02000000)
      mdClass = 0x02000001;

  hr = pIMetaDataImport->GetTypeDefProps (mdClass, wszTypeDef, cchTypeDef,
                                          &cchTypeDef, 0, 0);
  if (FAILED(hr)) return false;

  lstrcpyW (wszClass, wszTypeDef);
  return true;
}

static BOOL GetMethodNameFromFunctionId (FunctionID functionId, LPWSTR wszClass, LPWSTR wszMethod)
{
  mdToken dwToken;

  IMetaDataImport * pMetaDataImport = 0;
  if (FAILED (profiler_info->GetTokenAndMetaDataFromFunction (functionId,
                                                              IID_IMetaDataImport,
                                                              (LPUNKNOWN *)&pMetaDataImport,
                                                              &dwToken)))
      return false;

  BOOL result = GetMethodNameFromTokenAndMetaData (dwToken, pMetaDataImport,
                                                   wszClass, wszMethod);
  pMetaDataImport->Release();

  //
  // If we were ambitious, we'd save every FunctionID away in a map to avoid
  //  needing to hit the metatdata APIs every time.
  //
  return result;
}

static BOOL GetMethodNameFromFunction (ICorDebugFunction * pFunction, LPWSTR wszClass, LPWSTR wszMethod)
{
  ICorDebugModule * pModule = NULL;
  if (FAILED (pFunction->GetModule (&pModule)))
      _THROW_EXCEPTION ("GetModule failed.");

  IMetaDataImport * pMetaDataImport = NULL;
  if (FAILED (pModule->GetMetaDataInterface (IID_IMetaDataImport, (IUnknown **)&pMetaDataImport)))
      _THROW_EXCEPTION ("GetMetaDataInterface failed.");

  mdMethodDef funToken;
  if (FAILED (pFunction->GetToken (&funToken)))
      _THROW_EXCEPTION ("GetToken of function failed.");

  BOOL result = GetMethodNameFromTokenAndMetaData (funToken, pMetaDataImport,
                                                   wszClass, wszMethod);

  if (pMetaDataImport != NULL) pMetaDataImport->Release();
  if (pModule != NULL) pModule->Release();
  return result;
}

static void display_method_name (ICorDebugFunction * pFunction)
{
  wchar_t wszClass[512];
  wchar_t wszMethod[512];

  if (!GetMethodNameFromFunction (pFunction, wszClass, wszMethod))
      _THROW_EXCEPTION ("GetMethodNameFromFunction failed.");

  fwprintf (stderr, wszClass);
  show_trace ("::");
  fwprintf (stderr, wszMethod);
  fflush (stderr);
}

// Return TRUE if we can safely call abort.
static BOOL scan_frame (ICorDebugProcess * pProcess, ICorDebugFrame * pFrame)
{
  ICorDebugFunction * pFunction = NULL;
  if (FAILED (pFrame->GetFunction (&pFunction)))
      _THROW_EXCEPTION ("GetFunction failed.");

  BOOL result = false;

  ULONG32 pnOffset;
  CorDebugMappingResult mappingResult;

  ICorDebugILFrame * pILFrame = NULL;
  if (FAILED (pFrame->QueryInterface (IID_ICorDebugILFrame, (void **)&pILFrame)))
      _THROW_EXCEPTION ("QueryInterface for IID_ICorDebugILFrame failed.");

  if (FAILED (pILFrame->GetIP (&pnOffset, &mappingResult)))
      _THROW_EXCEPTION ("GetIP failed.");
  if (pILFrame != NULL) pILFrame->Release();

  // display_method_name (pFunction);
  // show_trace (" (%x)", pnOffset);

  ICorDebugCode * pCode = NULL;
  if (FAILED (pFunction->GetILCode (&pCode)))
      _THROW_EXCEPTION ("GetCode failed.");

  if (pCode == NULL) {
      show_traceln ("No code??");
      result = false;
      }
  else {
      CORDB_ADDRESS code_start;
      ULONG32 code_size;

      if (FAILED (pCode->GetAddress (&code_start)))
          _THROW_EXCEPTION ("GetAddress failed.");

      if (FAILED (pCode->GetSize (&code_size)))
          _THROW_EXCEPTION ("GetSize failed.");

      pCode->Release();

      result = scan_frame1 (pProcess, pnOffset, mappingResult, code_start, code_size);
      }

  if (pFunction != NULL) pFunction->Release();
  return result;
}

// Return TRUE if we can safely call abort.
static BOOL scan_frames (ICorDebugProcess * pProcess, ICorDebugFrame * pFrame)
{
  // Bottom of stack, always safe.
  if (pFrame == NULL)
      return TRUE;

  // If this frame says no, then no.
  else if (!scan_frame (pProcess, pFrame))
      return FALSE;

  // Otherwise, check the next frame.
  else {
      ICorDebugFrame * pCaller;
      if (FAILED (pFrame->GetCaller (&pCaller)))
          _THROW_EXCEPTION ("GetCaller failed.");

      BOOL result = scan_frames (pProcess, pCaller);
      pCaller->Release();
      return result;
      }
}

static void display_thread_state (ICorDebugThread * pICorDebugThread)
{
  // Check out the state of the thread.
  CorDebugThreadState debug_state;
  CorDebugUserState user_state;

  if (FAILED (pICorDebugThread->GetDebugState (&debug_state)))
      _THROW_EXCEPTION ("GetDebugState failed.");
  if (FAILED (pICorDebugThread->GetUserState (&user_state)))
      _THROW_EXCEPTION ("GetUserState failed.");
  show_traceln ("DS %x, US %x", debug_state, user_state);
}

// This causes the debugging interface to parse the stack of
// the thread in question.
static void ensure_stack_parsed (ICorDebugThread * pICorDebugThread)
{
  ICorDebugChainEnum * pChains = NULL;

  if (FAILED (pICorDebugThread->EnumerateChains (&pChains)))
      _THROW_EXCEPTION ("EnumerateChains failed.");

  if (pChains != NULL) pChains->Release();
}

static BOOL BeforeAbort1 (HANDLE thread_handle, int threadID)
{
  // Get a hold of the debugger to start examining thread.
  BOOL result = false;

  ThreadInfo * target_thread_info = win32ThreadID_to_ThreadInfo (threadID);

  if (target_thread_info == NULL) {
      show_traceln ("Could not find target thread info.");
      return true;
      }
  else if (target_thread_info->m_status != MANAGED) {
      show_traceln ("Abort deferred (thread not in MANAGED state).");
      return false;
      }
  else if (target_thread_info->abort_deferred == TRUE) {
      show_traceln ("Abort deferred.");
      return false;
      }
  else if (target_thread_info->throw_depth != 0) {
      show_traceln ("Abort deferred, target thread throwing.");
      return false;
      }

  IUnknown * pIUnknown = NULL;
  if (FAILED (profiler_info->GetInprocInspectionInterface (&pIUnknown)))
      _THROW_EXCEPTION ("GetInprocInspectionInterface failed.");

  ICorDebug * pICorDebug = NULL;
  if (FAILED (pIUnknown->QueryInterface (IID_ICorDebug, (void **)&pICorDebug)))
      _THROW_EXCEPTION ("QueryInterface IID_ICorDebug failed.");

  if (pIUnknown != NULL) pIUnknown->Release();

  ICorDebugProcess * pICorDebugProcess = NULL;
  if (FAILED (pICorDebug->GetProcess (GetCurrentProcessId(), &pICorDebugProcess)))
      _THROW_EXCEPTION ("GetProcess failed.");

  ICorDebugThread * pICorDebugThread = NULL;
  if (FAILED (pICorDebugProcess->GetThread (threadID, &pICorDebugThread)))
      _THROW_EXCEPTION ("GetThread failed.");

  // display_thread_state (pICorDebugThread);
  ensure_stack_parsed (pICorDebugThread);

  ICorDebugFrame * pFrame = NULL;
  if (FAILED (pICorDebugThread->GetActiveFrame (&pFrame)))
      _THROW_EXCEPTION ("GetActiveFrame failed.");

  result = scan_frames (pICorDebugProcess, pFrame);

  if (pFrame != NULL) pFrame->Release();
  if (pICorDebugThread != NULL) pICorDebugThread->Release();
  if (pICorDebugProcess != NULL) pICorDebugProcess->Release();
  if (pICorDebug != NULL) pICorDebug->Release();

  if (pFrame != NULL && result == TRUE)
      SuspendThread (thread_handle);

  return result;
}

// Return TRUE if we can safely call abort.
BOOL __stdcall BeforeAbort (int threadID)
{
  // Must grab this first in case target thread wants to use it.
  Synchronize guard (critical_section);
  HANDLE thread_handle = OpenThread (THREAD_SUSPEND_RESUME, false, threadID);

  if (thread_handle == NULL)
      return false;

  DWORD dwProfContext;

  // BeginInprocDebugging interface first to halt all the threads.
  if (FAILED (profiler_info->BeginInprocDebugging (false, &dwProfContext)))
      _THROW_EXCEPTION ("BeginInprocDebugging failed.");

  BOOL result = BeforeAbort1 (thread_handle, threadID);

  if (FAILED (profiler_info->EndInprocDebugging (dwProfContext)))
      _THROW_EXCEPTION ("EndInprocDebugging failed.");

  if (thread_handle != NULL) CloseHandle (thread_handle);
  return result;
}

void __stdcall AfterAbort (int threadID)
{
  HANDLE thread_handle = OpenThread (THREAD_SUSPEND_RESUME, false, threadID);
  if (thread_handle == NULL)
      show_traceln ("Could not open windows thread %d.", threadID);
  else {
      ResumeThread (thread_handle);
      CloseHandle (thread_handle);
      }
  return;
}

void PatchJIT (FunctionID functionID)
{

  wchar_t wszClass[512];
  wchar_t wszMethod[512];

  if (GetMethodNameFromFunctionId (functionID, wszClass, wszMethod)) {
      if (wcscmp (wszClass, L"System.Threading.Thread") == 0
          && wcscmp (wszMethod, L"Abort") == 0) {

          ClassID classId = 0;
          ModuleID moduleId = 0;
          mdToken tkMethod = 0;
          IMetaDataEmit* pMetaDataEmit = NULL;
          IMethodMalloc* pMalloc = NULL;
          PVOID pMethod;

          if (FAILED (profiler_info->GetFunctionInfo (functionID, &classId, &moduleId, &tkMethod)))
              _THROW_EXCEPTION ("GetFunctionInfo failed.");

          if (FAILED (profiler_info->GetModuleMetaData (moduleId, ofRead | ofWrite, IID_IMetaDataEmit,
                                                        (IUnknown **)&pMetaDataEmit)))
              _THROW_EXCEPTION ("GetModuleMetaData failed.");

          // Get token of System.Threading.Thread
          mdTypeRef tkThread = get_type_token (pMetaDataEmit,
                                               L"mscorlib", 1, 1, 5000, 0,
                                               0xB7, 0x7A, 0x5C, 0x56, 0x19, 0x34, 0xE0, 0x89,
                                               L"System.Threading.Thread");

          // Get token of SchemeBridge.TestClass
          mdTypeRef tkTestClass = get_type_token (pMetaDataEmit,
                                                  L"bridge", 1, 0, 0, 0,
                                                  0x64, 0x52,  0x72, 0x4b, 0xac, 0x30, 0xf0, 0xec,
                                                  L"SchemeBridge.TestClass");

// This is the chunk of .NET opcodes that we're replacing
// the existing Thread.Abort method with.

#pragma pack(push, 1)
          struct {
            unsigned char Flags_CodeSize;
            unsigned char bs0; unsigned char target0;
            unsigned char ldc1;
            unsigned char call0; int sig0;
            unsigned char ldarg0_0;
            unsigned char call2; int sig2;
            unsigned char ldci4_1; int val_1;
            unsigned char calli; int sig;
            unsigned char brf; unsigned char target1;
            unsigned char ldarg0_1;
            unsigned char ldfld; int field0;
            unsigned char call1; int target2;
            unsigned char ldarg0_2;
            unsigned char call3; int sig3;
            unsigned char ldci4_2; int val_2;
            unsigned char calli_1; int sig_1;
            unsigned char ret; unsigned char pad1, pad2, pad3;
          } ilcode = {
              0xDA,
              0x2B, 0x06,
              0x16,
              0x28, signature_one_arg (pMetaDataEmit,
                                       tkThread,
                                       L"Sleep",
                                       IMAGE_CEE_CS_CALLCONV_DEFAULT,
                                       ELEMENT_TYPE_VOID,
                                       ELEMENT_TYPE_I4),
              0x02,
              0x28, signature_one_arg (pMetaDataEmit,
                                       tkTestClass,
                                       L"GetThreadId",
                                       IMAGE_CEE_CS_CALLCONV_DEFAULT,
                                       ELEMENT_TYPE_I4,
                                       ELEMENT_TYPE_OBJECT),
              0x20, (int)(size_t)BeforeAbort,
              0x29, signature_unmanaged_one_arg (pMetaDataEmit,
                                                 IMAGE_CEE_UNMANAGED_CALLCONV_STDCALL,
                                                 ELEMENT_TYPE_BOOLEAN,
                                                 ELEMENT_TYPE_U4),
              0x2C, 0xE8,
              0x02,
              0x00, (int)0x0000,
              0x28, signature_no_args (pMetaDataEmit,
                                       tkThread,
                                       L"AbortInternal",
                                       IMAGE_CEE_CS_CALLCONV_HASTHIS,
                                       ELEMENT_TYPE_VOID),
              0x02,
              0x28, signature_one_arg (pMetaDataEmit,
                                       tkTestClass,
                                       L"GetThreadId",
                                       IMAGE_CEE_CS_CALLCONV_DEFAULT,
                                       ELEMENT_TYPE_I4,
                                       ELEMENT_TYPE_OBJECT),
              0x20, (int)(size_t)AfterAbort,
              0x29, signature_unmanaged_one_arg (pMetaDataEmit,
                                                 IMAGE_CEE_UNMANAGED_CALLCONV_STDCALL,
                                                 ELEMENT_TYPE_VOID,
                                                 ELEMENT_TYPE_U4),
              0x2A, 0x00, 0x00, 0x00
          };
#pragma pack(pop)

          profiler_info->GetILFunctionBodyAllocator (moduleId, &pMalloc);
          pMethod = pMalloc->Alloc (sizeof (ilcode));
          memcpy (pMethod, &ilcode, sizeof (ilcode));
          profiler_info->SetILFunctionBody (moduleId, tkMethod, (LPCBYTE)pMethod);
          pMalloc->Release();
          pMetaDataEmit->Release();
          show_traceln ("Replaced the code for abort.");
          }

      // show_traceln ("Compiling %ls::%ls", wszClass, wszMethod);
      }
}
