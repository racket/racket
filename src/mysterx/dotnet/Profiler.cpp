// Profiler.cpp : Implementation of CProfiler

#include "stdafx.h"
#include <stdio.h>
#include "corhdr.h"
#include "cor.h"
#include "corhlpr.h"
#include "corprof.h"
#include "Patch.h"
#include "Profiler.h"
#include "utils.h"

// the header file declares the types wrong
void __stdcall enter_function (FunctionID)
{
    return;
}

void __stdcall leave_function (FunctionID)
{
    return;
}

void __stdcall tailcall_function (FunctionID)
{
    return;
}

void __declspec (naked) EnterNaked()
{
    __asm {
        push eax
        push ecx
        push edx
        push [esp+16]
        call enter_function
        pop edx
        pop ecx
        pop eax
        ret 4
    }
}


void __declspec (naked) LeaveNaked()
{
    __asm {
        push eax
        push ecx
        push edx
        push [esp+16]
        call leave_function
        pop edx
        pop ecx
        pop eax
        ret 4
    }
}


void __declspec (naked) TailcallNaked()
{
    __asm {
        push eax
        push ecx
        push edx
        push [esp+16]
        call tailcall_function
        pop edx
        pop ecx
        pop eax
        ret 4
    }
}

// CProfiler

STDMETHODIMP CProfiler::Initialize (IUnknown* pICorProfilerInfoUnk)
{
    // cannot use inproc debugging in this method

    // We're going to hang around for a while.
    this->AddRef();
    InitializePatch(pICorProfilerInfoUnk);
    return S_OK;
}

STDMETHODIMP CProfiler::Shutdown ()
{
   // cannot use inproc debugging in this method
  ShutdownPatch();
  this->Release();
  return S_OK;
}

STDMETHODIMP CProfiler::AppDomainCreationStarted (AppDomainID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AppDomainCreationFinished (AppDomainID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AppDomainShutdownStarted (AppDomainID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AppDomainShutdownFinished (AppDomainID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AssemblyLoadStarted (AssemblyID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AssemblyLoadFinished (AssemblyID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AssemblyUnloadStarted (AssemblyID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::AssemblyUnloadFinished (AssemblyID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ModuleLoadStarted (ModuleID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ModuleLoadFinished (ModuleID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ModuleUnloadStarted (ModuleID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ModuleUnloadFinished (ModuleID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ModuleAttachedToAssembly (ModuleID,AssemblyID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ClassLoadStarted (ClassID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ClassLoadFinished (ClassID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ClassUnloadStarted (ClassID)
{
  return S_OK;
}

STDMETHODIMP CProfiler::ClassUnloadFinished (ClassID,HRESULT)
{
  return S_OK;
}

STDMETHODIMP CProfiler::FunctionUnloadStarted (FunctionID)
{
  return S_OK;
}

// Do you *really* want to know what this is doing?
// It is hooking the JIT compiler so that when the Thread.Abort method
// is compiled it is replaced with a working version of same.
// Insane.

STDMETHODIMP CProfiler::JITCompilationStarted (FunctionID functionID, BOOL)
{
    PatchJIT (functionID);
    return S_OK;
}

STDMETHODIMP CProfiler::JITCompilationFinished (FunctionID, HRESULT, BOOL)
{
    return S_OK;
}

STDMETHODIMP CProfiler::JITCachedFunctionSearchStarted (FunctionID, BOOL*)
{
    return S_OK;
}

STDMETHODIMP CProfiler::JITCachedFunctionSearchFinished (FunctionID, COR_PRF_JIT_CACHE)
{
    return S_OK;
}

STDMETHODIMP CProfiler::JITFunctionPitched (FunctionID)
{
    return S_OK;
}

STDMETHODIMP CProfiler::JITInlining (FunctionID, FunctionID, BOOL*)
{
    return S_OK;
}

STDMETHODIMP CProfiler::ThreadCreated (ThreadID threadID)
{
    // cannot use inproc debugging in this method
    AddThread (threadID);
    show_traceln (".NET thread 0x%x created.", threadID);
    return S_OK;
}

STDMETHODIMP CProfiler::ThreadDestroyed (ThreadID threadID)
{
    RemoveThread (threadID);
    return S_OK;
}

STDMETHODIMP CProfiler::ThreadAssignedToOSThread (ThreadID managedThreadID, DWORD osThreadID)
{
    UpdateOSThreadID (managedThreadID, osThreadID);
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingClientInvocationStarted()
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingClientSendingMessage (GUID*, BOOL)
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingClientReceivingReply (GUID*, BOOL)
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingClientInvocationFinished()
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingServerReceivingMessage (GUID*, BOOL)
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingServerInvocationStarted()
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingServerInvocationReturned()
{
    return S_OK;
}

STDMETHODIMP CProfiler::RemotingServerSendingReply (GUID*, BOOL)
{
    return S_OK;
}

STDMETHODIMP CProfiler::UnmanagedToManagedTransition (FunctionID functionID, COR_PRF_TRANSITION_REASON reason)
{
    NoteThreadManaged();
    return S_OK;
}

STDMETHODIMP CProfiler::ManagedToUnmanagedTransition (FunctionID functionID, COR_PRF_TRANSITION_REASON reason)
{
    NoteThreadUnmanaged();
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeSuspendStarted (COR_PRF_SUSPEND_REASON)
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeSuspendFinished ()
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeSuspendAborted ()
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeResumeStarted ()
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeResumeFinished ()
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeThreadSuspended (ThreadID)
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RuntimeThreadResumed (ThreadID)
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::MovedReferences (ULONG, ObjectID[], ObjectID[], ULONG[])
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::ObjectAllocated (ObjectID, ClassID)
{
    return S_OK;
}

STDMETHODIMP CProfiler::ObjectsAllocatedByClass (ULONG, ClassID[], ULONG[])
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::ObjectReferences (ObjectID, ClassID, ULONG, ObjectID[])
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::RootReferences (ULONG, ObjectID[])
{
    // cannot use inproc debugging in this method
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionThrown (ObjectID)
{
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionSearchFunctionEnter (FunctionID)
{
    show_traceln ("Exception Search Function Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionSearchFunctionLeave ()
{
    show_traceln ("Exception Search Function Leave");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionSearchFilterEnter (FunctionID)
{
    show_traceln ("Exception Search Filter Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionSearchFilterLeave ()
{
    show_traceln ("Exception Search Filter Leave");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionSearchCatcherFound (FunctionID)
{
    show_traceln ("Exception Search Catcher Found");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionOSHandlerEnter (UINT_PTR)
{
    // cannot use inproc debugging in this method
    show_traceln ("Exception OS Handler Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionOSHandlerLeave (UINT_PTR)
{
    // cannot use inproc debugging in this method
    show_traceln ("Exception OS Handler Leave");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionUnwindFunctionEnter (FunctionID)
{
    show_traceln ("Exception Unwind Function Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionUnwindFunctionLeave()
{
    show_traceln ("Exception Unwind Function Leave");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionUnwindFinallyEnter (FunctionID)
{
    show_traceln ("Exception Unwind Finally Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionUnwindFinallyLeave()
{
    show_traceln ("Exception Unwind Finally Leave");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionCatcherEnter (FunctionID, ObjectID)
{
    show_traceln ("Exception Catcher Enter");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionCatcherLeave()
{
    return S_OK;
}

STDMETHODIMP CProfiler::COMClassicVTableCreated (ClassID, REFGUID, void *, ULONG)
{
    return S_OK;
}

STDMETHODIMP CProfiler::COMClassicVTableDestroyed (ClassID, REFGUID, void *)
{
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionCLRCatcherFound()
{
    // cannot use inproc debugging in this method
    show_traceln ("Exception CLR Catcher Found");
    return S_OK;
}

STDMETHODIMP CProfiler::ExceptionCLRCatcherExecute()
{
    // cannot use inproc debugging in this method
    return S_OK;
}
