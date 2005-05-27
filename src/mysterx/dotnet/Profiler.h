// Profiler.h : Declaration of the CProfiler

#pragma once
#include "resource.h"       // main symbols

// IProfiler
[
	object,
	uuid("83D97963-F095-4B79-93CD-654785404021"),
	dual,	helpstring("IProfiler Interface"),
	pointer_default(unique)
]
__interface IProfiler : IDispatch
{
};



// CProfiler

[
	coclass,
	threading("single"),
	aggregatable("never"),
	vi_progid("MysterX.DotnetProfiler"),
	progid("MysterX.DotnetProfiler.1"),
	version(1.0),
	uuid("00CAE45B-E089-4EB1-AA4B-0AB5C74932F8"),
	helpstring("Profiler Class")
]
class ATL_NO_VTABLE CProfiler :
      public IProfiler,
      public ICorProfilerCallback
{
public:
	CProfiler()
	{
	}


	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}

	void FinalRelease()
	{
	}

public:
 STDMETHOD(Initialize)(IUnknown*);
 STDMETHOD(Shutdown)();
 STDMETHOD(AppDomainCreationStarted)(AppDomainID);
 STDMETHOD(AppDomainCreationFinished)(AppDomainID,HRESULT);
 STDMETHOD(AppDomainShutdownStarted)(AppDomainID);
 STDMETHOD(AppDomainShutdownFinished)(AppDomainID,HRESULT);
 STDMETHOD(AssemblyLoadStarted)(AssemblyID);
 STDMETHOD(AssemblyLoadFinished)(AssemblyID,HRESULT);
 STDMETHOD(AssemblyUnloadStarted)(AssemblyID);
 STDMETHOD(AssemblyUnloadFinished)(AssemblyID,HRESULT);
 STDMETHOD(ModuleLoadStarted)(ModuleID);
 STDMETHOD(ModuleLoadFinished)(ModuleID,HRESULT);
 STDMETHOD(ModuleUnloadStarted)(ModuleID);
 STDMETHOD(ModuleUnloadFinished)(ModuleID,HRESULT);
 STDMETHOD(ModuleAttachedToAssembly)(ModuleID,AssemblyID);
 STDMETHOD(ClassLoadStarted)(ClassID);
 STDMETHOD(ClassLoadFinished)(ClassID,HRESULT);
 STDMETHOD(ClassUnloadStarted)(ClassID);
 STDMETHOD(ClassUnloadFinished)(ClassID,HRESULT);
 STDMETHOD(FunctionUnloadStarted)(FunctionID);
 STDMETHOD(JITCompilationStarted)(FunctionID,BOOL);
 STDMETHOD(JITCompilationFinished)(FunctionID,HRESULT,BOOL);
 STDMETHOD(JITCachedFunctionSearchStarted)(FunctionID,BOOL*);
 STDMETHOD(JITCachedFunctionSearchFinished)(FunctionID,COR_PRF_JIT_CACHE);
 STDMETHOD(JITFunctionPitched)(FunctionID);
 STDMETHOD(JITInlining)(FunctionID,FunctionID,BOOL*);
 STDMETHOD(ThreadCreated)(ThreadID);
 STDMETHOD(ThreadDestroyed)(ThreadID);
 STDMETHOD(ThreadAssignedToOSThread)(ThreadID,DWORD);
 STDMETHOD(RemotingClientInvocationStarted)();
 STDMETHOD(RemotingClientSendingMessage)(GUID*,BOOL);
 STDMETHOD(RemotingClientReceivingReply)(GUID*,BOOL);
 STDMETHOD(RemotingClientInvocationFinished)();
 STDMETHOD(RemotingServerReceivingMessage)(GUID*,BOOL);
 STDMETHOD(RemotingServerInvocationStarted)();
 STDMETHOD(RemotingServerInvocationReturned)();
 STDMETHOD(RemotingServerSendingReply)(GUID*,BOOL);
 STDMETHOD(UnmanagedToManagedTransition)(FunctionID,COR_PRF_TRANSITION_REASON);
 STDMETHOD(ManagedToUnmanagedTransition)(FunctionID,COR_PRF_TRANSITION_REASON);
 STDMETHOD(RuntimeSuspendStarted)(COR_PRF_SUSPEND_REASON);
 STDMETHOD(RuntimeSuspendFinished)();
 STDMETHOD(RuntimeSuspendAborted)();
 STDMETHOD(RuntimeResumeStarted)();
 STDMETHOD(RuntimeResumeFinished)();
 STDMETHOD(RuntimeThreadSuspended)(ThreadID);
 STDMETHOD(RuntimeThreadResumed)(ThreadID);
 STDMETHOD(MovedReferences)(ULONG,ObjectID[],ObjectID[],ULONG[]);
 STDMETHOD(ObjectAllocated)(ObjectID,ClassID);
 STDMETHOD(ObjectsAllocatedByClass)(ULONG,ClassID[],ULONG[]);
 STDMETHOD(ObjectReferences)(ObjectID,ClassID,ULONG,ObjectID[]);
 STDMETHOD(RootReferences)(ULONG,ObjectID[]);
 STDMETHOD(ExceptionThrown)(ObjectID);
 STDMETHOD(ExceptionSearchFunctionEnter)(FunctionID);
 STDMETHOD(ExceptionSearchFunctionLeave)();
 STDMETHOD(ExceptionSearchFilterEnter)(FunctionID);
 STDMETHOD(ExceptionSearchFilterLeave)();
 STDMETHOD(ExceptionSearchCatcherFound)(FunctionID);
 STDMETHOD(ExceptionOSHandlerEnter)(UINT_PTR);
 STDMETHOD(ExceptionOSHandlerLeave)(UINT_PTR);
 STDMETHOD(ExceptionUnwindFunctionEnter)(FunctionID);
 STDMETHOD(ExceptionUnwindFunctionLeave)();
 STDMETHOD(ExceptionUnwindFinallyEnter)(FunctionID);
 STDMETHOD(ExceptionUnwindFinallyLeave)();
 STDMETHOD(ExceptionCatcherEnter)(FunctionID,ObjectID);
 STDMETHOD(ExceptionCatcherLeave)();
 STDMETHOD(COMClassicVTableCreated)(ClassID,REFGUID,void*,ULONG);
 STDMETHOD(COMClassicVTableDestroyed)(ClassID,REFGUID,void*);
 STDMETHOD(ExceptionCLRCatcherFound)();
 STDMETHOD(ExceptionCLRCatcherExecute)();
};

