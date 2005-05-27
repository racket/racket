// Some random globals.

void InitializePatch (IUnknown * pICorProfilerInfoUnk);
void ShutdownPatch(void);
void AddThread (ThreadID threadID);
void RemoveThread (ThreadID threadID);
void UpdateOSThreadID (ThreadID managedThreadID, DWORD win32ThreadID);
void PatchJIT (FunctionID functionID);
void NoteThreadManaged(void);
void NoteThreadUnmanaged(void);




