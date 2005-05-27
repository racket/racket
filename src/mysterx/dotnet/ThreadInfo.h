#pragma once
#include "baseinfo.h"

enum TransitionState
{
        UNINITIALIZED,
        MANAGED,
        UNMANAGED
};

class ThreadInfo :
    public BaseInfo
{
public:
    ThreadInfo(ThreadID threadID);
    virtual ~ThreadInfo(void);
    virtual void Dump(void);
    UINT_PTR m_originalTID;
    HANDLE m_hThread;
    TransitionState m_status;
    DWORD m_win32ThreadID;
    DWORD throw_depth;
    BOOL abort_deferred;
};
