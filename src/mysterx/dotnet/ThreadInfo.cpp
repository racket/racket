#include "StdAfx.h"
#include <stdio.h>
#include "corhdr.h"
#include "cor.h"
#include "corhlpr.h"
#include "corprof.h"
#include "cordebug.h"
#include "threadinfo.h"

ThreadInfo::ThreadInfo(ThreadID threadID) :
    BaseInfo (threadID)
	, m_originalTID(threadID)
        , m_hThread(NULL)
        , m_status (UNINITIALIZED)
	, m_win32ThreadID(0)
	, throw_depth(0)
	, abort_deferred(FALSE)
    {
      wcscpy (m_name, L"THREAD");
}

ThreadInfo::~ThreadInfo(void)
{
}

void ThreadInfo::Dump()
{
    fwprintf (stderr, L"\n" );
    fwprintf (stderr, L"THREAD ID: 0x%08x (%d)\n", m_id, m_id);
    fwprintf (stderr, L"\tHANDLE: 0x%08x (%d)\n", m_hThread, m_hThread);
    fwprintf (stderr, L"\tWIN32 ID: 0x%08x (%d)\n", m_win32ThreadID, m_win32ThreadID);
    fwprintf (stderr, L"\tTHROW DEPTH: 0x%08x\n", throw_depth);
    fwprintf (stderr, L"\n" );
}
