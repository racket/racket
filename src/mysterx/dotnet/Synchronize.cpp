#include "StdAfx.h"
#include "synchronize.h"

Synchronize::Synchronize(CRITICAL_SECTION &criticalSection)
    : m_block (criticalSection)
{
  EnterCriticalSection (&m_block);
}

Synchronize::~Synchronize(void)
{
  LeaveCriticalSection (&m_block);
}
