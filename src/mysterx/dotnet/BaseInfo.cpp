#include "StdAfx.h"
#include "baseinfo.h"

BaseInfo::BaseInfo(UINT_PTR id)
: m_id(id)
, m_isValid(TRUE)
{
  wcscpy (m_name, L"UNKNOWN");
}

BaseInfo::~BaseInfo(void)
{
}

BOOL BaseInfo::Compare (UINT_PTR key)
{
    return (BOOL)(m_id == key);
}

void BaseInfo::Dump()
{
    wprintf (L"\n");
    wprintf (L"ID: 0x%08x\n", m_id);
    wprintf (L"\n");
}
