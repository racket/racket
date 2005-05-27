#include "StdAfx.h"
#include <stdio.h>
#include "corhdr.h"
#include "cor.h"
#include "corhlpr.h"
#include "corprof.h"
#include "cordebug.h"
#include "threadinfo.h"
#include "SimpleException.h"
#include "utils.h"

SimpleException::SimpleException(const char * reason)
: m_reason(NULL)
{
    SIZE_T length = strlen (reason);

    m_reason = new char[(length + 1)];
    strcpy (m_reason, reason);
}

SimpleException::~SimpleException(void)
{
    if (m_reason != NULL)
    delete [] m_reason;
}

void SimpleException::ReportFailure(void)
{
    TEXT_OUTLN (m_reason);
}
