#pragma once

class SimpleException
{
public:
    SimpleException(const char * reason);
    virtual ~SimpleException(void);
private:
    // Why the exception was thrown.
    char *m_reason;
public:
    virtual void ReportFailure(void);
};
