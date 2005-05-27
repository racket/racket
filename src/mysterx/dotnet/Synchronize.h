#pragma once

class Synchronize
{
public:
    Synchronize(CRITICAL_SECTION &criticalSection);
    ~Synchronize(void);
private:
    // Cheesy unwind-protect in C++.
    CRITICAL_SECTION m_block;
};
