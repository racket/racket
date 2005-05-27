#pragma once

class BaseInfo
{
public:
    BaseInfo(UINT_PTR id);
    virtual ~BaseInfo(void);

    virtual void Dump(void);
    BOOL Compare(UINT_PTR key);

    UINT_PTR m_id;
    BOOL m_isValid;
    WCHAR m_name[MAX_LENGTH];
};
