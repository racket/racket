#ifndef _MZCOMCP_H_
#define _MZCOMCP_H_

template <class T>
class CProxy_IMzObjEvents : public IConnectionPointImpl<T, &DIID__IMzObjEvents, CComDynamicUnkArray>
{
	//Warning this class may be recreated by the wizard.
public:
	VOID Fire_SchemeError(BSTR description)
	{
		T* pT = static_cast<T*>(this);
		int nConnectionIndex;
		CComVariant* pvars = new CComVariant[1];
		int nConnections = m_vec.GetSize();
		
		for (nConnectionIndex = 0; nConnectionIndex < nConnections; nConnectionIndex++)
		{
			pT->Lock();
			CComPtr<IUnknown> sp = m_vec.GetAt(nConnectionIndex);
			pT->Unlock();
			IDispatch* pDispatch = reinterpret_cast<IDispatch*>(sp.p);
			if (pDispatch != NULL)
			{
				pvars[0] = description;
				DISPPARAMS disp = { pvars, NULL, 1, 0 };
				pDispatch->Invoke(0x1, IID_NULL, LOCALE_USER_DEFAULT, DISPATCH_METHOD, &disp, NULL, NULL, NULL);
			}
		}
		delete[] pvars;
	}
};
#endif
