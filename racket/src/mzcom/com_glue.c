/* Much of this code is from "COM in Plain C" by Jeff Gatt on Code
   Project. That code is licensed under the Code Project Open License
   (CPOL). */

#include <windows.h>
#include <objbase.h>
#include <activscp.h>
#include <olectl.h>
#include <stddef.h>
#define FOR_GLUE
#include "com_glue.h"

// A count of how many objects our DLL has created (by some
// app calling our IClassFactory object's CreateInstance())
// which have not yet been Release()'d by the app
static DWORD		OutstandingObjects;

// A count of how many apps have locked our DLL via calling our
// IClassFactory object's LockServer()
static DWORD		LockCount;

// Where I store a pointer to my type library's TYPEINFO
static ITypeInfo	*MyTypeInfo;

// The MzObj object ////////////////////////////////////////////////////////////

// In our .H file, we use a macro which defines our MzObj struct
// as so:
//
// typedef struct {
//    IMzObjVtbl  *lpVtbl;
// } MzObj;
//
// In other words, the .H file defines our MzObj to have nothing
// but a pointer to its VTable. And of course, every COM object must
// start with a pointer to its VTable.
//
// But we actually want to add some more members to our MzObj.
// We just don't want any app to be able to know about, and directly
// access, those members. So here we'll define a MyRealMzObj that
// contains those extra members. The app doesn't know that we're
// really allocating and giving it a MyRealMzObj object. We'll
// lie and tell it we're giving a plain old MzObj. That's ok
// because a MyRealMzObj starts with the same VTable pointer.
//
// We add a DWORD reference count so that this MzObj
// can be allocated (which we do in our IClassFactory object's
// CreateInstance()) and later freed. And, we have an extra
// BSTR (pointer) string, which is used by some of the functions we'll
// add to MzObj
typedef struct {
  IMzObjVtbl *lpVtbl;
  DWORD      count;
  void      *obj;
  IConnectionPointContainer container;
  IConnectionPoint          point;
  IMzObjEvents              *evts;
} MyRealMzObj;

// Here are MzObj's functions.
//
// Every COM object's interface must have the 3 functions QueryInterface(),
// AddRef(), and Release().
//
// I also chose to add 2, extra functions to MzObj, which a program
// will call with the names GetString and SetString.

// MzObj's QueryInterface()
static HRESULT STDMETHODCALLTYPE QueryInterface(IMzObj *com_obj, REFIID vTableGuid, void **ppv)
{
  // Because our IMzObj sources events, we must return an
  // IConnectionPointContainer sub-object if the app asks for one. Because we've
  // embedded our IConnectionPointContainer object inside of our MyRealIMzObj,
  // we can get that sub-object very easily using pointer arithmetic
  if (IsEqualIID(vTableGuid, &IID_IConnectionPointContainer))
    *ppv = ((unsigned char *)com_obj + offsetof(MyRealMzObj, container));
  else if (IsEqualIID(vTableGuid, &IID_IConnectionPoint))
    *ppv = ((unsigned char *)com_obj + offsetof(MyRealMzObj, point));

  // Check if the GUID matches MzObj VTable's GUID. We gave the C variable name
  // IID_MzObj to our VTable GUID. We can use an OLE function called
  // IsEqualIID to do the comparison for us. Also, if the caller passed a
  // IUnknown GUID, then we'll likewise return the MzObj, since it can
  // masquerade as an IUnknown object too. Finally, if the called passed a
  // IDispatch GUID, then we'll return the MzObj, since it can masquerade
  // as an IDispatch too
  else if (!IsEqualIID(vTableGuid, &IID_IUnknown) && !IsEqualIID(vTableGuid, &IID_IMzObj) && !IsEqualIID(vTableGuid, &IID_IDispatch))
    {
      // We don't recognize the GUID passed to us. Let the caller know this,
      // by clearing his handle, and returning E_NOINTERFACE.
      *ppv = 0;
      return(E_NOINTERFACE);
    }
  else
    // Fill in the caller's handle
    *ppv = com_obj;

  // Increment the count of callers who have an outstanding pointer to this object
  com_obj->lpVtbl->AddRef(com_obj);

  return(NOERROR);
}

// MzObj's AddRef()
static ULONG STDMETHODCALLTYPE AddRef(IMzObj *com_obj)
{
  // Increment MzObj's reference count, and return the updated value.
  // NOTE: We have to typecast to gain access to any data members. These
  // members are not defined in our .H file (so that an app can't directly
  // access them). Rather they are defined only above in our MyRealMzObj
  // struct. So typecast to that in order to access those data members
  return(++((MyRealMzObj *)com_obj)->count);
}

// MzObj's Release()
static ULONG STDMETHODCALLTYPE Release(IMzObj *com_obj)
{
  // Decrement MzObj's reference count. If 0, then we can safely free
  // this MzObj now
  if (--((MyRealMzObj *)com_obj)->count == 0)
    {
      delete_mzobj(((MyRealMzObj *)com_obj)->obj);
      GlobalFree(com_obj);
      InterlockedDecrement(&OutstandingObjects);
      
      if (com_can_unregister()) {
        /* Only allowed object is released... */
        PostMessage(NULL, WM_QUIT, 0, 0);
      }

      return(0);
    }
  return(((MyRealMzObj *)com_obj)->count);
}

// ================== The standard IDispatch functions

// This is just a helper function for the IDispatch functions below
static HRESULT loadMyTypeInfo(void)
{
  register HRESULT	hr;
  LPTYPELIB			pTypeLib;

  // Load our type library and get a ptr to its TYPELIB. Note: This does an
  // implicit pTypeLib->lpVtbl->AddRef(pTypeLib)
  if (!(hr = LoadRegTypeLib(&CLSID_TypeLib, 1, 0, 0, &pTypeLib)))
    {
      // Get Microsoft's generic ITypeInfo, giving it our loaded type library. We only
      // need one of these, and we'll store it in a global Tell Microsoft this is for
      // our MzObj's VTable, by passing that VTable's GUID
      if (!(hr = pTypeLib->lpVtbl->GetTypeInfoOfGuid(pTypeLib, &IID_IMzObj, &MyTypeInfo)))
        {
          // We no longer need the ptr to the TYPELIB now that we've given it
          // to Microsoft's generic ITypeInfo. Note: The generic ITypeInfo has done
          // a pTypeLib->lpVtbl->AddRef(pTypeLib), so this TYPELIB ain't going away
          // until the generic ITypeInfo does a pTypeLib->lpVtbl->Release too
          pTypeLib->lpVtbl->Release(pTypeLib);

          // Since caller wants us to return our ITypeInfo pointer,
          // we need to increment its reference count. Caller is
          // expected to Release() it when done
          MyTypeInfo->lpVtbl->AddRef(MyTypeInfo);
        }
    }

  return(hr);
}

// MzObj's GetTypeInfoCount()
static ULONG STDMETHODCALLTYPE GetTypeInfoCount(IMzObj *com_obj, UINT *pCount)
{
  *pCount = 1;
  return(S_OK);
}

// MzObj's GetTypeInfo()
static ULONG STDMETHODCALLTYPE GetTypeInfo(IMzObj *com_obj, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo)
{
  register HRESULT	hr;

  // Assume an error
  *pTypeInfo = 0;
	
  if (itinfo)
    hr = ResultFromScode(DISP_E_BADINDEX);

  // If our ITypeInfo is already created, just increment its ref count. NOTE: We really should
  // store the LCID of the currently created TYPEINFO and compare it to what the caller wants.
  // If no match, unloaded the currently created TYPEINFO, and create the correct one. But since
  // we support only one language in our IDL file anyway, we'll ignore this
  else if (MyTypeInfo)
    {
      MyTypeInfo->lpVtbl->AddRef(MyTypeInfo);
      hr = 0;
    }
  else
    {
      // Load our type library and get Microsoft's generic ITypeInfo object. NOTE: We really
      // should pass the LCID to match, but since we support only one language in our IDL
      // file anyway, we'll ignore this
      hr = loadMyTypeInfo();
    }

  if (!hr) *pTypeInfo = MyTypeInfo;

  return(hr);
}

// MzObj's GetIDsOfNames()
static ULONG STDMETHODCALLTYPE GetIDsOfNames(IMzObj *com_obj, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid)
{
  if (!MyTypeInfo)
    {
      register HRESULT	hr;

      if ((hr = loadMyTypeInfo())) return(hr);
    }
	
  // Let OLE32.DLL's DispGetIDsOfNames() do all the real work of using our type
  // library to look up the DISPID of the requested function in our object
  return(DispGetIDsOfNames(MyTypeInfo, rgszNames, cNames, rgdispid));
}

// MzObj's Invoke()
static ULONG STDMETHODCALLTYPE Invoke(IMzObj *com_obj, DISPID dispid, REFIID riid, LCID lcid, WORD wFlags,
                                      DISPPARAMS *params, VARIANT *result, EXCEPINFO *pexcepinfo, 
                                      UINT *puArgErr)
{
  // We implement only a "default" interface
  if (!IsEqualIID(riid, &IID_NULL))
    return(DISP_E_UNKNOWNINTERFACE);

  // We need our type lib's TYPEINFO (to pass to DispInvoke)
  if (!MyTypeInfo)
    {
      register HRESULT	hr;

      if ((hr = loadMyTypeInfo())) return(hr);
    }

  // Let OLE32.DLL's DispInvoke() do all the real work of calling the appropriate
  // function in our object, and massaging the passed args into the correct format
  return(DispInvoke(com_obj, MyTypeInfo, dispid, wFlags, params, result, pexcepinfo, puArgErr));
}

// ================== The following are my own extra functions added to MzObj

static HRESULT STDMETHODCALLTYPE Eval(IMzObj *com_obj, BSTR str, BSTR *res)
{
  if (!str) return(E_POINTER);

  return mzobj_eval(((MyRealMzObj*)com_obj)->obj, str, res);
}

static HRESULT STDMETHODCALLTYPE About(IMzObj *com_obj)
{
  return mzobj_about(((MyRealMzObj*)com_obj)->obj);
}

static HRESULT STDMETHODCALLTYPE Reset(IMzObj *com_obj)
{
  return mzobj_reset(((MyRealMzObj*)com_obj)->obj);
}

// Here's MzObj's VTable. It never changes so we can declare it
// static
static const IMzObjVtbl IMzObj_Vtbl = {QueryInterface,
                                       AddRef,
                                       Release,
                                       GetTypeInfoCount,
                                       GetTypeInfo,
                                       GetIDsOfNames,
                                       Invoke,
                                       Eval,
                                       About,
                                       Reset};


VOID Fire_SchemeError(IMzObj *com_obj, BSTR description)
{
  if (((MyRealMzObj*)com_obj)->evts) {
    VARIANTARG pvars[1];
    DISPPARAMS disp = { pvars, NULL, 1, 0 };
    memset(pvars, 0, sizeof(pvars));
    pvars[0].vt = VT_BSTR;
    pvars[0].bstrVal = description;
    ((MyRealMzObj*)com_obj)->evts->lpVtbl->Invoke(((MyRealMzObj*)com_obj)->evts, 0x1, &IID_NULL,
                                                  LOCALE_USER_DEFAULT, DISPATCH_METHOD, &disp, 
                                                  NULL, NULL, NULL);
  }
}

// Our IConnectionPointContainer sub-object (for IMzObj) ////////////////////////

static STDMETHODIMP QueryInterface_Connect(IConnectionPointContainer *com_obj, REFIID vTableGuid, void **ppv)
{
  // Because this is a sub-object of our IMzObj (ie, MyRealMzObj) object,
  // we delegate to IMzObj's QueryInterface. And because we embedded the
  // IConnectionPointContainer directly inside of MyRealMzObj, all we need
  // is a little pointer arithmetic to get our IMzObj
  return(QueryInterface((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, container)), vTableGuid, ppv));
}

static STDMETHODIMP_(ULONG) AddRef_Connect(IConnectionPointContainer *com_obj)
{
  // Because we're a sub-object of IMzObj, delegate to its AddRef()
  // in order to increment IMzObj's reference count
  return(AddRef((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, container))));
}

static STDMETHODIMP_(ULONG) Release_Connect(IConnectionPointContainer *com_obj)
{
  // Because we're a sub-object of IMzObj, delegate to its Release()
  // in order to decrement IMzObj's reference count
  return(Release((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, container))));
}

static STDMETHODIMP EnumConnectionPoints(IConnectionPointContainer *com_obj, IEnumConnectionPoints **enumPoints)
{
  // The app had better know the GUIDs of whatever objects our
  // IMzObj supports for callbacks (ie, an IMzObjEvents), because
  // we're not going to bother providing him with an object to
  // enumerate the VTable GUIDs of all those supported objects
  *enumPoints = 0;
  return(E_NOTIMPL);
}
 
static STDMETHODIMP FindConnectionPoint(IConnectionPointContainer *com_obj, REFIID vTableGuid, IConnectionPoint **ppv) 
{
  // Is the app asking us to return an IConnectionPoint object it can use
  // to give us its IMzObjEvents object? The app asks this by passing us
  // IMzObjEvents VTable's GUID (which we defined in IMzObj.h)
  if (IsEqualIID(vTableGuid, &DIID_IMzObjEvents))
    {
      MyRealMzObj *iExample;

      // The app obviously wants to connect its IMzObjEvents object
      // to IMzObj. In order to do that, we need to give the app a
      // standard IConnectionPoint, so the app can call its Advise function
      // to give us its IMzObjEvents. This is easy to do since we embedded both
      // our IConnectionPointContainer and IConnectionPoint inside of our 
      // IMzObj. All we need is a little pointer arithmetic
      iExample = (MyRealMzObj *)((char *)com_obj - offsetof(MyRealMzObj, container));
      *ppv = &iExample->point;

      // Because we're giving the app a pointer to our IConnectionPoint, and
      // our IConnectionPoint is a sub-object of IMzObj, we need to
      // increment IMzObj's reference count. The easiest way to do this is to call
      // our IConnectionPointContainer's AddRef, because all we do there is delegate
      // to our IMzObj's AddRef
      AddRef_Connect(com_obj);

      return(S_OK);
    }

  // We don't support any other app objects connecting to IMzObj
  // events. All we've defined, and support, is an IMzObjEvents object. Tell
  // the app we don't know anything about the GUID he passed to us, and
  // do not give him any IConnectPoint object
  *ppv = 0;
  return(E_NOINTERFACE);
}


static const IConnectionPointContainerVtbl IConnectionPointContainer_Vtbl = {QueryInterface_Connect,
                                                                             AddRef_Connect,
                                                                             Release_Connect,
                                                                             EnumConnectionPoints,
                                                                             FindConnectionPoint};

// Our IConnectionPoint sub-object (for IMzObj) ////////////////////////////

static STDMETHODIMP QueryInterface_Point(IConnectionPoint *com_obj, REFIID vTableGuid, void **ppv)
{
  // Because this is a sub-object of our IMzObj (ie, MyRealMzObj) object,
  // we delegate to IMzObj's QueryInterface. And because we embedded the
  // IConnectionPoint directly inside of MyRealMzObj, all we need
  // is a little pointer arithmetic to get our IMzObj
  return(QueryInterface((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point)), vTableGuid, ppv));
}

static STDMETHODIMP_(ULONG) AddRef_Point(IConnectionPoint *com_obj)
{
  // Because we're a sub-object of IMzObj, delegate to its AddRef()
  // in order to increment IMzObj's reference count
  return(AddRef((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point))));
}

static STDMETHODIMP_(ULONG) Release_Point(IConnectionPoint *com_obj)
{
  // Because we're a sub-object of IMzObj, delegate to its Release()
  // in order to decrement IMzObj's reference count
  return(Release((IMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point))));
}

// Called by the app to get our IMzObjEvents VTable's GUID (which we defined in IMzObj.h).
// The app would call GetConnectionInterface() if it didn't link with IMzObj.h, and
// therefore doesn't know our IMzObjEvents VTable's GUID. The app needs to know this GUID
// because our Advise function below is going to pass this same GUID to some app object's
// QueryInterface. The app's QueryInterface had better recognize this GUID if it intends
// to honor our request to give us its IMzObjEvents object
static STDMETHODIMP GetConnectionInterface(IConnectionPoint *com_obj, IID *vTableGuid) 
{
  // Tell the app to recognize our IMzObjEvents VTable GUID (defined as
  // DIID_IFeedback in IMzObj.h) when our Advise function calls
  // some app QueryInterface function
  CopyMemory(vTableGuid, &DIID_IMzObjEvents, sizeof(GUID));
  return(S_OK);
}
 
// Called by the app to get the IConnectionPointContainer sub-object for our
// IMzObj object.
static STDMETHODIMP GetConnectionPointContainer(IConnectionPoint *com_obj, IConnectionPointContainer **ppv) 
{
  MyRealMzObj	*iExample;

  // Get the MyRealMzObj that this IConnectionPoint sub-object belongs
  // to. Because this IConnectPoint sub-object is embedded directly inside its
  // MyRealMzObj, all we need is a little pointer arithmetic
  iExample = (MyRealMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point));

  // Because the IConnectionPointContainer sub-object is also embedded right inside
  // the same MyRealMzObj, we can get a pointer to it easily as so
  *ppv = &iExample->container;

  // Because we're giving the app a pointer to our IConnectionPointContainer, and
  // our IConnectionPointContainer is a sub-object of IMzObj, we need to
  // increment IMzObj's reference count. The easiest way to do this is to call
  // our IConnectionPoint's AddRef, because all we do there is delegate
  // to our IMzObj's AddRef
  AddRef_Point(com_obj);

  return(S_OK);
}

// Called by the app to give us its IMzObjEvents object. Actually, the app doesn't
// just give us its IMzObjEvents. Rather, the app calls our Advise, passing us some
// app object from which we can request the app to give us its IMzObjEvents. All of
// this convoluted stuff is a combination of poor pre-planning by Microsoft
// programmers when they designed this stuff, as well as the colossal blunder of
// designing COM to accomodate the limitations of early, primitive editions of
// Visual Basic.
//
// The second arg passed here is some app object whose QueryInterface function
// we call to request the app's IMzObjEvents. We pass the GUID DIID_IMzObjEvents to
// this QueryInterface in order to tell the app to give us its IMzObjEvents
static STDMETHODIMP Advise(IConnectionPoint *com_obj, IUnknown *obj, DWORD *cookie) 
{
  HRESULT			hr;
  MyRealMzObj	*iExample;

  // Get the MyRealMzObj that this IConnectionPoint sub-object belongs
  // to. Because this IConnectPoint sub-object is embedded directly inside its
  // MyRealMzObj, all we need is a little pointer arithmetic
  iExample = (MyRealMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point));

  // We allow only one IMzObjEvents for our IMzObj, so see if the app already
  // called our Advise(), and we got one. If so, let the app know that it is trying
  // to give us more IFeedbacks2 than we allow
  if (iExample->evts) return(CONNECT_E_ADVISELIMIT);
 
  // Ok, we haven't yet gotten the one IMzObjEvents we allow from the app. Get the app's 
  // IMzObjEvents object. We do this by calling the QueryInterface function of the
  // app object passed to us. We pass IMzObjEvents VTable's GUID (which we defined
  // in IMzObj.h).
  //
  // Save the app's IMzObjEvents pointer in our IMzObj feedback member, so we
  // can get it when we need it
  hr = obj->lpVtbl->QueryInterface(obj, &DIID_IMzObjEvents, (void **)&iExample->evts);

  // We need to return (to the app) some value that will clue our Unadvise() function
  // below how to locate this app IMzObjEvents. The simpliest thing is to just use the
  // app's IMzObjEvents pointer as that returned value
  *cookie = (DWORD)iExample->evts;

  return(hr);
}

// Called by the app to tell us to stop using, and Release(), its IMzObjEvents object.
// The second arg passed here is the value our Advise() function above returned when
// we got the IMzObjEvents from the app. This value should help us locate wherever we
// stored that IMzObjEvents pointer we got in Advise()
static STDMETHODIMP Unadvise(IConnectionPoint *com_obj, DWORD cookie) 
{
  MyRealMzObj	*iExample;

  // Get the MyRealMzObj that this IConnectionPoint sub-object belongs
  // to. Because this IConnectPoint sub-object is embedded directly inside its
  // MyRealMzObj, all we need is a little pointer arithmetic
  iExample = (MyRealMzObj *)((char *)com_obj - offsetof(MyRealMzObj, point));

  // Use the passed value to find wherever we stored his IMzObjEvents pointer.
  // Well, since we allow only one IMzObjEvents for our IMzObj, we already
  // know we stored it in our IMzObj->feedback member. And Advise()
  // returned that pointer as the "cookie" value. So we already got the
  // IMzObjEvents right now.
  //		
  // Let's just make sure the cookie he passed is really the pointer we expect
  if (cookie && (IMzObjEvents *)cookie == iExample->evts)
    {
      // Release the app's IMzObjEvents
      ((IMzObjEvents *)cookie)->lpVtbl->Release((IMzObjEvents *)cookie);

      // We no longer have the app's IMzObjEvents, so clear the IMzObj
      // feedback member
      iExample->evts = 0;

      return(S_OK);
    }
  return(CONNECT_E_NOCONNECTION);
}

static STDMETHODIMP EnumConnections(IConnectionPoint *com_obj, IEnumConnections **enumConnects)
{
  *enumConnects = 0;
  return(E_NOTIMPL);
}


static const IConnectionPointVtbl IConnectionPoint_Vtbl = {
  QueryInterface_Point,
  AddRef_Point,
  Release_Point,
  GetConnectionInterface,
  GetConnectionPointContainer,
  Advise,
  Unadvise,
  EnumConnections};

// The IClassFactory object ///////////////////////////////////////////////////////

// Since we only ever need one IClassFactory object, we declare
// it static. The only requirement is that we ensure any
// access to its members is thread-safe
static IClassFactory	MyIClassFactoryObj;

// IClassFactory's AddRef()
static ULONG STDMETHODCALLTYPE classAddRef(IClassFactory *com_obj)
{
  // Someone is obtaining my IClassFactory, so inc the count of
  // pointers that I've returned which some app needs to Release()
  InterlockedIncrement(&OutstandingObjects);

  // Since we never actually allocate/free an IClassFactory (ie, we
  // use just 1 static one), we don't need to maintain a separate
  // reference count for our IClassFactory. We'll just tell the caller
  // that there's at least one of our IClassFactory objects in existance
  return(1);
}

// IClassFactory's QueryInterface()
static HRESULT STDMETHODCALLTYPE classQueryInterface(IClassFactory *com_obj, REFIID factoryGuid, void **ppv)
{
  // Make sure the caller wants either an IUnknown or an IClassFactory.
  // In either case, we return the same IClassFactory pointer passed to
  // us since it can also masquerade as an IUnknown
  if (IsEqualIID(factoryGuid, &IID_IUnknown) || IsEqualIID(factoryGuid, &IID_IClassFactory))
    {
      // Call my IClassFactory's AddRef
      com_obj->lpVtbl->AddRef(com_obj);

      // Return (to the caller) a ptr to my IClassFactory
      *ppv = com_obj;

      return(NOERROR);
    }

  // We don't know about any other GUIDs
  *ppv = 0;
  return(E_NOINTERFACE);
}

// IClassFactory's Release()
static ULONG STDMETHODCALLTYPE classRelease(IClassFactory *com_obj)
{
  // One less object that an app has not yet Release()'ed
  return(InterlockedDecrement(&OutstandingObjects));
}

// IClassFactory's CreateInstance() function. It is called by
// someone who has a pointer to our IClassFactory object and now
// wants to create and retrieve a pointer to our MzObj
static HRESULT STDMETHODCALLTYPE classCreateInstance(IClassFactory *com_obj, IUnknown *punkOuter, REFIID vTableGuid, void **objHandle)
{
  HRESULT hr;
  IMzObj *thisobj;

  // Assume an error by clearing caller's handle
  *objHandle = 0;

  // We don't support aggregation in this example
  if (punkOuter)
    hr = CLASS_E_NOAGGREGATION;
  else
    {
      // Allocate our MzObj object (actually a MyRealMzObj)
      if (!(thisobj = (IMzObj *)GlobalAlloc(GMEM_FIXED, sizeof(MyRealMzObj))))
        hr = E_OUTOFMEMORY;
      else
        {
          // Store MzObj's VTable in the object
          thisobj->lpVtbl = (IMzObjVtbl *)&IMzObj_Vtbl;

          // Our MyRealIMzObj is a multiple interface object. It has an
          // IConnectionPointContainer sub-object embedded directly inside of
          // it. And we just allocated it when we allocated the MyRealIMzObj
          // above. Now we need to set its VTable into its lpVtbl member and
          // we're done initializing this sub-object
          ((MyRealMzObj *)thisobj)->container.lpVtbl = (IConnectionPointContainerVtbl *)&IConnectionPointContainer_Vtbl;
          
          // Our MyRealIMzObj also has an IConnectionPoint sub-object
          // embedded directly inside of it. And we just allocated it when we
          // allocated the MyRealIMzObj above. Now we need to set its
          // VTable into its lpVtbl member and we're done initializing this sub-object
          ((MyRealMzObj *)thisobj)->point.lpVtbl = (IConnectionPointVtbl *)&IConnectionPoint_Vtbl;

          // Increment the reference count so we can call Release() below and
          // it will deallocate only if there is an error with QueryInterface()
          ((MyRealMzObj *)thisobj)->count = 1;

          // Initialize any other members we added to the MzObj. We added
          // a string member
          ((MyRealMzObj *)thisobj)->obj = new_mzobj(thisobj);

          ((MyRealMzObj *)thisobj)->evts = NULL;

          // Fill in the caller's handle with a pointer to the MzObj we just
          // allocated above. We'll let MzObj's QueryInterface do that, because
          // it also checks the GUID the caller passed, and also increments the
          // reference count (to 2) if all goes well
          hr = IMzObj_Vtbl.QueryInterface(thisobj, vTableGuid, objHandle);

          // Decrement reference count. NOTE: If there was an error in QueryInterface()
          // then Release() will be decrementing the count back to 0 and will free the
          // MzObj for us. One error that may occur is that the caller is asking for
          // some sort of object that we don't support (ie, it's a GUID we don't recognize)
          IMzObj_Vtbl.Release(thisobj);

          // If success, inc static object count to keep this DLL loaded
          if (!hr) InterlockedIncrement(&OutstandingObjects);
        }
    }

  return(hr);
}

// IClassFactory's LockServer(). It is called by someone
// who wants to lock this DLL in memory
static HRESULT STDMETHODCALLTYPE classLockServer(IClassFactory *com_obj, BOOL flock)
{
  if (flock) InterlockedIncrement(&LockCount);
  else InterlockedDecrement(&LockCount);

  return(NOERROR);
}

// IClassFactory's VTable
static const IClassFactoryVtbl IClassFactory_Vtbl = {classQueryInterface,
                                                     classAddRef,
                                                     classRelease,
                                                     classCreateInstance,
                                                     classLockServer};



// Miscellaneous functions ///////////////////////////////////////////////////////

static DWORD reg_cookie;

HRESULT com_register()
{
  // Initialize my IClassFactory with the pointer to its vtable
  MyIClassFactoryObj.lpVtbl = (IClassFactoryVtbl *)&IClassFactory_Vtbl;

  return CoRegisterClassObject(&CLSID_IMzObj, &MyIClassFactoryObj,
                               CLSCTX_LOCAL_SERVER, REGCLS_SINGLEUSE, &reg_cookie);
}

int com_can_unregister()
/* called from multiple threads */
{
  /* Note that OutstandingObjects will stay at least 1 after the class is registered. */
  return !((OutstandingObjects > 1) || (LockCount > 0));
}

int com_unregister()
{
  // If someone has retrieved pointers to any of our objects, and
  // not yet Release()'ed them, then we return S_FALSE to indicate
  // not to unload this DLL. Also, if someone has us locked, return
  // S_FALSE
  if (!com_can_unregister())
    return 0;
  else {
    if (MyTypeInfo) MyTypeInfo->lpVtbl->Release(MyTypeInfo);
    CoRevokeClassObject(reg_cookie);
    return 1;
  }
}

const GUID com_get_class_iid()
{
  return IID_IMzObj;
}
