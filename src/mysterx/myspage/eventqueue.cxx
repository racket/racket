// eventqueue.cxx : Implementation of CEventQueue

#include "stdafx.h"
#include <stdio.h>
#include <limits.h>

#include <stdarg.h>

#include "escheme.h"
#include "myspage.h"
#include "dhtmlpage.h"

#include "eventqueue.h"


/////////////////////////////////////////////////////////////////////////////
// CEventQueue

CEventQueue::CEventQueue (void)
{
  queueLength = 0;
  readerNdx = writerNdx = 0;

  readSem = CreateSemaphore (NULL, 0, LONG_MAX, NULL);  // using MAXQUEUELENGTH doesn't work
  mutex = CreateSemaphore (NULL, 1, 1, NULL);

  if (readSem == NULL || mutex == NULL)
      ::failureBox ("Error creating event semaphore(s)");
}

CEventQueue::~CEventQueue (void)
{
  if (readSem != NULL)
      CloseHandle (readSem);

  if (mutex != NULL)
      CloseHandle (mutex);
}

STDMETHODIMP CEventQueue::QueueEvent (IEvent *pEvent)
{
  BOOL signalReader;

  WaitForSingleObject (mutex, INFINITE);

  if (queueLength < MAXQUEUELENGTH) {
      queueLength++;
      signalReader = TRUE;
      }
  else {
      readerNdx = ++readerNdx % MAXQUEUELENGTH;
      signalReader = FALSE;
      }

  theQueue[writerNdx] = pEvent;

  writerNdx = ++writerNdx % MAXQUEUELENGTH;

  ReleaseSemaphore (mutex, 1, NULL);

  if (signalReader)
      ReleaseSemaphore (readSem, 1, NULL);

  return S_OK;

}

STDMETHODIMP CEventQueue::GetEvent (IEvent **ppEvent)
{
  *ppEvent = NULL;

  WaitForSingleObject (readSem, INFINITE);

  WaitForSingleObject (mutex, INFINITE);

  *ppEvent = theQueue[readerNdx];

  readerNdx = ++readerNdx % MAXQUEUELENGTH;
  queueLength--;

  ReleaseSemaphore (mutex, 1, NULL);

  return S_OK;
}

STDMETHODIMP CEventQueue::get_EventAvailable (VARIANT_BOOL *pVal)
{
  WaitForSingleObject (mutex, INFINITE);

  *pVal = (queueLength == 0) ? 0 : -1;

  ReleaseSemaphore (mutex, 1, NULL);

  return S_OK;
}

STDMETHODIMP CEventQueue::GetReaderSemaphore (long * pReadSem)
{
  * ((HANDLE *)pReadSem) = readSem;

  return S_OK;
}

// This method is deprecated.  It is still here so that older
// code won't break.
STDMETHODIMP CEventQueue::set_extension_table (int)
{
  /* scheme_extension_table = (Scheme_Extension_Table *)p; */
  return S_OK;
}
