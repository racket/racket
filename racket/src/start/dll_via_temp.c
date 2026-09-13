#include <windows.h>
#include <strsafe.h>
#include <string.h>
#include "dll_via_temp.h"

static BOOL WriteAllBytes(HANDLE hFile, const void *data, SIZE_T size)
{
  const BYTE *p = (const BYTE *)data;
  SIZE_T remaining = size;
  while (remaining > 0) {
    DWORD chunk = (remaining > 0xFFFFFFFF) ? 0xFFFFFFFF : (DWORD)remaining;
    DWORD written = 0;
    if (!WriteFile(hFile, p, chunk, &written, NULL) || written == 0)
      return FALSE;
    p += written;
    remaining -= written;
  }
  return TRUE;
}

// Compares the open file's content against data/size.
// On success sets *equal; returns FALSE only on an actual I/O error.
static BOOL FileContentEquals(HANDLE hFile, const void *data, SIZE_T size, BOOL *equal)
{
  *equal = FALSE;

  LARGE_INTEGER fileSize;
  if (!GetFileSizeEx(hFile, &fileSize))
    return FALSE;

  if ((ULONGLONG)fileSize.QuadPart != (ULONGLONG)size)
    return TRUE;   // sizes differ -> not equal, but not an error

  const BYTE *p = (const BYTE *)data;
  SIZE_T remaining = size;
  BYTE buf[64 * 1024];

  while (remaining > 0) {
    DWORD toRead = (remaining > sizeof(buf)) ? (DWORD)sizeof(buf) : (DWORD)remaining;
    DWORD read = 0;
    if (!ReadFile(hFile, buf, toRead, &read, NULL))
      return FALSE;
    if (read == 0)
      return FALSE;  // unexpected EOF despite matching size
    if (memcmp(buf, p, read) != 0)
      return TRUE;   // content differs -> *equal stays FALSE
    p += read;
    remaining -= read;
  }

  *equal = TRUE;
  return TRUE;
}

// Writes data/size to <temp>\<directory><suffix>\<fileName>.
// Succeeds if the file already exists with identical content, or if it is
// newly written; fails otherwise (different content, or any I/O error).
// On success returns the full path as a heap string (free() it); NULL on failure.
LPWSTR SaveToTempFile(LPCWSTR directory, unsigned suffix, LPCWSTR fileName,
                      const void *data, SIZE_T size)
{
  WCHAR tempPath[MAX_PATH + 1];
  DWORD n = GetTempPathW(MAX_PATH + 1, tempPath);
  if (n == 0 || n > MAX_PATH)
    return NULL;

  // <temp> already ends with a backslash; append directory + numeric suffix.
  WCHAR dirPath[MAX_PATH];
  if (FAILED(StringCchPrintfW(dirPath, MAX_PATH, L"%s%s%u",
			      tempPath, directory, suffix)))
    return NULL;

  if (!CreateDirectoryW(dirPath, NULL) &&
      GetLastError() != ERROR_ALREADY_EXISTS)
    return NULL;

  WCHAR filePath[MAX_PATH];
  if (FAILED(StringCchPrintfW(filePath, MAX_PATH, L"%s\\%s", dirPath, fileName)))
    return NULL;

  // Existing file? Compare content.
  HANDLE hFile = CreateFileW(filePath, GENERIC_READ, FILE_SHARE_READ, NULL,
			     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile != INVALID_HANDLE_VALUE) {
    BOOL equal = FALSE;
    BOOL ok = FileContentEquals(hFile, data, size, &equal);
    CloseHandle(hFile);
    return (ok && equal) ? _wcsdup(filePath) : NULL;
  }

  DWORD err = GetLastError();
  if (err != ERROR_FILE_NOT_FOUND && err != ERROR_PATH_NOT_FOUND)
    return NULL;  // permissions, etc.

  // No file yet - create it. CREATE_NEW fails if someone races us to it.
  hFile = CreateFileW(filePath, GENERIC_WRITE, 0, NULL,
		      CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE)
    return NULL;

  BOOL ok = WriteAllBytes(hFile, data, size);
  CloseHandle(hFile);

  if (!ok) {
    DeleteFileW(filePath);  // don't leave a partial file behind
    return NULL;
  }
  return _wcsdup(filePath);
}

// Converts a UTF-8 string to a freshly-allocated wide string.
// Returns NULL on failure; free the result with free() on success.
static LPWSTR Utf8ToWide(const char *utf8)
{
  if (utf8 == NULL)
    return NULL;

  // Measure. Passing -1 for the input length means "NUL-terminated",
  // and the returned count then *includes* the terminating L'\0'.
  int wlen = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
				 utf8, -1, NULL, 0);
  if (wlen == 0)
    return NULL;   // GetLastError() for the reason

  LPWSTR wide = (LPWSTR)malloc((size_t)wlen * sizeof(WCHAR));
  if (wide == NULL)
    return NULL;

  int written = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
				    utf8, -1, wide, wlen);
  if (written == 0) {
    free(wide);
    return NULL;
  }

  return wide;
}

HMODULE TempLoadLibrary(const char *name, const void *data, size_t size) {
  LPCWSTR fileName = Utf8ToWide(name);
  LPCWSTR directory = L"rkt_dll";
  LPCWSTR path = NULL;
  int i;
  HMODULE h;

  for (i = 0; !path; i++) {
    path = SaveToTempFile(directory, i, fileName, data, size);
  }

  h = LoadLibraryW(path);

  free((void *)path);
  free((void *)fileName);

  return h;
}
