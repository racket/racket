#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <utime.h>
#include <sys/stat.h>
#include <stdlib.h>
#ifdef RKTIO_SYSTEM_UNIX
# include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <uuid/uuid.h>
#include <dirent.h>
#endif

#ifdef RKTIO_SYSTEM_UNIX
# define A_PATH_SEP '/'
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
# define A_PATH_SEP '\\'
# define IS_A_DOS_SEP(c) IS_A_SEP(c)
#endif
#define IS_A_SEP(c) ((c) == A_PATH_SEP)

#ifdef USE_TRANSITIONAL_64_FILE_OPS
# define BIG_OFF_T_IZE(n) n ## 64
#else
# define BIG_OFF_T_IZE(n) n
#endif

#if defined(RKTIO_SYSTEM_UNIX) && !defined(NO_UNIX_USERS)
static int have_user_ids = 0;
static uid_t uid;
static uid_t euid;
static gid_t gid;
static gid_t egid;

#define GROUP_MEMBER_CACHE_STATE_UNUSED 0
#define GROUP_MEMBER_CACHE_STATE_IN     1
#define GROUP_MEMBER_CACHE_STATE_NOT_IN 2
typedef struct group_member_cache_entry_t {
  int state;
  gid_t gid;
  uid_t uid;
} group_member_cache_entry_t;
# define GROUP_CACHE_SIZE 10
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
static int procs_initied = 0;
typedef BOOLEAN (WINAPI*CreateSymbolicLinkProc_t)(wchar_t *dest, wchar_t *src, DWORD flags);
static CreateSymbolicLinkProc_t CreateSymbolicLinkProc = NULL;

typedef BOOL (WINAPI*DeviceIoControlProc_t)(HANDLE hDevice, DWORD dwIoControlCode, LPVOID lpInBuffer,
					    DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize,
					    LPDWORD lpBytesReturned, LPOVERLAPPED lpOverlapped);
static DeviceIoControlProc_t DeviceIoControlProc;
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
static void init_procs()
{
  if (!procs_inited) {
    HMODULE hm;

    procs_inited = 1;
    
    hm = LoadLibrary("kernel32.dll");

    CreateSymbolicLinkProc = (CreateSymbolicLinkProc_t)GetProcAddress(hm, "CreateSymbolicLinkW");
    DeviceIoControlProc = (DeviceIoControlProc_t)GetProcAddress(hm, "DeviceIoControl");

    FreeLibrary(hm);
  }
}
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# define MZ_UNC_READ  RKTIO_PERMISSION_READ
# define MZ_UNC_WRITE RKTIO_PERMISSION_WRITE
# define MZ_UNC_EXEC  RKTIO_PERMISSION_EXEC

# define FIND_FIRST FindFirstFileW
# define FIND_NEXT FindNextFileW
# define FIND_CLOSE FindClose
# define FF_TYPE WIN32_FIND_DATAW
# define FF_HANDLE_TYPE HANDLE
# define FIND_FAILED(h) (h == INVALID_HANDLE_VALUE)
# define FF_A_RDONLY FILE_ATTRIBUTE_READONLY
# define FF_A_DIR FILE_ATTRIBUTE_DIRECTORY
# define FF_A_LINK 0x400
# define GET_FF_ATTRIBS(fd) (fd.dwFileAttributes)
# define GET_FF_MODDATE(fd) convert_date(&fd.ftLastWriteTime)
# define GET_FF_NAME(fd) fd.cFileName

static time_t convert_date(const FILETIME *ft)
{
  LONGLONG l, delta;
  FILETIME ft2;
  SYSTEMTIME st;
  TIME_ZONE_INFORMATION tz;

  /* FindFirstFile incorrectly shifts for daylight saving. It
     subtracts an hour to get to UTC when daylight saving is in effect
     now, even when daylight saving was not in effect when the file
     was saved.  Counteract the difference. There's a race condition
     here, because we might cross the daylight-saving boundary between
     the time that FindFirstFile runs and GetTimeZoneInformation
     runs. Cross your fingers... */
  FileTimeToLocalFileTime(ft, &ft2);
  FileTimeToSystemTime(&ft2, &st);
  
  delta = 0;
  if (GetTimeZoneInformation(&tz) == TIME_ZONE_ID_DAYLIGHT) {
    /* Daylight saving is in effect now, so there may be a bad
       shift. Check the file's date. */
    int start_day_of_month, end_day_of_month, first_day_of_week, diff, end_shift;

    /* Valid only when the months match: */
    first_day_of_week = (st.wDayOfWeek - (st.wDay - 1 - (((st.wDay - 1) / 7) * 7)));
    if (first_day_of_week < 0)
      first_day_of_week += 7;

    diff = (tz.DaylightDate.wDayOfWeek - first_day_of_week);
    if (diff < 0)
      diff += 7;
    start_day_of_month = 1 + (((tz.DaylightDate.wDay - 1) * 7)
			      + diff);
	
    diff = (tz.StandardDate.wDayOfWeek - first_day_of_week);
    if (diff < 0)
      diff += 7;
    end_day_of_month = 1 + (((tz.StandardDate.wDay - 1) * 7)
			    + diff);

    /* Count ambigious range (when the clock goes back) as
       in standard time. We assume that subtracting the 
       ambiguous range does not go back into the previous day,
       and that the shift is a multiple of an hour. */
    end_shift = ((tz.StandardBias - tz.DaylightBias) / 60);

    if ((st.wMonth < tz.DaylightDate.wMonth)
	|| ((st.wMonth == tz.DaylightDate.wMonth)
	    && ((st.wDay < start_day_of_month)
		|| ((st.wDay == start_day_of_month)
		    && (st.wHour < tz.DaylightDate.wHour))))) {
      /* Daylight saving had not yet started. */
      delta = ((tz.StandardBias - tz.DaylightBias) * 60);
    } else if ((st.wMonth > tz.StandardDate.wMonth)
	       || ((st.wMonth == tz.StandardDate.wMonth)
		   && ((st.wDay > end_day_of_month)
		       || ((st.wDay == end_day_of_month)
			   && (st.wHour >= (tz.StandardDate.wHour
					    - end_shift)))))) {
      /* Daylight saving was already over. */
      delta = ((tz.StandardBias - tz.DaylightBias) * 60);
    }
  }

  l = ((((LONGLONG)ft->dwHighDateTime << 32) | ft->dwLowDateTime)
       - (((LONGLONG)0x019DB1DE << 32) | 0xD53E8000));
  l /= 10000000;
  l += delta;

  return (time_t)l;
}

typedef struct mz_REPARSE_DATA_BUFFER {
  ULONG  ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union {
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG  Flags;
      WCHAR  PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR  PathBuffer[1];
    } MountPointReparseBuffer;
    struct {
      UCHAR DataBuffer[1];
    } GenericReparseBuffer;
  } u;
} mz_REPARSE_DATA_BUFFER;

#define mzFILE_FLAG_OPEN_REPARSE_POINT 0x200000
#define mzFSCTL_GET_REPARSE_POINT 0x900A8

static char *UNC_readlink(const char *fn)
{
  HANDLE h;
  DWORD got;
  char *buffer;
  int size = 1024;
  mz_REPARSE_DATA_BUFFER *rp;
  int len, off;
  wchar_t *lk;

  init_procs();

  if (!DeviceIoControlProc) return NULL;

  h = CreateFileW(WIDE_PATH_temp(fn), FILE_READ_ATTRIBUTES,
		  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
		  OPEN_EXISTING,
		  FILE_FLAG_BACKUP_SEMANTICS | mzFILE_FLAG_OPEN_REPARSE_POINT,
		  NULL);

  if (h == INVALID_HANDLE_VALUE) {
    get_windows_error();
    return NULL;
  }

  while (1) {
    buffer = (char *)malloc(size);
    if (DeviceIoControlProc(h, mzFSCTL_GET_REPARSE_POINT, NULL, 0, buffer, size,
			    &got, NULL))
      break;
    else if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
      size *= 2;
      free(buffer);
      buffer = (char *)malloc(size);
    } else {
      get_windows_error();
      CloseHandle(h);
      free(buffer);
      return NULL;
    }
  }

  CloseHandle(h);

  rp = (mz_REPARSE_DATA_BUFFER *)buffer;
  if ((rp->ReparseTag != IO_REPARSE_TAG_SYMLINK)
      && (rp->ReparseTag != IO_REPARSE_TAG_MOUNT_POINT)) {
    free(buffer);
    set_racket_error(RKTIO_ERROR_LINK_FAILED);
    return NULL;
  }

  if (rp->ReparseTag == IO_REPARSE_TAG_SYMLINK) {
    off = rp->u.SymbolicLinkReparseBuffer.SubstituteNameOffset;
    len = rp->u.SymbolicLinkReparseBuffer.SubstituteNameLength;
    if (!len) {
      off = rp->u.SymbolicLinkReparseBuffer.PrintNameOffset;
      len = rp->u.SymbolicLinkReparseBuffer.PrintNameLength;
    }
  } else {
    off = rp->u.MountPointReparseBuffer.SubstituteNameOffset;
    len = rp->u.MountPointReparseBuffer.SubstituteNameLength;
    if (!len) {
      off = rp->u.MountPointReparseBuffer.PrintNameOffset;
      len = rp->u.MountPointReparseBuffer.PrintNameLength;
    }
  }

  lk = (wchar_t *)malloc(len + 2);

  if (rp->ReparseTag == IO_REPARSE_TAG_SYMLINK)
    memcpy(lk, (char *)rp->u.SymbolicLinkReparseBuffer.PathBuffer + off, len);
  else
    memcpy(lk, (char *)rp->u.MountPointReparseBuffer.PathBuffer + off, len);
  
  lk[len>>1] = 0;

  if ((lk[0] == '\\') && (lk[1] == '?') && (lk[2] == '?') && (lk[3] == '\\')) {
    /* "?\\" is a prefix that means "unparsed", or something like that */
    memmove(lk, lk+4, len - 8);
    len -= 8;
    lk[len>>1] = 0;

    if ((lk[0] == 'U') && (lk[1] == 'N') && (lk[2] == 'C') && (lk[3] == '\\')) {
      /* "UNC\" is a further prefix that means "UNC"; replace "UNC" with "\" */
      memmove(lk, lk+2, len - 4);
      lk[0] = '\\';
      len -= 4;
      lk[len>>1] = 0;
    }
  }

  free(buffer);

  /* Make sure it's not empty, because that would form a bad path: */
  if (!lk[0]) {
    free(lk);
    set_racket_error(RKTIO_ERROR_LINK_FAILED);
    return NULL;
  }

  buffer = NARROW_PATH_copy(lk);
  free(lk);

  return buffer;
}

static int UNC_stat(rktio_t *rktio, const char *dirname, int *flags, int *isdir, int *islink, 
		    rktio_timestamp_t **date, rktio_file_size_t *filesize,
		    const char **resolved_path, int set_flags)
/* dirname must be absolute */
{
  /* Note: stat() doesn't work with UNC "drive" names or \\?\ paths.
     Also, stat() doesn't distinguish between the ability to
     list a directory's content and whether the directory exists. 
     So, we use GetFileAttributesExW(). */
  char *copy;
  WIN32_FILE_ATTRIBUTE_DATA fad;
  int len, must_be_dir = 0;

  if (resolved_path)
    *resolved_path = NULL;

 retry:

  if (islink)
    *islink = 0;
  if (isdir)
    *isdir = 0;
  if (date)
    *date = NULL;

  len = strlen(dirname);

  copy = malloc(len+1);
  memcpy(copy, dirname, len+1);

  if (!rktio->windows_nt_or_later
      || ((len >= 4)
          && (copy[0] == '\\')
          && (copy[1] == '\\')
          && (copy[2] == '?')
          && (copy[3] == '\\'))) {
    /* Keep `copy` as-is */
  } else {
    /* Strip trailing separators */
    while (IS_A_DOS_SEP(copy[len - 1])) {
      --len;
      copy[len] = 0;
      must_be_dir = 1;
    }
  }

  /* If we ended up with "\\?\X:" (and nothing after), then drop the "\\?\" */
  if ((copy[0] == '\\')&& (copy[1] == '\\') && (copy[2] == '?') && (copy[3] == '\\') 
      && is_drive_letter(copy[4]) && (copy[5] == ':') && !copy[6]) {
    memmove(copy, copy + 4, len - 4);
    len -= 4;
    copy[len] = 0;
  }
  /* If we ended up with "\\?\\X:" (and nothing after), then drop the "\\?\\" */
  if ((copy[0] == '\\') && (copy[1] == '\\') && (copy[2] == '?') && (copy[3] == '\\') 
      && (copy[4] == '\\') && is_drive_letter(copy[5]) && (copy[6] == ':') && !copy[7]) {
    memmove(copy, copy + 5, len - 5);
    len -= 5;
    copy[len] = 0;
  }
  /* If we ended up with "X:[/]", then add a "." at the end so that we get information
     for the drive, not the current directory of the drive: */
  if (is_drive_letter(copy[0]) && (copy[1] == ':')
      && (!copy[2]
	  || (IS_A_DOS_SEP(copy[2]) && !copy[3]))) {
    copy[2] = '\\';
    copy[3] = '.';
    copy[4] = 0;
  }

  if (!GetFileAttributesExW(WIDE_PATH_temp(copy), GetFileExInfoStandard, &fad)) {
    get_windows_error();
    free(copy);
    return 0;
  } else {
    if (GET_FF_ATTRIBS(fad) & FF_A_LINK) {
      if (islink) {
	*islink = 1;
	return 1;
      } else {
	/* Resolve a link by opening the link and then getting
           the path from the handle. */
        HANDLE h;
        wchar_t *dest = NULL;
        DWORD len = 255;

        h = CreateFileW(WIDE_PATH_temp(copy), FILE_READ_ATTRIBUTES,
                        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                        OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS,
                        NULL);

        if (!h) {
          get_windows_error();
          free(copy);
	  return 0;
	}

        do {
          if (dest) free(dest);
          dest_len = len + 1;
          dest = malloc(dest_len);
          len = GetFinalPathNameByHandleW(h, dest, dest_len, FILE_NAME_NORMALIZED);
        } while (len > dest_len);

        if (!len) {
          get_windows_error();
          CloseHandle(h);
          free(copy);
          free(dest);
	  return 0;
        }

        CloseHandle(h);

        dirname = NARROW_PATH_copy(dest);
	if (resolved_path)
	  *resolved_path = dirname;

        free(dest);
        free(copy);

	goto retry;
      }
    }

    if (set_flags != -1) {
      DWORD attrs = GET_FF_ATTRIBS(fad);

      if (!(set_flags & MZ_UNC_WRITE))
        attrs |= FF_A_RDONLY;
      else if (attrs & FF_A_RDONLY)
        attrs -= FF_A_RDONLY;
      
      if (!SetFileAttributesW(WIDE_PATH_temp(copy), attrs)) {
        get_windows_error();
        free(copy);
        return 0;
      }
    } else {
      if (must_be_dir && !(GET_FF_ATTRIBS(fad) & FF_A_DIR)) {
        set_racket_error(RKTIO_ERROR_NOT_A_DIRECTORY);
        free(copy);
        return 0;
      }
      if (flags)
        *flags = MZ_UNC_READ | MZ_UNC_EXEC | ((GET_FF_ATTRIBS(fad) & FF_A_RDONLY) ? 0 : MZ_UNC_WRITE);
      if (date) {
        rktio_timestamp_t *dt;
        time_t mdt;
        mdt = GET_FF_MODDATE(fad);
        dt = malloc(sizeof(rktio_timestamp_t));
        *dt = mdt;
        *date = dt;
      }
      if (isdir) {
        *isdir = (GET_FF_ATTRIBS(fad) & FF_A_DIR);
      }
      if (filesize) {
        rktio_size_t *fsz;
        fsz = malloc(sizeof(rktio_size_t));
        fsz->lo = fad.nFileSizeLow;
        fsz->hi = fad.nFileSizeHigh;
      }
    }
    free(copy);
    return 1;
  }
}
#endif

int rktio_file_exists(rktio_t *rktio, char *filename)
/* Windows: check for special filenames before calling */
{
# ifdef NO_STAT_PROC
  int fd;

  fd = open(filename, O_RDONLY);
  if (fd != -1) {
    fclose(fd);
    return 1;
  } else
    return 0;
# else
#  ifdef RKTIO_SYSTEM_WINDOWS
  {
    int isdir;
    return (UNC_stat(rktio, filename, NULL, &isdir, NULL, NULL, NULL, NULL, -1)
	    && !isdir);
  }
#  else
  struct MSC_IZE(stat) buf;
  int ok;

  do {
    ok = MSC_W_IZE(stat)(MSC_WIDE_PATH_temp(filename), &buf);
  } while ((ok == -1) && (errno == EINTR));

  return !ok && !S_ISDIR(buf.st_mode);
#  endif
# endif
}

int rktio_directory_exists(rktio_t *rktio, char *dirname)
{
# ifdef NO_STAT_PROC
  return 0;
# else
#  ifdef RKTIO_SYSTEM_WINDOWS
  int isdir;

  return (UNC_stat(rktio, dirname, NULL, &isdir, NULL, NULL, NULL, NULL, -1)
	  && isdir);
#  else
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!MSC_IZE(stat)(dirname, &buf))
      break;
    else if (errno != EINTR)
      return 0;
  }

  return S_ISDIR(buf.st_mode);
#  endif
# endif
}

int rktio_is_regular_file(rktio_t *rktio, char *filename)
/* Windows: check for special filenames before calling */
{
# ifdef NO_STAT_PROC
  return 0;
# else
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!MSC_W_IZE(stat)(MSC_WIDE_PATH_temp(filename), &buf))
      break;
    else if (errno != EINTR)
      return 0;
  }

  return S_ISREG(buf.st_mode);
# endif  
}

int rktio_link_exists(rktio_t *rktio, char *filename)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    int islink;
    if (UNC_stat(rktio, filename, NULL, NULL, &islink, NULL, NULL, NULL, -1)
	&& islink)
      return 1;
    else
      return 0;
  }
#else
  {
    struct MSC_IZE(stat) buf;
    while (1) {
      if (!MSC_W_IZE(lstat)(MSC_WIDE_PATH_temp(filename), &buf))
	break;
      else if (errno != EINTR)
	return 0;
    }

    if (S_ISLNK(buf.st_mode))
      return 1;
    else
      return 0;
  }
#endif
}

char *rktio_get_current_directory(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
 int need_l, bl = 256;
 wchar_t *wbuf;
 char *r;

 wbuf = malloc(bl);
 while (1) {
   need_l = GetCurrentDirectoryW(bl, wbuf);
   if (need_l > bl) {
     free(wbuf);
     wbuf = malloc(need_l * sizeof(wchar_t));
     bl = need_l;
   } else
     break;
 }

 if (!need_l) {
   get_windows_error();
   return NULL;
 }

 r = NARROW_PATH_copy_then_free(wbuf);

 return r;
#else
 char *r, *s;
 int len = 256;

 s = malloc(len);
 while (1) {
   r = MSC_IZE(getcwd)(s, len);
   if (r)
     break;
   if (errno == ERANGE) {
     free(s);
     len *= 2;
     s = malloc(len);
   } else
     break;
 }
 if (!r)
   get_posix_error();
 return r;
#endif
}

int rktio_set_current_directory(rktio_t *rktio, char *expanded)
{
  int err;

  while (1) {
    err = MSC_W_IZE(chdir)(MSC_WIDE_PATH_temp(expanded));
    if (!err || (errno != EINTR))
      break;
  }

  get_posix_error();

  return !err;
}

static rktio_identity_t *get_identity(rktio_t *rktio, rktio_fd_t *fd, char *path, int follow_links)
{
  uintptr_t devi = 0, inoi = 0, inoi2 = 0;

#ifdef FILES_HAVE_FDS
  int errid = 0;
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!path && !MSC_IZE(fstat)(rktio_fd_system_fd(rktio, fd), &buf))
      break;
    else if (path && follow_links && !MSC_IZE(stat)(path, &buf))
      break;
    else if (path && !follow_links && !MSC_IZE(lstat)(path, &buf))
      break;
    else if (errno != EINTR) {
      errid = errno;
      break;
    }
  }

  if (errid) {
    get_posix_error();
    return NULL;
  } else {
    /* Warning: we assume that dev_t and ino_t fit in a pointer-sized integer. */
    devi = (uintptr_t)buf.st_dev;
    inoi = (uintptr_t)buf.st_ino;
  }
#endif
#ifdef WINDOWS_FILE_HANDLES
  BY_HANDLE_FILE_INFORMATION info;
  HANDLE fdh;

  init_procs();

  if (path) {
    fdh = CreateFileW(WIDE_PATH_temp(path),
                      0, /* not even read access => just get info */
                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                      NULL,
                      OPEN_EXISTING,
                      FILE_FLAG_BACKUP_SEMANTICS 
                      | ((fd && CreateSymbolicLinkProc)
                         ? mzFILE_FLAG_OPEN_REPARSE_POINT 
                         : 0),
                      NULL);
    if (fdh == INVALID_HANDLE_VALUE) {
      get_windows_error();
      return NULL;
    }
  } else
    fdh = (HANDLE)rktio_fd_system_fd(rktio, fd);

  if (!GetFileInformationByHandle(fdh, &info)) {
    get_windows_error();
    if (path) CloseHandle(fdh);
    return NULL;
  }
      
  if (path) CloseHandle(fdh);

  devi = info.dwVolumeSerialNumber;
  inoi = info.nFileIndexLow;
  inoi2 = info.nFileIndexHigh;
#endif

  {
    rktio_identity_t *id;
    
    id = malloc(sizeof(rktio_identity_t));
    
    id->a = devi;
    id->b = inoi;
    id->c = inoi2;

    return id;
  }
}

rktio_identity_t *rktio_fd_identity(rktio_t *rktio, rktio_fd_t *fd)
{
  return get_identity(rktio, fd, NULL, 0);
}

rktio_identity_t *rktio_path_identity(rktio_t *rktio, char *path, int follow_links)
{
  return get_identity(rktio, NULL, path, follow_links);
}

#ifdef RKTIO_SYSTEM_WINDOWS
static int enable_write_permission(const char *fn)
{
  int flags;

  return UNC_stat(rktio, fn, &flags, NULL, NULL, NULL, NULL, NULL, MZ_UNC_WRITE);
}
#endif

int rktio_delete_file(rktio_t *rktio, char *fn, int enable_write_on_fail)
{
  int errid;

#ifdef RKTIO_SYSTEM_WINDOWS
  if (DeleteFileW(WIDE_PATH_temp(fn)))
    return 1;
  errid = GetLastError();
  if ((errid == ERROR_ACCESS_DENIED) && enable_write_on_fail) {
    /* Maybe it's just that the file has no write permission. Provide a more
       Unix-like experience by attempting to change the file's permission. */
    if (enable_write_permission(fn)) {
      if (DeleteFileW(WIDE_PATH_temp(fn)))
        return 1;
    }
  }

  get_windows_error();
  return 0;
#else
  while (1) {
    if (!MSC_W_IZE(unlink)(MSC_WIDE_PATH_temp(fn)))
      return 1;
    else if (errno != EINTR)
      break;
  }
  
  get_posix_error();
  return 0;
#endif
}

int rktio_rename_file(rktio_t *rktio, char *dest, char *src, int exists_ok)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  int errid;
  wchar_t *src_w = WIDE_PATH_copy(src);
  
  if (MoveFileExW(src_w, WIDE_PATH_temp(dest), (exists_ok ? MOVEFILE_REPLACE_EXISTING : 0))) {
    free(src_w);
    return 1;
  }
  
  errid = GetLastError();

  if (errid == ERROR_CALL_NOT_IMPLEMENTED) {
    /* Then we have the great misfortune of running in Windows 9x. If
       exists_ok, then do something no less stupid than the OS
       itself: */
    int errid;
    if (exists_ok)
      MSC_W_IZE(unlink)(MSC_WIDE_PATH_temp(dest));
    if (MoveFileW(src_w, WIDE_PATH_temp(dest))) {
      free(src_w);
      return 1;
    }
    get_windows_error();
  } else
    get_windows_error();
  
  free(src_w);
  return 0;
#else
  if (!exists_ok && (rktio_file_exists(rktio, dest) || rktio_directory_exists(rktio, dest))) {
    /* We use a specialized error here, because it's not
       a system error (e.g., setting `errval` to `EEXIST` would
       be a lie). */
    set_racket_error(RKTIO_ERROR_EXISTS);
    return 0;
  }
  
  while (1) {
    if (!rename(src, dest))
      return 1;
    else if (errno != EINTR)
      break;
  }

  get_posix_error();  
  return 0;
#endif
}

char *rktio_readlink(rktio_t *rktio, char *fullfilename)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (UNC_stat(rktio, fullfilename, NULL, NULL, &is_link, NULL, NULL, NULL, -1)
      && is_link) {
    return UNC_readlink(fullfilename);
  } else {
    set_racket_error(RKTIO_ERROR_NOT_A_LINK);
    return NULL;
  }
#else
  int len, buf_len = 256;
  char *buffer = malloc(buf_len);

  while (1) {
    len = readlink(fullfilename, buffer, buf_len);
    if (len == -1) {
      if (errno != EINTR) {
        get_posix_error();
        return NULL;
      }
    } else if (len == buf_len) {
      /* maybe too small */
      free(buffer);
      buf_len *= 2;
      buffer = malloc(buf_len);
    } else
      break;
  }
  buffer[len] = 0;
  return buffer;
#endif
}

int rktio_make_directory(rktio_t *rktio, char *filename)
{
#ifdef NO_MKDIR
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return 0;
#else
  int exists_already = 0;
  int len, copied = 0;

  /* Make sure path doesn't have trailing separator: */
  len = strlen(filename);
  while (len && IS_A_SEP(filename[len - 1])) {
    if (!copied) {
      filename = strdup(filename);
      copied = 1;
    }
    filename[--len] = 0;
  }

  while (1) {
    if (!MSC_W_IZE(mkdir)(MSC_WIDE_PATH_temp(filename)
# ifndef MKDIR_NO_MODE_FLAG
			  , 0777
# endif
			  )) {
      if (copied) free(filename);
      return 1;
    } else if (errno != EINTR)
      break;
  }

  if (errno == EEXIST)
    set_racket_error(RKTIO_ERROR_EXISTS);
  else
    get_posix_error();

  if (copied) free(filename);

  return 0;
}

int rktio_delete_directory(rktio_t *rktio, char *filename, char *current_directory, int enable_write_on_fail)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  int tried_cwd = 0, tried_perm = 0;
#endif
  
  while (1) {
    if (!MSC_W_IZE(rmdir)(MSC_WIDE_PATH_temp(filename)))
      return 1;
# ifdef RKTIO_SYSTEM_WINDOWS
    else if ((errno == EACCES) && !tried_cwd) {
      /* Maybe we're using the target directory. Try a real setcwd. */
      (void)rktio_set_current_directory(current_directory);
      tried_cwd = 1;
    } else if ((errno == EACCES) && !tried_perm && enable_write_on_fail) {
      /* Maybe the directory doesn't have write permission. */
      (void)enable_write_permission(filename);
      tried_perm = 1;
    }
# endif
    else if (errno != EINTR)
      break;
  }

  get_posix_error();
  return 0;
#endif
}

int rktio_make_link(rktio_t *rktio, char *src, char *dest, int dest_is_directory)
/* `src` is the file that is written, and `dest` is written to that
   file */
{
#if defined(RKTIO_SYSTEM_WINDOWS)
  init_procs();
    
  if (CreateSymbolicLinkProc) {
    int flags;
    wchar_t *src_w = WIDE_PATH_COPY(src);

    if (dest_is_directory)
      flags = 0x1; /* directory */
    else
      flags = 0; /* file */

    if (CreateSymbolicLinkProc(src_w, WIDE_PATH_temp(dest), flags)) {
      free(src_w);
      return 1;
    }
    get_windows_error();
    free(src_w);
  } else
    set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  
  return 0;
#else
  while (1) {
    if (!symlink(dest, src))
      return 1;
    else if (errno != EINTR)
      break;
  }
  get_posix_error();
  return 0;
#endif
}

rktio_timestamp_t *rktio_get_file_modify_seconds(rktio_t *rktio, char *file)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_timestamp_t *secs;
  
  if (UNC_stat(rktio, file, NULL, NULL, NULL, &secs, NULL, NULL, -1))
    return secs;
  return NULL;
#else
  struct MSC_IZE(stat) buf;
  
  while (1) {
    if (!MSC_W_IZE(stat)(MSC_WIDE_PATH_temp(file), &buf)){
      rktio_timestamp_t *ts = malloc(sizeof(rktio_timestamp_t));
      *ts = buf.st_mtime;
      return ts;
    }
    if (errno != EINTR)
      break;
  }

  get_posix_error();
  return NULL;
#endif
}

int rktio_set_file_modify_seconds(rktio_t *rktio, char *file, rktio_timestamp_t secs)
{
  while (1) {
    struct MSC_IZE(utimbuf) ut;
    ut.actime = secs;
    ut.modtime = secs;
    if (!MSC_W_IZE(utime)(MSC_WIDE_PATH_temp(file), &ut))
      return 1;
    if (errno != EINTR)
      break;
  }

  get_posix_error();
  return 0;
}

#if defined(RKTIO_SYSTEM_UNIX) && !defined(NO_UNIX_USERS)
static int user_in_group(rktio_t *rktio, uid_t uid, gid_t gid)
{
  struct group *g;
  struct passwd *pw;
  int i, in;

  if (!rktio->group_member_cache)
    rktio->group_member_cache = calloc(GROUP_CACHE_SIZE, sizeof(group_member_cache_entry_t));

  for (i = 0; i < GROUP_CACHE_SIZE; i++) {
    if ((rktio->group_member_cache[i].state != GROUP_MEMBER_CACHE_STATE_UNUSED)
        && (rktio->group_member_cache[i].gid == gid)
        && (rktio->group_member_cache[i].uid == uid))
      return (rktio->group_member_cache[i].state == GROUP_MEMBER_CACHE_STATE_IN);
  }

  pw = getpwuid(uid);
  if (!pw)
    return 0;

  g = getgrgid(gid);
  if (!g)
    return 0;

  for (i = 0; g->gr_mem[i]; i++) {
    if (!strcmp(g->gr_mem[i], pw->pw_name))
      break;
  }

  in = !!(g->gr_mem[i]);

  for (i = 0; i < GROUP_CACHE_SIZE; i++) {
    if (rktio->group_member_cache[i].state == GROUP_MEMBER_CACHE_STATE_UNUSED) {
      rktio->group_member_cache[i].gid = gid;
      rktio->group_member_cache[i].uid = uid;
      rktio->group_member_cache[i].state = (in
                                     ? GROUP_MEMBER_CACHE_STATE_IN
                                     : GROUP_MEMBER_CACHE_STATE_NOT_IN);
      break;
    }
  }

  return in;
}
#endif

int rktio_get_file_or_directory_permissions(rktio_t *rktio, char *filename, int all_bits)
/* -1 result indicates an error */
{
# ifdef NO_STAT_PROC
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return -1;
# else
#  ifdef RKTIO_SYSTEM_UNIX
  /* General strategy for permissions (to deal with setuid)
     taken from euidaccess() in coreutils... */
#   ifndef NO_UNIX_USERS
  if (have_user_ids == 0) {
    have_user_ids = 1;
    uid = getuid();
    gid = getgid();
    euid = geteuid();
    egid = getegid();
  }

  if (!all_bits && (uid == euid) && (gid == egid)) {
    /* Not setuid; use access() */
    int read, write, execute, ok;
    
    do {
      ok = access(filename, R_OK);
    } while ((ok == -1) && (errno == EINTR));
    read = !ok;

    if (ok && (errno != EACCES)) {
      get_posix_error();
      return -1;
    } else {
      do {
	ok = access(filename, W_OK);
      } while ((ok == -1) && (errno == EINTR));
      write = !ok;
      
      /* Don't fail at the exec step if errno is EPERM; under Mac OS
         X, at least, such a failure seems to mean that the file is
         not writable. (We assume it's not a directory-access issue,
         since the read test succeeded.) */
      if (ok && (errno != EACCES) && (errno != EPERM) && (errno != EROFS)) {
	get_posix_error();
        return -1;
      } else {
	do {
	  ok = access(filename, X_OK);
	} while ((ok == -1) && (errno == EINTR));
	execute = !ok;
      
        /* Don't fail at the exec step if errno is EPERM; under Mac OS
           X, at least, such a failure simply means that the file is
           not executable. */
	if (ok && (errno != EACCES) && (errno != EPERM)) {
	  get_posix_error();
          return -1;
	} else {
          return ((read ? S_IRUSR : 0)
                  | (write ? S_IWUSR : 0)
                  | (execute ? S_IXUSR : 0));
	}
      }
    }
  }
#  endif
  {
    /* Use stat, because setuid, or because or no user info available */
    struct stat buf;
    int cr, read, write, execute;

    do {
      cr = stat(filename, &buf);
    } while ((cr == -1) && (errno == EINTR));

    if (cr) {
      get_posix_error();
      return -1;
    } else {
      if (all_bits) {
        int bits = buf.st_mode;
#   ifdef S_IFMT
        bits -= (bits & S_IFMT);
#   endif
        return bits;
      } else {
#   ifndef NO_UNIX_USERS
        if (euid == 0) {
          /* Super-user can read/write anything, and can
             execute anything that someone can execute */
          read = 1;
          write = 1;
          execute = !!(buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
        } else if (buf.st_uid == euid) {
          read = !!(buf.st_mode & S_IRUSR);
          write = !!(buf.st_mode & S_IWUSR);
          execute = !!(buf.st_mode & S_IXUSR);
        } else if ((egid == buf.st_gid) || user_in_group(rktio, euid, buf.st_gid)) {
          read = !!(buf.st_mode & S_IRGRP);
          write = !!(buf.st_mode & S_IWGRP);
          execute = !!(buf.st_mode & S_IXGRP);
        } else {
          read = !!(buf.st_mode & S_IROTH);
          write = !!(buf.st_mode & S_IWOTH);
          execute = !!(buf.st_mode & S_IXOTH);
        }
#   else
        read = !!(buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH));
        write = !!(buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH));
        execute = !!(buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
#   endif

        return ((read ? S_IRUSR : 0)
                | (write ? S_IWUSR : 0)
                | (execute ? S_IXUSR : 0));
      }
    }
  }
#  endif  
#  ifdef RKTIO_SYSTEM_WINDOWS
  {
    int flags;

    if (UNC_stat(rktio, filename, &flags, NULL, NULL, NULL, NULL, NULL, -1)) {
      if (as_bits)
        return (flags | (flags << 3) | (flags << 6));
      else {
        return ((read ? MZ_UNC_READ : 0)
                | (write ? MZ_UNC_WRITE : 0)
                | (execute ? MZ_UNC_EXEC : 0));
      }
    } else {
      return -1;
    }
  }
#  endif
# endif
}

int rktio_set_file_or_directory_permissions(rktio_t *rktio, char *filename, int new_bits)
{
# ifdef NO_STAT_PROC
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return -1;
# else
#  ifdef RKTIO_SYSTEM_UNIX
  {
    int r;
    
    do {
      r = chmod(filename, new_bits);
    } while ((r == -1) && (errno == EINTR));
    if (r) {
      get_posix_error();
      return 0;
    } else
      return 1;
  }
#  endif  
#  ifdef RKTIO_SYSTEM_WINDOWS
  {
    int len = strlen(filename);
    int ALWAYS_SET_BITS = ((MZ_UNC_READ | MZ_UNC_EXEC)
                           | ((MZ_UNC_READ | MZ_UNC_EXEC) << 3)
                           | ((MZ_UNC_READ | MZ_UNC_EXEC) << 6));
    if (((new_bits & ALWAYS_SET_BITS) != ALWAYS_SET_BITS)
        || ((new_bits & MZ_UNC_WRITE) != ((new_bits & (MZ_UNC_WRITE << 3)) >> 3))
        || ((new_bits & MZ_UNC_WRITE) != ((new_bits & (MZ_UNC_WRITE << 6)) >> 6))
        || (new_bits >= (1 << 9))) {
      set_racket_error(RKTIO_ERROR_BAD_PERMISSION);
      return 0;
    }
    
    if (UNC_stat(rktio, filename, NULL, NULL, NULL, NULL, NULL, NULL, new_bits))
      return 1;
    
    return 0;
  }
#  endif
# endif
}

rktio_size_t *rktio_file_size(rktio_t *rktio, char *filename)
{
  rktio_size_t *sz = NULL;
#ifdef RKTIO_SYSTEM_WINDOWS
 {
   if (UNC_stat(rktio, filename, NULL, NULL, NULL, NULL, &sz, NULL, -1)) {
     return sz;
   }
   return NULL;
 }
#else
  {
    struct BIG_OFF_T_IZE(stat) buf;

    while (1) {
      if (!BIG_OFF_T_IZE(stat)(MSC_WIDE_PATH_temp(filename), &buf))
	break;
      else if (errno != EINTR) {
        get_posix_error();
	return NULL;
      }
    }

    if (S_ISDIR(buf.st_mode)) {
      set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      return NULL;
    }

    sz = malloc(sizeof(rktio_size_t));
    sz->lo = ((unsigned)buf.st_size & 0xFFFFFFFF);
    if (sizeof(buf.st_size) > 4)
      sz->hi = (buf.st_size >> 32);
    else
      sz->hi = 0;

    return sz;
  }
#endif
}

/*************************************************************/
/* directory list                                            */
/*************************************************************/

#ifdef USE_FINDFIRST

struct rktio_directory_list_t {
  FF_HANDLE_TYPE hfile;
  FF_TYPE info;
};

rktio_directory_list_t *rktio_directory_list_start(rktio_t *rktio, char *filename, int is_drive)
/* path must be normalized */
{
  char *pattern;
  int len;
  FF_HANDLE_TYPE hfile;
  FF_TYPE info;
  rktio_directory_list_t *dl;

 retry:

  {
    char *nf;
    int is_ssq = 0, is_unc = 0, d, nd;
    len = strlen(filename);
    pattern = malloc(len + 14);

    if (IS_A_DOS_SEP(filename[0])
        && IS_A_DOS_SEP(filename[1])) {
      if (filename[2] == '?') 
        is_ssq = 1;
      else
        is_unc = 1;
    }

    if (is_ssq) {
      d = 0;
      nd = 0;
    } else {
      pattern[0] = '\\';
      pattern[1] = '\\';
      pattern[2] = '?';
      pattern[3] = '\\';
      if (is_unc) {
	pattern[4] = 'U';
	pattern[5] = 'N';
	pattern[6] = 'C';
	pattern[7] = '\\';
	d = 8;
	nd = 2;
      } else {
	d = 4;
	nd = 0;
      }
    }
    memcpy(pattern + d, nf + nd, len - nd);
    len += (d - nd);
    if (len && !IS_A_DOS_SEP(pattern[len - 1]))
      pattern[len++] = '\\';      
    memcpy(pattern + len, "*.*", 4);
  }

  hfile = FIND_FIRST(WIDE_PATH_temp(pattern), &info);
  if (FIND_FAILED(hfile)) {
    int err_val;
    err_val = GetLastError();
    if ((err_val == ERROR_DIRECTORY) && CreateSymbolicLinkProc) {
      /* check for symbolic link */
      const char *resolved;
      if (UNC_stat(rktio, filename, NULL, NULL, NULL, NULL, NULL, &resolved, -1)) {
	if (resolved) {
	  filename = (char *)resolved;
	  goto retry;
	}
      }
    }
    get_windows_error();
    return NULL;
  }

  dl = malloc(sizeof(rktio_directory_list_t));
  memcpy(&dl->info, &info);
  dl->hfile = hfile;

  return dl;
}

char *rktio_directory_list_step(rktio_t *rktio, rktio_directory_list_t *dl)
/* empty-strng result (don't deallocate) means done */
{
  while (dl->first_ready || FIND_NEXT(dl->hfile, &dl->info)) {
    dl->first_ready = 0;
    if ((GET_FF_NAME(dl->info)[0] == '.')
	&& (!GET_FF_NAME(dl->info)[1] || ((GET_FF_NAME(dl->info)[1] == '.')
                                          && !GET_FF_NAME(dl->info)[2]))) {
      /* skip . and .. */
    } else {
      return NARROW_PATH_copy(info.cFileName);
    }
  }

  FIND_CLOSE(dl->hfile);

  free(dl);

  return "";
}

# elif !defined(NO_READDIR)

struct rktio_directory_list_t {
  DIR *dir;
};

rktio_directory_list_t *rktio_directory_list_start(rktio_t *rktio, char *filename, int is_drive)
{
  rktio_directory_list_t *dl;
  DIR *dir;
  int nlen;

  dir = opendir(filename ? filename : ".");
  if (!dir) {
    get_posix_error();
    return NULL;
  }

  dl = malloc(sizeof(rktio_directory_list_t));
  dl->dir = dir;

  return dl;
}

char *rktio_directory_list_step(rktio_t *rktio, rktio_directory_list_t *dl)
/* empty-strng result (don't deallocate) means done */
{
  struct dirent *e;  
  
  while ((e = readdir(dl->dir))) {
    int nlen;

# ifdef DIRENT_NO_NAMLEN
    nlen = strlen(e->d_name);
# elif defined(__QNX__) || defined(__QNXNTO__)
    nlen = e->d_namelen;
# else
    nlen = e->d_namlen;
# endif

# if defined(RKTIO_SYSTEM_UNIX) || defined(RKTIO_SYSTEM_WINDOWS)
    if (nlen == 1 && e->d_name[0] == '.')
      continue;
    if (nlen == 2 && e->d_name[0] == '.' && e->d_name[1] == '.')
      continue;
# endif

    return strndup(e->d_name, nlen);
  }

  closedir(dl->dir);
  free(dl);

  return "";
}

#else

rktio_directory_list_t *rktio_directory_list_start(rktio_t *rktio, char *filename, int is_drive)
{
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
}

char *rktio_directory_list_step(rktio_t *rktio, rktio_directory_list_t *dl)
{
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
}

#endif

/*************************************************************/
/* copy file                                                 */
/*************************************************************/

struct rktio_file_copy_t {
  int done;
  rktio_fd_t *src_fd, *dest_fd;
};

rktio_file_copy_t *rktio_copy_file_start(rktio_t *rktio, char *dest, char *src, int exists_ok)
{
#ifdef RKTIO_SYSTEM_UNIX
  {
# define COPY_BUFFER_SIZE 2048
    int ok;
    struct stat buf;
    rktio_fd_t *src_fd, *dest_fd;

    src_fd = rktio_open(rktio, src, RKTIO_OPEN_READ);
    if (!src_fd)
      return NULL;

    do {
      ok = fstat(rktio_fd_system_fd(rktio, src_fd), &buf);
    } while ((ok == -1) && (errno == EINTR));

    if (ok || S_ISDIR(buf.st_mode)) {
      if (ok)
        get_posix_error();
      else
        set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      rktio_close(rktio, src_fd);
      return NULL;
    }

    dest_fd = rktio_open(rktio, dest, (RKTIO_OPEN_WRITE
                                       | (exists_ok ? RKTIO_OPEN_TRUNCATE : 0)));
    if (!dest_fd) {
      rktio_close(rktio, src_fd);
      return NULL;
    }

    {
      rktio_file_copy_t *fc;

      fc = malloc(sizeof(rktio_file_copy_t));

      fc->done = 0;
      fc->src_fd = src_fd;
      fc->dest_fd = dest_fd;

      return fc;
    }
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int err_val = 0;
  wchar_t *src_w = WIDE_PATH_copy(src);

  if (CopyFileW(src_w, WIDE_PATH_temp(dest), !exists_ok)) {
    rktio_file_copy_t *fc;
    free(src_w);
    /* Return a pointer to indicate success: */
    fc = malloc(sizeof(rktio_file_copy_t));
    fc->done = 1;
    return fc;
  }
  
  err_val = GetLastError();
  if ((err_val == ERROR_FILE_EXISTS)
      || (err_val == ERROR_ALREADY_EXISTS))
    set_racket_error(RKTIO_ERROR_EXISTS);
  else
    get_windows_error();

  free(src_w);

  return NULL;
#endif
}

int rktio_copy_file_is_done(rktio_t *rktio, rktio_file_copy_t *fc)
{
  return fc->done;
}

int rktio_copy_file_step(rktio_t *rktio, rktio_file_copy_t *fc)
{
#ifdef RKTIO_SYSTEM_UNIX
  char buffer[4096];
  intptr_t len;

  if (fc->done)
    return 1;

  len = rktio_read(rktio, fc->src_fd, buffer, sizeof(buffer));
  if (len == RKTIO_READ_EOF) {
    fc->done = 1;
    return 1;
  } else if (len == RKTIO_READ_ERROR) {
    return 0;
  } else {
    intptr_t done = 0, amt;
    
    while (done < len) {
      amt = rktio_write(rktio, fc->dest_fd, buffer + done, len - done);
      if (amt < 0)
        return 0;
      done += amt;
    }
    return 1;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  return 1;
#endif
}

void rktio_copy_file_stop(rktio_t *rktio, rktio_file_copy_t *fc)
{
#ifdef RKTIO_SYSTEM_UNIX
  rktio_close(rktio, fc->src_fd);
  rktio_close(rktio, fc->dest_fd);
#endif
  free(fc);
}

/*************************************************************/
/* filesystem root list                                      */
/*************************************************************/

char **rktio_filesystem_root_list(rktio_t *rktio)
/* returns a NULL-terminated array of strings */
{
#ifdef RKTIO_SYSTEM_WINDOWS
  {
#   define DRIVE_BUF_SIZE 1024
    char drives[DRIVE_BUF_SIZE], *s;
    intptr_t len, ds, ss_len, ss_count = 0;
    UINT oldmode;
    char **ss, *new_ss;

    len = GetLogicalDriveStrings(DRIVE_BUF_SIZE, drives);
    if (len <= DRIVE_BUF_SIZE)
      s = drives;
    else {
      s = malloc(len + 1);
      GetLogicalDriveStrings(len + 1, s);
    }

    ss_len = 8;
    ss = malloc(sizeof(char*) * ss_len);

    ds = 0;
    oldmode = SetErrorMode(SEM_FAILCRITICALERRORS);      
    while (s[ds]) {
      DWORD a, b, c, d;
      /* GetDiskFreeSpace effectively checks whether we can read the disk: */
      if (GetDiskFreeSpace(s XFORM_OK_PLUS ds, &a, &b, &c, &d)) {
        if ((ss_count + 1) == ss_len) {
          new_ss = malloc(sizeof(char*) * ss_len * 2);
          mempcy(ss, new_ss, ss_count * sizeof(char*));
          ss = new_ss;
          ss_len *= 2;
        }

        ss[ss_count++] = strdup(s + ds);
      }
      ds += strlen(s XFORM_OK_PLUS ds) + 1;
    }
    SetErrorMode(oldmode);

    if (s != drived)
      free(s);

    ss[ss_count] = 0;

    return ss;
  }
#else
  char **ss;
  
  ss = malloc(sizeof(char*) * 2);
  ss[1] = strdup("/");
  ss[0] = NULL;

  return ss;
#endif
}

/*************************************************************/
/* expand user tilde & system paths                          */
/*************************************************************/

char *rktio_expand_user_tilde(rktio_t *rktio, char *filename) {
#ifdef RKTIO_SYSTEM_WINDOWS
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
#else
  char user[256], *home = NULL, *naya;
  struct passwd *who = NULL;
  intptr_t u, f, len, flen, ilen;

  if (filename[0] != '~') {
    set_racket_error(RKTIO_ERROR_NO_TILDE);
    return NULL;
  }
  
  for (u = 0, f = 1; 
       u < 255 && filename[f] && filename[f] != '/'; 
       u++, f++) {
    user[u] = filename[f];
  }

  if (filename[f] && filename[f] != '/') {
    set_racket_error(RKTIO_ERROR_ILL_FORMED_USER);
    return NULL;
  }
  user[u] = 0;

  if (!user[0]) {
    if (!(home = getenv("HOME"))) {
      char *ptr;
          
      ptr = getenv("USER");
      if (!ptr)
        ptr = getenv("LOGNAME");
          
      who = ptr ? getpwnam(ptr) : NULL;
          
      if (!who)
        who = getpwuid(getuid());
    }
  } else
    who = getpwnam(user);

  if (!home && who)
    home = who->pw_dir;

  if (!home) {
    set_racket_error(RKTIO_ERROR_UNKNOWN_USER);
    return NULL;
  }

  ilen = strlen(filename);
  len = strlen(home);
  if (f < ilen) 
    flen = ilen - f - 1;
  else
    flen = 0;
  naya = (char *)malloc(len + flen + 2);
  memcpy(naya, home, len);
  naya[len] = '/';
  memcpy(naya + len + 1, filename + f + 1, flen);
  naya[len + flen + 1] = 0;

  filename = naya;

  return filename;
#endif
}

static char *append_paths(char *a, char *b, int free_a, int free_b)
{
  int alen = strlen(a);
  int blen = strlen(b);
  int sep_len = 0;
  char *s;

  if (!IS_A_SEP(a[alen]))
    sep_len = 1;

  s = malloc(alen + sep_len + blen + 1);

  memcpy(s, a, alen);
  if (sep_len)
    s[alen] = A_PATH_SEP;
  memcpy(s+alen+sep_len, b, blen);
  s[alen + blen] = 0;

  if (free_a) free(a);
  if (free_b) free(b);

  return s;
}

char *rktio_system_path(rktio_t *rktio, int which)
{
#ifdef RKTIO_SYSTEM_UNIX
  if (which == RKTIO_PATH_SYS_DIR) {
    return strdup("/");
  }

  if (which == RKTIO_PATH_TEMP_DIR) {
    char *p;
    
    if ((p = getenv("TMPDIR"))) {
      if (rktio_directory_exists(rktio, p))
	return strdup(p);
    }

    if (rktio_directory_exists(rktio, "/var/tmp"))
      return strdup("/var/tmp");

    if (rktio_directory_exists(rktio, "/usr/tmp"))
      return strdup("/usr/tmp");

    if (rktio_directory_exists(rktio, "/tmp"))
      return strdup("/tmp");

    return rktio_get_current_directory(rktio);
  }
  
  {
    /* Everything else is in ~: */
    char *home_str, *ex_home, *alt_home, *home;

    /* cast here avoids a clang warning: */
# define mz_STR_OFFSET(s, d) ((const char *)s XFORM_OK_PLUS d)

    if ((which == RKTIO_PATH_PREF_DIR) 
	|| (which == RKTIO_PATH_PREF_FILE)
	|| (which == RKTIO_PATH_ADDON_DIR)) {
#if defined(OS_X) && !defined(XONX)
      if (which == RKTIO_PATH_ADDON_DIR)
	home_str = "~/Library/Racket/";
      else
	home_str = "~/Library/Preferences/";
#else
      home_str = "~/.racket/";
#endif 
    } else {
#if defined(OS_X) && !defined(XONX)
      if (which == RKTIO_PATH_DESK_DIR)
	home_str = "~/Desktop/";
      else if (which == RKTIO_PATH_DOC_DIR)
	home_str = "~/Documents/";
      else
#endif
        home_str = "~/";
    }

    alt_home = getenv("PLTUSERHOME");
    if (alt_home)
      home = append_paths(alt_home, home_str + 2, 0, 0);
    else {
      home = rktio_expand_user_tilde(rktio, home_str);
      
      if (!home) {
        /* Something went wrong with the user lookup. Just drop "~'. */
        int h_len = strlen(home_str);
        home = (char *)malloc(h_len - 2 + 1);
        strcpy(home, home_str+2);
      }
    }
    
    if ((which == RKTIO_PATH_PREF_DIR) || (which == RKTIO_PATH_INIT_DIR) 
	|| (which == RKTIO_PATH_HOME_DIR) || (which == RKTIO_PATH_ADDON_DIR)
	|| (which == RKTIO_PATH_DESK_DIR) || (which == RKTIO_PATH_DOC_DIR))
      return home;

    if (which == RKTIO_PATH_INIT_FILE)
      return append_paths(home, ".racketrc", 1, 0);
    if (which == RKTIO_PATH_PREF_FILE) {
#if defined(OS_X) && !defined(XONX)
      return append_paths(home, "org.racket-lang.prefs.rktd", 1, 0);
#else      
      return append_paths(home, "racket-prefs.rktd", 1, 0);
#endif
    } else
      return strdup("/");
  }
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  if (which == RKTIO_PATH_SYS_DIR) {
    int size;
    wchar_t *s;
    size = GetSystemDirectoryW(NULL, 0);
    s = (wchar_t *)malloc((size + 1) * sizeof(wchar_t));
    GetSystemDirectoryW(s, size + 1);
    return NARROW_PATH_copy_then_free(s);
  }

  {
    char *d, *p;
    Scheme_Object *home;
    
    if (which == RKTIO_PATH_TEMP_DIR) {
      if ((p = getenv("TMP")) || (p = getenv("TEMP"))) {
	if (p && rktio_directory_exists(p))
	  return strdup(p);
      }
      
      return rktio_current_drectory();
    }

    home = NULL;

    p = getenv("PLTUSERHOME");
    if (p)
      home = strdup(p);

    if (!home) {
      /* Try to get Application Data directory: */
      LPITEMIDLIST items;
      int which_folder;

      if ((which == RKTIO_PATH_ADDON_DIR)
	  || (which == RKTIO_PATH_PREF_DIR)
	  || (which == RKTIO_PATH_PREF_FILE)) 
	which_folder = CSIDL_APPDATA;
      else if (which == RKTIO_PATH_DOC_DIR) {
#       ifndef CSIDL_PERSONAL
#         define CSIDL_PERSONAL 0x0005
#       endif
	which_folder = CSIDL_PERSONAL;
      } else if (which == RKTIO_PATH_DESK_DIR)	
	which_folder = CSIDL_DESKTOPDIRECTORY;
      else {
#       ifndef CSIDL_PROFILE
#         define CSIDL_PROFILE 0x0028
#       endif
	which_folder = CSIDL_PROFILE;
      }

      if (SHGetSpecialFolderLocation(NULL, which_folder, &items) == S_OK) {
	int ok;
	IMalloc *mi;
	wchar_t *buf;

	buf = (wchar_t *)malloc(MAX_PATH * sizeof(wchar_t));
	ok = SHGetPathFromIDListW(items, buf);

	SHGetMalloc(&mi);
	mi->lpVtbl->Free(mi, items);
	mi->lpVtbl->Release(mi);

        home = NARROW_PATH_copy_then_free(buf);
      }
    }

    if (!home) {
      /* Back-up: try USERPROFILE environment variable */
      d = getenv("USERPROFILE");
      if (d && rktio_directory_exists(d))
        return strdup(d);
    }

    if (!home) {
      /* Last-ditch effort: try HOMEDRIVE+HOMEPATH */
      d = getenv("HOMEDRIVE");
      p = getenv("HOMEPATH");

      if (d && p) {
        home = append_paths(d, p);
      
	if (rktio_directory_exists(home))
          return home;
        else {
          free(home);
          home = NULL;
        }
      } else 
	home = NULL;
    
      if (!home) {
	wchar_t name[1024];
      
	if (!GetModuleFileNameW(NULL, name, 1024)) {
	  /* Disaster. Use CWD. */
	  home = rktio_current_drectory();
          if (!home)
            return NULL;
	} else {
	  int i;
	  wchar_t *s;
	
	  s = name;
	
	  i = wc_strlen(s) - 1;
	
	  while (i && (s[i] != '\\')) {
	    --i;
	  }
	  s[i] = 0;
	  home = NARROW_PATH_copy(s);
	}
      }
    }
    
    if ((which == RKTIO_PATH_INIT_DIR)
	|| (which == RKTIO_PATH_HOME_DIR)
	|| (which == RKTIO_PATH_DOC_DIR)
	|| (which == RKTIO_PATH_DESK_DIR))
      return home;

    if ((which == RKTIO_PATH_ADDON_DIR)
	|| (which == RKTIO_PATH_PREF_DIR)
	|| (which == RKTIO_PATH_PREF_FILE)) {
      home = append_paths(home, "Racket", 1, 0);
    }

    if (which == RKTIO_PATH_INIT_FILE)
      return append_paths(home, "racketrc.rktl", 1, 0);
    if (which == RKTIO_PATH_PREF_FILE)
      return append_paths(home, "racket-prefs.rktd", 1, 0);
    return home;
  }
#endif
}
