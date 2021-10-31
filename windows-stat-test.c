#include <stdio.h>
#include <windows.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fileapi.h>
#include <errno.h>


typedef int rktio_bool_t;

typedef struct rktio_stat_t {
  /* Eventually, this should use `int64_t`, available in C99 and up */
  uintptr_t device_id, inode, mode, hardlink_count, user_id, group_id,
            device_id_for_special_file, size, block_size, block_count,
            access_time_seconds, access_time_nanoseconds,
            modify_time_seconds, modify_time_nanoseconds,
            ctime_seconds, ctime_nanoseconds;
  /* The `st_ctime` field is status change time for Posix and creation time
     for Windows. */
  rktio_bool_t ctime_is_change_time;
} rktio_stat_t;


long long int filetime_to_nanoseconds(FILETIME filetime) {
  ULARGE_INTEGER hundred_ns_units;
  hundred_ns_units.u.LowPart = filetime.dwLowDateTime;
  hundred_ns_units.u.HighPart = filetime.dwHighDateTime;
  /* Mask out full-seconds part. */
  long long int hundred_ns_fraction = hundred_ns_units.QuadPart % 10000000; /* 10^7 */
  return 100 * hundred_ns_fraction;
}

int main(void) {
  int stat_result;
  struct _stat64 stat_buf;
  struct rktio_stat_t *rktio_stat_buf;

  HANDLE file_handle;
  FILETIME creation_time, access_time, modify_time;
  BOOL gft_result;

  do {
    /* No stat/lstat distinction under Windows */
    stat_result = _stat64("stat-test.c", &stat_buf);
  } while ((stat_result == -1) && (errno == EINTR));

  if (stat_result) {
    printf("errno %d, exiting", errno);
    return 1;
  }

  rktio_stat_buf = (struct rktio_stat_t *) malloc(sizeof(struct rktio_stat_t));
  /* Corresponds to drive on Windows. 0 = A:, 1 = B: etc. */
  rktio_stat_buf->device_id = stat_buf.st_dev;
  rktio_stat_buf->inode = stat_buf.st_ino;
  rktio_stat_buf->mode = stat_buf.st_mode;
  rktio_stat_buf->hardlink_count = stat_buf.st_nlink;
  rktio_stat_buf->user_id = stat_buf.st_uid;
  rktio_stat_buf->group_id = stat_buf.st_gid;
  rktio_stat_buf->device_id_for_special_file = stat_buf.st_rdev;
  rktio_stat_buf->size = stat_buf.st_size;
  /* `st_blksize` and `st_blocks` don't exist under Windows,
     so set them to an arbitrary integer, for example 0. */
  rktio_stat_buf->block_size = 0;
  rktio_stat_buf->block_count = 0;
  /* The stat result under Windows doesn't contain nanoseconds
     information, so set them to 0, corresponding to times in
     whole seconds. */
  rktio_stat_buf->access_time_seconds = stat_buf.st_atime;
  rktio_stat_buf->access_time_nanoseconds = 0;
  rktio_stat_buf->modify_time_seconds = stat_buf.st_mtime;
  rktio_stat_buf->modify_time_nanoseconds = 0;
  rktio_stat_buf->ctime_seconds = stat_buf.st_ctime;
  rktio_stat_buf->ctime_nanoseconds = 0;
  rktio_stat_buf->ctime_is_change_time = 0;

  /* _S_IFREG had more than one bit, so it's not enough to
     check for a true value. */
  if ((int) (stat_buf.st_mode & _S_IFREG) == (int) _S_IFREG) {
    file_handle = CreateFileA("stat-test.c",
                              GENERIC_READ,
                              FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                              NULL,
                              OPEN_EXISTING,
                              FILE_ATTRIBUTE_NORMAL,
                              NULL);
    if (file_handle == INVALID_HANDLE_VALUE) {
      // TODO: Error handling
      printf("Invalid file handle\n");
      abort();
    }

    gft_result = GetFileTime(file_handle, &creation_time, &access_time, &modify_time);
    if (!gft_result) {
      // TODO: Error handling
      printf("Invalid GetFileTime result\n");
      abort();
    } else {
      rktio_stat_buf->ctime_nanoseconds = filetime_to_nanoseconds(creation_time);
      rktio_stat_buf->access_time_nanoseconds = filetime_to_nanoseconds(access_time);
      rktio_stat_buf->modify_time_nanoseconds = filetime_to_nanoseconds(modify_time);
    }
  }

  /* Use `GetFileTime` to get sub-second precision. */

  printf("device id: %lld\n", rktio_stat_buf->device_id);
  printf("inode: %lld\n", rktio_stat_buf->inode);
  printf("mode: %lld\n", rktio_stat_buf->mode);
  printf("hardlink_count: %lld\n", rktio_stat_buf->hardlink_count);
  printf("user_id: %lld\n", rktio_stat_buf->user_id);
  printf("group_id: %lld\n", rktio_stat_buf->group_id);
  printf("device_id_for_special_file: %lld\n", rktio_stat_buf->device_id_for_special_file);
  printf("size: %lld\n", rktio_stat_buf->size);
  printf("atime: %lld\n", rktio_stat_buf->access_time_seconds);
  printf("atime_ns: %lld\n", rktio_stat_buf->access_time_nanoseconds);
  printf("mtime: %lld\n", rktio_stat_buf->modify_time_seconds);
  printf("mtime_ns: %lld\n", rktio_stat_buf->modify_time_nanoseconds);
  printf("ctime: %lld\n", rktio_stat_buf->ctime_seconds);
  printf("ctime_ns: %lld\n", rktio_stat_buf->ctime_nanoseconds);
  printf("ctime_is_change_time: %d\n", rktio_stat_buf->ctime_is_change_time);

  return 0;
}
