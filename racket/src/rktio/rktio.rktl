;; Extracted from rktio.h by rktio/parse.rkt
(begin
(define-constant RKTIO_OPEN_READ (<< 1 0))
(define-constant RKTIO_OPEN_WRITE (<< 1 1))
(define-constant RKTIO_OPEN_TEXT (<< 1 2))
(define-constant RKTIO_OPEN_TRUNCATE (<< 1 3))
(define-constant RKTIO_OPEN_APPEND (<< 1 4))
(define-constant RKTIO_OPEN_MUST_EXIST (<< 1 5))
(define-constant RKTIO_OPEN_CAN_EXIST (<< 1 6))
(define-constant RKTIO_OPEN_SOCKET (<< 1 7))
(define-constant RKTIO_OPEN_UDP (<< 1 8))
(define-constant RKTIO_OPEN_REGFILE (<< 1 9))
(define-constant RKTIO_OPEN_NOT_REGFILE (<< 1 10))
(define-constant RKTIO_OPEN_DIR (<< 1 11))
(define-constant RKTIO_OPEN_NOT_DIR (<< 1 12))
(define-constant RKTIO_OPEN_INIT (<< 1 13))
(define-constant RKTIO_OPEN_OWN (<< 1 14))
(define-constant RKTIO_STDIN 0)
(define-constant RKTIO_STDOUT 1)
(define-constant RKTIO_STDERR 2)
(define-constant RKTIO_READ_EOF -1)
(define-constant RKTIO_READ_ERROR -2)
(define-constant RKTIO_WRITE_ERROR -2)
(define-constant RKTIO_POLL_NOT_READY 0)
(define-constant RKTIO_POLL_READY 1)
(define-constant RKTIO_POLL_ERROR -2)
(define-constant RKTIO_LOCK_ERROR -2)
(define-constant RKTIO_LOCK_ACQUIRED 1)
(define-constant RKTIO_LOCK_NOT_ACQUIRED 0)
(define-constant RKTIO_POSITION_FROM_START 0)
(define-constant RKTIO_POSITION_FROM_END 1)
(define-constant RKTIO_NO_INHERIT_INPUT (<< 1 0))
(define-constant RKTIO_NO_INHERIT_OUTPUT (<< 1 1))
(define-constant RKTIO_FAMILY_ANY -1)
(define-constant RKTIO_SHUTDOWN_READ 0)
(define-constant RKTIO_SHUTDOWN_WRITE 1)
(define-constant RKTIO_PROP_ERROR -2)
(define-constant RKTIO_ADD_MEMBERSHIP 0)
(define-constant RKTIO_DROP_MEMBERSHIP 1)
(define-constant RKTIO_PROCESS_NEW_GROUP (<< 1 0))
(define-constant RKTIO_PROCESS_STDOUT_AS_STDERR (<< 1 1))
(define-constant RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE (<< 1 2))
(define-constant RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION (<< 1 3))
(define-constant RKTIO_PROCESS_ERROR -2)
(define-constant RKTIO_PROCESS_DONE 1)
(define-constant RKTIO_PROCESS_RUNNING 0)
(define-constant RKTIO_FS_CHANGE_SUPPORTED (<< 1 0))
(define-constant RKTIO_FS_CHANGE_SCALABLE (<< 1 1))
(define-constant RKTIO_FS_CHANGE_LOW_LATENCY (<< 1 2))
(define-constant RKTIO_FS_CHANGE_FILE_LEVEL (<< 1 3))
(define-constant RKTIO_FS_CHANGE_NEED_LTPS (<< 1 4))
(define-constant RKTIO_POLL_READ RKTIO_OPEN_READ)
(define-constant RKTIO_POLL_WRITE RKTIO_OPEN_WRITE)
(define-constant RKTIO_POLL_FLUSH (<< RKTIO_OPEN_WRITE 2))
(define-constant RKTIO_LTPS_CREATE_READ 1)
(define-constant RKTIO_LTPS_CREATE_WRITE 2)
(define-constant RKTIO_LTPS_CHECK_READ 3)
(define-constant RKTIO_LTPS_CHECK_WRITE 4)
(define-constant RKTIO_LTPS_REMOVE 5)
(define-constant RKTIO_LTPS_CREATE_VNODE 6)
(define-constant RKTIO_LTPS_CHECK_VNODE 7)
(define-constant RKTIO_LTPS_REMOVE_VNODE 8)
(define-constant RKTIO_LTPS_HANDLE_NONE 0)
(define-constant RKTIO_LTPS_HANDLE_ZERO 1)
(define-constant RKTIO_LTPS_HANDLE_FREE 2)
(define-constant RKTIO_PERMISSION_READ 4)
(define-constant RKTIO_PERMISSION_WRITE 2)
(define-constant RKTIO_PERMISSION_EXEC 1)
(define-constant RKTIO_PERMISSION_ERROR -1)
(define-constant RKTIO_COPY_STEP_UNKNOWN 0)
(define-constant RKTIO_COPY_STEP_OPEN_SRC 1)
(define-constant RKTIO_COPY_STEP_OPEN_DEST 2)
(define-constant RKTIO_COPY_STEP_READ_SRC_DATA 3)
(define-constant RKTIO_COPY_STEP_WRITE_DEST_DATA 4)
(define-constant RKTIO_COPY_STEP_READ_SRC_METADATA 5)
(define-constant RKTIO_COPY_STEP_WRITE_DEST_METADATA 6)
(define-constant RKTIO_PATH_SYS_DIR 0)
(define-constant RKTIO_PATH_TEMP_DIR 1)
(define-constant RKTIO_PATH_PREF_DIR 2)
(define-constant RKTIO_PATH_PREF_FILE 3)
(define-constant RKTIO_PATH_ADDON_DIR 4)
(define-constant RKTIO_PATH_HOME_DIR 5)
(define-constant RKTIO_PATH_DESK_DIR 6)
(define-constant RKTIO_PATH_DOC_DIR 7)
(define-constant RKTIO_PATH_INIT_DIR 8)
(define-constant RKTIO_PATH_INIT_FILE 9)
(define-constant RKTIO_OS_SIGNAL_NONE -1)
(define-constant RKTIO_OS_SIGNAL_INT 0)
(define-constant RKTIO_OS_SIGNAL_TERM 1)
(define-constant RKTIO_OS_SIGNAL_HUP 2)
(define-constant RKTIO_NUM_OS_SIGNALS 3)
(define-constant RKTIO_SW_HIDE 0)
(define-constant RKTIO_SW_MAXIMIZE 1)
(define-constant RKTIO_SW_MINIMIZE 2)
(define-constant RKTIO_SW_RESTORE 3)
(define-constant RKTIO_SW_SHOW 4)
(define-constant RKTIO_SW_SHOWDEFAULT 5)
(define-constant RKTIO_SW_SHOWMAXIMIZED 6)
(define-constant RKTIO_SW_SHOWMINIMIZED 7)
(define-constant RKTIO_SW_SHOWMINNOACTIVE 8)
(define-constant RKTIO_SW_SHOWNA 9)
(define-constant RKTIO_SW_SHOWNOACTIVATE 10)
(define-constant RKTIO_SW_SHOWNORMAL 11)
(define-constant RKTIO_LOG_FATAL 1)
(define-constant RKTIO_LOG_ERROR 2)
(define-constant RKTIO_LOG_WARNING 3)
(define-constant RKTIO_LOG_INFO 4)
(define-constant RKTIO_LOG_DEBUG 5)
(define-constant RKTIO_CONVERTER_SUPPORTED (<< 1 0))
(define-constant RKTIO_CONVERT_STRCOLL_UTF16 (<< 1 1))
(define-constant RKTIO_CONVERT_RECASE_UTF16 (<< 1 2))
(define-constant RKTIO_CONVERT_ERROR -1)
(define-constant RKTIO_SHA1_DIGEST_SIZE 20)
(define-constant RKTIO_SHA224_DIGEST_SIZE 28)
(define-constant RKTIO_SHA256_DIGEST_SIZE 32)
(define-constant RKTIO_ERROR_KIND_POSIX 0)
(define-constant RKTIO_ERROR_KIND_WINDOWS 1)
(define-constant RKTIO_ERROR_KIND_GAI 2)
(define-constant RKTIO_ERROR_KIND_RACKET 3)
(define-constant RKTIO_ERROR_UNSUPPORTED 1)
(define-constant RKTIO_ERROR_INVALID_PATH 2)
(define-constant RKTIO_ERROR_DOES_NOT_EXIST 3)
(define-constant RKTIO_ERROR_EXISTS 4)
(define-constant RKTIO_ERROR_ACCESS_DENIED 5)
(define-constant RKTIO_ERROR_LINK_FAILED 6)
(define-constant RKTIO_ERROR_NOT_A_LINK 7)
(define-constant RKTIO_ERROR_BAD_PERMISSION 8)
(define-constant RKTIO_ERROR_IS_A_DIRECTORY 9)
(define-constant RKTIO_ERROR_NOT_A_DIRECTORY 10)
(define-constant RKTIO_ERROR_UNSUPPORTED_TEXT_MODE 11)
(define-constant RKTIO_ERROR_CANNOT_FILE_POSITION 12)
(define-constant RKTIO_ERROR_NO_TILDE 13)
(define-constant RKTIO_ERROR_ILL_FORMED_USER 14)
(define-constant RKTIO_ERROR_UNKNOWN_USER 15)
(define-constant RKTIO_ERROR_INIT_FAILED 16)
(define-constant RKTIO_ERROR_LTPS_NOT_FOUND 17)
(define-constant RKTIO_ERROR_LTPS_REMOVED 18)
(define-constant RKTIO_ERROR_CONNECT_TRYING_NEXT 19)
(define-constant RKTIO_ERROR_ACCEPT_NOT_READY 20)
(define-constant RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED 21)
(define-constant RKTIO_ERROR_INFO_TRY_AGAIN 22)
(define-constant RKTIO_ERROR_TRY_AGAIN 23)
(define-constant RKTIO_ERROR_TRY_AGAIN_WITH_IPV4 24)
(define-constant RKTIO_ERROR_TIME_OUT_OF_RANGE 25)
(define-constant RKTIO_ERROR_NO_SUCH_ENVVAR 26)
(define-constant RKTIO_ERROR_SHELL_EXECUTE_FAILED 27)
(define-constant RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE 28)
(define-constant RKTIO_ERROR_CONVERT_BAD_SEQUENCE 29)
(define-constant RKTIO_ERROR_CONVERT_PREMATURE_END 30)
(define-constant RKTIO_ERROR_CONVERT_OTHER 31)
(define-constant RKTIO_ERROR_DLL 32)
(define-type rktio_ok_t int)
(define-type rktio_tri_t int)
(define-type rktio_bool_t int)
(define-type rktio_char16_t unsigned-short)
(define-type rktio_const_string_t (*ref char))
(define-type rktio_filesize_t rktio_int64_t)
(define-struct-type
 rktio_length_and_addrinfo_t
 ((intptr_t len) ((ref (ref char)) address)))
(define-struct-type
 rktio_process_result_t
 (((ref rktio_process_t) process)
  ((ref rktio_fd_t) stdin_fd)
  ((ref rktio_fd_t) stdout_fd)
  ((ref rktio_fd_t) stderr_fd)))
(define-struct-type rktio_status_t ((rktio_bool_t running) (int result)))
(define-type rktio_timestamp_t intptr_t)
(define-struct-type
 rktio_identity_t
 ((uintptr_t a)
  (uintptr_t b)
  (uintptr_t c)
  (int a_bits)
  (int b_bits)
  (int c_bits)))
(define-struct-type
 rktio_date_t
 ((int nanosecond)
  (int second)
  (int minute)
  (int hour)
  (int day)
  (int month)
  (intptr_t year)
  (int day_of_week)
  (int day_of_year)
  (int is_dst)
  (int zone_offset)
  ((ref char) zone_name)))
(define-struct-type
 rktio_convert_result_t
 ((intptr_t in_consumed) (intptr_t out_produced) (intptr_t converted)))
(define-struct-type
 rktio_sha1_ctx_t
 (((array 5 unsigned) state)
  ((array 2 unsigned) count)
  ((array 64 unsigned-8) buffer)))
(define-struct-type
 rktio_sha2_ctx_t
 (((array 2 unsigned) total)
  ((array 8 unsigned) state)
  ((array 64 unsigned-8) buffer)
  (int is224)))
(define-type dll_open_proc function-pointer)
(define-type dll_find_object_proc function-pointer)
(define-function () (ref rktio_t) rktio_init ())
(define-function () void rktio_destroy (((ref rktio_t) rktio)))
(define-function () void rktio_free (((ref void) p)))
(define-function () void rktio_set_dll_path (((*ref rktio_char16_t) p)))
(define-function/errno
 NULL
 ()
 (ref rktio_char16_t)
 rktio_get_dll_path
 (((*ref rktio_char16_t) p)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_system_fd
 (((ref rktio_t) rktio) (intptr_t system_fd) (int modes)))
(define-function
 ()
 intptr_t
 rktio_fd_system_fd
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_regular_file
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_directory
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_socket
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_udp
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_terminal
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 rktio_bool_t
 rktio_fd_is_text_converted
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 int
 rktio_fd_modes
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_open
 (((ref rktio_t) rktio) (rktio_const_string_t src) (int modes)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_close
 (((ref rktio_t) rktio) ((ref rktio_fd_t) fd)))
(define-function
 ()
 void
 rktio_close_noerr
 (((ref rktio_t) rktio) ((ref rktio_fd_t) fd)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_dup
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 void
 rktio_forget
 (((ref rktio_t) rktio) ((ref rktio_fd_t) fd)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_std_fd
 (((ref rktio_t) rktio) (int which)))
(define-function () void rktio_create_console ())
(define-function/errno
 RKTIO_READ_ERROR
 ()
 intptr_t
 rktio_read
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) fd)
  ((*ref char) buffer)
  (intptr_t len)))
(define-function/errno
 RKTIO_WRITE_ERROR
 ()
 intptr_t
 rktio_write
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) fd)
  ((*ref char) buffer)
  (intptr_t len)))
(define-function/errno
 RKTIO_READ_ERROR
 ()
 intptr_t
 rktio_read_converted
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) fd)
  ((*ref char) buffer)
  (intptr_t len)
  ((*ref char) is_converted)))
(define-function/errno
 RKTIO_READ_ERROR
 ()
 intptr_t
 rktio_read_in
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) fd)
  ((*ref char) buffer)
  (intptr_t start)
  (intptr_t end)))
(define-function/errno
 RKTIO_WRITE_ERROR
 ()
 intptr_t
 rktio_write_in
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) fd)
  ((*ref char) buffer)
  (intptr_t start)
  (intptr_t end)))
(define-function
 ()
 intptr_t
 rktio_buffered_byte_count
 (((ref rktio_t) rktio) ((ref rktio_fd_t) fd)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_read_ready
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_write_ready
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_write_flushed
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 RKTIO_LOCK_ERROR
 ()
 rktio_tri_t
 rktio_file_lock_try
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (rktio_bool_t excl)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_file_unlock
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_set_file_position
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  (rktio_filesize_t pos)
  (int whence)))
(define-function/errno
 NULL
 ()
 (ref rktio_filesize_t)
 rktio_get_file_position
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_set_file_size
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (rktio_filesize_t sz)))
(define-function
 ()
 (ref rktio_fd_transfer_t)
 rktio_fd_detach
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function
 ()
 (ref rktio_fd_t)
 rktio_fd_attach
 (((ref rktio_t) rktio) ((ref rktio_fd_transfer_t) rfdt)))
(define-function
 ()
 void
 rktio_fd_close_transfer
 (((ref rktio_fd_transfer_t) rfdt)))
(define-function/errno
 NULL
 ()
 (ref (ref rktio_fd_t))
 rktio_make_pipe
 (((ref rktio_t) rktio) (int flags)))
(define-function/errno
 NULL
 ()
 (ref rktio_addrinfo_lookup_t)
 rktio_start_addrinfo_lookup
 (((ref rktio_t) rktio)
  (rktio_const_string_t hostname)
  (int portno)
  (int family)
  (rktio_bool_t passive)
  (rktio_bool_t tcp)))
(define-function () int rktio_get_ipv4_family (((ref rktio_t) rktio)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_addrinfo_lookup_ready
 (((ref rktio_t) rktio) ((ref rktio_addrinfo_lookup_t) lookup)))
(define-function/errno
 NULL
 ()
 (ref rktio_addrinfo_t)
 rktio_addrinfo_lookup_get
 (((ref rktio_t) rktio) ((ref rktio_addrinfo_lookup_t) lookup)))
(define-function
 ()
 void
 rktio_addrinfo_lookup_stop
 (((ref rktio_t) rktio) ((ref rktio_addrinfo_lookup_t) lookup)))
(define-function
 ()
 void
 rktio_addrinfo_free
 (((ref rktio_t) rktio) ((ref rktio_addrinfo_t) a)))
(define-function/errno
 NULL
 ()
 (ref rktio_listener_t)
 rktio_listen
 (((ref rktio_t) rktio)
  ((ref rktio_addrinfo_t) local)
  (int backlog)
  (rktio_bool_t reuse)))
(define-function
 ()
 void
 rktio_listen_stop
 (((ref rktio_t) rktio) ((ref rktio_listener_t) l)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_accept_ready
 (((ref rktio_t) rktio) ((ref rktio_listener_t) listener)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_accept
 (((ref rktio_t) rktio) ((ref rktio_listener_t) listener)))
(define-function/errno
 NULL
 ()
 (ref rktio_connect_t)
 rktio_start_connect
 (((ref rktio_t) rktio)
  ((ref rktio_addrinfo_t) remote)
  ((ref (nullable rktio_addrinfo_t)) local)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_connect_finish
 (((ref rktio_t) rktio) ((ref rktio_connect_t) conn)))
(define-function
 ()
 void
 rktio_connect_stop
 (((ref rktio_t) rktio) ((ref rktio_connect_t) conn)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_connect_ready
 (((ref rktio_t) rktio) ((ref rktio_connect_t) conn)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_connect_trying
 (((ref rktio_t) rktio) ((ref rktio_connect_t) conn)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_socket_shutdown
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (int mode)))
(define-function/errno
 NULL
 ()
 (ref rktio_fd_t)
 rktio_udp_open
 (((ref rktio_t) rktio) ((ref (nullable rktio_addrinfo_t)) addr) (int family)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_disconnect
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_bind
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref rktio_addrinfo_t) addr)
  (rktio_bool_t reuse)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_connect
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) ((ref rktio_addrinfo_t) addr)))
(define-function/errno
 RKTIO_WRITE_ERROR
 ()
 intptr_t
 rktio_udp_sendto
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref (nullable rktio_addrinfo_t)) addr)
  ((*ref char) buffer)
  (intptr_t len)))
(define-function/errno
 RKTIO_WRITE_ERROR
 ()
 intptr_t
 rktio_udp_sendto_in
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref (nullable rktio_addrinfo_t)) addr)
  ((*ref char) buffer)
  (intptr_t start)
  (intptr_t end)))
(define-function/errno
 NULL
 ()
 (ref rktio_length_and_addrinfo_t)
 rktio_udp_recvfrom
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((*ref char) buffer)
  (intptr_t len)))
(define-function/errno
 NULL
 ()
 (ref rktio_length_and_addrinfo_t)
 rktio_udp_recvfrom_in
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((*ref char) buffer)
  (intptr_t start)
  (intptr_t end)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_set_receive_buffer_size
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (int size)))
(define-function/errno
 RKTIO_PROP_ERROR
 ()
 rktio_tri_t
 rktio_udp_get_multicast_loopback
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_set_multicast_loopback
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (rktio_bool_t on)))
(define-function/errno
 RKTIO_PROP_ERROR
 ()
 rktio_tri_t
 rktio_udp_get_multicast_ttl
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_set_multicast_ttl
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd) (int ttl_val)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_udp_multicast_interface
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_set_multicast_interface
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref (nullable rktio_addrinfo_t)) addr)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_udp_change_multicast_group
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref rktio_addrinfo_t) group_addr)
  ((ref (nullable rktio_addrinfo_t)) intf_addr)
  (int action)))
(define-function/errno
 NULL
 ()
 (ref (ref char))
 rktio_socket_address
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 NULL
 ()
 (ref (ref char))
 rktio_socket_peer_address
 (((ref rktio_t) rktio) ((ref rktio_fd_t) rfd)))
(define-function/errno
 NULL
 ()
 (ref (ref char))
 rktio_listener_address
 (((ref rktio_t) rktio) ((ref rktio_listener_t) lnr)))
(define-function
 ()
 rktio_bool_t
 rktio_is_ok_envvar_name
 (((ref rktio_t) rktio) (rktio_const_string_t name)))
(define-function
 ()
 rktio_bool_t
 rktio_are_envvar_names_case_insensitive
 (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_getenv
 (((ref rktio_t) rktio) (rktio_const_string_t name)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_setenv
 (((ref rktio_t) rktio)
  (rktio_const_string_t name)
  (rktio_const_string_t val)))
(define-function/errno
 NULL
 ()
 (ref rktio_envvars_t)
 rktio_envvars
 (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_envvars_t)
 rktio_empty_envvars
 (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_envvars_t)
 rktio_envvars_copy
 (((ref rktio_t) rktio) ((ref rktio_envvars_t) envvars)))
(define-function
 ()
 void
 rktio_envvars_free
 (((ref rktio_t) rktio) ((ref rktio_envvars_t) envvars)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_envvars_get
 (((ref rktio_t) rktio)
  ((ref rktio_envvars_t) envvars)
  (rktio_const_string_t name)))
(define-function
 ()
 void
 rktio_envvars_set
 (((ref rktio_t) rktio)
  ((ref rktio_envvars_t) envvars)
  (rktio_const_string_t name)
  (rktio_const_string_t value)))
(define-function
 ()
 intptr_t
 rktio_envvars_count
 (((ref rktio_t) rktio) ((ref rktio_envvars_t) envvars)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_envvars_name_ref
 (((ref rktio_t) rktio) ((ref rktio_envvars_t) envvars) (intptr_t i)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_envvars_value_ref
 (((ref rktio_t) rktio) ((ref rktio_envvars_t) envvars) (intptr_t i)))
(define-function/errno
 NULL
 ()
 (ref rktio_process_result_t)
 rktio_process
 (((ref rktio_t) rktio)
  (rktio_const_string_t command)
  (int argc)
  ((*ref rktio_const_string_t) argv)
  ((ref (nullable rktio_fd_t)) stdout_fd)
  ((ref (nullable rktio_fd_t)) stdin_fd)
  ((ref (nullable rktio_fd_t)) stderr_fd)
  ((ref (nullable rktio_process_t)) group_proc)
  (rktio_const_string_t current_directory)
  ((ref rktio_envvars_t) envvars)
  (int flags)))
(define-function () int rktio_process_allowed_flags (((ref rktio_t) rktio)))
(define-function
 ()
 int
 rktio_process_pid
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_process_kill
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_process_interrupt
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function
 ()
 void
 rktio_process_forget
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function/errno
 RKTIO_PROCESS_ERROR
 ()
 rktio_tri_t
 rktio_poll_process_done
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function/errno
 NULL
 ()
 (ref rktio_status_t)
 rktio_process_status
 (((ref rktio_t) rktio) ((ref rktio_process_t) sp)))
(define-function () void rktio_reap_processes (((ref rktio_t) rktio)))
(define-function () int rktio_fs_change_properties (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_fs_change_t)
 rktio_fs_change
 (((ref rktio_t) rktio) (rktio_const_string_t path) ((ref rktio_ltps_t) ltps)))
(define-function
 ()
 void
 rktio_fs_change_forget
 (((ref rktio_t) rktio) ((ref rktio_fs_change_t) fc)))
(define-function/errno
 RKTIO_POLL_ERROR
 ()
 rktio_tri_t
 rktio_poll_fs_change_ready
 (((ref rktio_t) rktio) ((ref rktio_fs_change_t) fc)))
(define-function/errno
 NULL
 ()
 (ref rktio_poll_set_t)
 rktio_make_poll_set
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_poll_set_forget
 (((ref rktio_t) rktio) ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_add
 (((ref rktio_t) rktio)
  ((ref rktio_fd_t) rfd)
  ((ref rktio_poll_set_t) fds)
  (int modes)))
(define-function
 ()
 void
 rktio_poll_add_accept
 (((ref rktio_t) rktio)
  ((ref rktio_listener_t) listener)
  ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_add_connect
 (((ref rktio_t) rktio)
  ((ref rktio_connect_t) conn)
  ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_add_addrinfo_lookup
 (((ref rktio_t) rktio)
  ((ref rktio_addrinfo_lookup_t) lookup)
  ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_add_process
 (((ref rktio_t) rktio)
  ((ref rktio_process_t) sp)
  ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_add_fs_change
 (((ref rktio_t) rktio)
  ((ref rktio_fs_change_t) fc)
  ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_set_add_nosleep
 (((ref rktio_t) rktio) ((ref rktio_poll_set_t) fds)))
(define-function
 ()
 void
 rktio_poll_set_add_handle
 (((ref rktio_t) rktio)
  (intptr_t h)
  ((ref rktio_poll_set_t) fds)
  (int repost)))
(define-function
 ()
 void
 rktio_poll_set_add_eventmask
 (((ref rktio_t) rktio) ((ref rktio_poll_set_t) fds) (int mask)))
(define-function () void rkio_reset_sleep_backoff (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_ltps_t)
 rktio_ltps_open
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_ltps_close
 (((ref rktio_t) rktio) ((ref rktio_ltps_t) lt)))
(define-function/errno
 NULL
 ()
 (ref rktio_ltps_handle_t)
 rktio_ltps_add
 (((ref rktio_t) rktio)
  ((ref rktio_ltps_t) lt)
  ((ref rktio_fd_t) rfd)
  (int mode)))
(define-function
 ()
 void
 rktio_ltps_handle_set_data
 (((ref rktio_t) rktio) ((ref rktio_ltps_handle_t) h) ((ref void) data)))
(define-function
 ()
 (ref void)
 rktio_ltps_handle_get_data
 (((ref rktio_t) rktio) ((ref rktio_ltps_handle_t) h)))
(define-function
 ()
 void
 rktio_ltps_remove_all
 (((ref rktio_t) rktio) ((ref rktio_ltps_t) lt)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_ltps_poll
 (((ref rktio_t) rktio) ((ref rktio_ltps_t) lt)))
(define-function/errno
 NULL
 ()
 (ref rktio_ltps_handle_t)
 rktio_ltps_get_signaled_handle
 (((ref rktio_t) rktio) ((ref rktio_ltps_t) lt)))
(define-function
 ()
 void
 rktio_ltps_handle_set_auto
 (((ref rktio_t) rktio) ((ref rktio_ltps_handle_t) lth) (int auto_mode)))
(define-function
 (blocking)
 void
 rktio_sleep
 (((ref rktio_t) rktio)
  (float nsecs)
  ((ref rktio_poll_set_t) fds)
  ((ref rktio_ltps_t) lt)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_start_sleep
 (((ref rktio_t) rktio)
  (float nsecs)
  ((ref rktio_poll_set_t) fds)
  ((ref rktio_ltps_t) lt)
  (int woke_fd)))
(define-function () void rktio_end_sleep (((ref rktio_t) rktio)))
(define-function
 ()
 rktio_bool_t
 rktio_file_exists
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function
 ()
 rktio_bool_t
 rktio_directory_exists
 (((ref rktio_t) rktio) (rktio_const_string_t dirname)))
(define-function
 ()
 rktio_bool_t
 rktio_link_exists
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function
 ()
 rktio_bool_t
 rktio_is_regular_file
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_delete_file
 (((ref rktio_t) rktio)
  (rktio_const_string_t fn)
  (rktio_bool_t enable_write_on_fail)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_rename_file
 (((ref rktio_t) rktio)
  (rktio_const_string_t dest)
  (rktio_const_string_t src)
  (rktio_bool_t exists_ok)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_get_current_directory
 (((ref rktio_t) rktio)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_set_current_directory
 (((ref rktio_t) rktio) (rktio_const_string_t path)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_make_directory
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_delete_directory
 (((ref rktio_t) rktio)
  (rktio_const_string_t filename)
  (rktio_const_string_t current_directory)
  (rktio_bool_t enable_write_on_fail)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_readlink
 (((ref rktio_t) rktio) (rktio_const_string_t fullfilename)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_make_link
 (((ref rktio_t) rktio)
  (rktio_const_string_t src)
  (rktio_const_string_t dest)
  (rktio_bool_t dest_is_directory)))
(define-function/errno
 NULL
 ()
 (ref rktio_filesize_t)
 rktio_file_size
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function/errno
 NULL
 ()
 (ref rktio_timestamp_t)
 rktio_get_file_modify_seconds
 (((ref rktio_t) rktio) (rktio_const_string_t file)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_set_file_modify_seconds
 (((ref rktio_t) rktio) (rktio_const_string_t file) (rktio_timestamp_t secs)))
(define-function/errno
 NULL
 ()
 (ref rktio_identity_t)
 rktio_fd_identity
 (((ref rktio_t) rktio) ((ref rktio_fd_t) fd)))
(define-function/errno
 NULL
 ()
 (ref rktio_identity_t)
 rktio_path_identity
 (((ref rktio_t) rktio)
  (rktio_const_string_t path)
  (rktio_bool_t follow_links)))
(define-function/errno
 RKTIO_PERMISSION_ERROR
 ()
 int
 rktio_get_file_or_directory_permissions
 (((ref rktio_t) rktio)
  (rktio_const_string_t filename)
  (rktio_bool_t all_bits)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_set_file_or_directory_permissions
 (((ref rktio_t) rktio) (rktio_const_string_t filename) (int new_bits)))
(define-function/errno
 NULL
 ()
 (ref rktio_directory_list_t)
 rktio_directory_list_start
 (((ref rktio_t) rktio) (rktio_const_string_t dirname)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_directory_list_step
 (((ref rktio_t) rktio) ((ref rktio_directory_list_t) dl)))
(define-function
 ()
 void
 rktio_directory_list_stop
 (((ref rktio_t) rktio) ((ref rktio_directory_list_t) dl)))
(define-function/errno
 NULL
 ()
 (ref (ref char))
 rktio_filesystem_roots
 (((ref rktio_t) rktio)))
(define-function/errno+step
 NULL
 ()
 (ref rktio_file_copy_t)
 rktio_copy_file_start
 (((ref rktio_t) rktio)
  (rktio_const_string_t dest)
  (rktio_const_string_t src)
  (rktio_bool_t exists_ok)))
(define-function
 ()
 rktio_bool_t
 rktio_copy_file_is_done
 (((ref rktio_t) rktio) ((ref rktio_file_copy_t) fc)))
(define-function/errno+step
 #f
 ()
 rktio_ok_t
 rktio_copy_file_step
 (((ref rktio_t) rktio) ((ref rktio_file_copy_t) fc)))
(define-function/errno+step
 #f
 ()
 rktio_ok_t
 rktio_copy_file_finish_permissions
 (((ref rktio_t) rktio) ((ref rktio_file_copy_t) fc)))
(define-function
 ()
 void
 rktio_copy_file_stop
 (((ref rktio_t) rktio) ((ref rktio_file_copy_t) fc)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_system_path
 (((ref rktio_t) rktio) (int which)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_expand_user_tilde
 (((ref rktio_t) rktio) (rktio_const_string_t filename)))
(define-function
 ()
 (ref rktio_signal_handle_t)
 rktio_get_signal_handle
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_signal_received_at
 (((ref rktio_signal_handle_t) h)))
(define-function () void rktio_signal_received (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_wait_until_signal_received
 (((ref rktio_t) rktio)))
(define-function () void rktio_flush_signals_received (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_install_os_signal_handler
 (((ref rktio_t) rktio)))
(define-function () int rktio_poll_os_signal (((ref rktio_t) rktio)))
(define-function () intptr_t rktio_get_milliseconds ())
(define-function () double rktio_get_inexact_milliseconds ())
(define-function
 ()
 intptr_t
 rktio_get_process_milliseconds
 (((ref rktio_t) rktio)))
(define-function
 ()
 intptr_t
 rktio_get_process_children_milliseconds
 (((ref rktio_t) rktio)))
(define-function
 ()
 rktio_timestamp_t
 rktio_get_seconds
 (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_date_t)
 rktio_seconds_to_date
 (((ref rktio_t) rktio)
  (rktio_timestamp_t seconds)
  (int nanoseconds)
  (int get_gmt)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_shell_execute
 (((ref rktio_t) rktio)
  (rktio_const_string_t verb)
  (rktio_const_string_t target)
  (rktio_const_string_t arg)
  (rktio_const_string_t dir)
  (int show_mode)))
(define-function/errno
 NULL
 ()
 (ref rktio_char16_t)
 rktio_path_to_wide_path
 (((ref rktio_t) rktio) (rktio_const_string_t p)))
(define-function
 ()
 (ref char)
 rktio_wide_path_to_path
 (((ref rktio_t) rktio) ((*ref rktio_char16_t) wp)))
(define-function () int rktio_processor_count (((ref rktio_t) rktio)))
(define-function/errno
 #f
 ()
 rktio_ok_t
 rktio_syslog
 (((ref rktio_t) rktio)
  (int level)
  (rktio_const_string_t name)
  (rktio_const_string_t msg)
  (rktio_const_string_t exec_name)))
(define-function () int rktio_convert_properties (((ref rktio_t) rktio)))
(define-function/errno
 NULL
 ()
 (ref rktio_converter_t)
 rktio_converter_open
 (((ref rktio_t) rktio)
  (rktio_const_string_t to_enc)
  (rktio_const_string_t from_enc)))
(define-function
 ()
 void
 rktio_converter_close
 (((ref rktio_t) rktio) ((ref rktio_converter_t) cvt)))
(define-function/errno
 RKTIO_CONVERT_ERROR
 ()
 intptr_t
 rktio_convert
 (((ref rktio_t) rktio)
  ((ref rktio_converter_t) cvt)
  ((*ref (ref char)) in)
  ((*ref intptr_t) in_left)
  ((*ref (ref char)) out)
  ((*ref intptr_t) out_left)))
(define-function/errno
 NULL
 ()
 (ref rktio_convert_result_t)
 rktio_convert_in
 (((ref rktio_t) rktio)
  ((ref rktio_converter_t) cvt)
  ((*ref char) in)
  (intptr_t in_start)
  (intptr_t in_end)
  ((*ref char) out)
  (intptr_t out_start)
  (intptr_t out_end)))
(define-function
 ()
 (ref char)
 rktio_locale_recase
 (((ref rktio_t) rktio) (rktio_bool_t to_up) (rktio_const_string_t in)))
(define-function
 ()
 (ref rktio_char16_t)
 rktio_recase_utf16
 (((ref rktio_t) rktio)
  (rktio_bool_t to_up)
  ((*ref rktio_char16_t) s1)
  (intptr_t len)
  ((*ref intptr_t) olen)))
(define-function
 ()
 int
 rktio_locale_strcoll
 (((ref rktio_t) rktio) (rktio_const_string_t s1) (rktio_const_string_t s2)))
(define-function
 ()
 int
 rktio_strcoll_utf16
 (((ref rktio_t) rktio)
  ((*ref rktio_char16_t) s1)
  (intptr_t l1)
  ((*ref rktio_char16_t) s2)
  (intptr_t l2)
  (rktio_bool_t cvt_case)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_locale_encoding
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_set_locale
 (((ref rktio_t) rktio) (rktio_const_string_t name)))
(define-function
 ()
 (ref char)
 rktio_push_c_numeric_locale
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_pop_c_numeric_locale
 (((ref rktio_t) rktio) ((*ref char) prev)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_system_language_country
 (((ref rktio_t) rktio)))
(define-function () void rktio_sha1_init (((*ref rktio_sha1_ctx_t) context)))
(define-function
 ()
 void
 rktio_sha1_update
 (((*ref rktio_sha1_ctx_t) context)
  ((*ref unsigned-8) data)
  (intptr_t start)
  (intptr_t end)))
(define-function
 ()
 void
 rktio_sha1_final
 (((*ref rktio_sha1_ctx_t) context) ((*ref unsigned-8) digest)))
(define-function
 ()
 void
 rktio_sha2_init
 (((*ref rktio_sha2_ctx_t) ctx) (rktio_bool_t is224)))
(define-function
 ()
 void
 rktio_sha2_update
 (((*ref rktio_sha2_ctx_t) ctx)
  ((*ref unsigned-8) data)
  (intptr_t start)
  (intptr_t end)))
(define-function
 ()
 void
 rktio_sha2_final
 (((*ref rktio_sha2_ctx_t) ctx) ((*ref unsigned-8) digest)))
(define-function/errno
 NULL
 ()
 (ref rktio_dll_t)
 rktio_dll_open
 (((ref rktio_t) rktio) (rktio_const_string_t name) (rktio_bool_t as_global)))
(define-function/errno
 NULL
 ()
 (ref void)
 rktio_dll_find_object
 (((ref rktio_t) rktio) ((ref rktio_dll_t) dll) (rktio_const_string_t name)))
(define-function/errno
 NULL
 ()
 (ref char)
 rktio_dll_get_error
 (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_set_dll_procs
 ((dll_open_proc dll_open) (dll_find_object_proc dll_find_object)))
(define-function () int rktio_get_last_error_kind (((ref rktio_t) rktio)))
(define-function () int rktio_get_last_error (((ref rktio_t) rktio)))
(define-function () int rktio_get_last_error_step (((ref rktio_t) rktio)))
(define-function
 ()
 void
 rktio_set_last_error
 (((ref rktio_t) rktio) (int kind) (int errid)))
(define-function
 ()
 void
 rktio_set_last_error_step
 (((ref rktio_t) rktio) (int step)))
(define-function () void rktio_remap_last_error (((ref rktio_t) rktio)))
(define-function
 ()
 (ref char)
 rktio_get_last_error_string
 (((ref rktio_t) rktio)))
(define-function
 ()
 (ref char)
 rktio_get_error_string
 (((ref rktio_t) rktio) (int kind) (int errid)))
)
