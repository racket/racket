#lang scribble/doc
@(require "utils.rkt")

@title{Ports and the Filesystem}

Ports are represented as Racket values with the types
@cppi{scheme_input_port_type} and @cppi{scheme_output_port_type}.  The
function @cppi{scheme_read} takes an input port value and returns the
next S-expression from the port.  The function @cppi{scheme_write}
takes an output port and a value and writes the value to the
port. Other standard low-level port functions are also provided, such
as @cppi{scheme_getc}.

File ports are created with @cppi{scheme_make_file_input_port} and
@cppi{scheme_make_file_output_port}; these functions take a @cpp{FILE
*} file pointer and return a Scheme port. Strings are read or written
with @cppi{scheme_make_byte_string_input_port}, which takes a
nul-terminated byte string, and
@cppi{scheme_make_byte_string_output_port}, which takes no arguments.
The contents of a string output port are obtained with
@cppi{scheme_get_byte_string_output}.

Custom ports, with arbitrary read/write handlers, are created with
@cppi{scheme_make_input_port} and @cppi{scheme_make_output_port}.

When opening a file for any reason using a name provided from Racket,
use @cppi{scheme_expand_filename} to normalize the filename and
resolve relative paths.

@function[(Scheme_Object* scheme_read
           [Scheme_Object* port])]{

@racket[read]s the next S-expression from the given input port.}

@function[(void scheme_write
           [Scheme_Object* obj]
           [Scheme_Object* port])]{

@racket[write]s the Scheme value @var{obj} to the given output port.}

@function[(void scheme_write_w_max
           [Scheme_Object* obj]
           [Scheme_Object* port]
           [int n])]{

Like @cpp{scheme_write}, but the printing is truncated to @var{n} bytes.
(If printing is truncated, the last bytes are printed as ``.''.)}

@function[(void scheme_display
           [Scheme_Object* obj]
           [Scheme_Object* port])]{

@racket[display]s the Racket value @var{obj} to the given output
port.}

@function[(void scheme_display_w_max
           [Scheme_Object* obj]
           [Scheme_Object* port]
           [int n])]{

Like @cpp{scheme_display}, but the printing is truncated to @var{n} bytes.
(If printing is truncated, the last three bytes are printed as ``.''.)}


@function[(void scheme_write_byte_string
           [char* str]
           [intptr_t len]
           [Scheme_Object* port])]{

Writes @var{len} bytes of @var{str} to the given output port.}

@function[(void scheme_write_char_string
           [mzchar* str]
           [intptr_t len]
           [Scheme_Object* port])]{

Writes @var{len} characters of @var{str} to the given output port.}


@function[(intptr_t scheme_put_byte_string
           [const-char* who]
           [Scheme_Object* port]
           [char* str]
           [intptr_t d]
           [intptr_t len]
           [int rarely_block])]{

Writes @var{len} bytes of @var{str}, starting with the @var{d}th
character. Bytes are written to the given output port, and errors are
reported as from @var{who}.

If @var{rarely_block} is @cpp{0}, the write blocks until all @var{len}
bytes are written, possibly to an internal buffer. If
@var{rarely_block} is @cpp{2}, the write never blocks, and written
bytes are not buffered. If @var{rarely_block} is @cpp{1}, the write
blocks only until at least one byte is written (without buffering) or
until part of an internal buffer is flushed.

Supplying @cpp{0} for @var{len} corresponds to a buffer-flush
request. If @var{rarely_block} is @cpp{2}, the flush request is
non-blocking, and if @var{rarely_block} is @cpp{0}, it is blocking.
(A @var{rarely_block} of @cpp{1} is the same as @cpp{0} in this case.)

The result is @cpp{-1} if no bytes are written from @var{str} and
unflushed bytes remain in the internal buffer. Otherwise, the return
value is the number of written characters.}

@function[(intptr_t scheme_put_char_string
           [const-char* who]
           [Scheme_Object* port]
           [char* str]
           [intptr_t d]
           [intptr_t len])]{

Like @cpp{scheme_put_byte_string}, but for a @cpp{mzchar} string, and
without the non-blocking option.}

@function[(char* scheme_write_to_string
           [Scheme_Object* obj]
           [intptr_t* len])]{

Prints the Racket value @var{obj} using @racket[write] to a newly
allocated string. If @var{len} is not @cpp{NULL}, @cpp{*@var{len}} is
set to the length of the bytes string.}

@function[(void scheme_write_to_string_w_max
           [Scheme_Object* obj]
           [intptr_t* len]
           [int n])]{

Like @cpp{scheme_write_to_string}, but the string is truncated to
@var{n} bytes.  (If the string is truncated, the last three bytes are
``.''.)}

@function[(char* scheme_display_to_string
           [Scheme_Object* obj]
           [intptr_t* len])]{

Prints the Racket value @var{obj} using @racket[display] to a newly
allocated string. If @var{len} is not @cpp{NULL}, @cpp{*@var{len}} is
set to the length of the string.}


@function[(void scheme_display_to_string_w_max
           [Scheme_Object* obj]
           [intptr_t* len]
           [int n])]{

Like @cpp{scheme_display_to_string}, but the string is truncated to
@var{n} bytes.  (If the string is truncated, the last three bytes are
``.''.)}


@function[(void scheme_debug_print
           [Scheme_Object* obj])]{

Prints the Racket value @var{obj} using @racket[write] to the main
thread's output port.}

@function[(void scheme_flush_output
           [Scheme_Object* port])]{

If @var{port} is a file port, a buffered data is written to the file.
Otherwise, there is no effect. @var{port} must be an output port.}

@function[(int scheme_get_byte
           [Scheme_Object* port])]{

Get the next byte from the given input port. The result can be @cpp{EOF}.}

@function[(int scheme_getc
           [Scheme_Object* port])]{

Get the next character from the given input port (by decoding bytes as UTF-8).
  The result can be @cpp{EOF}.}

@function[(int scheme_peek_byte
           [Scheme_Object* port])]{

Peeks the next byte from the given input port.  The result can be @cpp{EOF}.}

@function[(int scheme_peekc
           [Scheme_Object* port])]{

Peeks the next character from the given input port (by decoding bytes as UTF-8).
  The result can be @cpp{EOF}.}

@function[(int scheme_peek_byte_skip
           [Scheme_Object* port]
           [Scheme_Object* skip])]{

Like @cpp{scheme_peek_byte}, but with a skip count.  The result can be @cpp{EOF}.}

@function[(int scheme_peekc_skip
           [Scheme_Object* port]
           [Scheme_Object* skip])]{

Like @cpp{scheme_peekc}, but with a skip count.  The result can be @cpp{EOF}.}


@function[(intptr_t scheme_get_byte_string
           [const-char* who]
           [Scheme_Object* port]
           [char* buffer]
           [int offset]
           [intptr_t size]
           [int only_avail]
           [int peek]
           [Scheme_Object* peek_skip])]{

Gets multiple bytes at once from a port, reporting errors with the
name @var{who}. The @var{size} argument indicates the number of
requested bytes, to be put into the @var{buffer} array starting at
@var{offset}.  The return value is the number of bytes actually read,
or @cpp{EOF} if an end-of-file is encountered without reading any
bytes.

If @var{only_avail} is @cpp{0}, then the function blocks until
@var{size} bytes are read or an end-of-file is reached. If
@var{only_avail} is @cpp{1}, the function blocks only until at least
one byte is read. If @var{only_avail} is @cpp{2}, the function never
blocks. If @var{only_avail} is @cpp{-1}, the function blocks only
until at least one byte is read but also allows breaks (with the
guarantee that bytes are read or a break is raised, but not both).

If @var{peek} is non-zero, then the port is peeked instead of
read. The @var{peek_skip} argument indicates a portion of the input
stream to skip as a non-negative, exact integer (fixnum or bignum). In
this case, an @var{only_avail} value of @cpp{1} means to continue the
skip until at least one byte can be returned, even if it means
multiple blocking reads to skip bytes.

If @var{peek} is zero, then @var{peek_skip} should be either
@cpp{NULL} (which means zero) or the fixnum zero.}

@function[(intptr_t scheme_get_char_string
           [const-char* who]
           [Scheme_Object* port]
           [char* buffer]
           [int offset]
           [intptr_t size]
           [int peek]
           [Scheme_Object* peek_skip])]{

Like @cpp{scheme_get_byte_string}, but for characters (by decoding
bytes as UTF-8), and without the non-blocking option.}


@function[(intptr_t scheme_get_bytes
           [Scheme_Object* port]
           [intptr_t size]
           [char* buffer]
           [int offset])]{

For backward compatibility: calls @cpp{scheme_get_byte_string} in
essentially the obvious way with @var{only_avail} as @cpp{0}; if
@var{size} is negative, then it reads @var{-size} bytes with
@var{only_avail} as @cpp{1}.}

@function[(void scheme_ungetc
           [int ch]
           [Scheme_Object* port])]{

Puts the byte @var{ch} back as the next character to be read from the
given input port. The character need not have been read from
@var{port}, and @cpp{scheme_ungetc} can be called to insert up to five
characters at the start of @var{port}.

Use @cpp{scheme_get_byte} followed by @cpp{scheme_ungetc} only when
your program will certainly call @cpp{scheme_get_byte} again to
consume the byte. Otherwise, use @cpp{scheme_peek_byte}, because some
a port may implement peeking and getting differently.}

@function[(int scheme_byte_ready
           [Scheme_Object* port])]{

Returns 1 if a call to @cpp{scheme_get_byte} is guaranteed not to
block for the given input port.}

@function[(int scheme_char_ready
           [Scheme_Object* port])]{

Returns 1 if a call to @cpp{scheme_getc} is guaranteed not to block
for the given input port.}

@function[(void scheme_need_wakeup
           [Scheme_Object* port]
           [void* fds])]{

Requests that appropriate bits are set in @var{fds} to specify which
file descriptors(s) the given input port reads from. (@var{fds} is
sortof a pointer to an @cppi{fd_set} struct; see
@secref["blockednonmainel"].)}

@function[(intptr_t scheme_tell
           [Scheme_Object* port])]{

Returns the current read position of the given input port, or the
 current file position of the given output port.}

@function[(intptr_t scheme_tell_line
           [Scheme_Object* port])]{

Returns the current read line of the given input port. If lines are
not counted, -1 is returned.}

@function[(void scheme_count_lines
           [Scheme_Object* port])]{

Turns on line-counting for the given input port. To get accurate line
counts, call this function immediately after creating a port.}

@function[(intptr_t scheme_set_file_position
           [Scheme_Object* port]
           [intptr_t pos])]{

Sets the file position of the given input or output port (from the
start of the file). If the port does not support position setting, an
exception is raised.}

@function[(void scheme_close_input_port
           [Scheme_Object* port])]{

Closes the given input port.}

@function[(void scheme_close_output_port
           [Scheme_Object* port])]{

Closes the given output port.}

@function[(int scheme_get_port_file_descriptor
           [Scheme_Object* port]
           [intptr_t* fd])]{

Fills @cpp{*@var{fd}} with a file-descriptor value for @var{port} if
one is available (i.e., the port is a file-stream port and it is not
closed). The result is non-zero if the file-descriptor value is
available, zero otherwise. On Windows, a ``file dscriptor'' is a
file @cpp{HANDLE}.}

@function[(intptr_t scheme_get_port_fd
           [Scheme_Object* port])]{

Like @cpp{scheme_get_port_file_descriptor}, but a file
 descriptor or @cpp{HANDLE} is returned directly, and the result is
 @cpp{-1} if no file descriptor or @cpp{HANDLE} is available.}

@function[(intptr_t scheme_get_port_socket
           [Scheme_Object* port]
           [intptr_t* s])]{

Fills @cpp{*@var{s}} with a socket value for @var{port} if one is
available (i.e., the port is a TCP port and it is not closed). The
result is non-zero if the socket value is available, zero
otherwise. On Windows, a socket value has type @cpp{SOCKET}.}

@function[(Scheme_Object* scheme_make_port_type
           [char* name])]{

Creates a new port subtype.}

@function[(Scheme_Input_Port* scheme_make_input_port
           [Scheme_Object* subtype]
           [void* data]
           [Scheme_Object* name]
           [Scheme_Get_String_Fun get_bytes_fun]
           [Scheme_Peek_String_Fun peek_bytes_fun]
           [Scheme_Progress_Evt_Fun progress_evt_fun]
           [Scheme_Peeked_Read_Fun peeked_read_fun]
           [Scheme_In_Ready_Fun char_ready_fun]
           [Scheme_Close_Input_Fun close_fun]
           [Scheme_Need_Wakeup_Input_Fun need_wakeup_fun]
           [int must_close])]{

Creates a new input port with arbitrary control functions. The
@var{subtype} is an arbitrary value to distinguish the port's class.
The pointer @var{data} will be installed as the port's user data,
which can be extracted/set with the @cppi{SCHEME_INPORT_VAL} macro.
The @var{name} object is used as the port's name (for
@racket[object-name] and as the default source name for
@racket[read-syntax]).

If @var{must_close} is non-zero, the new port will be registered with
the current custodian, and @var{close_fun} is guaranteed to be called
before the port is garbage-collected.

Although the return type of @cpp{scheme_make_input_port} is
@cppi{Scheme_Input_Port*}, it can be cast into a @cpp{Scheme_Object*}.

The functions are as follows.

 @subfunction[(intptr_t get_bytes_fun
               [Scheme_Input_Port* port]
               [char* buffer]
               [intptr_t offset]
               [intptr_t size]
               [int nonblock]
               [Scheme_Object* unless])]{
 
    Reads bytes into @var{buffer}, starting from @var{offset}, up to
    @var{size} bytes (i.e., @var{buffer} is at least
    @var{offset} plus @var{size} long). If @var{nonblock} is @cpp{0},
    then the function can block indefinitely, but it should return
    when at least one byte of data is available. If @var{nonblock} is
    @cpp{1}, the function should never block. If @var{nonblock} is
    @cpp{2}, a port in unbuffered mode should return only bytes
    previously forced to be buffered; other ports should treat a
    @var{nonblock} of @cpp{2} like @cpp{1}. If @var{nonblock} is
    @cpp{-1}, the function can block, but should enable breaks while
    blocking. The function should return @cpp{0} if no bytes are ready
    in non-blocking mode. It should return @cpp{EOF} if an end-of-file
    is reached (and no bytes were read into @var{buffer}). Otherwise,
    the function should return the number of read bytes. The function
    can raise an exception to report an error.

    The @var{unless} argument will be non-@cpp{NULL} only when
    @var{nonblocking} is non-zero (except as noted below), and only if
    the port supports progress events. If @var{unless} is
    non-@cpp{NULL} and @cpp{SCHEME_CDR(@var{unless})} is
    non-@cpp{NULL}, the latter is a progress event specific to the
    port. The @var{get_bytes_fun} function should return
    @cppi{SCHEME_UNLESS_READY} instead of reading bytes if the event
    in @var{unless} becomes ready before bytes can be read. In
    particular, @var{get_bytes_fun} should check the event in
    @var{unless} before taking any action, and it should check the
    event in @var{unless} after any operation that may allow Racket
    thread swaps. If the read must block, then it should unblock if
    the event in @var{unless} becomes ready.

    If @cpp{scheme_progress_evt_via_get} is used for
    @var{progress_evt_fun}, then @var{unless} can be non-@cpp{NULL}
    even when @var{nonblocking} is @cpp{0}. In all modes,
    @var{get_bytes_fun} must call @cpp{scheme_unless_ready} to check
    @var{unless_evt}.  Furthermore, after any potentially
    thread-swapping operation, @var{get_bytes_fun} must call
    @cpp{scheme_wait_input_allowed}, because another thread may be
    attempting to commit, and @var{unless_evt} must be checked after
    @cpp{scheme_wait_input_allowed} returns. To block, the port should
    use @cpp{scheme_block_until_unless} instead of
    @cpp{scheme_block_until}.  Finally, in blocking mode,
    @var{get_bytes_fun} must return after immediately reading data,
    without allowing a Racket thread swap.}

 @subfunction[(intptr_t peek_bytes_fun
               [Scheme_Input_Port* port]
               [char* buffer]
               [intptr_t offset]
               [intptr_t size]
               [Scheme_Object* skip]
               [int nonblock]
               [Scheme_Object* unless_evt])]{

    Can be @cpp{NULL} to use a default implementation of peeking that
    uses @var{get_bytes_fun}. Otherwise, the protocol is the same as
    for @var{get_bytes_fun}, except that an extra @var{skip} argument
    indicates the number of input elements to skip (but @var{skip}
    does not apply to @var{buffer}). The @var{skip} value will be a
    non-negative exact integer, either a fixnum or a bignum.}

 @subfunction[(Scheme_Object* progress_evt_fun
               [Scheme_Input_Port* port])]{

    Called to obtain a progress event for the port, such as for
    @racket[port-progress-evt]. This function can be @cpp{NULL} if the
    port does not support progress events. Use
    @cpp{scheme_progress_evt_via_get} to obtain a default implementation, in
    which case @var{peeked_read_fun} should be
    @cpp{scheme_peeked_read_via_get}, and @var{get_bytes_fun} and
    @var{peek_bytes_fun} should handle @var{unless} as described
    above.}

 @subfunction[(int peeked_read_fun
               [Scheme_Input_Port* port]
               [intptr_t amount]
               [Scheme_Object* unless_evt]
               [Scheme_Object* target_ch])]{

    Called to commit previously peeked bytes, just like the sixth
    argument to @racket[make-input-port]. Use
    @cpp{scheme_peeked_read_via_get} for the default implementation of
    commits when @var{progress_evt_fun} is
    @cpp{scheme_progress_evt_via_get}.

    The @var{peeked_read_fun} function must call
    @cpp{scheme_port_count_lines} on a successful commit to adjust the
    port's position. If line counting is enabled for the port and if
    line counting uses the default implementation,
    @var{peeked_read_fun} should supply a non-@cpp{NULL} byte-string
    argument to @cpp{scheme_port_count_lines}, so that character and
    line counts can be tracked correctly.}

 @subfunction[(int char_ready_fun
               [Scheme_Input_Port* port])]{

    Returns @cpp{1} when a non-blocking @var{get_bytes_fun} will
    return bytes or an @cpp{EOF}.}

 @subfunction[(void close_fun
               [Scheme_Input_Port* port])]{

    Called to close the port. The port is not considered closed until
    the function returns.}

 @subfunction[(void need_wakeup_fun
               [Scheme_Input_Port* port]
               [void* fds])]{

    Called when the port is blocked on a read; @var{need_wakeup_fun}
    should set appropriate bits in @var{fds} to specify which file
    descriptor(s) it is blocked on. The @var{fds} argument is
    conceptually an array of three @cppi{fd_set} structs (one for
    read, one for write, one for exceptions), but manipulate this
    array using @cppi{scheme_get_fdset} to get a particular element of
    the array, and use @cppi{MZ_FD_XXX} instead of @cpp{FD_XXX} to
    manipulate a single ``@cpp{fd_set}''. On Windows, the first
    ``@cpp{fd_set}'' can also contain OS-level semaphores or other
    handles via @cpp{scheme_add_fd_handle}.}
}

@function[(Scheme_Output_Port* scheme_make_output_port
           [Scheme_Object* subtype]
           [void* data]
           [Scheme_Object* name]
           [Scheme_Write_String_Evt_Fun write_bytes_evt_fun]
           [Scheme_Write_String_Fun write_bytes_fun]
           [Scheme_Out_Ready_Fun char_ready_fun]
           [Scheme_Close_Output_Fun close_fun]
           [Scheme_Need_Wakeup_Output_Fun need_wakeup_fun]
           [Scheme_Write_Special_Evt_Fun write_special_evt_fun]
           [Scheme_Write_Special_Fun write_special_fun]
           [int must_close])]{

Creates a new output port with arbitrary control functions.  The
@var{subtype} is an arbitrary value to distinguish the port's class.
The pointer @var{data} will be installed as the port's user data,
which can be extracted/set with the @cppi{SCHEME_OUTPORT_VAL}
macro. The @var{name} object is used as the port's name.

If @var{must_close} is non-zero, the new port will be registered with
the current custodian, and @var{close_fun} is guaranteed to be called
before the port is garbage-collected.

Although the return type of @cpp{scheme_make_output_port} is
@cppi{Scheme_Output_Port*}, it can be cast into a
@cpp{Scheme_Object*}.

The functions are as follows.

 @subfunction[(intptr_t write_bytes_evt_fun
               [Scheme_Output_Port* port]
               [const-char* buffer]
               [intptr_t offset]
               [intptr_t size])]{

    Returns an event that writes up to @var{size} bytes atomically
    when event is chosen in a synchronization. Supply @cpp{NULL} if
    bytes cannot be written atomically, or supply
    @cppi{scheme_write_evt_via_write} to use the default
    implementation in terms of @cpp{write_bytes_fun} (with
    @var{rarely_block} as @cpp{2}).}

 @subfunction[(intptr_t write_bytes_fun
               [Scheme_Output_Port* port]
               [const-char* buffer]
               [intptr_t offset]
               [intptr_t size]
               [int rarely_block]
               [int enable_break])]{

    Write bytes from @var{buffer}, starting from @var{offset}, up to
    @var{size} bytes (i.e., @var{buffer} is at least
    @var{offset} plus @var{size} long). If @var{rarely_block} is @cpp{0},
    then the function can block indefinitely, and it can buffer
    output. If @var{rarely_block} is @cpp{2}, the function should
    never block, and it should not buffer output. If
    @var{rarely_block} is @cpp{1}, the function should not buffer
    data, and it should block only until writing at least one byte,
    either from @var{buffer} or an internal buffer. The function
    should return the number of bytes from @var{buffer} that were
    written; when @var{rarely_block} is non-zero and bytes remain in
    an internal buffer, it should return @cpp{-1}. The @var{size}
    argument can be @cpp{0} when @var{rarely_block} is @cpp{0} for a
    blocking flush, and it can be @cpp{0} if @var{rarely_block} is
    @cpp{2} for a non-blocking flush.  If @var{enable_break} is true,
    then it should enable breaks while blocking. The function can
    raise an exception to report an error.}

 @subfunction[(int char_ready_fun
               [Scheme_Output_Port* port])]{

    Returns @cpp{1} when a non-blocking @var{write_bytes_fun} will
    write at least one byte or flush at least one byte from
    the port's internal buffer.}

  @subfunction[(void close_fun
                [Scheme_Output_Port* port])]{
 
    Called to close the port. The port is not considered closed until
    the function returns. This function is allowed to block (usually
    to flush a buffer) unless
    @cpp{scheme_close_should_force_port_closed} returns a non-zero
    result, in which case the function must return without blocking.}

 @subfunction[(void need_wakeup_fun
               [Scheme_Output_Port* port]
               [void* fds])]{

    Called when the port is blocked on a write; @var{need_wakeup_fun}
    should set appropriate bits in @var{fds} to specify which file
    descriptor(s) it is blocked on. The @var{fds} argument is
    conceptually an array of three @cppi{fd_set} structs (one for
    read, one for write, one for exceptions), but manipulate this
    array using @cppi{scheme_get_fdset} to get a particular element of
    the array, and use @cppi{MZ_FD_XXX} instead of @cpp{FD_XXX} to
    manipulate a single ``@cpp{fd_set}''. On Windows, the first
    ``@cpp{fd_set}'' can also contain OS-level semaphores or other
    handles via @cpp{scheme_add_fd_handle}.}

 @subfunction[(int write_special_evt_fun
               [Scheme_Output_Port* port]
               [Scheme_Object* v])]{

    Returns an event that writes @var{v} atomically when event is
    chosen in a synchronization. Supply @cpp{NULL} if specials cannot
    be written atomically (or at all), or supply
    @cppi{scheme_write_special_evt_via_write_special} to use the
    default implementation in terms of @cpp{write_special_fun} (with
    @var{non_block} as @cpp{1}).}

 @subfunction[(int write_special_fun
               [Scheme_Output_Port* port]
               [Scheme_Object* v]
               [int non_block])]{

    Called to write the special value @var{v} for
    @racket[write-special] (when @var{non_block} is @cpp{0}) or
    @racket[write-special-avail*] (when @var{non_block} is
    @cpp{1}). If @cpp{NULL} is supplied instead of a function pointer,
    then @racket[write-special] and @racket[write-special-avail*]
    produce an error for this port.}

}

@function[(void scheme_set_port_location_fun [Scheme_Port* port]
					     [Scheme_Location_Fun location_fun])]{

Sets the implementation of @racket[port-next-location] for @var{port},
which is used when line counting is enabled for @var{port}.

 @subfunction[(Scheme_Object* location_fun
               [Scheme_Port* port])]{
   Returns three values: a positive exact integer or @racket[#f] for a line number,
   a non-negative exact integer or @racket[#f] for a column (which must be @racket[#f]
   if and only if the line number is @racket[#f]), and
   a positive exact integer or @racket[#f] for a character position.
 }
}

@function[(void scheme_set_port_count_lines_fun [Scheme_Port* port]
					        [Scheme_Count_Lines_Fun count_lines_fun])]{

Installs a notification callback that is invoked if line counting is subsequently
enabled for @var{port}.

 @subfunction[(void count_lines_fun
               [Scheme_Port* port])]
}

@function[(void scheme_port_count_lines [Scheme_Port* port]
                                        [const-char* buffer]
                                        [intptr_t offset]
                                        [intptr_t got])]{

Updates the position of @var{port} as reported by
@racket[file-position] as well as the locations reported by
@racket[port-next-location] when the default implement of character
and line counting is used. This function is intended for use by a
peek-commit implementation in an input port.

The @var{got} argument indicates the number of bytes read from or
written to @var{port}. The @var{buffer} argument is used only when
line counting is enabled, and it represents specific bytes read or
written for the purposes of character and line coutning. The
@var{buffer} argument can be @cpp{NULL}, in which case @var{got}
non-newline characters are assumed. The @var{offset} argument
indicates a starting offset into @var{buffer}, so @racket{buffer} must
be at least @var{offset} plus @var{got} bytes long.}


@function[(Scheme_Object* scheme_make_file_input_port
           [FILE* fp])]{

Creates a Scheme input file port from an ANSI C file pointer. The file
 must never block on reads.}

@function[(Scheme_Object* scheme_open_input_file
           [const-char* filename]
           [const-char* who])]{

Opens @var{filename} for reading. In an exception is raised, the
 exception message uses @var{who} as the name of procedure that raised
 the exception.}

@function[(Scheme_Object* scheme_make_named_file_input_port
           [FILE* fp]
           [Scheme_Object* name])]{

Creates a Racket input file port from an ANSI C file pointer. The file
 must never block on reads. The @var{name} argument is used as the
 port's name.}

@function[(Scheme_Object* scheme_open_output_file
           [const-char* filename]
           [const-char* who])]{

Opens @var{filename} for writing in @racket['truncate/replace] mode. If
 an exception is raised, the exception message uses @var{who} as the
 name of procedure that raised the exception.}

@function[(Scheme_Object* scheme_make_file_output_port
           [FILE* fp])]{

Creates a Racket output file port from an ANSI C file pointer. The
 file must never block on writes.}

@function[(Scheme_Object* scheme_make_fd_input_port
           [int fd]
           [Scheme_Object* name]
           [int regfile]
           [int win_textmode])]{

Creates a Racket input port for a file descriptor @var{fd}. On
 Windows, @var{fd} can be a @cpp{HANDLE} for a stream, and it should
 never be a file descriptor from the C library or a WinSock socket.

The @var{name} object is used for the port's name. Specify a non-zero
 value for @var{regfile} only if the file descriptor corresponds to a
 regular file (which implies that reading never blocks, for example).

On Windows, @var{win_textmode} can be non-zero to make trigger
 auto-conversion (at the byte level) of CRLF combinations to LF.

Closing the resulting port closes the file descriptor.

Instead of calling both @cpp{scheme_make_fd_input_port} and
 @cpp{scheme_make_fd_output_port} on the same file descriptor, call
 @cpp{scheme_make_fd_output_port} with a non-zero last
 argument. Otherwise, closing one of the ports causes the file
 descriptor used by the other to be closed as well.}

@function[(Scheme_Object* scheme_make_fd_output_port
           [int fd]
           [Scheme_Object* name]
           [int regfile]
           [int win_textmode]
           [int read_too])]{

Creates a Racket output port for a file descriptor @var{fd}. On
 Windows, @var{fd} can be a @cpp{HANDLE} for a stream, and it should
 never be a file descriptor from the C library or a WinSock socket.

The @var{name} object is used for the port's name. Specify a non-zero
 value for @var{regfile} only if the file descriptor corresponds to a
 regular file (which implies that reading never blocks, for example).

On Windows, @var{win_textmode} can be non-zero to make trigger
 auto-conversion (at the byte level) of CRLF combinations to LF.

Closing the resulting port closes the file descriptor.

If @var{read_too} is non-zero, the function produces multiple values
 (see @secref["multiple"]) instead of a single port. The first result is
 an input port for @var{fd}, and the second is an output port for
 @var{fd}. These ports are connected in that the file descriptor is
 closed only when both of the ports are closed.}


@function[(void scheme_socket_to_ports
           [intptr_t s]
           [const-char* name]
           [int close]
           [Scheme_Object** inp]
           [Scheme_Object** outp])]{

Creates Racket input and output ports for a TCP socket @var{s}. The
 @var{name} argument supplies the name for the ports. If @var{close}
 is non-zero, then the ports assume responsibility for closing the
 socket. The resulting ports are written to @var{inp} and @var{outp}.}

@function[(Scheme_Object* scheme_make_byte_string_input_port
           [char* str])]{

Creates a Racket input port from a byte string; successive
 @racket[read-char]s on the port return successive bytes in the
 string.}

@function[(Scheme_Object* scheme_make_byte_string_output_port)]{

Creates a Racket output port; all writes to the port are kept in a byte string,
 which can be obtained with @cpp{scheme_get_byte_string_output}.}

@function[(char* scheme_get_byte_string_output
           [Scheme_Object* port])]{

Returns (in a newly allocated byte string) all data that has been
 written to the given string output port so far. (The returned string
 is nul-terminated.)}

@function[(char* scheme_get_sized_byte_string_output
           [Scheme_Object* port]
           [intptr_t* len])]{

Returns (in a newly allocated byte string) all data that has been
 written to the given string output port so far and fills in
 @cpp{*len} with the length of the string in bytes (not including the
 nul terminator).}

@function[(void scheme_pipe
           [Scheme_Object** read]
           [Scheme_Object** write])]{

Creates a pair of ports, setting @cpp{*@var{read}} and
 @cpp{*@var{write}}; data written to @cpp{*@var{write}} can be read
 back out of @cpp{*@var{read}}.  The pipe can store arbitrarily many
 unread characters,}

@function[(void scheme_pipe_with_limit
           [Scheme_Object** read]
           [Scheme_Object** write]
           [int limit])]{

Like @cpp{scheme_pipe} if @var{limit} is @cpp{0}. If @var{limit} is
 positive, creates a pipe that stores at most @var{limit} unread
 characters, blocking writes when the pipe is full.}

@function[(Scheme_Input_Port* scheme_input_port_record
           [Scheme_Object* port])]{

Returns the input-port record for @var{port}, which may be either a
raw-port object with type @cpp{scheme_input_port_type} or a structure
with the @racket[prop:input-port] property.}

@function[(Scheme_Output_Port* scheme_output_port_record
           [Scheme_Object* port])]{

Returns the output-port record for @var{port}, which may be either a
raw-port object with type @cpp{scheme_output_port_type} or a structure
with the @racket[prop:output-port] property.}

@function[(int scheme_file_exists
           [char* name])]{

Returns 1 if a file by the given name exists, 0 otherwise. If
@var{name} specifies a directory, FALSE is returned.
The @var{name} should be already expanded.}

@function[(int scheme_directory_exists
           [char* name])]{

Returns 1 if a directory by the given name exists, 0 otherwise.  The
@var{name} should be already expanded.}

@function[(char* scheme_expand_filename
           [const-char* name]
           [int len]
           [const-char* where]
           [int* expanded]
           [int checks])]{

Cleanses the pathname @var{name} (see @racket[cleanse-path]) and
resolves relative paths with respect to the current directory
parameter. The @var{len} argument is the length of the input string;
if it is -1, the string is assumed to be null-terminated.  The
@var{where} argument is used to raise an exception if there is an
error in the filename; if this is @cpp{NULL}, an error is not reported
and @cpp{NULL} is returned instead.  If @var{expanded} is not
@cpp{NULL}, *@var{expanded} is set to 1 if some expansion takes place,
or 0 if the input name is simply returned.

If @var{guards} is not @cpp{0}, then @cpp{scheme_security_check_file}
(see @secref["security"]) is called with @var{name}, @var{where}, and
@var{checks} (which implies that @var{where} should never be
@cpp{NULL} unless @var{guards} is @cpp{0}). Normally, @var{guards}
should be @cpp{SCHEME_GUARD_FILE_EXISTS} at a minimum. Note that a
failed access check will result in an exception.}

@function[(char* scheme_expand_string_filename
           [Scheme_Object* name]
           [const-char* where]
           [int* expanded]
           [int checks])]{

Like @cpp{scheme_expand_string}, but given a @var{name} that can be a
character string or a path value.}

@function[(Scheme_Object* scheme_char_string_to_path
           [Scheme_Object* s])]{

Converts a Racket character string into a Racket path value.}

@function[(Scheme_Object* scheme_path_to_char_string
           [Scheme_Object* s])]{

Converts a Racket path value into a Racket character string.}

@function[(Scheme_Object* scheme_make_path
           [char* bytes])]{

Makes a path value given a byte string. The @var{bytes} string is copied.}

@function[(Scheme_Object* scheme_make_path_without_copying
           [char* bytes])]{

Like @cpp{scheme_make_path}, but the string is not copied.}

@function[(Scheme_Object* scheme_make_sized_path
           [char* bytes]
           [intptr_t len]
           [int copy])]{

Makes a path whose byte form has size @var{len}. A copy of @var{bytes}
is made if @var{copy} is not 0. The string @var{bytes} should contain
@var{len} bytes, and if @var{copy} is zero, @var{bytes} must have a
nul terminator in addition. If @var{len} is negative, then the
nul-terminated length of @var{bytes} is used for the length.}

@function[(Scheme_Object* scheme_make_sized_offset_path
           [char* bytes]
           [intptr_t d]
           [intptr_t len]
           [int copy])]{

Like @cpp{scheme_make_sized_path}, except the @var{len} bytes start
from position @var{d} in @var{bytes}. If @var{d} is non-zero, then
@var{copy} must be non-zero.}

@function[(char* scheme_build_mac_filename
           [FSSpec* spec]
           [int isdir])]{

Mac OS X only: Converts an @cppi{FSSpec} record (defined by Mac OS X)
into a pathname string. If @var{spec} contains only directory
information (via the @cpp{vRefNum} and @cpp{parID} fields),
@var{isdir} should be @cpp{1}, otherwise it should be @cpp{0}.}

@function[(int scheme_mac_path_to_spec
           [const-char* filename]
           [FSSpec* spec]
           [intptr_t* type])]{

Mac OS X only: Converts a pathname into an @cppi{FSSpec} record
(defined by Mac OS X), returning @cpp{1} if successful and @cpp{0}
otherwise. If @var{type} is not @cpp{NULL} and @var{filename} is a
file that exists, @var{type} is filled with the file's four-character
Mac OS X type. If @var{type} is not @cpp{NULL} and @var{filename} is
not a file that exists, @var{type} is filled with @cpp{0}.}

@function[(char* scheme_os_getcwd
           [char* buf]
           [int buflen]
           [int* actlen]
           [int noexn])]{

Gets the @as-index{current working directory} according to the
operating system. This is separate from Racket's current directory
parameter.

The directory path is written into @var{buf}, of length @var{buflen},
if it fits. Otherwise, a new (collectable) string is allocated for the
directory path. If @var{actlen} is not @cpp{NULL}, *@var{actlen} is
set to the length of the current directory path. If @var{noexn} is
no 0, then an exception is raised if the operation fails.}

@function[(int scheme_os_setcwd
           [char* buf]
           [int noexn])]{

Sets the current working directory according to the operating system. This
is separate from Racket's current directory parameter.

If @var{noexn} is not 0, then an exception is raised if the operation
fails.}

@function[(char* scheme_format
           [mzchar* format]
           [int flen]
           [int argc]
           [Scheme_Object** argv]
           [intptr_t* rlen])]{

Creates a string like Racket's @racket[format] procedure, using the
format string @var{format} (of length @var{flen}) and the extra
arguments specified in @var{argc} and @var{argv}. If @var{rlen} is not
@cpp{NULL}, @cpp{*@var{rlen}} is filled with the length of the
resulting string.}

@function[(void scheme_printf
           [char* format]
           [int flen]
           [int argc]
           [Scheme_Object** argv])]{

Writes to the current output port like Racket's @racket[printf]
procedure, using the format string @var{format} (of length @var{flen})
and the extra arguments specified in @var{argc} and @var{argv}.}

@function[(char* scheme_format_utf8
           [char* format]
           [int flen]
           [int argc]
           [Scheme_Object** argv]
           [intptr_t* rlen])]{

Like @cpp{scheme_format}, but takes a UTF-8-encoding byte string.}

@function[(void scheme_printf_utf8
           [char* format]
           [int flen]
           [int argc]
           [Scheme_Object** argv])]{

Like @cpp{scheme_printf}, but takes a UTF-8-encoding byte string.}

@function[(int scheme_close_should_force_port_closed)]{

This function must be called by the close function for a port created
 with @cpp{scheme_make_output_port}.}
