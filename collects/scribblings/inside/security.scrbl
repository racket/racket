#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "security"]{Security Guards}

Before a primitive procedure accesses the filesystem or creates a
network connection, it should first consult the current security guard
to determine whether such access is allowed for the current thread.

File access is normally preceded by a call to
@cppi{scheme_expand_filename}, which accepts flags to indicate the
kind of filesystem access needed, so that the security guard is
consulted automatically.

An explicit filesystem-access check can be made by calling
@cpp{scheme_security_check_file}. Similarly, an explicit
network-access check is performed by calling
@cpp{scheme_security_check_network}.

@; ----------------------------------------------------------------------


@function[(void scheme_security_check_file
           [const-char* who]
           [char* filename]
           [int guards])]{

Consults the current security manager to determine whether access is
allowed to @var{filename}. The @var{guards} argument should be a
bitwise combination of the following:

@itemize[

 @item{@cppi{SCHEME_GUARD_FILE_READ}}
 @item{@cppi{SCHEME_GUARD_FILE_WRITE}}
 @item{@cppi{SCHEME_GUARD_FILE_EXECUTE}}
 @item{@cppi{SCHEME_GUARD_FILE_DELETE}}
 @item{@cppi{SCHEME_GUARD_FILE_EXISTS} (do not combine with other values)}

]

The @var{filename} argument can be @cpp{NULL} (in which case
@racket[#f] is sent to the security manager's procedure), and
@var{guards} should be @cppi{SCHEME_GUARD_FILE_EXISTS} in that case.

If access is denied, an exception is raised.}


@function[(void scheme_security_check_network
           [const-char* who]
           [char* host]
           [int portno])]{

Consults the current security manager to determine whether access is
 allowed for creating a client connection to @var{host} on port number
 @var{portno}. If @var{host} is @cpp{NULL}, the security manager is
 consulted for creating a server at port number @var{portno}.

If access is denied, an exception is raised.}

