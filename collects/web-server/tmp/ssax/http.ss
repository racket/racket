; Module header is generated automatically
#cs(module http mzscheme
(require (lib "defmacro.ss"))
(require "common.ss")
(require "myenv.ss")
(require "mime.ss")
(require "srfi-12.ss")
(require "util.ss")
(require (lib "string.ss" "srfi/13"))

;************************************************************************
; 
;		HyperText Transport Protocol (HTTP) support
;
; This code implements the basic flow of a HTTP transaction, as defined in
; a HTTP 1.1 document [RFC 2068]. That is, this code performs:
; 	- opening of an HTTP connection (directly or via a proxy),
; 	- sending of a request,
;	- listening to a reply,
;	- analyzing of the return code,
;	- parsing of the response headers,
;	- dispatching to handle reply's data,
;	- closing of the connection.
;
; INTERFACE
; http-transaction REQ-METHOD REQ-URL REQ-PARMS RESPONSE-HANDLER
;
; REQ-METHOD: a string, typically "GET" or "POST", although many others
; may be allowed. It's up to a particular server to accept or reject
; a request.
;
; REQ-URL: an absolute URL of the HTTP server
;
; REQ-PARMS: an associative list, a list of (name . value) pairs. The list
; may be regarded as "keyword arguments" of the http-transaction
; procedure. The following enumerates the supported "keyword parameters".
; All of them are optional: if omitted or specified with a value #f,
; a suitable default value will be used.
;	http-proxy: a string of the form "proxyname" or "proxyname:proxyport"
;		or (#f  or omitted) if no proxy is needed.
;		Here "proxyname" is the name or the IP address of an HTTP
;		proxy
;	user-agent: a string identifying the user agent
;	http-req: a list or a procedure
;		If it is a list, it should be a list of pairs
;			(http-header-name . http-header-value)
;		for additional HTTP headers to include in the request.
;		If http-req is a procedure, it is invoked with one
;		argument, the communication port to the HTTP server.
;		The procedure is expected to write as many HTTP headers as it
;		wishes, _followed by an empty line_ and optionally the
;		request body.
;	logger: a procedure PORT MESSAGE OTHER-MESSAGES*
;		The procedure is called on several occasions to tell
;		the progress of the transaction
;
; RESPONSE-HANDLER: a procedure RESP-CODE RESP-HEADERS RESP-PORT
;	RESP-CODE is a number, which is one of the HTTP codes, e.g.,
;		200, 304, 404, or 500, etc.
;	RESP-HEADERS: HTTP headers from the server response,
;		a list of pairs (http-header-name . http-header-val).
;		http-header-name is an upper-cased symbol.
;		In addition to the standard header names defined in the
;		HTTP recommendation, a special pair
;		(HTTP-RESPONSE . the-whole-response-line)
;		contains the entire HTTP response line.
;	RESP-PORT: an input port from which to read the body of the reply,
;		if any.
; RESPONSE-HANDLER should close the RESP-PORT. The result of the
; RESPONSE-HANDLER becomes the result of the HTTP transaction.
;
; EXCEPTIONS
; The function http-transaction may abort with the following condition:
;    make-property-condition 'HTTP-TRANSACTION 'REASON reason 'HEADERS headers
; where reason is a symbol:  'NO-REPLY, 'BAD-REQ-URL, 'BAD-RESP-LINE,
; 'headers' is the list of the headers read so far or '(),
; In addition, I/O conditions (such as i/o error, connection-refused, etc.)
; may be raised by the runtime system.
;
; The procedure http-transaction establishes the connection to a HTTP server
; or a proxy, sends the request line and the mandatory headers (Host: and
; Connection:) as well as User-Agent: and other headers as specified in the
; REQ-PARMS. Afterwards, we flush the stream and wait for the reply.
; Upon receiving the reply, we parse the response line, the response
; headers, and then invoke the RESPONSE-HANDLER to handle the rest.
;
; IMPORT
; The standard prelude: myenv.scm or its variations for particular Scheme
; systems.
; Functions declared in files util.scm and mime.scm
; SRFI-12 exception handling SRFI is assumed
; EXPORT
; http-transaction
;
; This code is rather similar to HTTP.cc
;
; See vhttp.scm for the validation tests, which can also serve as
; use cases.
;
; $Id: http.scm,v 2.0 2002/08/23 19:36:25 oleg Exp oleg $
;^^^^^^^^^^^^^^^^^^^^^

;;(include "myenv.scm")
	; The standard prelude and SRFI-12 are assumed
	; See http://pobox.com/~oleg/ftp/Scheme/
	; for myenv.scm and other input parsing functions used
	; in the present code. Again, see vhttp.scm how to run this code

;-------
; A system-dependent part
; Opening, closing and shutting down TCP connections and flushing the
; ports
;   open-tcp-connection hostname::string port-number::int -> (i-port . o-port)
;   flush-output-port port -> void
;   shutdown-sender port -> void  ; shutdown the sending part of the connection
;
; These functions are necessarily platform- and system-specific

(cond-expand
 (gambit
  ; For Gambit 4
  (define (open-tcp-connection host port-number)
    (assert (integer? port-number) (positive? port-number))
    (let ((p (open-tcp-client
              (list server-address: host
                    port-number: port-number))))
      (cons p p)))
  (define flush-output-port force-output)
  (define close-tcp-connection close-port)

  ; DL: by analogue with Gambit 3
  (define shutdown-sender force-output)

; Previous version for Gambit 3
;  ; The Gambit implementation relies on internal Gambit procedures,
;  ; whose names start with ##
;  ; Such identifiers cannot be _read_ on many other systems
;  ; The following macro constructs Gambit-specific ids on the fly
;  (define-macro (_gid id)
;    (string->symbol (string-append "##" (symbol->string id))))
;  (define (open-tcp-connection host port-number)
;    (assert (integer? port-number) (positive? port-number))
;    (let ((io-port ((_gid open-input-output-file)
;		    (string-append "tcp://" host ":" 
;				   (number->string port-number)))))
;      (cons io-port io-port)))
;  (define flush-output-port flush-output)
;  (define shutdown-sender flush-output)
  )
 (bigloo
  (define (open-tcp-connection host port-number)
    (let ((sock (make-client-socket host port-number)))
      (cons (socket-input sock) (socket-output sock))))
  ; flush-output-port is built-in
  (define shutdown-sender close-output-port)
  )
 ((or plt chicken)
  (define (open-tcp-connection host port-number)
    (call-with-values
     (lambda () (tcp-connect host port-number))
     (lambda (input-port output-port)
       (cons input-port output-port))))
  (define flush-output-port flush-output)
  (define shutdown-sender close-output-port)
  )
)
;^^^^^^^


; syntax: define-def ident assoc-list defaultvalue
; Bind a variable ident to a value found in an assoc list.
; assoc-list is a list of pairs (symbol . value)
; We look up 'ident' in the assoc-list, and bind it to the found value, unless
; the latter is #f.
; If the lookup fails, the defaultvalue is used.

(define-macro (define-def ident assoc-list defaultvalue)
  `(define ,ident 
     (or
      (cond
       ((assq ',ident ,assoc-list) => cdr)
       (else #f))
      ,defaultvalue)))

; The body of the function. 
; The function is written as a collection of mutually-recursive
; procedures that implement a transactional FSM.

(define (http-transaction req-method req-url req-parms response-handler)

  ; expected keyword arguments and their default values
  (define-def http-proxy req-parms  #f)
  (define-def user-agent req-parms  "Scheme-HTTP/1.0")
  (define-def http-req   req-parms  '())
  (define-def logger     req-parms
    (lambda (port msg . other-msgs) (cerr msg other-msgs nl)))

  (define CRLF (string (integer->char 13) (integer->char 10)))

  (define (die reason headers port)
    (if port (close-output-port port))
    (abort (make-property-condition 'HTTP-TRANSACTION
	      'REASON reason 'HEADERS headers)))

  ; re-throw the exception exc as a HTTP-TRANSACTION exception
  (define (die-again exc reason headers port)
    (if port (close-output-port port))
    (abort (make-composite-condition
	    (make-property-condition
	     'HTTP-TRANSACTION 'REASON reason 'HEADERS headers)
	    exc)))

  ; Open a connection, send the request, and if successful,
  ; invoke the read-resp-status-line on the opened http-port.
  (define (make-req schema dummy host resource)
    (let* ((target-host (or http-proxy host))
	   (target-addr-lst (string-split target-host '(#\:)))
	   (target-host-proper (car target-addr-lst))
	   (target-port
	    (if (pair? (cdr target-addr-lst))
		(string->integer (cadr target-addr-lst) 0
				 (string-length (cadr target-addr-lst)))
		80))
	   (dummy (logger #f "Connecting to " target-host-proper ":"
			  target-port))
	   ; prevent hacking
	   (dummy (if (string-index target-host-proper #\|)
		      (error "Bad target addr: " target-host-proper)))
	   (http-ports (open-tcp-connection target-host-proper target-port))
	   (http-i-port (car http-ports))
	   (http-o-port (cdr http-ports))
	   )

      (for-each
       (lambda (str) (display str http-o-port))
       `(,req-method " "
		  ; if the proxy is set, request the full REQ-URL; otherwise,
		  ; send only the relative URL
	 ,@(if http-proxy (list req-url) (list "/" resource))
	 " HTTP/1.0" ,CRLF
	 "Host: " ,host ,CRLF
	 "User-agent: " ,user-agent ,CRLF
	 "Connection: close" ,CRLF))
      (if (procedure? http-req)
	  (http-req http-o-port)	; let the user write other headers
	  (begin
	    (for-each (lambda (header-name-value)
			(display (car header-name-value) http-o-port)
			(write-char #\: http-o-port)
			(display (cdr header-name-value) http-o-port)
			(display CRLF http-o-port))
		      http-req)
	    (display CRLF http-o-port) ; An empty line ends headers
	    ))
      (flush-output-port http-o-port)
      (shutdown-sender http-o-port)
      (logger http-o-port "sent request. Now listening for the response...")
      (read-resp-status-line http-i-port)))


  ; Read the first line of the server's response, something like
  ; HTTP/1.x 200 OK
  ; and extract the response code
  ; Invoke
  ;  read-headers http-i-port resp-code
  ;		'(HTTP-RESPONSE . the-whole-response-line)
  ; or raise an exception if the response line is absent or invalid
  (define (read-resp-status-line http-port)
    (let* ((resp-line (read-line http-port))
	   (dummy (logger http-port "Got response :" resp-line)) 
	   (resp-headers (list (cons 'HTTP-RESPONSE resp-line))))
      (cond
       ((eof-object? resp-line)
	(die 'NO-REPLY '() http-port))
       ((not (string-prefix? "HTTP/1." resp-line))
	(die 'BAD-RESP-LINE resp-headers http-port))
       (else
	(let* ((resp-line-parts (string-split resp-line '() 3))
	       (resp-code
		(and (pair? resp-line-parts)
		     (pair? (cdr resp-line-parts))
		     (string->integer (cadr resp-line-parts) 0
				      (string-length (cadr resp-line-parts)))))
	       )
	  (if resp-code
	      (read-headers http-port resp-code resp-headers)
	      (die 'BAD-RESP-LINE resp-headers http-port)))))))


  ; read-headers http-port resp-code init-resp-headers
  ; The http-port is positioned after the response line.
  ; The procedure reads HTTP response headers and adds them to
  ; init-resp-headers.
  ; On success, the procedure exits to response-handler, passing
  ; it the response code, the read headers and the http-port. The
  ; port is positioned after the empty line that terminates the headers.
  (define (read-headers http-port resp-code init-resp-headers)
    (let ((headers
	   (with-exception-handler
            (lambda (exc)
              (die-again exc 'BAD-HEADER init-resp-headers http-port))
            (lambda ()
              (MIME:read-headers http-port)))))
      (response-handler resp-code (append init-resp-headers headers)
			 http-port)))

  ; parse the req-url and exit either to make-req, or to 
  ; the response-handler to handle the error
  (let ((url-parts (string-split req-url '(#\/) 4)))
    ; this stub is added by Dmitry Lizorkin for handling URIs consisting of
    ; just a schema and a host, say, "http://www.plt-scheme.org"
    (let ((url-parts
           (if (and (string=? "http:" (car url-parts))
                    (= 3 (length url-parts)))
               (append url-parts '(""))
               url-parts)))    
    (cond
     ((not (= 4 (length url-parts)))
      (die 'BAD-REQ-URL '() #f))
     ((string=? "http:" (car url-parts))
      (apply make-req url-parts))
     (else
      (die 'UNSUPPORTED-SCHEMA '() #f)
      ))))
)

(provide (all-defined)))
