#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         racket/tcp
         racket/port)

;; A native Win32 implementation of SSL ports, which can be useful if
;; the openssl library is not available (perhaps because the openssl
;; library is going to be downloaded and installed via HTTPS). Various
;; options, including certificate checking, are not currently supported.

(provide win32-ssl-connect
         win32-ssl-abandon-port
         ports->win32-ssl-ports
         win32-ssl-port?
         win32-ssl-available?)

(define (win32-ssl-connect host port [protocol 'auto])
  (define-values (i o) (tcp-connect host port))
  (ports->win32-ssl-ports i o #:encrypt protocol #:hostname host))

(define (win32-ssl-abandon-port port)
  ;; We don't try to implement shutdown, anyway
  (if (input-port? port)
      (close-input-port port)
      (close-output-port port)))

;; ----------------------------------------
;; Win32 bindings

(define secur32-lib (and (eq? 'windows (system-type))
                         (ffi-lib "secur32.dll")))

(define win32-ssl-available? (and secur32-lib #t))

(define-ffi-definer define-secur32 secur32-lib
  #:default-make-fail make-not-available)

(define _LONG _long)
(define _ULONG _ulong)
(define _DWORD _int32)

(define-cstruct _cred-handle ([a _intptr] [b _intptr]))
(define-cstruct _ctx-handle ([a _intptr] [b _intptr]))

(define _SECURITY_STATUS _ULONG)
(define _TimeStamp _int64)

(define SECPKG_CRED_INBOUND #x00000001)
(define SECPKG_CRED_OUTBOUND #x00000002)

(define ISC_REQ_REPLAY_DETECT #x00000004)
(define ISC_REQ_SEQUENCE_DETECT #x00000008)
(define ISC_REQ_CONFIDENTIALITY #x00000010)
(define ISC_REQ_ALLOCATE_MEMORY #x00000100)
(define ISC_REQ_STREAM #x00008000)
(define ISC_REQ_USE_SUPPLIED_CREDS #x00000080)
(define ISC_REQ_MANUAL_CRED_VALIDATION #x00080000)

(define SECURITY_NATIVE_DREP #x00000010)

(define SECBUFFER_VERSION 0)
(define SECBUFFER_EMPTY 0)
(define SECBUFFER_DATA 1)
(define SECBUFFER_TOKEN 2)
(define SECBUFFER_EXTRA 5)
(define SECBUFFER_STREAM_TRAILER 6)
(define SECBUFFER_STREAM_HEADER 7)
(define SECBUFFER_ALERT 17)

(define SEC_E_OK 0)
(define SEC_I_CONTINUE_NEEDED #x00090312)
(define SEC_I_CONTEXT_EXPIRED #x00090317)
(define SEC_E_INCOMPLETE_MESSAGE #x80090318)
(define SEC_E_BUFFER_TOO_SMALL #x80090321)

(define SECPKG_ATTR_STREAM_SIZES 4)

(define-cstruct _SecBuffer ([cbBuffer _ULONG]
                            [BufferType _ULONG]
                            [pvBuffer _pointer]))

(define-cstruct _SecBufferDesc ([vers _ULONG]
                                [cBuffers _ULONG]
                                [pBuffers _pointer])) ; array of _SecBuffers

(define-cstruct _SCHANNEL_CRED ([version _DWORD]
                                [cCreds _DWORD]
                                [paCred _pointer]
                                [hRootStore _pointer]
                                [cMappers _DWORD]
                                [aphMappers _pointer]
                                [cSupportedAlgs _DWORD]
                                [palgSupportedAlgs _pointer]
                                [grbitEnabledProtocols _DWORD]
                                [dwMinimumCipherStrength _DWORD]
                                [dwMaximumCipherStrength _DWORD]
                                [dwSessionLifespan _DWORD]
                                [dwFlags _DWORD]
                                [dwCredFormat _DWORD]))

(define-cstruct _SecPkgContext_StreamSizes ([cbHeader _ULONG]
                                            [cbTrailer _ULONG]
                                            [cbMaximumMessage _ULONG]
                                            [cBuffers _ULONG]
                                            [cbBlockSize _ULONG]))

(define SP_PROT_SSL2_SERVER #x00000004)
(define SP_PROT_SSL2_CLIENT #x00000008)
(define SP_PROT_SSL2 (bitwise-ior SP_PROT_SSL2_SERVER SP_PROT_SSL2_CLIENT))
(define SP_PROT_SSL3_SERVER #x00000010)
(define SP_PROT_SSL3_CLIENT #x00000020)
(define SP_PROT_SSL3 (bitwise-ior SP_PROT_SSL3_SERVER SP_PROT_SSL3_CLIENT))
(define SP_PROT_TLS1_SERVER #x00000040)
(define SP_PROT_TLS1_CLIENT #x00000080)
(define SP_PROT_TLS1 (bitwise-ior SP_PROT_TLS1_SERVER SP_PROT_TLS1_CLIENT))
(define SP_PROT_TLS1_1_SERVER #x00000100)
(define SP_PROT_TLS1_1_CLIENT #x00000200)
(define SP_PROT_TLS1_1 (bitwise-ior SP_PROT_TLS1_1_SERVER SP_PROT_TLS1_1_CLIENT))
(define SP_PROT_TLS1_2_SERVER #x00000400)
(define SP_PROT_TLS1_2_CLIENT #x00000800)
(define SP_PROT_TLS1_2 (bitwise-ior SP_PROT_TLS1_2_SERVER SP_PROT_TLS1_2_CLIENT))
(define SCH_CRED_MANUAL_CRED_VALIDATION #x00000008)
(define SCH_CRED_NO_DEFAULT_CREDS #x00000010)
(define SCHANNEL_CRED_VERSION #x00000004)

(define-secur32 InitSecurityInterfaceW
  (_fun #:abi winapi -> _pointer))

(define (check-status who r)
  (unless (zero? r)
    (network-error who "failed: ~x" r)))

(define-secur32 AcquireCredentialsHandleW
  (_fun #:abi winapi
        _string/utf-16 ; principal
        _string/utf-16 ; package, such as "Negotiate"
        _ULONG ; SECPKG_CRED_INBOUND or SECPKG_CRED_OUTBOUND
        _pointer ; pvLogonID, NULL ok
        _pointer ; pAuthData, NULL ok
        _pointer ; pGetKeyFn, NULL ok
        _pointer ; pvGetKeyArgument, NULL ok
        _cred-handle-pointer ; receives the result
        (ts : (_ptr o _TimeStamp))
        ->
        (r : _SECURITY_STATUS)
        ->
        (check-status 'AcquireCredentialsHandleW r)))

(define-secur32 FreeCredentialsHandle
  (_fun #:abi winapi
        _cred-handle-pointer
        ->
        (r : _SECURITY_STATUS)
        ->
        (check-status 'FreeCredentialsHandle r)))

(define-secur32 FreeContextBuffer
  (_fun #:abi winapi
        _pointer
        ->
        (r : _SECURITY_STATUS)
        ->
        (check-status  'FreeContextBuffer r)))

(define-secur32 InitializeSecurityContextW
  (_fun #:abi winapi
        _cred-handle-pointer
        _ctx-handle-pointer/null ; NULL on first call
        _string/utf-16 ; server name
        _ULONG ; ISC_REQ_ALLOCATE_MEMORY, etc.
        _ULONG ; reserved, 0
        _ULONG ; SECURITY_NATIVE_DREP
        _SecBufferDesc-pointer/null ; input, NULL on first call
        _ULONG ; reserved, 0
        _ctx-handle-pointer/null ; non-NULL on first call only
        _SecBufferDesc-pointer ; output buffer
        (attr : (_ptr o _ULONG))
        (ts : (_ptr o _TimeStamp)) ; timeout out, can ignore
        ->
        (r : _SECURITY_STATUS)
        ->
        (values r attr)))

(define-secur32 DeleteSecurityContext
  (_fun #:abi winapi
        _ctx-handle-pointer
        ->
        (r : _SECURITY_STATUS)
        ->
        (check-status 'DeleteSecurityContext r)))

(define-secur32 DecryptMessage
  (_fun #:abi winapi
        _ctx-handle-pointer
        _SecBufferDesc-pointer ; input and output buffer
        _ULONG
        _pointer
        ->
        _SECURITY_STATUS))

(define-secur32 EncryptMessage
  (_fun #:abi winapi
        _ctx-handle-pointer
        _ULONG
        _SecBufferDesc-pointer ; input and output buffer
        _ULONG
        ->
        _SECURITY_STATUS))

(define-secur32 QueryContextAttributesW
  (_fun #:abi winapi
        _ctx-handle-pointer
        _ULONG  ; attribute
        _pointer ; receives the result
        ->
        (r : _SECURITY_STATUS)
        ->
        (check-status 'QueryContextAttributes r)))

(define-logger win32-ssl)

;; ----------------------------------------
;; Credential and context finalization

;; We allocate a credential and context handle at the same time
;; (atomically), so we only have to finalize credential--context
;; pairs.

(define free-ctx
  ((deallocator)
   (lambda (ctx)
     (unless (and (zero? (ctx-handle-a (car ctx)))
                  (zero? (ctx-handle-b (car ctx))))
       (DeleteSecurityContext (car ctx)))
     (FreeCredentialsHandle (cdr ctx)))))
(define make-ctx
  ((allocator free-ctx)
   (lambda (cred)
     (cons (make-ctx-handle 0 0) cred))))
(define (ctx->handle ctx) (car ctx))

;; ----------------------------------------
;; Helpers to manage the clunky SecBuffer API

(define (make-SecBuffers n)
  (cast (malloc n _SecBuffer 'atomic-interior) _pointer _SecBuffer-pointer))

(define (make-SecBuffers! sbs . vals)
  (define n
    (let loop ([pos 0] [vals vals])
      (cond
       [(null? vals) pos]
       [else
        (define sb (ptr-ref sbs _SecBuffer pos))
        (set-SecBuffer-cbBuffer! sb (car vals))
        (set-SecBuffer-BufferType! sb (cadr vals))
        (set-SecBuffer-pvBuffer! sb (caddr vals))
        (loop (add1 pos) (cdddr vals))])))
  (make-SecBufferDesc SECBUFFER_VERSION
                      n
                      sbs))

;; ----------------------------------------
;; Creating a context (i.e., an SSL connection)

;; Returns a context plus initial bytes for stream
(define (create-context protocol hostname i o out-sb in-sb)
  ;; Pointers to particular SecBuffer records:
  (define out-sb0 (ptr-ref out-sb _SecBuffer 0))
  (define in-sb0 (ptr-ref in-sb _SecBuffer 0))
  (define in-sb1 (ptr-ref in-sb _SecBuffer 1))

  ;; To stream communication during protocol set-up:
  (define buffer-size 4096)
  (define buffer (make-sized-byte-string (malloc buffer-size 'atomic-interior)
                                         buffer-size))

  (call-as-atomic
   (lambda ()
     ;; Allocate credentials.
     (define cred (make-cred-handle 0 0))
     (AcquireCredentialsHandleW #f
                                "Microsoft Unified Security Protocol Provider"
                                SECPKG_CRED_OUTBOUND
                                #f
                                (make-SCHANNEL_CRED SCHANNEL_CRED_VERSION
                                                    0 #f
                                                    #f
                                                    0 #f ; mappers
                                                    0 #f ; algs
                                                    (case protocol
                                                      [(secure auto)
                                                       (bitwise-ior SP_PROT_TLS1 SP_PROT_TLS1_1 SP_PROT_TLS1_2)]
                                                      [(sslv2) SP_PROT_SSL2]
                                                      [(sslv3) SP_PROT_SSL3]
                                                      [(tls) SP_PROT_TLS1]
                                                      [(tls11) SP_PROT_TLS1_1]
                                                      [(tls12) SP_PROT_TLS1_2]
                                                      [else 0])
                                                    0 0 0
                                                    (if (eq? protocol 'secure)
                                                        0
                                                        SCH_CRED_MANUAL_CRED_VALIDATION)
                                                    0)
                                #f
                                #f
                                cred)

     ;; Allocate a content and take responsibility for freeing
     ;; credientials, but it's not a real content until the
     ;; 0 values are replaced with an new context:
     (define ctx (make-ctx cred))

     ;; Loop to let the client and server communicate to set up the protocol:
     (let loop ([data-len 0] [init? #t])
       (define-values (r attr)
         (InitializeSecurityContextW cred
                                     (if init? #f (ctx->handle ctx))
                                     (if (eq? protocol 'secure)
                                         hostname
                                         #f)
                                     (bitwise-ior ISC_REQ_REPLAY_DETECT ISC_REQ_SEQUENCE_DETECT
                                                  ISC_REQ_CONFIDENTIALITY ISC_REQ_STREAM 
                                                  ISC_REQ_ALLOCATE_MEMORY
                                                  (if (eq? protocol 'secure)
                                                      0
                                                      ISC_REQ_MANUAL_CRED_VALIDATION))
                                     0
                                     SECURITY_NATIVE_DREP
                                     (if init?
                                         #f
                                         (make-SecBuffers! in-sb
                                                           data-len
                                                           SECBUFFER_TOKEN
                                                           buffer
                                                           0
                                                           SECBUFFER_EMPTY
                                                           #f))
                                     0
                                     (if init? (ctx->handle ctx) #f)
                                     (make-SecBuffers! out-sb
                                                       0
                                                       SECBUFFER_TOKEN
                                                       #f)))
       (log-win32-ssl-debug "init context: status ~x" r)

       (when (or (= r SEC_E_OK)
                 (= r SEC_I_CONTINUE_NEEDED))
         (unless (zero? (SecBuffer-cbBuffer out-sb0))
           ;; Go back to non-atomic mode for a potentially blocking write:
           (call-as-nonatomic
            (lambda ()
              (log-win32-ssl-debug "init context: write ~a" (SecBuffer-cbBuffer out-sb0))
              (write-bytes (make-sized-byte-string (SecBuffer-pvBuffer out-sb0)
                                                   (SecBuffer-cbBuffer out-sb0))
                           o)
              (flush-output o)))
           (FreeContextBuffer (SecBuffer-pvBuffer out-sb0))))

       (define (get-leftover-bytes)
         (if (equal? (SecBuffer-BufferType in-sb1) SECBUFFER_EXTRA)
             ;; Save the leftover bytes:
             (let ([amt (SecBuffer-cbBuffer in-sb1)])
               (log-win32-ssl-debug "init context: leftover ~a" amt)
               (memcpy buffer (ptr-add buffer (- data-len amt)) amt)
               amt)
             0))

       (cond
        [(= r SEC_E_OK) 
         ;; Success:
         (log-win32-ssl-debug "init context: done")
         (values ctx
                 (let ([n (get-leftover-bytes)])
                   (subbytes buffer 0 n)))]
        [(or (= r SEC_I_CONTINUE_NEEDED)
             (= r SEC_E_INCOMPLETE_MESSAGE))
         ;; Pull more data from the server
         (define new-data-len (if (= r SEC_E_INCOMPLETE_MESSAGE)
                                  data-len
                                  (get-leftover-bytes)))
         ;; Unlikely, but maybe it's possible that we don't have room
         ;; to read more due to leftover bytes:
         (when (= new-data-len buffer-size)
           (define new-buffer (malloc (* 2 buffer-size) 'atomic-interior))
           (memcpy new-buffer buffer buffer-size)
           (set! buffer-size (* 2 buffer-size))
           (set! buffer (make-sized-byte-string new-buffer buffer-size)))
         ;; Go back to non-atomic mode for a potentially blocking read:
         (define n (call-as-nonatomic
                    (lambda ()
                      (read-bytes-avail! buffer i new-data-len buffer-size))))
         (log-win32-ssl-debug "init context: read ~a" n)
         (when (eof-object? n) (network-error "unexpected EOF"))
         (loop (+ new-data-len n) (if (= r SEC_I_CONTINUE_NEEDED)
                                      #f
                                      init?))]
        ;; Some other things are allowed to happen without implying
        ;; failure, but we don't handle all of them.
        [else (network-error 'create-context
                             "unexpected result: ~x" r)])))))

(define (decrypt ctx in-pre-r in-post-w out-sb)
  ;; Read encrypted byte from `in-pre-r', write decrypted bytes to
  ;; `in-port-w'.
  ;; Loop to try to get a big enough chunk from the input to be able
  ;; to decrypt it.
  (let loop ([size 4096] [prev-n 0])
    (define buffer (make-bytes size))
    (define n (peek-bytes-avail!* buffer 0 #f in-pre-r))
    (define r (DecryptMessage (ctx->handle ctx)
                              (make-SecBuffers! out-sb
                                                n
                                                SECBUFFER_DATA
                                                buffer
                                                0
                                                SECBUFFER_EMPTY
                                                #f
                                                0
                                                SECBUFFER_EMPTY
                                                #f
                                                0
                                                SECBUFFER_EMPTY
                                                #f)
                              0
                              #f))
    (log-win32-ssl-debug "decrypt status: ~x" r)
    (cond
     [(= r SEC_E_OK)
      ;; Successfully decrypted some. Figure out how many bytes
      ;; were used (to remove them from `in-pre-r') and
      ;; write decrypted bytes to `in-post-w'.
      (define sb
        (for/or ([i (in-range 0 4)])
          (define sb (ptr-ref out-sb _SecBuffer i))
          (and (= SECBUFFER_DATA (SecBuffer-BufferType sb))
               sb)))
      (unless sb
        (network-error "expected decrypted data"))
      (write-bytes (make-sized-byte-string (SecBuffer-pvBuffer sb)
                                           (SecBuffer-cbBuffer sb))
                   in-post-w)
      (define remain (or (for/or ([i (in-range 1 4)])
                           (define sb (ptr-ref out-sb _SecBuffer i))
                           (and (= SECBUFFER_EXTRA (SecBuffer-BufferType sb))
                                (SecBuffer-cbBuffer sb)))
                         0))
      (log-win32-ssl-debug "decrypted ~a to ~a (~a remain)" 
                           (- n remain)
                           (SecBuffer-cbBuffer sb)
                           remain)
      (read-bytes! buffer in-pre-r 0 (- n remain))
      (unless (zero? remain)
        (loop size 0))]
     [(= r SEC_E_INCOMPLETE_MESSAGE)
      ;; If `prev-n' is the same as `n', then we must have
      ;; tried everything that's currently available.
      (unless (= prev-n n)
        ;; Try with a larger buffer:
        (loop (* size 2) n))]
     [(= r SEC_I_CONTEXT_EXPIRED)
      ;; Other end closed the connection.
      (close-output-port in-post-w)]
     [else
      (network-error 'decrypt "unexpected result: ~x" r)])))

(define (encrypt ctx bstr start end out-sb sizes buffer)
  ;; Encrypt bytes [start, end) from bstr.
  ;; If we have too much to encrypt at once, we'll encrypt
  ;; halves separately:
  (define (divide-and-conquer)
    
    (define mid (quotient (+ start end) 2))
    (bytes-append (encrypt ctx bstr start mid sizes buffer)
                  (encrypt ctx bstr mid end sizes buffer)))
  (cond
   [((- end start) . > . (bytes-length buffer))
    ;; Too much right from the start:
    (divide-and-conquer)]
   [else
    ;; EncryptMessage expects certain size buffers in a
    ;; certain layout:
    (define msize (SecPkgContext_StreamSizes-cbMaximumMessage sizes))
    (define hsize (SecPkgContext_StreamSizes-cbHeader sizes))
    (define tsize (SecPkgContext_StreamSizes-cbTrailer sizes))
    (define dsize (- end start))
    (memcpy buffer hsize bstr start (- end start))
    (define r (EncryptMessage (ctx->handle ctx)
                              0
                              (make-SecBuffers! out-sb
                                                hsize
                                                SECBUFFER_STREAM_HEADER
                                                buffer
                                                dsize
                                                SECBUFFER_DATA
                                                (ptr-add buffer hsize)
                                                tsize
                                                SECBUFFER_STREAM_TRAILER
                                                (ptr-add buffer (+ hsize dsize))
                                                0
                                                SECBUFFER_EMPTY
                                                #f)
                              0))
    (log-win32-ssl-debug "encrypt status: ~x" r)
    (cond
     [(= r SEC_E_OK)
      ;; Success:
      (define len (+ (SecBuffer-cbBuffer (ptr-ref out-sb _SecBuffer 0))
                     (SecBuffer-cbBuffer (ptr-ref out-sb _SecBuffer 1))
                     (SecBuffer-cbBuffer (ptr-ref out-sb _SecBuffer 2))))
      (subbytes buffer 0 len)]
     [(= r SEC_E_BUFFER_TOO_SMALL)
      ;; The encrypted bytes don't fit in the unencrypted space?
      (divide-and-conquer)]
     [else
      (network-error 'decrypt "unexpected result: ~x" r)])]))

;; Wrap input and output ports to produce SSL versions of the ports:
(define (ports->win32-ssl-ports i o
                                #:encrypt [protocol 'auto]
                                #:hostname [hostname #f])
  ;; Working space for encoding, decoding, and more:
  (define out-sb (make-SecBuffers 4))
  (define in-sb (make-SecBuffers 2))
  
  ;; Allocate the encoding/decoding context:
  (define-values (ctx init-bytes) (create-context protocol hostname i o out-sb in-sb))

  ;; Get some sizes that we need for encoding:
  (define sizes (make-SecPkgContext_StreamSizes 0 0 0 0 0))
  (QueryContextAttributesW (ctx->handle ctx)
                           SECPKG_ATTR_STREAM_SIZES
                           sizes)
  (define msize (SecPkgContext_StreamSizes-cbMaximumMessage sizes))
  (define hsize (SecPkgContext_StreamSizes-cbHeader sizes))
  (define tsize (SecPkgContext_StreamSizes-cbTrailer sizes))

  ;; Some pipes to manage the decoding stream:
  (define-values (in-pre-r in-pre-w) (make-pipe))
  (define-values (in-post-r in-post-w) (make-pipe))

  (write-bytes init-bytes in-pre-w)
  (decrypt ctx in-pre-r in-post-w out-sb)

  ;; More working space:
  (define buffer (make-bytes (max 8000 (+ msize hsize tsize))))

  ;; Port lock and state:
  (define lock (make-semaphore 1))
  (define leftover-bytes #f)
  (define refcount 2)

  ;; Close original ports when both new ports are closed:
  (define (close!)
    (set! refcount (sub1 refcount))
    (when (zero? refcount)
      (close-input-port i)
      (close-output-port o)
      (let ([v ctx])
        (set! ctx #f)
        (when v (free-ctx v)))))

  ;; Callbacks used below (written here so that they're allocated once):
  (define (lock-unavailable/read) (wrap-evt lock (lambda () 0)))
  (define (lock-unavailable/write) (wrap-evt lock (lambda () #f)))

  (define (read-in bstr)
    (let loop ()
      (define n (read-bytes-avail!* bstr in-post-r))
      (cond
       [(eof-object? n) n]
       [(zero? n)
        ;; Any input on the underlying port?
        (define n (read-bytes-avail!* buffer i))
        (cond
         [(eof-object? n)
          ;; Nothing decrypted, hit eof; return eof, even though
          ;; we have leftover encrypted bytes:
          (close-output-port in-post-w)
          n]
         [(zero? n)
          ;; Nothing decrypted, no new input, so wait for input:
          (log-win32-ssl-debug "blocked")
          (wrap-evt i (lambda (v) 0))]
         [else
          (log-win32-ssl-debug "underlying receive: ~a" n)
          ;; Get some fresh bytes, so try decoding now:
          (write-bytes buffer in-pre-w 0 n)
          (decrypt ctx in-pre-r in-post-w out-sb)
          (loop)])]
       [else n])))

  ;; The new input port:
  (define in (make-input-port/read-to-peek
              (format "SSL ~a" (object-name i))
              ;; read:
              (lambda (bstr)
                (call-with-semaphore
                 lock
                 read-in
                 lock-unavailable/read
                 bstr))
              ;; peek:
              (lambda (bstr offset slow)
                ;; Try fast peek on decrypted port:
                (define n (peek-bytes-avail!* bstr offset #f in-post-r))
                (if (zero? n)
                    (slow bstr offset)
                    n))
              ;; close
              (lambda ()
                (call-with-semaphore
                 lock
                 close!))))

  
  (define (write-out bstr start end non-block? enable-break?)
    (cond
     [(and (= start end)
           (not leftover-bytes))
      ;; Nothing to flush:
      0]
     [(not leftover-bytes)
      ;; Nothing in the output buffer, so we can encrypt more
      (define encrypted-bstr (encrypt ctx bstr start end out-sb sizes buffer))
      (define n (write-bytes-avail* encrypted-bstr o))
      (cond
       [(zero? n)
        (wrap-evt o (lambda (v) #f))]
       [(= n (bytes-length encrypted-bstr))
        ;; all written
        (- end start)]
       [else
        ;; we're forced to save the leftover bytes and
        ;; claim that they're written anyway:
        (set! leftover-bytes (subbytes encrypted-bstr n))
        (- end start)])]
     [else
      ;; Try sending leftover bytes (for flush or not):
      (define n (write-bytes-avail* leftover-bytes o))
      (cond
       [(zero? n) 
        (wrap-evt o (lambda (v) #f))]
       [(= n (bytes-length leftover-bytes))
        (set! leftover-bytes #f)
        (if (= start end)
            0 ; flushed all
            #f)]
       [else
        (set! leftover-bytes (subbytes leftover-bytes n))
        #f])]))
  
  ;; The new output port:
  (define out (make-output-port
               (format "SSL ~a" (object-name 0))
               o
               ;; write-out
               (lambda (bstr start end non-block? enable-break?)
                 (call-with-semaphore
                  lock
                  write-out                  
                  lock-unavailable/write
                  bstr start end non-block? enable-break?))
               ;; close
               (lambda ()
                 ;; flush:
                 (let loop ()
                   (define r
                     (call-with-semaphore
                      lock
                      (lambda ()
                        (write-out #"" 0 0 #f #f))))
                   (cond
                    [(equal? r 0) (void)]
                    [(evt? r) (sync r) (loop)]
                    [else (loop)]))
                 ;; actually close:
                 (call-with-semaphore
                  lock
                  close!))))

  ;; Done:
  (values (register in) (register out)))

;; ----------------------------------------
;; Errors

(define network-error
  (case-lambda
    [(str) (network-error 'win32-ssl str)]
    [(who msg . args)
     (raise
      (exn:fail:network
       (format "~a: ~a" who (apply format msg args))
       (current-continuation-marks)))]))

;; ----------------------------------------
;; Recognizing win32 ports

(define win32-ssl-ports (make-weak-hash))

(define (register p)
  (hash-set! win32-ssl-ports p #t)
  p)

(define (win32-ssl-port? p)
  (hash-ref win32-ssl-ports p #f))

;; ----------------------------------------
;; Initialization

(when (eq? 'windows (system-type))
  (void (InitSecurityInterfaceW)))

;; ----------------------------------------

#;
(module+ main
  ;; Use `openssl' to implement server side for tests:
  (require openssl)
  (define server (ssl-make-server-context))
  (ssl-load-certificate-chain! server (collection-file-path "test.pem" "openssl"))
  (ssl-load-private-key! server (collection-file-path "test.pem" "openssl"))

  ;; Check that data is sent correctly:
  (define N 100)
  (define M 3)
  (define s (make-bytes N))
  (for ([i N])
    (bytes-set! s i (bitwise-and i 255)))
  (for ([c 100])
    (printf "~s\n" c)
    (define-values (i1 o1) (make-pipe (+ 4096 (random 4096))))
    (define-values (i2 o2) (make-pipe (+ 4096 (random 4096))))
    (define (fail who) (log-error "no good ~s" who) (exit 1))
    (define t1
      (thread
       (lambda ()
         (define-values (si so) (ports->ssl-ports i1 o2
                                                  #:mode 'accept
                                                  #:context server))
         (for ([j M]) (write s so))
         (flush-output so)
         (for ([j M])
           (unless (equal? s (read si))
             (fail 'server)))
         (close-output-port so)
         (close-input-port si))))
    (define t2
     (thread
      (lambda ()
        (define-values (ci co) (ports->win32-ssl-ports i2 o1))
        (for ([j M])
          (unless (equal? s (read ci))
            (fail 'client)))
        (for ([j M])
          (write s co))
        (close-output-port co)
        (close-input-port ci))))
    (sync t1)
    (sync t2)))
