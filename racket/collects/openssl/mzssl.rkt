
;; Disabled when `enforce-retry?' is #f:
;;  Warn clients: when a (non-blocking) write fails to write all the
;;   data, the stream is actually committed to writing the given data
;;   in the future. (This requirement comes from the SSL library.)

;; Another warning: data that is written and not buffered may still be
;;  in flight between Racket and the underlying ports. A `flush-output'
;;  won't return until sent data is actually in the underlying port.
;;  (This is due to the fact that unbuffered data cannot be written
;;  without blocking.)

;; One last warning: a write/read must block because a previous
;;  read/write (the opposite direction) didn't finish, and so that
;;  opposite must be completed, first.

#|
TO DO:
 - hostname checking should shutdown SSL negotiation w/ right alert code
 - CRL support (?)
 - alternative hostname checking styles
 - double-check refcounting of X509
|#

#lang racket/base
(require (rename-in racket/contract/base [-> c->])
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         ffi/unsafe/global
         ffi/file
         ffi/unsafe/custodian
         racket/list
         racket/port
         racket/tcp
         racket/string
         racket/lazy-require
         racket/include
         "libcrypto.rkt"
         "libssl.rkt"
         "private/ffi.rkt"
         (for-syntax racket/base))
(lazy-require
 ["private/win32.rkt" (load-win32-store)]
 ["private/macosx.rkt" (load-macosx-keychain)])

(define-logger openssl)

(when (and libssl (not v1.0.2/later?))
  (log-openssl-error "OpenSSL library too old, version 1.0.2 or later required."))

(define protocol-symbol/c
  (or/c 'secure 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12 'tls13))

(define (alpn-protocol-bytes/c v)
  (and (bytes? v) (< 0 (bytes-length v) 256)))

(define curve-nid-alist
  '((sect163k1 . 721)
    (sect163r1 . 722)
    (sect163r2 . 723)
    (sect193r1 . 724)
    (sect193r2 . 725)
    (sect233k1 . 726)
    (sect233r1 . 727)
    (sect239k1 . 728)
    (sect283k1 . 729)
    (sect283r1 . 730)
    (sect409k1 . 731)
    (sect409r1 . 732)
    (sect571k1 . 733)
    (sect571r1 . 734)
    (secp160k1 . 708)
    (secp160r1 . 709)
    (secp160r2 . 710)
    (secp192k1 . 711)
    (secp224k1 . 712)
    (secp224r1 . 713)
    (secp256k1 . 714)
    (secp384r1 . 715)
    (secp521r1 . 716)
    (prime192v1 . 409)
    (prime256v1 . 415)))

(define curve/c (apply or/c (map car curve-nid-alist)))

(define verify-source/c
  (or/c path-string?
        (list/c 'directory path-string?)
        (list/c 'win32-store string?)
        (list/c 'macosx-keychain (or/c #f path-string?))))

(provide
 ssl-dh4096-param-bytes
 (rename-out [protocol-symbol/c ssl-protocol-symbol/c])
 (contract-out
  [ssl-available? boolean?]
  [ssl-load-fail-reason (or/c #f string?)]
  [ssl-make-client-context
   (->* ()
        (protocol-symbol/c
         #:private-key (or/c (list/c 'pem path-string?) (list/c 'der path-string?) #f)
         #:certificate-chain (or/c path-string? #f))
        ssl-client-context?)]
  [ssl-secure-client-context
   (c-> ssl-client-context?)]
  [ssl-make-server-context
   (->* ()
        (protocol-symbol/c
         #:private-key (or/c (list/c 'pem path-string?) (list/c 'der path-string?) #f)
         #:certificate-chain (or/c path-string? #f))
        ssl-server-context?)]
  [ssl-server-context-enable-dhe!
   (->* (ssl-server-context?) ((or/c path-string? bytes?)) void?)]
  [ssl-server-context-enable-ecdhe!
   (->* (ssl-server-context?) (curve/c) void?)]
  [ssl-client-context?
   (c-> any/c boolean?)]
  [ssl-server-context?
   (c-> any/c boolean?)]
  [ssl-context?
   (c-> any/c boolean?)]
  [ssl-load-certificate-chain!
   (c-> (or/c ssl-context? ssl-listener?) path-string? void?)]
  [ssl-load-private-key!
   (->* ((or/c ssl-context? ssl-listener?) path-string?)
        (any/c any/c)
        void?)]
  [ssl-load-verify-root-certificates!
   (c-> (or/c ssl-context? ssl-listener? ssl-port?)
        path-string?
        void?)]
  [ssl-load-verify-source!
   (c-> ssl-context?
        verify-source/c
        void?)]
  [ssl-load-suggested-certificate-authorities!
   (c-> (or/c ssl-context? ssl-listener?)
        path-string?
        void?)]
  [ssl-set-ciphers!
   (c-> ssl-context? string? void?)]
  [ssl-set-server-name-identification-callback!
   (c-> ssl-server-context? (c-> string? (or/c ssl-server-context? #f)) void?)]
  [ssl-set-server-alpn!
   (->* [ssl-server-context? (listof alpn-protocol-bytes/c)] [boolean?] void?)]
  [ssl-seal-context!
   (c-> ssl-context? void?)]
  [ssl-default-verify-sources
   (parameter/c (listof verify-source/c))]
  [ssl-load-default-verify-sources!
   (c-> ssl-context? void?)]
  [ssl-set-verify!
   (c-> (or/c ssl-context? ssl-listener? ssl-port?)
        any/c
        void?)]
  [ssl-try-verify!
   (c-> (or/c ssl-context? ssl-listener? ssl-port?)
        any/c
        void?)]
  [ssl-set-verify-hostname!
   (c-> ssl-context? any/c void?)]
  [ssl-set-keylogger!
   (c-> ssl-context? (or/c #f logger?) void?)]
  [ssl-peer-verified?
   (c-> ssl-port? boolean?)]
  [ssl-peer-certificate-hostnames
   (c-> ssl-port? (listof string?))]
  [ssl-peer-check-hostname
   (c-> ssl-port? string? boolean?)]
  [ssl-peer-subject-name
   (c-> ssl-port? (or/c bytes? #f))]
  [ssl-peer-issuer-name
   (c-> ssl-port? (or/c bytes? #f))]
  [ssl-channel-binding
   (c-> ssl-port? (or/c 'tls-exporter 'tls-unique 'tls-server-end-point) bytes?)]
  [ssl-default-channel-binding
   (c-> ssl-port? (list/c symbol? bytes?))]
  [ssl-protocol-version
   (c-> ssl-port? (or/c symbol? #f))]
  [ssl-get-alpn-selected
   (c-> ssl-port? (or/c bytes? #f))]
  [ports->ssl-ports
   (->* [input-port?
         output-port?]
        [#:mode (or/c 'connect 'accept)
         #:context ssl-context?
         #:encrypt protocol-symbol/c
         #:close-original? any/c
         #:shutdown-on-close? any/c
         #:error/ssl procedure?
         #:hostname (or/c string? #f)
         #:alpn (listof alpn-protocol-bytes/c)]
        (values input-port? output-port?))]
  [ssl-listen
   (->* [listen-port-number?]
        [exact-nonnegative-integer?
         any/c
         (or/c string? #f)
         (or/c ssl-server-context? protocol-symbol/c)]
        ssl-listener?)]
  [ssl-close
   (c-> ssl-listener? void?)]
  [ssl-accept
   (c-> ssl-listener?
        (values input-port? output-port?))]
  [ssl-accept/enable-break
   (c-> ssl-listener?
        (values input-port? output-port?))]
  [ssl-connect
   (->* [string?
         (integer-in 1 (sub1 (expt 2 16)))]
        [(or/c ssl-client-context? protocol-symbol/c)
         #:alpn (listof alpn-protocol-bytes/c)]
        (values input-port? output-port?))]
  [ssl-connect/enable-break
   (->* [string?
         (integer-in 1 (sub1 (expt 2 16)))]
        [(or/c ssl-client-context? protocol-symbol/c)
         #:alpn (listof alpn-protocol-bytes/c)]
        (values input-port? output-port?))]
  [ssl-listener?
   (c-> any/c boolean?)]
  [ssl-addresses
   (->* [(or/c ssl-listener? ssl-port?)]
        [any/c]
        any)]
  [ssl-abandon-port
   (c-> ssl-port? void?)]
  [ssl-port?
   (c-> any/c boolean?)])
 ssl-max-client-protocol
 ssl-max-server-protocol
 supported-client-protocols
 supported-server-protocols)

(define ssl-load-fail-reason
  (or libssl-load-fail-reason
      libcrypto-load-fail-reason))

;; ----------------------------------------

(define (x509-root-sources)
  (cond
    [libcrypto
     ;; Workaround for natipkg openssl library: the default cert locations vary
     ;; from distro to distro, and there is no one configuration that works with
     ;; all. So build natipkg libssl.so with `--openssldir="/RACKET_USE_ALT_PATH"`
     ;; and this code will override with better guesses.
     ;; Cert locations for various distros:
     ;;   Debian: dir=/etc/ssl/certs, file=/etc/ssl/certs/ca-certificates.crt (prefer dir!)
     ;;   RedHat: file=/etc/pki/tls/certs/ca-bundle.crt; /etc/ssl/certs exists but useless!
     ;;   OpenSUSE: dir=/etc/ssl/certs, file=/var/lib/ca-certificates/ca-bundle.pem (prefer dir!)
     ;; So try file=/etc/pki/tls/certs/ca-bundle.crt, dir=/etc/ssl/certs.
     (define (use-alt-path? p) (regexp-match? #rx"^/RACKET_USE_ALT_PATH" p))
     (define (subst-cert-file p)
       (cond [(use-alt-path? p)
              (log-openssl-debug "cert file path is ~s; using alternatives" p)
              (filter file-exists? '("/etc/pki/tls/certs/ca-bundle.crt"))]
             [else p]))
     (define (subst-cert-dir p)
       (cond [(use-alt-path? p)
              (log-openssl-debug "cert dir path is ~s; using alternatives" p)
              (filter directory-exists? '("/etc/ssl/certs"))]
             [else p]))
     ;; ----
     (define dir-sep (case (system-type) [(windows) ";"] [else ":"]))
     (define cert-file0
       (or (getenv (X509_get_default_cert_file_env)) (X509_get_default_cert_file)))
     (define cert-dirs0
       (or (getenv (X509_get_default_cert_dir_env)) (X509_get_default_cert_dir)))
     ;; Use path-string? filter to avoid {file,directory}-exists? error on "".
     (define cert-files
       (filter path-string? (flatten (map subst-cert-file (list cert-file0)))))
     (define cert-dirs
       (filter path-string? (flatten (map subst-cert-dir (string-split cert-dirs0 dir-sep)))))
     ;; Log error only if *no* cert source exists (eg, on Debian/Ubuntu, default
     ;; cert file does not exist).
     (unless (or (ormap file-exists? cert-files) (ormap directory-exists? cert-dirs))
       (set! complain-on-cert
             (lambda ()
               (log-openssl-error
                "x509-root-sources: cert sources do not exist: ~s, ~s; ~a"
                cert-file0 cert-dirs0
                (format "override using ~a, ~a"
                        (X509_get_default_cert_file_env)
                        (X509_get_default_cert_dir_env))))))
     (log-openssl-debug "using cert sources: ~s, ~s" cert-files cert-dirs)
     (append cert-files (map (lambda (p) (list 'directory p)) cert-dirs))]
    [else null]))

(define complain-on-cert void)
(define (maybe-complain-on-cert)
  (complain-on-cert)
  (set! complain-on-cert void))

(define ssl-default-verify-sources
  (make-parameter
   (case (system-type 'os*)
     [(windows)
      ;; On Windows, x509-root-sources produces paths like "/usr/local/ssl/certs", which
      ;; aren't useful. So just skip them.
      '((win32-store "ROOT"))]
     [(macosx darwin)
      '((macosx-keychain #f))]
     [else
      (x509-root-sources)])))

(define ssl-dh4096-param-bytes
  (include/reader "dh4096.pem" (lambda (src port)
                                 (let loop ([accum '()])
                                   (define bstr (read-bytes 4096 port))
                                   (if (eof-object? bstr)
                                       (if (null? accum)
                                           eof
                                           (datum->syntax #'here (apply bytes-append (reverse accum))))
                                       (loop (cons bstr accum)))))))

;; Make this bigger than 4096 to accommodate at least
;; 4096 of unencrypted data
(define BUFFER-SIZE 8000)

;; The man pages for SSL_read and SSL_write say that they must be
;; retried with the same arguments when they return SSL_ERROR_WANT_READ
;; or SSL_ERROR_WANT_WRITE.  This may not actually be true, especially
;; when SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER is used, and "retry" may or
;; may not mean "retry without doing other things first". Set `enforce-retry?'
;; to #t to obey the manpage and retry without doing other things, which
;; has an implicitation for clients as noted at the top of this file.
(define enforce-retry? #f)

;; Needed for `renegotiate' prior to v1.1:
(define-cstruct _ssl_struct ([version _int]
                             [type _int]
                             [method _pointer]
                             [rbio _pointer]
                             [wbio _pointer]
                             [bbio _pointer]
                             [rwstate _int]
                             [in_handshake _int]
                             [handshake_func _fpointer]
                             [server _int]
                             [new_session _int]
                             [quiet_shutdown _int]
                             [shutdown _int]
                             [state _int]
                             ;; ...
                             ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

(define-syntax with-failure
  (syntax-rules ()
    [(_ thunk body ...)
     (with-handlers ([exn? (lambda (exn) (thunk) (raise exn))])
       body ...)]))

(define (get-error-message id)
  (let* ([buffer (make-bytes 512)])
    (ERR_error_string_n id buffer (bytes-length buffer))
    (regexp-match #rx#"^[^\0]*" buffer)))

(define (check-valid v who what)
  (when (ptr-equal? v #f)
    (let ([id (ERR_get_error)])
      (error who "~a failed ~a" what (get-error-message id)))))

(define (error/network who fmt . args)
  (raise (make-exn:fail:network
          (format "~a: ~a" who (apply format fmt args))
          (current-continuation-marks))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic blocks

(define-syntax-rule (atomically body ...)
  (call-as-atomic (lambda () body ...)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs

(define-struct ssl-context
  (ctx
   [verify-hostname? #:mutable]
   [sealed? #:mutable]
   [keylog-cb #:mutable]))
(define-struct (ssl-client-context ssl-context) ())
(define-struct (ssl-server-context ssl-context)
  ([sni-cb #:mutable #:auto]
   [alpn-cb #:mutable #:auto]))

(define-struct ssl-listener (l mzctx)
  #:property prop:evt (lambda (lst) (wrap-evt (ssl-listener-l lst) 
                                              (lambda (x) lst))))

;; internal:
;; mzctx: (or/c #f ssl-context?) - retained to prevent early garbage
;;        collection of its associated `keylog-cb`, if any.
(define-struct mzssl (ssl mzctx i o r-bio w-bio pipe-r pipe-w
                          buffer lock
                          w-closed? r-closed?
                          flushing? must-write must-read
                          refcount
                          close-original? shutdown-on-close?
                          error
                          server?
                          in-progress-sema out-progress-sema)
  #:mutable)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors

(define (do-save-errors thunk ssl)
  ;; Atomically run a function and get error results
  ;; so that this library is thread-safe (at the level of Racket threads)
  (atomically
   (define v (thunk))
   (define e (if (positive? v)
                 0
                 (SSL_get_error ssl v)))
   (define unknown "(unknown error)")
   (define estr
     (cond
      [(= e SSL_ERROR_SSL)
       (get-error-message (ERR_get_error))]
      [(= e SSL_ERROR_SYSCALL)
       (define v (ERR_get_error))
       (if (zero? v)
           unknown
           (get-error-message v))]
      [else unknown]))
   (values v e estr)))

(define-syntax-rule (save-errors e ssl)
  (do-save-errors (lambda () e) ssl))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts, certificates, etc.

(define default-encrypt 'auto)

;; In v1.1.0 and later, there does not seem to be a reliable way of testing
;; dynamically whether a protocol version is supported. So use version tests.
;; FIXME: Drop 'sslv3, 'tls1, and 'tls11 (deprecated by RFC 8996).

(define protocol-versions
  ;; (protocol security-level min-proto max-proto supported?)
  `((secure 2 ,TLS1_2_VERSION  0                #t)
    (auto   2 0                0                #t)
    (sslv3  1 ,SSL3_VERSION    ,SSL3_VERSION    ,(and SSLv3_client_method #t))
    (tls    2 ,TLS1_VERSION    ,TLS1_VERSION    ,(and TLSv1_client_method #t))
    (tls11  2 ,TLS1_1_VERSION  ,TLS1_1_VERSION  ,(and TLSv1_1_client_method #t))
    (tls12  2 ,TLS1_2_VERSION  ,TLS1_2_VERSION  ,(or v1.0.1/later? (and TLSv1_2_client_method #t)))
    (tls13  2 ,TLS1_3_VERSION  ,TLS1_3_VERSION  ,(or v1.1.1/later?))))

;; Keep symbols in best-last order for ssl-max-{client,server}-protocol.
(define the-supported-protocols
  (for/list ([pinfo (in-list protocol-versions)] #:when (list-ref pinfo 4))
    (car pinfo)))

(define (supported-client-protocols) the-supported-protocols)
(define (supported-server-protocols) the-supported-protocols)

(define (ssl-max-client-protocol)
  (let ([protocols (supported-client-protocols)])
    (and (pair? protocols) (last protocols))))

(define (ssl-max-server-protocol)
  (let ([protocols (supported-server-protocols)])
    (and (pair? protocols) (last protocols))))

(define (make-context who protocol-symbol client? priv-key cert-chain)
  (define ctx (make-raw-context who protocol-symbol client?))
  (define mzctx ((if client? make-ssl-client-context make-ssl-server-context) ctx #f #f #f))
  (when cert-chain (ssl-load-certificate-chain! mzctx cert-chain))
  (cond [(and (pair? priv-key) (eq? (car priv-key) 'pem))
         (ssl-load-private-key! mzctx (cadr priv-key) #f #f)]
        [(and (pair? priv-key) (eq? (car priv-key) 'der))
         (ssl-load-private-key! mzctx (cadr priv-key) #f #t)]
        [else (void)])
  mzctx)

(define (make-raw-context who protocol-symbol client?)
  (define meth (encrypt->method who protocol-symbol client?))
  (define ctx
    (atomically ;; connect SSL_CTX_new to subsequent check-valid (ERR_get_error)
     (let ([ctx (SSL_CTX_new meth)])
       (check-valid ctx who "context creation")
       ctx)))
  (SSL_CTX_set_mode ctx (bitwise-ior SSL_MODE_ENABLE_PARTIAL_WRITE
                                     SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER))
  (when v1.1.0/later?
    (define proto-info (assq protocol-symbol protocol-versions))
    (let ([security-level (max (cadr proto-info) (SSL_CTX_get_security_level ctx))]
          [min-proto (caddr proto-info)]
          [max-proto (cadddr proto-info)])
      (SSL_CTX_set_security_level ctx security-level)
      (unless (and (= 1 (SSL_CTX_set_min_proto_version ctx min-proto))
                   (= 1 (SSL_CTX_set_max_proto_version ctx max-proto)))
        (error who "failed setting min/max protocol versions: ~e" protocol-symbol))))
  ctx)

(define (encrypt->method who e client?)
  (unless (memq e (if client? (supported-client-protocols) (supported-server-protocols)))
    (raise-protocol-not-supported who e))
  (define f
    (if v1.1.0/later?
        (if client? TLS_client_method TLS_server_method)
        (case e
          [(secure auto sslv2-or-v3) (if client? TLS_client_method TLS_server_method)]
          [(sslv2) (if client? SSLv2_client_method SSLv2_server_method)]
          [(sslv3) (if client? SSLv3_client_method SSLv3_server_method)]
          [(tls) (if client? TLSv1_client_method TLSv1_server_method)]
          [(tls11) (if client? TLSv1_1_client_method TLSv1_1_server_method)]
          [(tls12) (if client? TLSv1_2_client_method TLSv1_2_server_method)]
          [else #f])))
  (unless f (raise-protocol-not-supported who e))
  (f))

(define (raise-protocol-not-supported who e)
  (raise (exn:fail:unsupported
          (format "~a: requested protocol not supported~a\n  requested: ~e"
                  who
                  (if ssl-available?
                      ""
                      ";\n SSL not available; check `ssl-load-fail-reason'")
                  e)
          (current-continuation-marks))))

(define (need-ctx-free? context-or-encrypt-method)
  (and (symbol? context-or-encrypt-method)
       (not (eq? context-or-encrypt-method 'secure))))

(define (ssl-make-client-context [protocol-symbol default-encrypt]
                                 #:private-key [priv-key #f]
                                 #:certificate-chain [cert-chain #f])
  (cond [(and (eq? protocol-symbol 'secure) (not priv-key) (not cert-chain))
         (ssl-secure-client-context)]
        [else (make-client-context* protocol-symbol priv-key cert-chain)]))

(define (make-client-context* protocol-symbol priv-key cert-chain)
  (define ctx (make-context 'ssl-make-client-context protocol-symbol #t priv-key cert-chain))
  (when (eq? protocol-symbol 'secure) (secure-client-context! ctx))
  ctx)

(define (ssl-make-server-context [protocol-symbol default-encrypt]
                                 #:private-key [priv-key #f]
                                 #:certificate-chain [cert-chain #f])
  (make-context 'ssl-make-server-context protocol-symbol #f priv-key cert-chain))

(define (get-context who context-or-encrypt-method client?
                     #:need-unsealed? [need-unsealed? #f])
  (cond
    [(ssl-context? context-or-encrypt-method)
     (values context-or-encrypt-method (extract-ctx who need-unsealed? context-or-encrypt-method))]
    [(and client? (eq? context-or-encrypt-method 'secure))
     (define mzctx (ssl-secure-client-context))
     (values mzctx (ssl-context-ctx mzctx))]
    [else
     (values #f (make-raw-context who context-or-encrypt-method client?))]))

(define (get-context/listener who ssl-context-or-listener [fail? #t]
                              #:need-unsealed? [need-unsealed? #f])
  (cond
   [(ssl-context? ssl-context-or-listener)
    (extract-ctx who need-unsealed? ssl-context-or-listener)]
   [(ssl-listener? ssl-context-or-listener)
    (extract-ctx who need-unsealed? (ssl-listener-mzctx ssl-context-or-listener))]
   [else
    (if fail?
        (raise-argument-error who
                              "(or/c ssl-context? ssl-listener?)"
                              ssl-context-or-listener)
        #f)]))

(define (extract-ctx who need-unsealed? mzctx)
  (when (and need-unsealed? (ssl-context-sealed? mzctx))
    (error who "context is sealed; no further changes are allowed"))
  (ssl-context-ctx mzctx))

(define (ssl-seal-context! mzctx)
  (set-ssl-context-sealed?! mzctx #t))

(define (ssl-server-context-enable-ecdhe! context [name 'secp521r1])
  (define (symbol->nid name)
    (cond [(assq name curve-nid-alist)
           => cdr]
          [else
           (error 'ssl-server-context-enable-ecdhe!
                  "bad curve name\n  curve name: ~e" name)]))
  (define ctx (extract-ctx 'ssl-server-context-enable-ecdhe! #t context))
  (define key (EC_KEY_new_by_curve_name (symbol->nid name)))
  (check-valid key 'ssl-server-context-enable-ecdhe! "enabling ECDHE")
  (unless (= 1 (SSL_CTX_ctrl ctx SSL_CTRL_SET_TMP_ECDH 0 key))
    (error 'ssl-server-context-enable-ecdhe! "enabling ECDHE"))
  (SSL_CTX_ctrl ctx SSL_CTRL_OPTIONS SSL_OP_SINGLE_ECDH_USE #f)
  (void))

(define (ssl-server-context-enable-dhe! context [ssl-dh4096-param ssl-dh4096-param-bytes])
  (define params (if (bytes? ssl-dh4096-param)
                     ssl-dh4096-param
                     (call-with-input-file* ssl-dh4096-param port->bytes)))
  (define params-bio (BIO_new_mem_buf params (bytes-length params)))
  (check-valid params-bio 'ssl-server-context-enable-dhe! "loading Diffie-Hellman parameters")
  (with-failure
    (lambda ()
      (BIO_free params-bio))
    (define ctx (extract-ctx 'ssl-server-context-enable-dhe! #t context))
    (define dh (PEM_read_bio_DHparams params-bio #f #f #f))
    (check-valid dh 'ssl-server-context-enable-dhe! "loading Diffie-Hellman parameters")
    (unless (= 1 (SSL_CTX_ctrl ctx SSL_CTRL_SET_TMP_DH 0 dh))
      (error 'ssl-server-context-enable-dhe! "failed to enable DHE"))
    (SSL_CTX_ctrl ctx SSL_CTRL_OPTIONS SSL_OP_SINGLE_DH_USE #f)
    (void)))

(define (ssl-load-... who load-it ssl-context-or-listener pathname
                      #:try? [try? #f])
  (let ([ctx (get-context/listener who ssl-context-or-listener
                                   #:need-unsealed? #t)])
    (let ([path 
           (path->complete-path (cleanse-path pathname)
                                (current-directory))])
      (security-guard-check-file who path '(read))
      (atomically ;; for to connect ERR_get_error to `load-it'
       (let ([n (load-it ctx path)])
         (unless (or (= n 1) try?)
           (error who "load failed from: ~e ~a"
                  pathname
                  (get-error-message (ERR_get_error)))))))))

(define (ssl-load-certificate-chain! ssl-context-or-listener pathname)
  (ssl-load-... 'ssl-load-certificate-chain! 
          SSL_CTX_use_certificate_chain_file
          ssl-context-or-listener pathname))

(define (ssl-load-suggested-certificate-authorities! ssl-listener pathname)
  (ssl-load-... 'ssl-load-suggested-certificate-authorities! 
          (lambda (ctx path)
            (let ([stk (SSL_load_client_CA_file path)])
              (if (ptr-equal? stk #f)
                  0
                  (begin
                    (SSL_CTX_set_client_CA_list ctx stk)
                    1))))
          ssl-listener pathname))

(define (ssl-load-private-key! ssl-context-or-listener pathname
                               [rsa? #t] [asn1? #f])
  (ssl-load-...
   'ssl-load-private-key!
   (lambda (ctx path)
     ((if rsa? SSL_CTX_use_RSAPrivateKey_file SSL_CTX_use_PrivateKey_file)
      ctx path
      (if asn1? SSL_FILETYPE_ASN1 SSL_FILETYPE_PEM)))
   ssl-context-or-listener pathname))

(define (ssl-load-verify-root-certificates! scl src)
  (ssl-load-... 'ssl-load-verify-root-certificates!
                (lambda (a b) (SSL_CTX_load_verify_locations a b #f))
                scl src))

(define (ssl-load-verify-source! context src #:try? [try? #f])
  (define (bad-source)
    (error 'ssl-load-verify-source!
           "internal error: bad source: ~e" src))
  (cond [(path-string? src)
         (ssl-load-... 'ssl-load-verify-source!
                       (lambda (a b) (SSL_CTX_load_verify_locations a b #f))
                       context src #:try? try?)]
        [(and (list? src) (= (length src) 2))
         (let ([tag (car src)]
               [val (cadr src)])
           (case tag
             [(directory)
              (ssl-load-... 'ssl-load-verify-source!
                            (lambda (a b) (SSL_CTX_load_verify_locations a #f b))
                            context val #:try? try?)]
             [(win32-store)
              (let ([ctx (get-context/listener 'ssl-load-verify-source! context
                                               #:need-unsealed? #t)])
                (load-win32-store 'ssl-load-verify-source!
                                               ctx val try?))]
             [(macosx-keychain)
              (let ([ctx (get-context/listener 'ssl-load-verify-source! context
                                               #:need-unsealed? #t)])
                (load-macosx-keychain 'ssl-load-verify-source! ctx val try?))]
             [else (bad-source)]))]
        [else (bad-source)]))

(define (ssl-load-default-verify-sources! ctx)
  (maybe-complain-on-cert)
  (for ([src (in-list (ssl-default-verify-sources))])
    (ssl-load-verify-source! ctx src #:try? #t)))

(define (ssl-set-ciphers! context cipher-spec)
  (let* ([ctx (extract-ctx 'ssl-set-ciphers! #t context)]
         [result (SSL_CTX_set_cipher_list ctx cipher-spec)])
    (unless (= result 1)
      (error 'ssl-set-ciphers! "setting cipher list failed"))
    (void)))

(define (ssl-set-server-name-identification-callback! ssl-context proc)
  (let ([cb (lambda (ssl ad ptr)
	      (let ([ret (proc (SSL_get_servername ssl TLSEXT_NAMETYPE_host_name))])
		(if (ssl-server-context? ret)
		    (begin
		      (SSL_set_SSL_CTX ssl (extract-ctx 'callback #f ret))
		      SSL_TLSEXT_ERR_OK)
		    ; this isn't an error, it just means "no change necessary"
		    SSL_TLSEXT_ERR_NOACK)))])

    ; hold onto cb so that the garbage collector doesn't reclaim
    ; the function that openssl is holding onto.
    (set-ssl-server-context-sni-cb! ssl-context cb)

    (unless (= (SSL_CTX_callback_ctrl
		(extract-ctx 'ssl-set-server-name-identification-callback! #t ssl-context)
		SSL_CTRL_SET_TLSEXT_SERVERNAME_CB
		cb) 1)
	    (error 'ssl-set-server-name-identification-callback! "setting server name identification callback failed"))))

(define (ssl-set-server-alpn! ssl-context alpn-protos [allow-no-match? #t])
  (define server-alpns ;; Hash[Bytes => Nat], lower value is more preferred
    (for/hash ([alpn (in-list alpn-protos)] [priority (in-naturals)])
      (values (bytes->immutable-bytes alpn) priority)))
  (define (cb ssl out outlen in inlen0 arg)
    (define inlen (max 0 inlen0))
    (define inbuf (make-bytes inlen))
    (memcpy inbuf in inlen)
    (define chosen-offset
      (let loop ([offset 0] [best-offset #f] [best-priority +inf.0])
        (cond [(< offset inlen)
               (define plen (bytes-ref inbuf offset))
               (cond [(<= (+ offset 1 plen) inlen)
                      (define alpn (subbytes inbuf (+ offset 1) (+ offset 1 plen)))
                      (define priority (hash-ref server-alpns alpn #f))
                      (if (and priority (< priority best-priority))
                          (loop (+ offset 1 plen) offset priority)
                          (loop (+ offset 1 plen) best-offset best-priority))]
                     [else best-offset])]
              [else best-offset])))
    (cond [chosen-offset
           (ptr-set! out _pointer (ptr-add in (+ chosen-offset 1)))
           (ptr-set! outlen _byte (bytes-ref inbuf chosen-offset))
           SSL_TLSEXT_ERR_OK]
          [allow-no-match?
           SSL_TLSEXT_ERR_NOACK]
          [else
           SSL_TLSEXT_ERR_ALERT_FATAL]))
  (set-ssl-server-context-alpn-cb! ssl-context cb)
  (let ([ctx (extract-ctx 'ssl-set-server-alpn! #t ssl-context)])
    (unless (zero? (SSL_CTX_set_alpn_select_cb ctx cb #f))
      (error 'ssl-set-server-alpn! "setting server ALPN selection callback failed"))))

(define (ssl-set-verify-hostname! ssl-context on?)
  ;; to check not sealed:
  (void (extract-ctx 'ssl-set-verify-hostname! #t ssl-context))
  (set-ssl-context-verify-hostname?! ssl-context (and on? #t)))

(define (ssl-try-verify! ssl-context-or-listener-or-port on?)
  (do-ssl-set-verify! ssl-context-or-listener-or-port on?
                      'ssl-try-verify!
                      SSL_VERIFY_PEER))

(define (ssl-set-verify! ssl-context-or-listener-or-port on?)
  (do-ssl-set-verify! ssl-context-or-listener-or-port on?
                      'ssl-set-verify!
                      (bitwise-ior SSL_VERIFY_PEER
                                   SSL_VERIFY_FAIL_IF_NO_PEER_CERT)))

(define (do-ssl-set-verify! ssl-context-or-listener-or-port on? who mode)
  (cond
   [(get-context/listener who
                          ssl-context-or-listener-or-port
                          #f
                          #:need-unsealed? #t)
    => (lambda (ctx)
         ;; required by openssl. This is more for when calling i2d_SSL_SESSION/d2i_SSL_SESSION
         ;; for instance if we were saving sessions in a database etc... We aren't using that
         ;; so a generic session name should be fine.
         (let ([bytes #"racket"])
           (SSL_CTX_set_session_id_context ctx bytes (bytes-length bytes)))
         
         (SSL_CTX_set_verify ctx
                             (if on?
                                 mode
                                 SSL_VERIFY_NONE)
                             #f))]
   [else
    (let-values ([(mzssl input?) (lookup who ssl-context-or-listener-or-port)])
      (SSL_set_verify (mzssl-ssl mzssl)
                      (if on?
                          mode
                          SSL_VERIFY_NONE)
                      #f)
      (let ([bytes #"racket"])
        (SSL_set_session_id_context (mzssl-ssl mzssl) bytes (bytes-length bytes)))
      (when on? (renegotiate who mzssl)))]))

(define (renegotiate who mzssl)
  (define (check-err thunk) 
    (let loop ()
      (define-values (v err estr) (save-errors (thunk) (mzssl-ssl mzssl)))
      (when (negative? v)
        (cond
         [(= err SSL_ERROR_WANT_READ)
          (let ([n (pump-input-once mzssl #f)])
            (if (eq? n 0)
                (let ([out-blocked? (pump-output mzssl)])
                  (sync (mzssl-i mzssl) 
                        (if out-blocked?
                            (mzssl-o mzssl) 
                            never-evt))
                  (loop))
                (loop)))]
         [(= err SSL_ERROR_WANT_WRITE)
          (if (pump-output-once mzssl #f #f)
              (loop)
              (begin
                (sync (mzssl-o mzssl))
                (loop)))]
         [else
          (error who "failed: ~a" estr)]))))
  (check-err (lambda () (SSL_renegotiate (mzssl-ssl mzssl))))
  (check-err (lambda () (SSL_do_handshake (mzssl-ssl mzssl))))
  (when (mzssl-server? mzssl)
    ;; Really demanding a negotiation from the server
    ;; side requires a hacky little dance
    (cond
     [SSLv23_client_method
      ;; OpenSSL 1.0 (could be replaced with 1.1 version?)
      (when (positive? (ssl_struct-server
                        (cast (mzssl-ssl mzssl) _pointer _ssl_struct-pointer)))
        (set-ssl_struct-state! (cast (mzssl-ssl mzssl) _pointer _ssl_struct-pointer)
                               SSL_ST_ACCEPT)
        (check-err (lambda () (SSL_do_handshake (mzssl-ssl mzssl)))))]
     [else
      ;; OpenSSL 1.1:
      ;; The thread
      ;;  https://groups.google.com/forum/#!topic/mailing.openssl.dev/Dzuwxgq19Ew
      ;; concludes that using SSL_peek with 0 is good enough to trigger renegotiation.
      ;; The peek might not actually return 0, but the pending-renegotiation state
      ;; should end.
      (check-err (lambda () 
                   (define v (SSL_peek (mzssl-ssl mzssl) (make-bytes 1) 0))
                   (if (and (negative? v)
                            (zero? (SSL_renegotiate_pending (mzssl-ssl mzssl))))
                       0
                       v)))])))

;; ----

(define (secure-client-context! ctx)
  ;; Load root certificates
  (ssl-load-default-verify-sources! ctx)
  ;; Require verification
  (ssl-set-verify! ctx #t)
  (ssl-set-verify-hostname! ctx #t)
  ;; No weak cipher suites; see discussion, patch at http://bugs.python.org/issue13636
  (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
  ;; Seal context so further changes cannot weaken it
  (ssl-seal-context! ctx)
  (void))

;; context-cache: (list (weak-box ssl-client-context) (listof path-string) nat) or #f
;; Cache for (ssl-secure-client-context) and (ssl-make-client-context 'secure) (w/o key, cert).
(define context-cache #f)

(define (ssl-secure-client-context)
  (maybe-complain-on-cert)
  (let ([locs (ssl-default-verify-sources)])
    (define (reset)
      (let* ([now (current-seconds)]
             [ctx (make-client-context* 'secure #f #f)])
        (set! context-cache (list (make-weak-box ctx) locs now))
        ctx))
    (let* ([cached context-cache]
           [c-wb (and cached (car cached))]
           [c-ctx (and c-wb (weak-box-value c-wb))]
           [c-locs (and cached (cadr cached))]
           [c-time (and cached (caddr cached))])
      (cond [c-ctx
             ;; May reuse only if locations haven't changed
             ;; FIXME: ideally, should also check that no file in locs has changed since
             ;;   c-time, but don't want to hit the filesystem so often
             (cond [(equal? locs c-locs) c-ctx]
                   [else (reset)])]
            [else (reset)]))))

(define (ssl-set-keylogger! ssl-context logger)
  (define ctx
    (extract-ctx 'ssl-set-keylogger! #t ssl-context))
  (cond
    [logger
     (define (cb _ssl line)
       (log-message logger 'debug 'openssl-keylogger "callback" line))
     (set-ssl-context-keylog-cb! ssl-context cb)
     (SSL_CTX_set_keylog_callback ctx cb)]
    [else
     (set-ssl-context-keylog-cb! ssl-context #f)
     (SSL_CTX_set_keylog_callback ctx #f)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSL ports

(define (mzssl-release mzssl)
  ;; Lock must be held
  (set-mzssl-refcount! mzssl (sub1 (mzssl-refcount mzssl)))
  (when (zero? (mzssl-refcount mzssl))
    (SSL_free (mzssl-ssl mzssl))
    (when (mzssl-close-original? mzssl)
      (close-input-port (mzssl-i mzssl))
      (close-output-port (mzssl-o mzssl)))))

(define (in-ready-evt mzssl)
  (define i (mzssl-i mzssl))
  ;; Using `i` to indicate that more input is available will usually
  ;; work, but it's possible for more input to become available and
  ;; then concurrently pumped in, after which `i` may not be ready
  ;; again. We can use progress evts if `i` supports that, otherwise
  ;; make our own progress evt.
  (choice-evt i
              (or (and (port-provides-progress-evts? i)
                       (port-progress-evt i))
                  (mzssl-in-progress-sema mzssl)
                  (let ([s (make-semaphore)])
                    (set-mzssl-in-progress-sema! mzssl s)
                    s))))

(define (in-progress! mzssl)
  (define s (mzssl-in-progress-sema mzssl))
  (when s
    (semaphore-post s)
    (set-mzssl-in-progress-sema! mzssl #f)))

(define (out-ready-evt mzssl)
  ;; Similar to `in-ready-evt`, we may need a kind of
  ;; progress evt to wait on:
  (choice-evt (mzssl-o mzssl)
              (or (mzssl-out-progress-sema mzssl)
                  (let ([s (make-semaphore)])
                    (set-mzssl-out-progress-sema! mzssl s)
                    s))))

(define (out-progress! mzssl)
  (define s (mzssl-out-progress-sema mzssl))
  (when s
    (semaphore-post s)
    (set-mzssl-out-progress-sema! mzssl #f)))

(define (pump-input-once mzssl need-progress?/out)
  (let ([buffer (mzssl-buffer mzssl)]
        [i (mzssl-i mzssl)]
        [r-bio (mzssl-r-bio mzssl)])
    (let ([n ((if (and need-progress?/out 
                       (not (output-port? need-progress?/out)))
                  read-bytes-avail!
                  read-bytes-avail!*)
              buffer i)])
      (cond
       [(eof-object? n) 
        (BIO_set_mem_eof_return r-bio 0)
        (in-progress! mzssl)
        eof]
       [(zero? n)
        (when need-progress?/out
          (sync need-progress?/out i))
        0]
       [else 
        (let ([m (BIO_write r-bio buffer n)])
          (unless (= m n)
            ((mzssl-error mzssl) 'pump-input-once "couldn't write all bytes to BIO!"))
          (in-progress! mzssl)
          m)]))))

(define (pump-output-once mzssl need-progress? output-blocked-result)
  (let ([buffer (mzssl-buffer mzssl)]
        [pipe-r (mzssl-pipe-r mzssl)]
        [pipe-w (mzssl-pipe-w mzssl)]
        [o (mzssl-o mzssl)]
        [w-bio (mzssl-w-bio mzssl)])
    (let ([n (peek-bytes-avail!* buffer 0 #f pipe-r)])
      (if (zero? n)
          (let ([n (BIO_read w-bio buffer (bytes-length buffer))])
            (if (n . <= . 0)
                (begin
                  (when need-progress?
                    ((mzssl-error mzssl) 'pump-output-once "no output to pump!"))
                  #f)
                (begin
                  (write-bytes buffer pipe-w 0 n)
                  (pump-output-once mzssl need-progress? output-blocked-result))))
          (let ([n ((if need-progress? write-bytes-avail write-bytes-avail*) buffer o 0 n)])
            (if (zero? n)
                output-blocked-result
                (begin
                  (out-progress! mzssl)
                  (port-commit-peeked n (port-progress-evt pipe-r) always-evt pipe-r)
                  #t)))))))

;; result is #t if there's more data to send out the
;;  underlying output port, but the port is full
(define (pump-output mzssl)
  (let ([v (pump-output-once mzssl #f 'blocked)])
    (if (eq? v 'blocked)
        #t
        (and v
             (pump-output mzssl)))))

(define (make-ssl-input-port mzssl)
  ;; If SSL_read produces NEED_READ or NEED_WRITE, then the next
  ;;  call to SSL_read must use the same arguments.
  ;; Use xfer-buffer so we have a consistent buffer to use with
  ;;  SSL_read across calls to the port's write function.
  (let-values ([(xfer-buffer) (make-bytes BUFFER-SIZE)]
         [(got-r got-w) (make-pipe)]
         [(must-read-len) #f])
    (make-input-port/read-to-peek
     (format "SSL ~a" (object-name (mzssl-i mzssl)))
     ;; read proc:
     (letrec ([do-read
         (lambda (buffer)
           (let ([len (or must-read-len (min (bytes-length xfer-buffer)
                                             (bytes-length buffer)))])
             (let-values ([(n err estr) (save-errors
                                               (SSL_read (mzssl-ssl mzssl) xfer-buffer len)
                                               (mzssl-ssl mzssl))])
               (if (n . >= . 1)
                   (begin
                     (set! must-read-len #f)
                     (if must-read-len
                         ;; If we were forced to try to read a certain amount,
                         ;; then we may have read too much for the immediate
                         ;; request.
                         (let ([orig-n (bytes-length buffer)])
                           (bytes-copy! buffer 0 xfer-buffer 0 (min n orig-n))
                           (when (n . > . orig-n)
                             (write-bytes buffer got-w orig-n n)))
                         (bytes-copy! buffer 0 xfer-buffer 0 n))
                     n)
                   (let ()
                     (cond
                      [(or (= err SSL_ERROR_ZERO_RETURN)
                           (and (= err SSL_ERROR_SYSCALL) (zero? n)))
                       ;; We hit the end-of-file
                       (set! must-read-len #f)
                       eof]
                      [(= err SSL_ERROR_WANT_READ)
                       (when enforce-retry?
                         (set! must-read-len len))
                       (let ([n (pump-input-once mzssl #f)])
                         (if (eq? n 0)
                             (let ([out-blocked? (pump-output mzssl)])
                               (when enforce-retry?
                                 (set-mzssl-must-read! mzssl (make-semaphore)))
                               (wrap-evt (choice-evt
                                          (in-ready-evt mzssl)
                                          (if out-blocked?
                                              (out-ready-evt mzssl)
                                              never-evt))
                                         (lambda (x) 0)))
                             (do-read buffer)))]
                      [(= err SSL_ERROR_WANT_WRITE)
                       (when enforce-retry?
                         (set! must-read-len len))
                       (if (pump-output-once mzssl #f #f)
                           (do-read buffer)
                           (begin
                             (when enforce-retry?
                               (set-mzssl-must-read! mzssl (make-semaphore)))
                             (wrap-evt (out-ready-evt mzssl) (lambda (x) 0))))]
                      [else
                       (set! must-read-len #f)
                       ((mzssl-error mzssl) 'read-bytes 
                                            "SSL read failed ~a"
                                            estr)]))))))]
        [top-read
         (lambda (buffer)
           (cond
            [(mzssl-flushing? mzssl)
             ;; Flush in progress; try again later:
             0]
            [(mzssl-must-write mzssl)
             => (lambda (sema)
                  (wrap-evt (semaphore-peek-evt sema) (lambda (x) 0)))]
            [(mzssl-r-closed? mzssl)
             0]
            [else
             (let ([sema (mzssl-must-read mzssl)])
               (when sema
                 (set-mzssl-must-read! mzssl #f)
                 (semaphore-post sema)))
             ;; First, try pipe for previously read data:
             (let ([n (read-bytes-avail!* buffer got-r)])
               (if (zero? n)
                   ;; Nothing already read, so use SSL_read:
                   (do-read buffer)
                   ;; Got previously read data:
                   n))]))]
        [lock-unavailable
         (lambda () (wrap-evt (semaphore-peek-evt (mzssl-lock mzssl))
                              (lambda (x) 0)))])
       (lambda (buffer)
         (call-with-semaphore
          (mzssl-lock mzssl)
          top-read
          lock-unavailable
          buffer)))
     ;; fast peek:
     #f
     ;; close proc:
     (lambda ()
       (call-with-semaphore
        (mzssl-lock mzssl)
        (lambda ()
          (unless (mzssl-r-closed? mzssl)
            (set-mzssl-r-closed?! mzssl #t)
            (mzssl-release mzssl))))))))

(define (flush-ssl mzssl enable-break?)
  ;; Make sure that this SSL connection has said everything that it
  ;; wants to say --- that is, move data from the SLL output to the
  ;; underlying output port. Depending on the transport, the other end
  ;; may be stuck trying to tell us something before it will listen, 
  ;; so we also have to read in any available information.
  (let loop ()
    (let ([v (pump-input-once mzssl #f)])
      (if (and (number? v) (positive? v))
          ;; Received some input, so start over
          (loop)
          ;; Try sending output
          (let ([v (pump-output-once mzssl #f 'blocked)])
            ;; If we sent something, continue tring in case there's more.
            ;; Further, if we blocked on the underlying output, then
            ;; wait until either input or output is ready:
            (when v
        (when (eq? v 'blocked)
          ((if enable-break? sync/enable-break sync) (mzssl-o mzssl) (mzssl-i mzssl)))
        (loop)))))))

(define (kernel-thread thunk)
  ;; Run with a custodian that is managed directly by the root custodian:
  (parameterize ([current-custodian (make-custodian-at-root)])
    (thread thunk)))

(define (make-ssl-output-port mzssl)
  ;; If SSL_write produces NEED_READ or NEED_WRITE, then the next
  ;;  call to SSL_write must use the same arguments.
  ;; Use xfer-buffer so we have a consistent buffer to use with
  ;;  SSL_write across calls to the port's write function.
  (let ([xfer-buffer (make-bytes BUFFER-SIZE)]
        [buffer-mode (or (file-stream-buffer-mode (mzssl-o mzssl)) 'block)]
        [flush-ch (make-channel)]
        [must-write-len #f])
    ;; This thread mkoves data from the SLL stream to the underlying
    ;; output port, because this port's write prodcue claims that the
    ;; data is flushed if it gets into the SSL stream. In other words,
    ;; this flushing thread is analogous to the OS's job of pushing
    ;; data from a socket through the actual network device. It therefore
    ;; runs with the highest possible custodian:
    (kernel-thread (lambda ()
               (let loop ()
                 (sync flush-ch)
                 (semaphore-wait (mzssl-lock mzssl))
                 (flush-ssl mzssl #f)
                 (semaphore-post (mzssl-flushing? mzssl))
                 (set-mzssl-flushing?! mzssl #f)
                 (semaphore-post (mzssl-lock mzssl))
                 (loop))))
    ;; Create the output port:
    (make-output-port
     (format "SSL ~a" (object-name (mzssl-o mzssl)))
     (mzssl-o mzssl)
     ;; write proc:
     (letrec ([do-write
         (lambda (len non-block? enable-break?)
           (let ([out-blocked? (pump-output mzssl)])
             (if (zero? len)
                 ;; Flush request; all data is in the SSL
                 ;;  stream, but make sure it's gone
                 ;;  through the ports:
                 (begin
                   (flush-ssl mzssl enable-break?)
                   0)
                 ;; Write request; even if blocking is ok, we treat
                 ;;  it as non-blocking and let Racket handle blocking
                 (let-values ([(n err estr) (save-errors (SSL_write (mzssl-ssl mzssl) xfer-buffer len)
                                                               (mzssl-ssl mzssl))])
                   (if (n . > . 0)
                       (begin
                         (set! must-write-len #f)
                         ;; Start flush as necessary:
                         (cond
                          [non-block?
                           ;; We can't block, so start the background thread
                           ;;  to flush from SSL to the underlying ports:
                           (set-mzssl-flushing?! mzssl (make-semaphore))
                           (channel-put flush-ch #t)]
                          [else
                           ;; We're allowed to block, and things seem to
                           ;;  work better if we, try to flush all the way
                           ;;  through (even though we're allowed to buffer):
                           (flush-ssl mzssl enable-break?)])
                         n)
                       (let ()
                         (cond
                          [(= err SSL_ERROR_WANT_READ)
                                 (when enforce-retry?
                                   (set! must-write-len len))
                           (let ([n (pump-input-once mzssl #f)])
                             (if (eq? n 0)
                                 (let ([out-blocked? (pump-output mzssl)])
                                         (when enforce-retry?
                                           (set-mzssl-must-write! mzssl (make-semaphore)))
                                   (wrap-evt (choice-evt
                                              (in-ready-evt mzssl)
                                              (if out-blocked?
                                                  (out-ready-evt mzssl)
                                                  never-evt))
                                             (lambda (x) #f)))
                                 (do-write len non-block? enable-break?)))]
                          [(= err SSL_ERROR_WANT_WRITE)
                                 (when enforce-retry?
                                   (set! must-write-len len))
                           (if (pump-output-once mzssl #f #f)
                               (do-write len non-block? enable-break?)
                                     (let ([n (pump-input-once mzssl #f)])
                                       (if (positive? n)
                                           (do-write len non-block? enable-break?)
                                           (begin
                                             (when enforce-retry?
                                               (set-mzssl-must-write! mzssl (make-semaphore)))
                                             (wrap-evt (out-ready-evt mzssl) (lambda (x) #f))))))]
                          [else
                           (set! must-write-len #f)
                           ((mzssl-error mzssl) 'write-bytes 
                                  "SSL write failed ~a"
                                  estr)])))))))]
        [top-write
         (lambda (buffer s e non-block? enable-break?)
           (cond
            [(mzssl-flushing? mzssl)
             ;; Need to wait until flush done
             (if (= s e)
                 ;; Let the background flush finish:
                 (list (semaphore-peek-evt (mzssl-flushing? mzssl)))
                 ;; Try again later:
                 (wrap-evt always-evt (lambda (v) #f)))]
            [(mzssl-w-closed? mzssl)
             #f]
            [(mzssl-must-read mzssl)
             ;; Read pending, so wait until it's done:
             => (lambda (sema)
                  (wrap-evt (semaphore-peek-evt sema) (lambda (x) #f)))]
            [else
             ;; Normal write (since no flush is active or read pending):
             (let ([sema (mzssl-must-write mzssl)])
               (when sema
                 (set-mzssl-must-write! mzssl #f)
                 (semaphore-post sema)))
             (let ([len (min (- e s) (bytes-length xfer-buffer))])
               (if must-write-len
                   ;; Previous SSL_write result obligates certain output:
                   (begin
                     (unless (and (len . >= . must-write-len)
                                  (bytes=? (subbytes xfer-buffer 0 must-write-len)
                                           (subbytes buffer s (+ s must-write-len))))
                       ((mzssl-error mzssl) 'write-bytes 
                              "SSL output request: ~e different from previous unsatisfied request: ~e"
                              (subbytes buffer s e)
                              (subbytes xfer-buffer 0 must-write-len)))
                     (do-write must-write-len non-block? enable-break?))
                   ;; No previous write obligation:
                   (begin
                     (bytes-copy! xfer-buffer 0 buffer s (+ s len))
                     (do-write len non-block? enable-break?))))]))]
        [lock-unavailable
         (lambda () (wrap-evt (semaphore-peek-evt (mzssl-lock mzssl))
                              (lambda (x) #f)))])
       (lambda (buffer s e non-block? enable-break?)
         (let ([v (call-with-semaphore
             (mzssl-lock mzssl)
             top-write
             lock-unavailable
             buffer s e non-block? enable-break?)])
           (if (pair? v)
         (begin
           ;; Wait on background flush to implement requested flush
           (sync (car v))
           0)
         v))))
     ;; close proc:
     (letrec ([do-close
         (lambda ()
           (cond
            [(mzssl-flushing? mzssl)
             (semaphore-peek-evt (mzssl-flushing? mzssl))]
            [(mzssl-w-closed? mzssl)
             #f]
            [else
             ;; issue shutdown (i.e., EOF on read end)
             (when (mzssl-shutdown-on-close? mzssl)
               (let loop ([cnt 0])
                 (let ()
                         (flush-ssl mzssl #f)
                   (let-values ([(n err estr) (save-errors (SSL_shutdown (mzssl-ssl mzssl))
                                                                 (mzssl-ssl mzssl))])
                           
                           (if (= n 1)
                               (flush-ssl mzssl #f)
                               (cond
                                [(= err SSL_ERROR_WANT_READ)
                                 (let ([out-blocked? (pump-output mzssl)])
                                   (pump-input-once mzssl (if out-blocked? (mzssl-o mzssl) #t)))
                                 (loop cnt)]
                                [(= err SSL_ERROR_WANT_WRITE)
                                 (pump-output-once mzssl #t #f)
                                 (loop cnt)]
                                [else
                                 (if (= n 0)
                                     ;; When 0 is returned, a shutdown depends on
                                     ;; input from the peer. If we've already tried twice,
                                     ;; wait for some input and try again.
                                     (begin
                                       (when (cnt . >= . 2)
                                         (pump-input-once mzssl #t))
                                       (loop (add1 cnt)))
                                     ((mzssl-error mzssl) 'read-bytes 
                                      "SSL shutdown failed ~a"
                                      estr))]))))))
             (set-mzssl-w-closed?! mzssl #t)
             (mzssl-release mzssl)
             #f]))]
        [close-loop
         (lambda ()
           (let ([v (call-with-semaphore
                     (mzssl-lock mzssl)
                     do-close)])
             (if v
                 (begin
                   ;; Wait for background flush to finish:
                   (sync v)
                   (close-loop))
                 v)))])
       (lambda ()
         (close-loop)))
     ;; Unimplemented port methods:
     #f #f #f #f
     void 1
     ;; Buffer mode proc:
     (case-lambda
      [() buffer-mode]
      [(mode)
       (set! buffer-mode mode)
       (define o (mzssl-o mzssl))
       (when (tcp-port? o)
         (file-stream-buffer-mode o mode))]))))

(define (ports->ssl-ports i o 
                          #:context [context #f]
                          #:encrypt [encrypt default-encrypt]
                          #:mode [mode 'connect]
                          #:close-original? [close-original? #f]
                          #:shutdown-on-close? [shutdown-on-close? #f]
                          #:error/ssl [error/ssl error]
                          #:hostname [hostname #f]
                          #:alpn [alpn null])
  (wrap-ports 'port->ssl-ports i o (or context encrypt) mode
              close-original? shutdown-on-close? error/ssl
              hostname alpn))

(define (create-ssl who context-or-encrypt-method connect/accept error/ssl)
  (define connect?
    (case connect/accept
      [(connect) #t]
      [(accept) #f]
      [else (error who "internal error: bad connect/accept: ~e" connect/accept)]))
  (unless (or (symbol? context-or-encrypt-method)
              (if connect?
                  (ssl-client-context? context-or-encrypt-method)
                  (ssl-server-context? context-or-encrypt-method)))
    (error who
           "'~a mode requires a ~a context, given: ~e"
           (if connect? 'connect 'accept)
           (if connect? "client" "server")
           context-or-encrypt-method))
  (atomically ;; connect functions to subsequent check-valid (ie, ERR_get_error)
   (let-values ([(mzctx ctx) (get-context who context-or-encrypt-method connect?)])
     (check-valid ctx who "context creation")
     (with-failure
      (lambda () (when (and ctx
                       (need-ctx-free? context-or-encrypt-method))
                   (SSL_CTX_free ctx)))
      (let ([r-bio (BIO_new (BIO_s_mem))]
            [w-bio (BIO_new (BIO_s_mem))]
            [free-bio? #t])
        (with-failure
         (lambda () (when free-bio?
                      (BIO_free r-bio)
                      (BIO_free w-bio)))
         (let ([ssl (SSL_new ctx)])
           (check-valid ssl who "ssl setup")
           ;; ssl has a ref count on ctx, so release:
           (when (need-ctx-free? context-or-encrypt-method)
             (SSL_CTX_free ctx)
             (set! ctx #f))
           (with-failure
            (lambda () (SSL_free ssl))
            (SSL_set_bio ssl r-bio w-bio)
            ;; ssl has r-bio & w-bio (no ref count?), so drop it:
            (set! free-bio? #f)
            (values ssl mzctx r-bio w-bio connect?)))))))))

(define (wrap-ports who i o context-or-encrypt-method connect/accept
                    close? shutdown-on-close? error/ssl
                    hostname alpn)
  ;; Create the SSL connection:
  (let-values ([(ssl mzctx r-bio w-bio connect?)
                (create-ssl who context-or-encrypt-method connect/accept error/ssl)]
               [(verify-hostname?)
                (cond [(ssl-context? context-or-encrypt-method)
                       (ssl-context-verify-hostname? context-or-encrypt-method)]
                      [else (eq? context-or-encrypt-method 'secure)])])
    (define verify? (not (zero? (bitwise-and (SSL_get_verify_mode ssl) SSL_VERIFY_PEER))))
    (when verify-hostname?
      (unless hostname
        (error/ssl who "~a failed (hostname not provided for verification)"
                   (if connect? "connect" "accept"))))
    (when (string? hostname)
      (SSL_set_tlsext_host_name ssl hostname)
      (when (and verify? verify-hostname?)
        ;; If verify? and verify-hostname? are true, then let OpenSSL
        ;; do hostname verification automatically during negotiation.
        (SSL_set1_host ssl hostname)))
    (when (pair? alpn)
      (unless (eq? connect/accept 'connect)
        (error who "#:alpn argument is supported only in connect mode~a"
               ";\n for accept mode, use `ssl-set-server-alpn!`"))
      (define proto-list
        (apply bytes-append (for/list ([proto (in-list alpn)])
                              (bytes-append (bytes (bytes-length proto)) proto))))
      (unless (zero? (SSL_set_alpn_protos ssl proto-list))
        (error who "failed setting ALPN protocol list")))

    ;; connect/accept:
    (let-values ([(buffer) (make-bytes BUFFER-SIZE)]
           [(pipe-r pipe-w) (make-pipe)])
      (let ([mzssl (make-mzssl ssl mzctx i o r-bio w-bio pipe-r pipe-w
                               buffer (make-semaphore 1)
                               #f #f
                               #f #f #f 2
                               close? shutdown-on-close?
                               error/ssl
                               (eq? connect/accept 'accept)
                               #f #f)])
        (let loop ()
          (let-values ([(status err estr) (save-errors (if connect?
                                                           (SSL_connect ssl)
                                                           (SSL_accept ssl))
                                                       ssl)])
            (let ([out-blocked? (pump-output mzssl)])
        (when (status . < . 1)
          (let ()
            (cond
             [(= err SSL_ERROR_WANT_READ)
              (let ([n (pump-input-once mzssl (if out-blocked? o #t))])
                (when (eof-object? n)
                  (error/ssl who "~a failed (input terminated prematurely)"
                                   (if connect? "connect" "accept"))))
              (loop)]
             [(= err SSL_ERROR_WANT_WRITE)
              (pump-output-once mzssl #t #f)
              (loop)]
             [else
              (error/ssl who "~a failed ~a"
                               (if connect? "connect" "accept")
                               estr)]))))))
        (when verify-hostname?
          ;; If verify? is true, then hostname already verified via SSL_set1_host.
          ;; Otherwise, check now.
          (unless verify?
            (unless (hostname-in-cert? hostname (SSL_get_peer_certificate ssl))
              (SSL_shutdown ssl) ;; FIXME: peer doesn't get failure alert
              (error/ssl who "~a failed (certificate not valid for hostname)"
                         (if connect? "connect" "accept")))))
        ;; Connection complete; make ports
        (values (register (make-ssl-input-port mzssl) mzssl #t)
          (register (make-ssl-output-port mzssl) mzssl #f))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSL port registry

(define ssl-ports (make-weak-hasheq))

(define (register port mzssl input?)
  (hash-set! ssl-ports port (make-ephemeron port (cons mzssl input?)))
  port)

(define (lookup who port)
  (let ([v (hash-ref ssl-ports port #f)])
    (unless v
      (error who "internal error: not an SSL port or listener: ~e" port))
    (let ([p (ephemeron-value v)])
      (values (car p) (cdr p)))))

(define (ssl-addresses p [port-numbers? #f])
  (let-values ([(mzssl input?) (lookup 'ssl-addresses p)])
    (define port
      (if (eq? 'listener input?)
          (ssl-listener-l mzssl)
          (if input? (mzssl-i mzssl) (mzssl-o mzssl))))
    (cond [(or (tcp-port? port) (tcp-listener? port))
           (tcp-addresses port port-numbers?)]
          [else (error 'ssl-addresses "not connected to TCP port")])))

(define (ssl-abandon-port p)
  (let-values ([(mzssl input?) (lookup 'ssl-abandon-port p)])
    (cond
     [(output-port? p)
      (set-mzssl-shutdown-on-close?! mzssl #f)
      ;; Call close-output-port to flush, shutdown, and decrement mzssl refcount.
      (close-output-port p)]
     [else
      (close-input-port p)])))

(define (ssl-peer-verified? p)
  (let-values ([(mzssl input?) (lookup 'ssl-peer-verified? p)])
    (and (eq? X509_V_OK (SSL_get_verify_result (mzssl-ssl mzssl)))
         (SSL_get_peer_certificate (mzssl-ssl mzssl))
         #t)))

(define (ssl-peer-subject-name p)
  (let ([cert (ssl-port->cert 'ssl-peer-subject-name p)])
    (if cert
        (let ([bytes (make-bytes 1024 0)])
          (X509_NAME_oneline (X509_get_subject_name cert) bytes (bytes-length bytes)))
        #f)))

(define (ssl-peer-issuer-name p)
  (let ([cert (ssl-port->cert 'ssl-peer-subject-name p)])
    (if cert
        (let ([bytes (make-bytes 1024 0)])
          (X509_NAME_oneline (X509_get_issuer_name cert) bytes (bytes-length bytes)))
        #f)))

;; ssl-peer-certificate-hostnames : ssl-port -> (listof string)
(define (ssl-peer-certificate-hostnames p)
  (let ([cert (ssl-port->cert 'ssl-peer-certificate-hostnames p)])
    (if cert (cert->names cert) null)))

;; ssl-peer-check-hostname : ssl-port string -> boolean
(define (ssl-peer-check-hostname p hostname)
  (let ([cert (ssl-port->cert 'ssl-peer-check-hostname p)])
    (hostname-in-cert? hostname cert)))

;; ssl-port->cert : symbol ssl-port -> Cert/#f
(define (ssl-port->cert who p)
  (let-values ([(mzssl _input?) (lookup who p)])
    (SSL_get_peer_certificate (mzssl-ssl mzssl))))

;; hostname-in-cert? : string Cert -> boolean
(define (hostname-in-cert? hostname cert)
  (= 1 (X509_check_host cert (string->bytes/latin-1 hostname) 0)))

(define (cert->names cert)
  ;; RFC 2818 (section 3.1) says use subjectAltName dNSName extensions
  ;; if present, else use final commonName entry.
  (let ([names (cert->altnames cert)])
    (cond [(pair? names) names]
          [else (let ([name (cert->name cert)])
                  (if name (list name) null))])))

(define (cert->name cert)
  ;; Returns commonName DNS name if exists, #f otherwise.
  (let* ([name (X509_get_subject_name cert)]
         [last-cn-index
          (let loop ([i -1])
            (let ([next (X509_NAME_get_index_by_NID name NID_commonName i)])
              (cond [(>= next 0) (loop next)]
                    [else i])))])
    (cond [(< last-cn-index 0) #f]
          [else
           (let* ([entry (X509_NAME_get_entry name last-cn-index)]
                  [asn1str (X509_NAME_ENTRY_get_data entry)])
             (asn1string->bytes asn1str))])))

(define (asn1string->bytes asn1str)
  (let* ([len (ASN1_STRING_length asn1str)]
         [data (ASN1_STRING_data asn1str)]
         [buf (make-bytes len 0)])
    (memcpy buf data len)
    ;; FIXME: detect UTF-8 strings?
    (bytes->string/latin-1 buf)))

(define (cert->altnames cert)
  ;; Returns list of DNS names in subjectAltName extension
  ;; FIXME: also return IP addresses?
  ;; Reference: curl-7.28.0/lib/ssluse.c verifyhost()
  ;;  from http://www.mail-archive.com/openssl-users@openssl.org/msg39142.html
  (let* ([namestack (X509_get_ext_d2i cert NID_subject_alt_name #f #f)]
         [names
          (reverse
           (for/fold ([acc null])
               ([i (in-range (if namestack (sk_num namestack) 0))])
             (let ([gn (sk_GENERAL_NAME_value namestack i)])
               (cond [(= (GENERAL_NAME-type gn) GEN_DNS)
                      (let* ([asn1str (GENERAL_NAME-d gn)])
                        (cons (asn1string->bytes asn1str) acc))]
                     [else acc]))))])
    (when namestack (sk_pop_free namestack GENERAL_NAME_free))
    names))

(define (ssl-default-channel-binding p)
  ;; Reference: RFC 9266 (https://datatracker.ietf.org/doc/html/rfc9266), Section 3
  (define who 'ssl-channel-binding)
  (define-values (mzssl _in?) (lookup 'ssl-channel-binding p))
  (define ssl (mzssl-ssl mzssl))
  (define tlsver (SSL_version ssl))
  (cond [(>= tlsver TLS1_3_VERSION)
         (list 'tls-exporter (ssl-channel-binding p 'tls-exporter))]
        [else
         (list 'tls-unique (ssl-channel-binding p 'tls-unique))]))

(define (ssl-channel-binding p type)
  ;; Reference: https://tools.ietf.org/html/rfc5929
  (define who 'ssl-channel-binding)
  (define-values (mzssl _in?) (lookup 'ssl-channel-binding p))
  (define ssl (mzssl-ssl mzssl))
  (define tlsver (SSL_version ssl))
  (case type
    [(tls-exporter)
     ;; Reference: RFC 9266 (https://datatracker.ietf.org/doc/html/rfc9266)
     ;; Only ok for TLS 1.3 and older TLS with Extended Master Secret extension!
     (unless (or (>= tlsver TLS1_3_VERSION)
                 (= (SSL_get_extms_support ssl) 1))
       (error who "tls-exporter channel binding undefined~a"
              ";\n requires TLS 1.3 or Extended Master Secret extension"))
     (define buf (make-bytes 32))
     (define label #"EXPORTER-Channel-Binding")
     (define context #"")
     (define r (SSL_export_keying_material ssl buf label context))
     (cond [(= r 1) buf]
           [else (error who "failed getting Exported Keying Material")])]
    [(tls-unique)
     (unless (< tlsver TLS1_3_VERSION)
       (error who "tls-unique channel binding is undefined for TLS 1.3"))
     (define MAX_FINISH_LEN 50) ;; usually 12 bytes, but be cautious (see RFC 5246 7.4.9)
     (define get-finished ;; assumes no session resumption
       (cond [(mzssl-server? mzssl) SSL_get_peer_finished]
             [else SSL_get_finished]))
     (define buf (make-bytes MAX_FINISH_LEN))
     (define r (get-finished ssl buf (bytes-length buf)))
     (cond [(zero? r) (error who "unable to get TLS Finished message")]
           [(< 0 r MAX_FINISH_LEN) (subbytes buf 0 r)]
           [else (error who "internal error: TLS Finished message too large")])]
    [(tls-server-end-point)
     (define x509 ;; ownership varies, don't free
       (cond [(mzssl-server? mzssl) (SSL_get_certificate ssl)]
             [else (SSL_get_peer_certificate ssl)]))
     (unless x509 (error who "failed to get server certificate"))
     (define sig-nid (X509_get_signature_nid x509))
     (define hash-nid (OBJ_find_sigid_algs sig-nid))
     (define hash-evp ;; change md5, sha1 to sha256, per RFC 5929 4.1
       (cond [(or (= hash-nid NID_md5) (= hash-nid NID_sha1)) (EVP_sha256)]
             [(= hash-nid NID_sha224) (EVP_sha224)]
             [(= hash-nid NID_sha256) (EVP_sha256)]
             [(= hash-nid NID_sha384) (EVP_sha384)]
             [(= hash-nid NID_sha512) (EVP_sha512)]
             [else (error who "unsupported digest in certificate")]))
     (define buflen (EVP_MD_size hash-evp))
     (unless (> buflen 0) (error who "internal error: bad digest length"))
     (define buf (make-bytes buflen))
     (define r (X509_digest x509 hash-evp buf buflen))
     (if (> r 0) buf (error who "internal error: certificate digest failed"))]))

(define (ssl-protocol-version p)
  (define-values (mzssl _in?) (lookup 'ssl-protocol-version p))
  (define ssl (mzssl-ssl mzssl))
  (define tlsver (SSL_version ssl))
  (cond [(= tlsver TLS1_3_VERSION) 'tls13]
        [(= tlsver TLS1_2_VERSION) 'tls12]
        [(= tlsver TLS1_1_VERSION) 'tls11]
        [(= tlsver TLS1_VERSION) 'tls]
        [(= tlsver SSL3_VERSION) 'sslv3]
        [else #f]))

(define (ssl-get-alpn-selected p)
  (define-values (mzssl _in?) (lookup 'ssl-get-alpn-selected p))
  (define ssl (mzssl-ssl mzssl))
  (SSL_get0_alpn_selected ssl))

(define (ssl-port? v)
  (and (hash-ref ssl-ports v #f) #t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSL listen

(define (ssl-listen port-k
                    [queue-k 5] [reuse? #f] [hostname-or-#f #f]
                    [protocol-symbol-or-context default-encrypt])
  (let* ([ctx (if (ssl-server-context? protocol-symbol-or-context)
               protocol-symbol-or-context
               (make-context 'ssl-listen protocol-symbol-or-context #f #f #f))]
        [l (tcp-listen port-k queue-k reuse? hostname-or-#f)]
        [ssl-l (make-ssl-listener l ctx)])
    (register ssl-l ssl-l 'listener)))

(define (ssl-close l)
  (tcp-close (ssl-listener-l l)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSL accept

(define (do-ssl-accept who tcp-accept ssl-listener)
  (let-values ([(i o) (tcp-accept (ssl-listener-l ssl-listener))])
    ;; Obviously, there's a race condition between accepting the
    ;; connections and installing the exception handler below. However,
    ;; if breaks are enabled, then i and o could get lost between
    ;; the time that tcp-accept returns and `i' and `o' are bound,
    ;; anyway. So we can assume that breaks are enabled without loss
    ;; of (additional) resources.
    (with-handlers ([void (lambda (exn)
                            (close-input-port i)
                            (close-output-port o)
                            (raise exn))])
      (wrap-ports who i o (ssl-listener-mzctx ssl-listener) 'accept #t #f error/network #f null))))

(define (ssl-accept ssl-listener)
  (do-ssl-accept 'ssl-accept tcp-accept ssl-listener))

(define (ssl-accept/enable-break ssl-listener)
  (do-ssl-accept 'ssl-accept/enable-break tcp-accept/enable-break ssl-listener))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSL connect

(define (do-ssl-connect who tcp-connect hostname port-k client-context-or-protocol-symbol alpn)
  (let-values ([(i o) (tcp-connect hostname port-k)])
    ;; See do-ssl-accept for note on race condition here:
    (with-handlers ([void (lambda (exn)
                            (close-input-port i)
                            (close-output-port o)
                            (raise exn))])
      (wrap-ports who i o client-context-or-protocol-symbol 'connect #t #f error/network
                  hostname alpn))))

(define (ssl-connect hostname port-k
                     [client-context-or-protocol-symbol default-encrypt]
                     #:alpn [alpn null])
  (do-ssl-connect 'ssl-connect
                  tcp-connect
                  hostname
                  port-k
                  client-context-or-protocol-symbol
                  alpn))

(define (ssl-connect/enable-break hostname port-k
                                  [client-context-or-protocol-symbol default-encrypt]
                                  #:alpn [alpn null])
  (do-ssl-connect 'ssl-connect/enable-break
                  tcp-connect/enable-break
                  hostname
                  port-k
                  client-context-or-protocol-symbol
                  alpn))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization

(define ssl-available? (and libssl #t))


(when ssl-available?
  ;; Make sure only one place tries to initialize OpenSSL,
  ;; and wait in case some other place is currently initializing
  ;; it.
  (begin
    (start-atomic)
    (let* ([done (ptr-add #f 1)]
           [v (register-process-global #"OpenSSL-support-initializing" done)])
      (if v
          ;; Some other place is initializing:
          (begin
            (end-atomic)
            (let loop ()
              (unless (register-process-global #"OpenSSL-support-initialized" #f)
                (sleep 0.01) ;; busy wait! --- this should be rare
                (loop))))
          ;; This place must initialize:
          (begin
            (SSL_library_init)
            (SSL_load_error_strings)
            (register-process-global #"OpenSSL-support-initialized" done)
            (end-atomic))))))
