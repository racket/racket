#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         ffi/unsafe/global
         ffi/file
         ffi/unsafe/custodian
         "../libcrypto.rkt"
         "../libssl.rkt"
         (for-syntax racket/base))
(provide (protect-out (all-defined-out)))

(define-ffi-definer define-crypto libcrypto
  #:default-make-fail make-not-available)
(define-ffi-definer define-ssl libssl
  #:default-make-fail make-not-available)

;; OpenSSL ownership conventions:
;; - "get0" means ownership retained by parent object, refcount not incremented
;; - "get1" means copied or shared & incremented refcount; we should free result
;; - "set0" means ownership passed to parent object
;; - "set1" means ownership not passed; copied or shared & incremented refcount
;; Sources:
;; - https://www.openssl.org/docs/man3.0/man7/crypto.html "Library Conventions"
;; - https://www.openssl.org/docs/man3.0/man7/openssl-threads.html
;; Unfortunately, many older function have ambiguous names.

(define ((do-not-free [remark #f]) v) v)

;; ----------------------------------------

(define X509_V_OK 0)

(define SSL_ERROR_SSL 1)
(define SSL_ERROR_WANT_READ 2)
(define SSL_ERROR_WANT_WRITE 3)
(define SSL_ERROR_SYSCALL 5)
(define SSL_ERROR_ZERO_RETURN 6)

(define BIO_C_SET_BUF_MEM_EOF_RETURN 130)

(define SSL_FILETYPE_PEM 1)
(define SSL_FILETYPE_ASN1 2)

(define SSL_VERIFY_NONE #x00)
(define SSL_VERIFY_PEER #x01)
(define SSL_VERIFY_FAIL_IF_NO_PEER_CERT #x02)

(define SSL_MODE_ENABLE_PARTIAL_WRITE #x01)
(define SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER #x02)
(define SSL_CTRL_MODE 33)

(define NID_subject_alt_name 85)
(define NID_commonName 13)
(define GEN_DNS 2)

(define SSL_CTRL_OPTIONS 32)
(define SSL_CTRL_SET_TLSEXT_SERVERNAME_CB 53)
(define SSL_CTRL_SET_TLSEXT_HOSTNAME 55)
(define SSL_CTRL_SET_TMP_DH 3)
(define SSL_CTRL_SET_TMP_ECDH 4)
(define SSL_CTRL_GET_EXTMS_SUPPORT 122)
(define SSL_CTRL_SET_MIN_PROTO_VERSION 123)
(define SSL_CTRL_SET_MAX_PROTO_VERSION 124)

(define SSL_OP_NO_SSLv2    #x01000000)
(define SSL_OP_NO_SSLv3    #x02000000)
(define SSL_OP_NO_TLSv1    #x04000000)
(define SSL_OP_NO_TLSv1_2  #x08000000)
(define SSL_OP_NO_TLSv1_1  #x10000000)

(define SSL_OP_SINGLE_ECDH_USE #x00080000)
(define SSL_OP_SINGLE_DH_USE #x00100000)

(define TLSEXT_NAMETYPE_host_name 0)

(define SSL_TLSEXT_ERR_OK 0)
(define SSL_TLSEXT_ERR_ALERT_FATAL 2)
(define SSL_TLSEXT_ERR_NOACK 3)

(define NID_md5 4)
(define NID_sha1 64)
(define NID_sha224 675)
(define NID_sha256 672)
(define NID_sha384 673)
(define NID_sha512 674)

(define SSL_ST_ACCEPT #x2000)

(define SSL3_VERSION    #x0300)
(define TLS1_VERSION    #x0301)
(define TLS1_1_VERSION  #x0302)
(define TLS1_2_VERSION  #x0303)
(define TLS1_3_VERSION  #x0304)

;; ----------------------------------------

(define-cpointer-type _BIO_METHOD*)
(define-cpointer-type _BIO*)
(define-cpointer-type _SSL_METHOD*)
(define-cpointer-type _SSL_CTX*)
(define-cpointer-type _SSL*)
(define-cpointer-type _X509_NAME*)
(define-cpointer-type _X509_NAME_ENTRY*)
(define-cpointer-type _X509*)
(define-cpointer-type _ASN1_STRING*)
(define-cpointer-type _STACK*)
(define-cpointer-type _DH*)
(define-cpointer-type _EC_KEY*)
(define-cstruct _GENERAL_NAME ([type _int] [d _ASN1_STRING*]))
(define-cpointer-type _EVP_MD*)
(define-cpointer-type _X509_VERIFY_PARAM*)

;; ----------------------------------------

(begin ;; removed in v1.1.0
  (define-crypto SSLeay (_fun -> _ulong) #:fail (lambda () (lambda () 0))))
(begin ;; added in v1.1.0
  (define-crypto OpenSSL_version_num (_fun -> _ulong) #:fail (lambda () SSLeay)))

(define v1.0.1/later? (>= (OpenSSL_version_num) #x10001000)) ;; MNNPPxxx
(define v1.0.2/later? (>= (OpenSSL_version_num) #x10002000))
(define v1.1.0/later? (>= (OpenSSL_version_num) #x10100000))
(define v1.1.1/later? (>= (OpenSSL_version_num) #x10101000))
(define v3.0.0/later? (>= (OpenSSL_version_num) #x30000000)) ;; MNN00PP0

;; Since v1.1.0, version-specific *_{client,server}_methods are
;; deprecated, use TLS_{client,server}_method instead.
(begin ;; deprecated in v1.1.0
  (define-ssl SSLv23_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl SSLv23_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl SSLv2_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl SSLv2_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl SSLv3_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl SSLv3_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_1_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_1_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_2_client_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f))
  (define-ssl TLSv1_2_server_method (_fun -> _SSL_METHOD*) #:fail (lambda () #f)))
(begin ;; added in v1.1.0
  (define-ssl TLS_client_method (_fun -> _SSL_METHOD*)
    #:fail (lambda () SSLv23_client_method))
  (define-ssl TLS_server_method (_fun -> _SSL_METHOD*)
    #:fail (lambda () SSLv23_server_method)))

(define-crypto DH_free (_fun _DH* -> _void) #:wrap (deallocator))
(define-crypto EC_KEY_free (_fun _EC_KEY* -> _void) #:wrap (deallocator))

(define-crypto EC_KEY_new_by_curve_name (_fun _int -> _EC_KEY*)
  #:wrap (allocator EC_KEY_free))

(define-crypto BIO_s_mem (_fun -> _BIO_METHOD*))
(define-crypto BIO_new (_fun _BIO_METHOD* -> _BIO*/null))
(define-crypto BIO_new_mem_buf (_fun _pointer _int -> _BIO*))
(define-crypto BIO_free (_fun _BIO* -> _void))

(define-crypto BIO_read (_fun _BIO* _bytes _int -> _int))
(define-crypto BIO_write (_fun _BIO* _bytes _int -> _int))
(define-crypto BIO_ctrl (_fun _BIO* _int _long _long -> _long))
(define (BIO_set_mem_eof_return b v)
  (BIO_ctrl b BIO_C_SET_BUF_MEM_EOF_RETURN v 0))

(define-ssl SSL_CTX_free (_fun _SSL_CTX* -> _void)
  #:wrap (deallocator))
(define-ssl SSL_CTX_new (_fun _SSL_METHOD* -> _SSL_CTX*/null)
  #:wrap (allocator SSL_CTX_free))
(define-ssl SSL_CTX_callback_ctrl
  (_fun _SSL_CTX* _int
        (_fun #:in-original-place? #t _SSL* _pointer _pointer -> _int)
        -> _long))
(define-ssl SSL_CTX_ctrl (_fun _SSL_CTX* _int _long _pointer -> _long))
(define (SSL_CTX_set_mode ctx m)
  (SSL_CTX_ctrl ctx SSL_CTRL_MODE m #f))
(define (SSL_CTX_set_options ctx opts)
  (SSL_CTX_ctrl ctx SSL_CTRL_OPTIONS opts #f))

(define-ssl SSL_CTX_set_verify (_fun _SSL_CTX* _int _pointer -> _void))
(define-ssl SSL_CTX_use_certificate_chain_file (_fun _SSL_CTX* _path -> _int))
(define-ssl SSL_CTX_load_verify_locations (_fun _SSL_CTX* _path _path -> _int))
(define-ssl SSL_CTX_set_client_CA_list (_fun _SSL_CTX* _X509_NAME* -> _int))
(define-ssl SSL_CTX_set_session_id_context (_fun _SSL_CTX* _bytes _int -> _int))
(define-ssl SSL_CTX_use_RSAPrivateKey_file (_fun _SSL_CTX* _path _int -> _int))
(define-ssl SSL_CTX_use_PrivateKey_file (_fun _SSL_CTX* _path _int -> _int))
(define-ssl SSL_load_client_CA_file (_fun _path -> _X509_NAME*/null))
(define-ssl SSL_CTX_set_cipher_list (_fun _SSL_CTX* _string -> _int))

(begin ;; added in v1.1.0
  (define-ssl SSL_CTX_get_security_level (_fun _SSL_CTX* -> _int)
    #:fail (lambda () (lambda (ctx) 2 #|default security level|#)))
  (define-ssl SSL_CTX_set_security_level (_fun _SSL_CTX* _int -> _void)
    #:fail (lambda () (lambda (ctx) 1 #|success|#)))
  (define (SSL_CTX_set_min_proto_version ctx version)
    (SSL_CTX_ctrl ctx SSL_CTRL_SET_MIN_PROTO_VERSION version #f))
  (define (SSL_CTX_set_max_proto_version ctx version)
    (SSL_CTX_ctrl ctx SSL_CTRL_SET_MAX_PROTO_VERSION version #f)))

(define-ssl SSL_free (_fun _SSL* -> _void)
  #:wrap (deallocator))
(define-ssl SSL_new (_fun _SSL_CTX* -> _SSL*)
  #:wrap (allocator SSL_free))
(define-ssl SSL_set_bio (_fun _SSL* _BIO* _BIO* -> _void))
(define-ssl SSL_connect (_fun _SSL* -> _int))
(define-ssl SSL_accept (_fun _SSL* -> _int))
(define-ssl SSL_read (_fun _SSL* _bytes _int -> _int))
(define-ssl SSL_peek (_fun _SSL* _bytes _int -> _int))
(define-ssl SSL_write (_fun _SSL* _bytes _int -> _int))
(define-ssl SSL_shutdown (_fun _SSL* -> _int))
(define-ssl SSL_get_verify_result (_fun _SSL* -> _long))
(define-ssl SSL_get_servername (_fun _SSL* _int -> _string))
(define-ssl SSL_set_verify (_fun _SSL* _int _pointer -> _void))
(define-ssl SSL_set_session_id_context (_fun _SSL* _bytes _int -> _int))
(define-ssl SSL_renegotiate (_fun _SSL* -> _int))
(define-ssl SSL_renegotiate_pending (_fun _SSL* -> _int))
(define-ssl SSL_do_handshake (_fun _SSL* -> _int))
(define-ssl SSL_ctrl/bytes (_fun _SSL* _int _long _bytes/nul-terminated -> _long)
  #:c-id SSL_ctrl)
(define-ssl SSL_set_SSL_CTX (_fun _SSL* _SSL_CTX* -> _SSL_CTX*))
(define-ssl SSL_version (_fun _SSL* -> _int))
(define-ssl SSL_get_verify_mode (_fun _SSL* -> _int))

(define-crypto X509_free (_fun _X509* -> _void)
  #:wrap (deallocator))
(define-ssl SSL_get_peer_certificate (_fun _SSL* -> _X509*/null)
  #:make-fail (lambda (name)
                (lambda ()
                  (and libssl
                       (get-ffi-obj 'SSL_get1_peer_certificate libssl (_fun _SSL* -> _X509*/null)
                                    (lambda () (make-not-available name))))))
  #:wrap (allocator X509_free))
(define-ssl SSL_get_certificate (_fun _SSL* -> _X509*/null)
  #:wrap (do-not-free "owned by SSL object"))

(define-crypto X509_get_subject_name (_fun _X509* -> _X509_NAME*)
  #:wrap (do-not-free "owned by X509"))
(define-crypto X509_get_issuer_name (_fun _X509* -> _X509_NAME*)
  #:wrap (do-not-free "owned by X509"))
(define-crypto X509_NAME_oneline (_fun _X509_NAME* _bytes _int -> _bytes))

(define-ssl SSL_get_error (_fun _SSL* _int -> _int))

(define-crypto ERR_get_error (_fun -> _long))
(define-crypto ERR_error_string_n (_fun _long _bytes _long -> _void))

(begin ;; removed in v1.1.0
  (define-ssl SSL_library_init (_fun -> _void) #:fail (lambda () void))
  (define-ssl SSL_load_error_strings (_fun -> _void) #:fail (lambda () void)))

(define-crypto GENERAL_NAME_free _fpointer)
(define-crypto PEM_read_bio_DHparams (_fun _BIO* _pointer _pointer _pointer -> _DH*)
  #:wrap (allocator DH_free))
(define-crypto ASN1_STRING_length (_fun _ASN1_STRING* -> _int))
(define-crypto ASN1_STRING_data (_fun _ASN1_STRING* -> _pointer))
(define-crypto X509_NAME_get_index_by_NID (_fun _X509_NAME* _int _int -> _int))
(define-crypto X509_NAME_get_entry (_fun _X509_NAME* _int -> _X509_NAME_ENTRY*/null))
(define-crypto X509_NAME_ENTRY_get_data (_fun _X509_NAME_ENTRY* -> _ASN1_STRING*))
(define-crypto X509_get_ext_d2i (_fun _X509* _int _pointer _pointer -> _STACK*/null))
(define-crypto sk_num (_fun _STACK* -> _int)
  #:fail (lambda ()
           (define-crypto OPENSSL_sk_num (_fun _STACK* -> _int))
           OPENSSL_sk_num))
(define-crypto sk_GENERAL_NAME_value (_fun _STACK* _int -> _GENERAL_NAME-pointer)
  #:c-id sk_value
  #:fail (lambda ()
           (define-crypto OPENSSL_sk_value (_fun _STACK* _int -> _GENERAL_NAME-pointer))
           OPENSSL_sk_value))
(define-crypto sk_pop_free (_fun _STACK* _fpointer -> _void)
  #:fail (lambda ()
           (define-crypto OPENSSL_sk_pop_free (_fun _STACK* _fpointer -> _void))
           OPENSSL_sk_pop_free))

;; (define-crypto X509_get_default_cert_area (_fun -> _string))
(define-crypto X509_get_default_cert_dir  (_fun -> _string))
(define-crypto X509_get_default_cert_file (_fun -> _string))
(define-crypto X509_get_default_cert_dir_env (_fun -> _string))
(define-crypto X509_get_default_cert_file_env (_fun -> _string))

(define-ssl SSL_get_peer_finished (_fun _SSL* _pointer _size -> _size))
(define-ssl SSL_get_finished (_fun _SSL* _pointer _size -> _size))

(define-ssl SSL_CTX_set_alpn_protos
  (_fun _SSL_CTX*
        (bs : _bytes)
        (_uint = (bytes-length bs))
        -> _int)) ;; Note: 0 means success, other means failure!
(define-ssl SSL_set_alpn_protos
  (_fun _SSL*
        (bs : _bytes)
        (_uint = (bytes-length bs))
        -> _int)) ;; Note: 0 means success, other means failure!
(define-ssl SSL_get0_alpn_selected
  (_fun _SSL*
        (p : (_ptr o _pointer))
        (len : (_ptr o _uint))
        -> _void
        -> (cond [(and p (> len 0))
                  (let ([bs (make-bytes len)])
                    (memcpy bs p len)
                    bs)]
                 [else #f])))

(define-ssl SSL_CTX_set_alpn_select_cb
  (_fun [ctx : _SSL_CTX*]
        [cb : (_fun #:in-original-place? #t
                    [ssl : _SSL*]
                    [out : _pointer] ;; const unsigned char**
                    [outlen : _pointer] ;; char *
                    [in : _pointer]
                    [inlen : _int]
                    [arg : _pointer]
                    -> _int)]
        [arg : _pointer]
        -> _int))

(define-ssl SSL_CTX_set_keylog_callback
  (_fun [ctx : _SSL_CTX*]
        [cb : (_fun [ssl : _SSL*]
                    [line : _bytes/nul-terminated] ;; const char *
                    -> _void)]
        -> _void))

(define-crypto EVP_sha224 (_fun -> _EVP_MD*/null))
(define-crypto EVP_sha256 (_fun -> _EVP_MD*/null))
(define-crypto EVP_sha384 (_fun -> _EVP_MD*/null))
(define-crypto EVP_sha512 (_fun -> _EVP_MD*/null))
(define-crypto EVP_MD_size (_fun _EVP_MD* -> _int)
  #:make-fail (lambda (name)
                (lambda ()
                  (get-ffi-obj 'EVP_MD_get_size libcrypto (_fun _EVP_MD* -> _int)
                               (make-not-available name)))))

(define-ssl OBJ_find_sigid_algs
  (_fun _int (alg : (_ptr o _int)) (_pointer = #f) -> (r : _int)
        -> (if (> r 0) alg 0)))

(define-ssl X509_get_signature_nid
  (_fun _X509* -> _int))

(define-ssl X509_digest
  (_fun _X509* _EVP_MD* _pointer (_ptr i _uint) -> _int))

(begin ;; added in v1.0.2
  (define-crypto X509_check_host
    (_fun [cert : _X509*]
          [name : _bytes]
          [namelen : _size = (bytes-length name)]
          [flags : _uint]
          [peername : _pointer = #f]
          -> _int))
  (define-ssl SSL_get0_param
    (_fun _SSL* -> _X509_VERIFY_PARAM*))
  (define-crypto X509_VERIFY_PARAM_set1_host
    (_fun [param : _X509_VERIFY_PARAM*]
          [name : _bytes]
          [namelen : _size = (bytes-length name)]
          -> _int))
  (define-crypto X509_VERIFY_PARAM_set_hostflags
    (_fun _X509_VERIFY_PARAM* _uint -> _void)))

(begin ;; added in v1.1.0
  (define-ssl SSL_set_hostflags
    (_fun _SSL* _uint -> _void)
    #:fail (lambda ()
             (lambda (ssl flags)
               (X509_VERIFY_PARAM_set_hostflags (SSL_get0_param ssl) flags))))
  (define-ssl SSL_set1_host
    (_fun _SSL* _string/latin-1 -> _void)
    #:fail (lambda ()
             (lambda (ssl host)
               (X509_VERIFY_PARAM_set1_host (SSL_get0_param ssl)
                                            (string->bytes/latin-1 host))))))

(define-ssl SSL_export_keying_material
  (_fun [ssl : _SSL*]
        [out : _bytes] [olen : _size = (bytes-length out)]
        [label : _bytes] [llen : _size = (bytes-length label)]
        [context : _bytes]
        [contextlen : _size = (if context (bytes-length context) 0)]
        [use-context? : _bool = (and context #t #f)]
        -> _int))

(define (SSL_get_extms_support s)
  (SSL_ctrl/bytes s SSL_CTRL_GET_EXTMS_SUPPORT 0 #f))

(define (SSL_set_tlsext_host_name s hostname)
  (SSL_ctrl/bytes s SSL_CTRL_SET_TLSEXT_HOSTNAME
                  TLSEXT_NAMETYPE_host_name (string->bytes/latin-1 hostname)))
