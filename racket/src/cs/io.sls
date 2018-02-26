(library (io)
  (export)
  (import (except (chezpart)
                  close-port)
          (rename (only (chezscheme)
                        read-char peek-char
                        current-directory
                        error
                        input-port? output-port?
                        file-position flush-output-port
                        file-symbolic-link?)
                  [input-port? chez:input-port?]
                  [output-port? chez:output-port?]
                  [flush-output-port flush-output])
          (rumble)
          (thread))
  ;; ----------------------------------------
  ;; Tie knots:

  (define (path? v) (is-path? v))
  (define (path->string v) (1/path->string v))
  (define path->complete-path
    (case-lambda
     [(v) (1/path->complete-path v)]
     [(v wrt) (1/path->complete-path v wrt)]))
  (define (absolute-path? v) (1/absolute-path? v))
  (define (relative-path? v) (1/relative-path? v))

  ;; ----------------------------------------

  (module (|#%rktio-instance| ptr->address)
    (meta define (convert-type t)
          (syntax-case t (ref *ref rktio_bool_t rktio_const_string_t)
            [(ref . _) #'uptr]
            [(*ref rktio_const_string_t) #'uptr]
            [(*ref . _) #'u8*]
            [rktio_bool_t #'boolean]
            [rktio_const_string_t #'u8*]
            [else t]))

    (define-ftype intptr_t iptr)
    (define-ftype uintptr_t uptr)
    (define-ftype rktio_int64_t integer-64)
    (define _uintptr _uint64)
    (define NULL 0)

    (define (<< a b) (bitwise-arithmetic-shift-left a b))

    (define-syntax define-constant
      (syntax-rules ()
        [(_ id expr) (define id expr)]))
    
    (define-syntax (define-type stx)
      (syntax-case stx (rktio_const_string_t rktio_ok_t rktio_bool_t)
        [(_ rktio_const_string_t old_type)
         ;; skip
         #'(begin)]
        [(_ rktio_ok_t old_type)
         (with-syntax ([(_ type _) stx])
           #'(define-ftype type boolean))]
        [(_ rktio_bool_t old_type)
         (with-syntax ([(_ type _) stx])
           #'(define-ftype type boolean))]
        [(_ type old-type)
         (with-syntax ([old-type (convert-type #'old-type)])
           #'(define-ftype type old-type))]))

    (define-syntax (define-struct-type stx)
      (syntax-case stx ()
        [(_ type ([old-type field] ...))
         (with-syntax ([(old-type ...) (map convert-type #'(old-type ...))])
           #'(define-ftype type (struct [field old-type] ...)))]))

    ;; Wrap foreign-pointer addressed in a record so that
    ;; the value can be finalized
    (define-record ptr (address))
    (define (ptr->address v) (if (eqv? v NULL) v (ptr-address v)))
    (define (address->ptr v) (if (eqv? v NULL) v (make-ptr v)))

    (define-syntax (let-unwrappers stx)
      ;; Unpack plain pointers; when an argument has type
      ;; `rktio_const_string_t`, add an explicit NUL terminator byte;
      ;; when an argument has a `nullable` wrapper, then add a #f -> 0
      ;; conversion
      (syntax-case stx (rktio_const_string_t ref nullable)
        [(_ () body) #'body]
        [(_ ([rktio_const_string_t arg-name] . args) body)
         #'(let ([arg-name (add-nul-terminator arg-name)])
             (let-unwrappers args body))]
        [(_ ([(ref (nullable type)) arg-name] . args) body)
         #'(let ([arg-name (ptr->address (or arg-name NULL))])
             (let-unwrappers args body))]
        [(_ ([(ref type) arg-name] . args) body)
         #'(let ([arg-name (ptr->address arg-name)])
             (let-unwrappers args body))]
        [(_ ([(*ref rktio_const_string_t) arg-name] . args) body)
         #'(let ([arg-name (ptr->address arg-name)])
             (let-unwrappers args body))]
        [(_ (_ . args) body)
         #'(let-unwrappers args body)]))

    (define (add-nul-terminator bstr)
      (and bstr (bytes-append bstr '#vu8(0))))

    (define-syntax (wrap-result stx)
      (syntax-case stx (ref)
        [(_ (ref _) v) #'(address->ptr v)]
        [(_ _ v) #'v]))
    
    (meta define (convert-function stx)
          (syntax-case stx ()
            [(_ (flag ...) orig-ret-type name ([orig-arg-type arg-name] ...))
             (with-syntax ([ret-type (convert-type #'orig-ret-type)]
                           [(arg-type ...) (map convert-type #'(orig-arg-type ...))]
                           [(conv ...) (if (#%memq 'blocking (map syntax->datum #'(flag ...)))
                                           #'(__thread)
                                           #'())])
               #'(let ([proc (foreign-procedure conv ... (rktio-lookup 'name)
                                                (arg-type ...)
                                                ret-type)])
                   (lambda (arg-name ...)
                     (let-unwrappers
                      ([orig-arg-type arg-name] ...)
                      (wrap-result orig-ret-type (proc arg-name ...))))))]))

    (define-syntax (define-function stx)
      (syntax-case stx ()
        [(_ _ _ name . _)
         (with-syntax ([rhs (convert-function stx)])
           #'(define name rhs))]))

    (define-syntax (define-function*/errno stx)
      (syntax-case stx ()
        [(_ err-val err-expr flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (with-syntax ([rhs (convert-function
                             #'(define-function flags ret-type name ([rktio-type rktio] [arg-type arg] ...)))])
           #'(define name
               (let ([proc rhs])
                 (lambda (rktio arg ...)
                   (let ([v (proc rktio arg ...)])
                     (if (eqv? v err-val)
                         err-expr
                         v))))))]))

    (define-syntax define-function/errno
      (syntax-rules ()
        [(_ err-val flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio))
           flags ret-type name ([rktio-type rktio] [arg-type arg] ...))]))
    
    (define-syntax define-function/errno+step
      (syntax-rules ()
        [(_ err-val flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio)
                   (rktio_get_last_error_step rktio))
           flags ret-type name ([rktio-type rktio] [arg-type arg] ...))]))

    (define loaded-librktio
      (or (foreign-entry? "rktio_init")
          (load-shared-object (string-append (string-append (current-directory) "/../../lib/librktio")
                                             (utf8->string (system-type 'so-suffix))))))

    (define (rktio-lookup name)
      (foreign-entry (symbol->string name)))

    (include "../rktio/rktio.rktl")

    (define (rktio_filesize_ref fs)
      (ftype-ref rktio_filesize_t () (make-ftype-pointer rktio_filesize_t (ptr->address fs))))
    (define (rktio_timestamp_ref fs)
      (ftype-ref rktio_timestamp_t () (make-ftype-pointer rktio_timestamp_t (ptr->address fs))))
    (define (rktio_is_timestamp v)
      (let ([radix (arithmetic-shift 1 (sub1 (* 8 (ftype-sizeof rktio_timestamp_t))))])
        (<= (- radix) v (sub1 radix))))

    (define (rktio_recv_length_ref fs)
      (ftype-ref rktio_length_and_addrinfo_t (len) (make-ftype-pointer rktio_length_and_addrinfo_t (ptr->address fs)) 0))

    (define (rktio_recv_address_ref fs)
      (ftype-ref rktio_length_and_addrinfo_t (address) (make-ftype-pointer rktio_length_and_addrinfo_t (ptr->address fs)) 0))

    (define (rktio_identity_to_vector p)
      (let ([p (make-ftype-pointer rktio_identity_t (ptr->address p))])
        (vector
         (ftype-ref rktio_identity_t (a) p)
         (ftype-ref rktio_identity_t (b) p)
         (ftype-ref rktio_identity_t (c) p)
         (ftype-ref rktio_identity_t (a_bits) p)
         (ftype-ref rktio_identity_t (b_bits) p)
         (ftype-ref rktio_identity_t (c_bits) p))))
    
    (define (rktio_convert_result_to_vector p)
      (let ([p (make-ftype-pointer rktio_convert_result_t (ptr->address p))])
        (vector
         (ftype-ref rktio_convert_result_t (in_consumed) p)
         (ftype-ref rktio_convert_result_t (out_produced) p)
         (ftype-ref rktio_convert_result_t (converted) p))))
      (define (cast v from to)
        (let ([p (malloc from)])
          (ptr-set! p from v)
          (ptr-ref p to)))

    (define (rktio_to_bytes fs)
      (cast (ptr->address fs) _uintptr _bytes))

    (define (rktio_to_shorts fs)
      (cast (ptr->address fs) _uintptr _short_bytes))

    ;; Unlike `rktio_to_bytes`, frees the array and strings
    (define rktio_to_bytes_list
      (case-lambda
       [(lls) (rktio_to_bytes_list lls #f)]
       [(lls len)
        (begin0
         (let loop ([i 0])
           (cond
            [(and len (fx= i len))
             '()]
            [else
             (let ([bs (foreign-ref 'uptr (ptr->address lls) (* i (foreign-sizeof 'uptr)))])
               (if (not (eqv? NULL bs))
                   (cons (begin0
                          (cast bs _uintptr _bytes)
                          (rktio_free (make-ptr bs)))
                         (loop (add1 i)))
                   '()))]))
         (rktio_free lls))]))

    ;; Allocates pointers that must be released via `rktio_free_bytes_list`:
    (define (rktio_from_bytes_list bstrs)
      (let ([p (foreign-alloc (fx* (length bstrs) (foreign-sizeof 'uptr)))])
        (let loop ([bstrs bstrs] [i 0])
          (cond
           [(null? bstrs) p]
           [else
            (let* ([bstr (car bstrs)]
                   [len (bytes-length bstr)]
                   [s (foreign-alloc (fx+ len 1))])
              (let loop ([j 0])
                (cond
                 [(= j len)
                  (foreign-set! 'unsigned-8 s j 0)]
                 [else
                  (foreign-set! 'unsigned-8 s j (bytes-ref bstr j))
                  (loop (fx+ 1 j))]))
              (foreign-set! 'uptr p (fx* i (foreign-sizeof 'uptr)) s)
              (loop (cdr bstrs) (fx+ 1 i)))]))
        (address->ptr p)))

    (define (rktio_free_bytes_list lls len)
      (rktio_to_bytes_list lls len)
      (void))

    (define (null-to-false v) (if (eqv? v NULL) #f v))

    (define (rktio_process_result_stdin_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stdin_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_stdout_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stdout_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_stderr_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stderr_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_process r)
      (address->ptr (ftype-ref rktio_process_result_t (process) (make-ftype-pointer rktio_process_result_t (ptr->address r)))))

    (define (rktio_status_running r)
      (ftype-ref rktio_status_t (running) (make-ftype-pointer rktio_status_t (ptr->address r))))
    (define (rktio_status_result r)
      (ftype-ref rktio_status_t (result) (make-ftype-pointer rktio_status_t (ptr->address r))))

    (define (rktio_do_install_os_signal_handler rktio)
      (rktio_install_os_signal_handler rktio))

    (define (rktio_get_ctl_c_handler)
      (get-ctl-c-handler))

    (define |#%rktio-instance|
      (let ()
        (define-syntax extract-functions
          (syntax-rules (define-constant
                          define-type
                          define-struct-type
                          define-function
                          define-function/errno
                          define-function/errno+step)
            [(_ accum) (hasheq . accum)]
            [(_ accum (define-constant . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-type . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-struct-type . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-function _ _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]
            [(_ accum (define-function/errno _ _ _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]
            [(_ accum (define-function/errno+step _ _ _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]))
        (define-syntax begin
          (syntax-rules ()
            [(begin form ...)
             (extract-functions ['rktio_NULL
                                 NULL
                                 'rktio_filesize_ref rktio_filesize_ref
                                 'rktio_timestamp_ref rktio_timestamp_ref
                                 'rktio_is_timestamp rktio_is_timestamp
                                 'rktio_recv_length_ref rktio_recv_length_ref
                                 'rktio_recv_address_ref rktio_recv_address_ref
                                 'rktio_identity_to_vector rktio_identity_to_vector
                                 'rktio_convert_result_to_vector rktio_convert_result_to_vector
                                 'rktio_to_bytes rktio_to_bytes
                                 'rktio_to_bytes_list rktio_to_bytes_list
                                 'rktio_to_shorts rktio_to_shorts
                                 'rktio_from_bytes_list rktio_from_bytes_list
                                 'rktio_free_bytes_list rktio_free_bytes_list
                                 'rktio_from_bytes_list rktio_from_bytes_list
                                 'rktio_free_bytes_list rktio_free_bytes_list
                                 'rktio_process_result_stdin_fd rktio_process_result_stdin_fd
                                 'rktio_process_result_stdout_fd rktio_process_result_stdout_fd
                                 'rktio_process_result_stderr_fd rktio_process_result_stderr_fd
                                 'rktio_process_result_process rktio_process_result_process
                                 'rktio_status_running rktio_status_running
                                 'rktio_status_result rktio_status_result
                                 'rktio_do_install_os_signal_handler rktio_do_install_os_signal_handler
                                 'rktio_get_ctl_c_handler rktio_get_ctl_c_handler]
                                form ...)]))
        (include "../rktio/rktio.rktl"))))
  
  ;; ----------------------------------------

  (define format
    (case-lambda
     [(fmt arg)
      (unless (equal? fmt "~s")
        (raise-arguments-error 'format "should only be used as a fallback"
                               "format string" fmt
                               "argument" arg))
      (cond
       [(and (record? arg)
             (or (not (impersonator? arg))
                 (record? (unsafe-struct*-ref arg 0))))
        (let ([arg (if (impersonator? arg)
                       (unsafe-struct*-ref arg 0)
                       arg)])
          (chez:format "#<~a>" (record-type-name (record-rtd arg))))]
       [else
        (chez:format "~s" arg)])]
     [(fmt . args)
      (raise-arguments-error 'format "should only be used as a fallback"
                             "format string" fmt
                             "arguments" args)]))

  ;; ----------------------------------------

  (export system-library-subpath)
  (define system-library-subpath
    (case-lambda
     [() (system-library-subpath (system-type 'gc))]
     [(mode)
      (1/string->path
       (string-append
        system-library-subpath-string
        (cond
         [(eq? mode '3m) (if (eq? 'windows (system-path-convention-type))
                             "\\3m"
                             "/3m")]
         [(eq? mode 'cs) (if (eq? 'windows (system-path-convention-type))
                             "\\cs"
                             "/cs")]
         [(or (eq? mode 'cgc) (not mode)) ""]
         [else (raise-argument-error 'system-library-subpath
                                     "(or/c '3m 'cgc 'cs #f)"
                                     mode)])))]))

  (define (primitive-table key)
    (case key
      [(|#%thread|) |#%thread-instance|]
      [(|#%rktio|) |#%rktio-instance|]
      [else #f]))

  (include "include.ss")
  (include-generated "io.scm")
  
  ;; Initialize:
  (|#%app| 1/current-directory (current-directory))
  (|#%app| 1/current-directory-for-user (current-directory))
  (set-log-system-message! (lambda (level str)
                             (1/log-message (|#%app| 1/current-logger) level str #f)))
  (set-error-display-eprintf! (lambda (fmt . args)
                                (apply 1/fprintf (|#%app| 1/current-error-port) fmt args)))
  (set-ffi-get-lib-and-obj! ffi-get-lib ffi-get-obj ptr->address)
  (set-async-callback-poll-wakeup! 1/unsafe-signal-received))
