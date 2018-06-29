
;; For now, don't try to use the JIT database from multiple threads
(meta-cond
 [(threaded?)
  (begin
    (define original-thread-id (get-thread-id))
    (define (wrong-jit-db-thread?)
      (not (eqv? original-thread-id (get-thread-id)))))]
 [else
  (define (wrong-jit-db-thread?) #f)])

(define (db-error who fmt . args)
  (let ([str (string-append (symbol->string who)
                            ": "
                            (apply #%format fmt args))])
    (log-message root-logger 'error 'jit-db str #f)
    #f))

(define (no-db-procedures)
  (values (lambda (hash) #f)
          (lambda (hash code) (void))
          (lambda (hash) (void))))

;; Gets Sqlite3-based lookup, insert, and delete on demand,
;; returning the dummy functions from `no-db-procedures`
;; if something goes wrong setting up the database
(define (get-code-database-procedures)
  (with-interrupts-disabled
   (guard
    (exn [else (db-error 'load "could not load sqlite ~s"
                         (if (message-condition? exn)
                             (condition-message exn)
                             exn))
               (no-db-procedures)])
    (let ([ok (begin
                ;; FIXME: look in the Racket "lib" directory, first
                (case (system-type)
                  [(macosx) (load-shared-object "libsqlite3.0.dylib")]
                  [(windows) (load-shared-object "sqlite3.dll")]
                  [else (load-shared-object "libsqlite3.so.0")])
                (void))])
      (define SQLITE_OPEN_READONLY  #x00000001)
      (define SQLITE_OPEN_READWRITE #x00000002)
      (define SQLITE_OPEN_CREATE    #x00000004)

      (define SQLITE_OK 0)
      (define SQLITE_CONSTRAINT 19)
      (define SQLITE_ROW  100)
      (define SQLITE_DONE 101)

      (define SQLITE_TRANSIENT -1)

      (define memcpy_pp (foreign-procedure "(cs)byte-copy" (uptr iptr uptr iptr iptr) void))
      (define memcpy_bp (foreign-procedure "(cs)byte-copy" (u8* iptr uptr iptr iptr) void))
      (define memcpy_pb (foreign-procedure "(cs)byte-copy" (uptr iptr u8* iptr iptr) void))
      (define memcpy_bb (foreign-procedure "(cs)byte-copy" (u8* iptr u8* iptr iptr) void))
      
      (define (memcpy dest src len)
        (cond
         [(bytevector? dest)
          (if (bytevector? src)
              (memcpy_bb src 0 dest 0 len)
              (memcpy_pb src 0 dest 0 len))]
         [else
          (if (bytevector? src)
              (memcpy_bp src 0 dest 0 len)
              (memcpy_pp src 0 dest 0 len))]))

      (define sqlite3_open_v2
        (foreign-procedure "sqlite3_open_v2"
                           (u8*   ; path
                            uptr  ; receives a pointer result
                            int   ; flags
                            uptr) ; VFS
                           int))

      (define sqlite3_prepare_v2
        (foreign-procedure "sqlite3_prepare_v2"
                           (uptr  ; db
                            uptr  ; statement string
                            int   ; statement length
                            uptr  ; ptr to result
                            uptr) ; ptr to leftover statement string
                           int))

      (define sqlite3_step
        (foreign-procedure "sqlite3_step"
                           (uptr) ; statement
                           int))

      (define sqlite3_reset
        (foreign-procedure "sqlite3_reset"
                           (uptr) ; statement
                           int))

      (define sqlite3_clear_bindings
        (foreign-procedure "sqlite3_clear_bindings"
                           (uptr) ; statement
                           int))

      (define sqlite3_finalize
        (foreign-procedure "sqlite3_finalize"
                           (uptr) ; statement
                           int))

      (define sqlite3_bind_blob
        (foreign-procedure "sqlite3_bind_blob"
                           (uptr  ; statement
                            int   ; parameter index
                            u8*   ; data
                            int   ; length
                            iptr) ; use SQLITE_TRANSIENT
                           int))

      (define sqlite3_column_blob
        (foreign-procedure "sqlite3_column_blob"
                           (uptr  ; statement
                            int)  ; column
                           uptr))
      (define sqlite3_column_bytes
        (foreign-procedure "sqlite3_column_bytes"
                           (uptr  ; statement
                            int)  ; column
                           int))

      (define sqlite3_errstr
        (foreign-procedure "sqlite3_errstr"
                           (int)
                           string))

      (define sqlite3_errmsg
        (foreign-procedure "sqlite3_errmsg"
                           (uptr) ; database
                           string))

      (define (errstr r)
        (sqlite3_errstr r))

      (define db
        (let ([db-ptr (foreign-alloc (foreign-sizeof 'uptr))])
          (define r
            (sqlite3_open_v2 (bytes-append (path->bytes jit-db-path)
                                           '#vu8(0))
                             db-ptr
                             (bitwise-ior SQLITE_OPEN_READWRITE
                                          SQLITE_OPEN_CREATE)
                             0))
          (let ([db (foreign-ref 'uptr 0 db-ptr)])
            (foreign-free db-ptr)
            (cond
             [(= r SQLITE_OK) db]
             [else (db-error 'open "failed ~s" (errstr r))]))))

      (define (prepare db stmt-str)
        (let* ([stmt (string->utf8 stmt-str)]
               [stmt-len (bytevector-length stmt)]
               [stmt-copy (foreign-alloc stmt-len)]
               [s-ptr (foreign-alloc (foreign-sizeof 'uptr))]
               [rest-ptr (foreign-alloc (foreign-sizeof 'uptr))])
          (memcpy stmt-copy stmt stmt-len)
          (let ([r (sqlite3_prepare_v2 db
                                       stmt-copy
                                       stmt-len
                                       s-ptr
                                       rest-ptr)])
            (let* ([s (foreign-ref 'uptr 0 s-ptr)]
                   [rest (foreign-ref 'uptr 0 rest-ptr)])
              (foreign-free stmt-copy)
              (cond
               [(= r SQLITE_OK)
                (cond
                 [(= rest (+ stmt-copy stmt-len))
                  ;; Success
                  s]
                 [else
                  (finalize s)
                  (db-error 'prepare "more than one statement ~s" stmt-str)])]
               [else
                (db-error 'prepare "error ~s" (errstr r))])))))

      (define (finalize s)
        (define r (sqlite3_finalize s))
        (unless (= r SQLITE_OK)
          (db-error 'finalize "error ~s" (errstr r))))

      (define (step s result-shape)
        (define r (sqlite3_step s))
        (cond
         [(= r SQLITE_ROW)
          (let loop ([result-shape result-shape] [col 0])
            (case result-shape
              [(bytes)
               (let* ([blob (sqlite3_column_blob s col)]
                      [len (sqlite3_column_bytes s col)]
                      [bstr (make-bytevector len)])
                 (memcpy bstr blob len)
                 bstr)]
              [(void ignore-constraint) (void)]
              [else
               (cond
                [else (db-error 'step "unrecognized result format ~s" result-shape)])]))]
         [(= r SQLITE_DONE)
          #f]
         [(and (= r SQLITE_CONSTRAINT)
               (eq? result-shape 'ignore-constraint))
          ;; Ignore a constraint failure, because we assume it reflects a
          ;; lost race trying to insert code for the same hash
          (void)]
         [else
          (db-error 'step "error ~s" (errstr r))]))

      (define initialized-db
        (when db
          (let ([s (prepare db "SELECT name FROM sqlite_master WHERE type='table' AND name='compiled'")])
            (unless (step s 'void)
              (let ([s2 (prepare db "CREATE TABLE compiled (hash blob(24), code blob(1024), PRIMARY KEY (hash))")])
                (step s2 'void)
                (finalize s2)))
            (finalize s))
          ;; FIXME: this pragma is needed for reasonable performance on Linux, but
          ;; we should instead batch updates in `insert` (since it's ok for an
          ;; update to get lost, but not ok for the database to be corrupted)
          (let ([s (prepare db "PRAGMA synchronous = OFF")])
            (step s 'void)
            (finalize s))))

      (define (check who r)
        (unless (= r SQLITE_OK)
          (db-error who "error ~s" (errstr r))))

      (define (bind s pos v)
        (check 'bind (sqlite3_bind_blob s pos v (bytevector-length v) SQLITE_TRANSIENT)))

      (define lookup-s (prepare db "SELECT code FROM compiled WHERE hash=$1"))
      (define delete-s (prepare db "DELETE FROM compiled WHERE hash=$1"))
      (define insert-s (prepare db "INSERT INTO compiled VALUES ($1, $2)"))

      (define (reset s)
        (sqlite3_reset s) ; ignore any error, since it's a repeat of recent error
        (check 'clear-bindings (sqlite3_clear_bindings s)))

      (define (lookup hash)
        (with-interrupts-disabled
         (bind lookup-s 1 hash)
         (let ([r (step lookup-s 'bytes)])
           (reset lookup-s)
           r)))

      (define (insert hash code)
        (with-interrupts-disabled
         (bind insert-s 1 hash)
         (bind insert-s 2 code)
         (step insert-s 'ignore-constraint)
         (reset insert-s)  
         (void)))

      (define (delete hash)
        (with-interrupts-disabled
         (bind delete-s 1 hash)
         (step delete-s 'void)
         (reset delete-s)
         (void)))

      (if db
          (values lookup insert delete)
          (no-db-procedures))))))
