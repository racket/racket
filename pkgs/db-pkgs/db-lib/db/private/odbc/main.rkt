#lang racket/base
(require racket/class
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/place-client
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide odbc-connect
         odbc-driver-connect
         odbc-data-sources
         odbc-drivers)

(define (odbc-connect #:dsn dsn
                      #:user [user #f]
                      #:password [auth #f]
                      #:notice-handler [notice-handler void]
                      #:strict-parameter-types? [strict-parameter-types? #f]
                      #:character-mode [char-mode 'wchar]
                      #:use-place [use-place #f])
  (cond [use-place
         (place-connect (list 'odbc dsn user auth strict-parameter-types? char-mode)
                        odbc-proxy%)]
        [else
         (let ([notice-handler (make-handler notice-handler "notice")])
           (call-with-env 'odbc-connect
             (lambda (env)
               (call-with-db 'odbc-connect env
                 (lambda (db)
                   (let ([status (SQLConnect db dsn user auth)])
                     (handle-status* 'odbc-connect status db)
                     (new connection%
                          (env env)
                          (db db)
                          (notice-handler notice-handler)
                          (strict-parameter-types? strict-parameter-types?)
                          (char-mode char-mode))))))))]))

(define (odbc-driver-connect connection-string
                             #:notice-handler [notice-handler void]
                             #:strict-parameter-types? [strict-parameter-types? #f]
                             #:character-mode [char-mode 'wchar]
                             #:use-place [use-place #f])
  (cond [use-place
         (place-connect (list 'odbc-driver connection-string strict-parameter-types? char-mode)
                        odbc-proxy%)]
        [else
         (let ([notice-handler (make-handler notice-handler "notice")])
           (call-with-env 'odbc-driver-connect
             (lambda (env)
               (call-with-db 'odbc-driver-connect env
                 (lambda (db)
                   (let ([status (SQLDriverConnect db connection-string SQL_DRIVER_NOPROMPT)])
                     (handle-status* 'odbc-driver-connect status db)
                     (new connection%
                          (env env)
                          (db db)
                          (notice-handler notice-handler)
                          (strict-parameter-types? strict-parameter-types?)
                          (char-mode char-mode))))))))]))

(define (odbc-data-sources)
  (define server-buf (make-bytes 1024))
  (define descr-buf (make-bytes 1024))
  (call-with-env 'odbc-data-sources
    (lambda (env)
      (begin0
          (let loop ()
            (let-values ([(status name description)
                          (SQLDataSources env SQL_FETCH_NEXT server-buf descr-buf)])
              (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                     (cons (list name description) (loop))]
                    [else ;; SQL_NO_DATA
                     null])))
        (handle-status* 'odbc-data-sources (SQLFreeHandle SQL_HANDLE_ENV env))))))

(define (odbc-drivers)
  (define driver-buf (make-bytes 1000))
  (define attr-buf (make-bytes 2000))
  (call-with-env 'odbc-drivers
   (lambda (env)
     (let ([result
            (let loop ()
              (let-values ([(status name attrlen) ;; & writes to attr-buf
                            (SQLDrivers env SQL_FETCH_NEXT driver-buf attr-buf)])
                (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                       (cons (list name (parse-driver-attrs attr-buf attrlen))
                             (loop))]
                      [else null])))])  ;; SQL_NO_DATA
       (handle-status* 'odbc-drivers (SQLFreeHandle SQL_HANDLE_ENV env))
       result))))

(define (parse-driver-attrs buf len)
  (let* ([attrs (regexp-split #rx#"\0" buf 0 len)])
    (filter values
            (for/list ([p (in-list attrs)]
                       #:when (positive? (bytes-length p)))
              (let* ([s (bytes->string/utf-8 p)]
                     [m (regexp-match-positions #rx"=" s)])
                ;; Sometimes (eg iodbc on openbsd), returns ill-formatted attr-buf; just discard
                (and m
                     (let ([=-pos (caar m)])
                       (cons (substring s 0 =-pos) (substring s (+ 1 =-pos))))))))))

(define odbc-proxy%
  (class place-proxy-connection%
    (super-new)
    (define/override (get-dbsystem) dbsystem)))

;; ----

;; Aux functions to free handles on error.

(define (call-with-env fsym proc)
  (let-values ([(status env) (SQLAllocHandle SQL_HANDLE_ENV #f)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_ENV env)
                       (raise e))])
      (handle-status* fsym status env)
      (handle-status* fsym (SQLSetEnvAttr env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC3))
      (proc env))))

(define (call-with-db fsym env proc)
  (let-values ([(status db) (SQLAllocHandle SQL_HANDLE_DBC env)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_DBC db)
                       (raise e))])
      (handle-status* fsym status db)
      (proc db))))
