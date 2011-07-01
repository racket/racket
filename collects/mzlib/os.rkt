#lang mzscheme

(require mzlib/etc 
	 ffi/unsafe 
	 ffi/cvector
	 ffi/winapi)

(define kernel32
  (delay (and (eq? 'windows (system-type)) (ffi-lib "kernel32"))))

(define (delay-ffi-obj name lib type)
  (delay (get-ffi-obj name lib type)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gethostbyname

(define BUFFER-SIZE 1024)
(define (extract-terminated-string proc)
  (let ([s (make-bytes BUFFER-SIZE)])
    (if (proc s BUFFER-SIZE)
      (bytes->string/utf-8 (car (regexp-match #rx#"^[^\0]*" s)))
      (error 'gethostname "could not get hostname"))))

(define unix-gethostname
  (delay-ffi-obj "gethostname" #f (_fun _bytes _int -> _int)))

(define windows-getcomputername
  (delay-ffi-obj "GetComputerNameExA" (force kernel32)
                 (_fun #:abi winapi _int _bytes _cvector -> _int)))

(define (gethostname)
  (case (system-type)
    [(unix macosx)
     (let ([ghn (force unix-gethostname)])
       (extract-terminated-string (lambda (s sz) (zero? (ghn s sz)))))]
    [(windows)
     (let ([gcn (force windows-getcomputername)]
           [DNS_FULLY_QUALIFIED 3])
       (extract-terminated-string
        (lambda (s sz)
          (let ([sz_ptr (cvector _int sz)])
            (and (not (zero? (gcn DNS_FULLY_QUALIFIED s sz_ptr)))
                 (let ([sz (cvector-ref sz_ptr 0)])
                   (when (sz . < . (bytes-length s)) (bytes-set! s sz 0))
                   #t))))))]
    [else #f]))

(provide gethostname)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getpid

(define unix-getpid
  (delay-ffi-obj "getpid" #f (_fun -> _int)))

(define windows-getpid
  (delay-ffi-obj "GetCurrentProcessId" (force kernel32)
                 (_fun #:abi 'stdcall -> _int)))

(define (getpid)
  ((force (case (system-type)
            [(macosx unix) unix-getpid]
            [(windows) windows-getpid]
            [else (error 'getpid "unknown platform ~e" (system-type))]))))

(provide getpid)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; truncate-file

;; From fcntl.h
(define O_RDONLY #x0000)
(define O_WRONLY #x0001)
(define O_RDWR   #x0002)
(define O_APPEND #x0008)
(define O_CREAT  #x0100)
(define O_TRUNC  #x0200)
(define O_EXCL   #x0400)

;; winize : string -> string
(define (winize fn-name)
  (if (eq? 'windows (system-type)) (string-append "_" fn-name) fn-name))

;; open : string int -> int
(define open
  (delay-ffi-obj (winize "open") #f (_fun #:save-errno 'posix _string _int -> _int)))

;; close : int -> int
(define close
  (delay-ffi-obj (winize "close") #f (_fun #:save-errno 'posix _int -> _int)))

;; ftruncate : int int -> int
(define ftruncate
  (if (eq? 'windows (system-type))
    (delay-ffi-obj "_chsize" #f (_fun #:save-errno 'posix _int _llong -> _int))
    (delay-ffi-obj "ftruncate" #f (_fun #:save-errno 'posix _int _llong -> _int))))

;; on-c-fail : int (-> X) int or X
(define (on-c-fail thunk fail-k)
  (let ([val (thunk)])
    (cond
     [(> val -1) val]
     [(= (saved-errno) (lookup-errno 'EINTR))
      ;; interrupted by a signal; retry
      (on-c-fail thunk fail-k)]
     [else (fail-k)])))

(define scheme_security_check_file
  (delay-ffi-obj "scheme_security_check_file" #f
                 (_fun _string _string _int -> _void)))
(define SCHEME_GUARD_FILE_WRITE #x2)

;; truncate-file : path int -> void
(define truncate-file
  (opt-lambda (file [size 0])
    (when (not (path-string? file))
      (error 'truncate-file
             "expects argument of type <string> or <path>; given ~s" file))
    (when (not (integer? size))
      (error 'truncate-file
             "expects argument of type <integer>; given ~s" size))
    ((force scheme_security_check_file)
     "truncate-file"
     (if (path? file) (path->string file) file)
     SCHEME_GUARD_FILE_WRITE)
    (let ([fd (on-c-fail
               (lambda ()
                 ((force open) file O_WRONLY))
               (lambda ()
                 (error 'truncate-file "could not open file")))])
      (on-c-fail
       (lambda ()
         ((force ftruncate) fd size))
       (lambda ()
         ((force close) fd)
         (error 'truncate-file "could not truncate file")))
      (on-c-fail
       (lambda ()
         ((force close) fd))
       void)
      (void))))

(provide truncate-file)
