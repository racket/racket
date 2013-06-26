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

;; truncate-file : path int -> void
(define truncate-file
  (opt-lambda (file [size 0])
    (when (not (path-string? file))
      (error 'truncate-file
             "expects argument of type <string> or <path>; given ~s" file))
    (when (not (integer? size))
      (error 'truncate-file
             "expects argument of type <integer>; given ~s" size))
    (let ([c (make-custodian)]) ; avoid leaks on errors
      (dynamic-wind
          void
          (lambda ()
            (define out (open-output-file file 'update))
            (file-truncate out size))
          (lambda ()
            (custodian-shutdown-all c))))))

(provide truncate-file)
