#lang racket/base
(require "../subprocess/main.rkt"
         "../security/main.rkt"
         "../file/main.rkt"
         "../port/main.rkt"
         "../locale/main.rkt"
         "../format/main.rkt"
         "../host/windows-version.rkt")

(provide get-machine-info)

(define uname-paths
  (list "/bin/uname"
        "/usr/bin/uname"
        "/sbin/uname"
        "/usr/sbin/uname"
        "/usr/local/bin/uname"
        "/usr/local/uname"))

(define (get-machine-info)
  (case (system-type)
    [(windows)
     (define-values (major minor build-number CSD-vers) (get-windows-version))
     (format "Windows NT ~a.~a (Build ~a)~a~a"
             major minor build-number
             (if (equal? CSD-vers #"") "" " ")
             CSD-vers)]
    [else
     (let/ec done
       (parameterize ([current-security-guard
                       (unsafe-make-security-guard-at-root)])
         (for ([uname (in-list uname-paths)])
           (when (file-exists? uname)
             (with-handlers (#;[exn:fail? void])
               (define-values (subproc stdout stdin stderr) (subprocess #f #f #f uname "-a"))
               (close-output-port stdin)
               (close-input-port stderr)
               (define bstr (read-bytes 1024 stdout))
               (close-input-port stdout)
               (subprocess-wait subproc)
               (when (bytes? bstr)
                 ;; Strip trailing whitespace, especially newlines
                 (let loop ([i (bytes-length bstr)])
                   (cond
                     [(zero? i) (done "")]
                     [(char-whitespace? (integer->char (bytes-ref bstr (sub1 i))))
                      (loop (sub1 i))]
                     [else
                      (done (bytes->string/locale (subbytes bstr 0 i)))])))))))
       "<unknown machine>")]))



