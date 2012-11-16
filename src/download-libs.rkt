#lang racket/base

(require racket/tcp)
(provide do-download)

(define url-host "download.racket-lang.org")
(define url-path "/libs/9/")
(define url-base (string-append "http://" url-host url-path))
(define architecture #f) ;; set in `do-download'

(define (delete-path path)
  (cond [(directory-exists? path)
         (parameterize ([current-directory path])
           (for-each delete-path (directory-list)))
         (delete-directory path)]
        [(or (file-exists? path) (link-exists? path)) (delete-file path)]))

(define (purify-port port)
  (let ([m (regexp-match-peek-positions #rx#"^HTTP/.*?(?:\r\n\r\n|\n\n|\r\r)"
                                        port)])
    (if m (read-bytes (cdar m) port) "")))

(define (copy-port src dest)
  (let ([s (make-bytes 4096)])
    (let loop ()
      (let ([c (read-bytes-avail! s src)])
        (cond [(number? c)
               (let loop ([start 0])
                 (unless (= start c)
                   (let ([c2 (write-bytes-avail s dest start c)])
                     (loop (+ start c2)))))
               (loop)]
              ;; Must be EOF
              [else (void)])))))

(define (download* file target)
  (define src (format "~a~a/~a" url-path architecture file))
  (define-values [i o] (tcp-connect url-host 80))
  (fprintf o "GET ~a HTTP/1.0\r\nHost: ~a\r\n\r\n" src url-host)
  (flush-output o) (tcp-abandon-port o)
  (purify-port i)
  (call-with-output-file target #:exists 'truncate/replace
    (λ (out) (copy-port i out))))

(define (download file size)
  (define tmp (format "~a.download" file))
  (let loop ([n 0])
    (when (> n 0) (printf " retry #~a," n) (flush-output))
    (define thd (thread (λ () (download* file tmp))))
    (unless (sync/timeout (+ 10 (* 5 n)) thd)
      (kill-thread thd)
      (when (> n 3) (raise-user-error 'download "could not retrieve ~a" file))
      (when (zero? n) (printf " timeout,"))
      (loop (add1 n))))
  (rename-file-or-directory tmp file #t)
  (define sz (file-size file))
  (unless (= size sz)
    (eprintf "\n")
    (raise-user-error 'get-libs
                      "size of ~a is ~a; doesn't match expected size ~a"
                      file sz size)))

(define (unpack-tgz tgz)
  (printf " unpacking...") (flush-output)
  (define-values [p pout pin perr]
    (subprocess
     (current-output-port) (current-input-port) (current-error-port)
     (find-executable-path "tar") "zxf" tgz))
  (subprocess-wait p)
  (delete-file tgz))

(define (install file)
  (cond [(regexp-match? #rx"[.]tgz" file) (unpack-tgz file)]
        [else (eprintf "\n")
              (raise-user-error 'get-libs "don't know how to install file: ~a"
                                file)]))

(define (do-download needed really-needed arch)
  (set! architecture arch)
  (printf ">> Downloading files from\n>>   ~a~a\n" url-base architecture)
  (printf ">> If you don't want automatic download, download each file\n")
  (printf ">> yourself from there to\n")
  (printf ">>   ~a\n" (path->complete-path (current-directory)))
  (for ([file+size (in-list needed)])
    (define file (car file+size))
    (define size (cadr file+size))
    (printf "  ~a" file)
    (if (member file+size really-needed)
        (begin (printf " downloading...") (flush-output)
               (download file size)
               (when (pair? (cddr file+size))
                 (delete-path (caddr file+size))
                 (install file))
               (printf " done.\n"))
        (printf " already exists.\n"))))
