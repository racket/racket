#lang racket/base
(require racket/cmdline
         racket/tcp)
;; This program avoids racket/port and net/url, because
;; it is loaded without using bytecode.

(define mode (make-parameter 'download))
(define touch-ready? (make-parameter #f))

(define-values (src-dir dest-dir)
  (command-line
   #:once-any
   [("--download") "download mode (the default)" (mode 'download)]
   [("--install") "install mode" (mode 'install)]
   #:once-each
   [("--ready") "touch `ready' on download success" (touch-ready? #t)]
   #:args
   (src-dir dest-dir)
   (values src-dir dest-dir)))

(define url-host "github.com")
(define url-path "/mflatt/gracket-libs/raw/master/")
(define url-base (string-append "http://" url-host url-path))

(define needed-files
  (case (system-type)
    [(unix)
     ;; Pre-built binaries are for Windows and Mac only
     null]
    [(macosx)
     '("libcairo.2.dylib"
       "libintl.8.dylib"
       "libgio-2.0.0.dylib"
       "libjpeg.62.dylib"
       "libglib-2.0.0.dylib"
       "libpango-1.0.0.dylib"
       "libgmodule-2.0.0.dylib"
       "libpangocairo-1.0.0.dylib"
       "libgobject-2.0.0.dylib"
       "libpixman-1.0.dylib"
       "libgthread-2.0.0.dylib"
       "libpng14.14.dylib")]
    [(windows)
     '("freetype6.dll"
       "libgobject-2.0-0.dll"
       "libatk-1.0-0.dll"
       "libgtk-win32-2.0-0.dll"
       "libcairo-2.dll"
       "libjpeg-7.dll"
       "libexpat-1.dll"
       "libpango-1.0-0.dll"
       "libfontconfig-1.dll"
       "libpangocairo-1.0-0.dll"
       "libgdk-win32-2.0-0.dll"
       "libpangoft2-1.0-0.dll"
       "libgdk_pixbuf-2.0-0.dll"
       "libpangowin32-1.0-0.dll"
       "libgio-2.0-0.dll"
       "libpng14-14.dll"
       "libglib-2.0-0.dll"
       "libwimp.dll"
       "libgmodule-2.0-0.dll"
       "zlib1.dll"
       "gtkrc")]))

(define explained? #f)

(define (purify-port port)
  (let ([m (regexp-match-peek-positions
            #rx"^HTTP/.*?(?:\r\n\r\n|\n\n|\r\r)" port)])
    (if m (read-string (cdar m) port) "")))

(define (copy-port src dest)
  (let ([s (make-bytes 4096)])
    (let loop ()
      (let ([c (read-bytes-avail! s src)])
        (cond
          [(number? c)
           (let loop ([start 0])
             (unless (= start c)
               (let ([c2 (write-bytes-avail s dest start c)])
                 (loop (+ start c2)))))
           (loop)]
          [else
           ;; Must be EOF
           (void)])))))

(define (download-if-needed dest-dir file)
  (let ([dest (build-path dest-dir file)]
        [tmp (build-path dest-dir (format "~a.download" file))])
    (if (file-exists? dest)
        (printf " ~a is ready\n" file)
        (let ([src (format "~a~a/~a"
                           url-path
                           (system-library-subpath #f)
                           file)])
          (unless explained?
            (set! explained? #t)
            (printf ">> Downloading files from\n>>  ~a~a\n" url-base (system-library-subpath #f))
            (printf ">> If you don't want automatic download, downlaod each file\n")
            (printf ">> yourself from there to\n")
            (printf ">>  ~a\n" (path->complete-path dest-dir)))
          (printf " ~a downloading..." file)
          (flush-output)
          (let-values ([(i o) (tcp-connect url-host 80)])
            (fprintf o "GET ~a HTTP/1.0\r\n" (string-append src))
            (fprintf o "Host: ~a\r\n" url-host)
            (fprintf o "\r\n")
            (flush-output o)
            (tcp-abandon-port o)
            (purify-port i)
            (call-with-output-file tmp
              #:exists 'truncate/replace
              (lambda (out)
                (copy-port i out)))
            (rename-file-or-directory tmp dest #t)
            (printf "done\n"))))))

(define (same-content? f1 f2)
  ;; approximate:
  (and (file-exists? f1)
       (file-exists? f2)
       (= (file-size f1) (file-size f2))))

(define (install-file src dest)
  (unless (same-content? src dest)
    (printf "Updating ~a\n" dest)
    (when (file-exists? dest)
      (delete-file dest))
    (copy-file src dest)))

(case (mode)
  [(download)
   (let ([libs dest-dir])
     (unless (directory-exists? libs)
       (make-directory libs))
     (for-each (lambda (file)
                 (download-if-needed libs file))
               needed-files)
     (when (touch-ready?)
       (let ([ok (build-path libs "ready")])
         (unless (file-exists? ok)
           (with-output-to-file ok void)))))]
  [(install)
   (for-each (lambda (file)
               (install-file (build-path src-dir "libs" file)
                             (build-path dest-dir file)))
             needed-files)])
