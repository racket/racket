#lang racket/base

(require racket/cmdline racket/tcp)

;; This program avoids racket/port and net/url, because it is loaded
;; without using bytecode.

(define mode 'download)
(define touch #f)

(define url-host "download.racket-lang.org")
(define url-path "/libs/1/")
(define url-base (string-append "http://" url-host url-path))

(define all-files+sizes
  `(;; Core Libraries
    [core
     [win32/i386
      ["UnicoWS.dll" 245408]
      ["iconv.dll" 892928]
      ["libeay32.dll" 1089536]
      ["ssleay32.dll" 237568]]]
    ;; GUI Libraries
    [gui
     [i386-macosx
      ["libcairo.2.dylib" 848488]
      ["libintl.8.dylib" 57536]
      ["libgio-2.0.0.dylib" 748360]
      ["libjpeg.62.dylib" 412024]
      ["libglib-2.0.0.dylib" 1015008]
      ["libpango-1.0.0.dylib" 347180]
      ["libgmodule-2.0.0.dylib" 19016]
      ["libpangocairo-1.0.0.dylib" 84340]
      ["libgobject-2.0.0.dylib" 288252]
      ["libpixman-1.0.dylib" 459304]
      ["libgthread-2.0.0.dylib" 24592]
      ["libpng14.14.dylib" 182992]
      ["PSMTabBarControl.tgz" 89039]]
     [x86_64-macosx
      ["libcairo.2.dylib" 944552]
      ["libintl.8.dylib" 61016]
      ["libgio-2.0.0.dylib" 897624]
      ["libjpeg.62.dylib" 153360]
      ["libglib-2.0.0.dylib" 1162256]
      ["libpango-1.0.0.dylib" 394768]
      ["libgmodule-2.0.0.dylib" 19832]
      ["libpangocairo-1.0.0.dylib" 94952]
      ["libgobject-2.0.0.dylib" 344024]
      ["libpixman-1.0.dylib" 499440]
      ["libgthread-2.0.0.dylib" 21728]
      ["libpng14.14.dylib" 192224]
      ["PSMTabBarControl.tgz" 105765]]
     [ppc-macosx
      ["libcairo.2.dylib" 2716096]
      ["libintl.8.dylib" 133156]
      ["libgio-2.0.0.dylib" 936176]
      ["libjpeg.62.dylib" 209688]
      ["libglib-2.0.0.dylib" 1242368]
      ["libpango-1.0.0.dylib" 761292]
      ["libgmodule-2.0.0.dylib" 19872]
      ["libpangocairo-1.0.0.dylib" 199440]
      ["libgobject-2.0.0.dylib" 352728]
      ["libpixman-1.0.dylib" 1366816]
      ["libgthread-2.0.0.dylib" 25416]
      ["libpng14.14.dylib" 505920]
      ["PSMTabBarControl.tgz" 95862]]
     [win32/i386
      ["libjpeg-7.dll" 233192]
      ["libcairo-2.dll" 921369]
      ["libpango-1.0-0.dll" 336626]
      ["libexpat-1.dll" 143096]
      ["libpng14-14.dll" 219305]
      ["zlib1.dll" 55808]
      ["freetype6.dll" 535264]
      ["libfontconfig-1.dll" 279059]
      ["libglib-2.0-0.dll" 1110713]
      ["libgobject-2.0-0.dll" 316586]
      ["libgmodule-2.0-0.dll" 31692]
      ["libpangocairo-1.0-0.dll" 94625]
      ["libpangowin32-1.0-0.dll" 102210]
      ["libpangoft2-1.0-0.dll" 679322]
      ["libplplot.dll" 245760]
      ["libfit.dll" 73728]
      ,@(if (getenv "PLT_WIN_GTK")
          '(["libatk-1.0-0.dll" 153763]
            ["libgtk-win32-2.0-0.dll" 4740156]
            ["libgdk-win32-2.0-0.dll" 827670]
            ["libgdk_pixbuf-2.0-0.dll" 252150]
            ["libgio-2.0-0.dll" 669318]
            ["libwimp.dll" 69632]
            ["gtkrc" 1181])
          '())]]))

(define-values (package src-dir dest-dir)
  (command-line
   #:once-any
   [("--download") "download mode (the default)" (set! mode 'download)]
   [("--install") "install mode" (set! mode 'install)]
   #:once-each
   [("--touch") file "touch `<file>' on download success" (set! touch file)]
   #:args [package src-dir dest-dir]
   (values (string->symbol package) src-dir dest-dir)))

(define (unixize p)
  (let-values ([(base name dir?) (split-path p)])
    (if (path? base)
      (string-append (unixize base) "/" (path->string name))
      (path->string name))))

(define needed-files+sizes
  (let* ([files+sizes
          (cdr (or (assq package all-files+sizes)
                   (error 'get-libs "bad package: ~s, expecting one of ~s"
                          package (map car all-files+sizes))))]
         [arch (unixize (system-library-subpath))]
         [arch (string->symbol (regexp-replace #rx"/3m$" arch ""))])
    (cond [(assq arch files+sizes) => cdr]
          [else '()])))

(define explained? #f)

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

(define (download-if-needed dest-dir file size)
  (define dest (build-path dest-dir file))
  (if (and (file-exists? dest) (= (file-size dest) size))
    (printf " ~a is ready\n" file)
    (let* ([sub (unixize (system-library-subpath #f))]
           [src (format "~a~a/~a" url-path sub file)])
      (unless explained?
        (set! explained? #t)
        (printf ">> Downloading files from\n>>  ~a~a\n" url-base sub)
        (printf ">> If you don't want automatic download, download each file\n")
        (printf ">> yourself from there to\n")
        (printf ">>  ~a\n" (path->complete-path dest-dir)))
      (printf "  ~a downloading..." file)
      (flush-output)
      (define-values [i o] (tcp-connect url-host 80))
      (fprintf o "GET ~a HTTP/1.0\r\n" src)
      (fprintf o "Host: ~a\r\n" url-host)
      (fprintf o "\r\n")
      (flush-output o)
      (tcp-abandon-port o)
      (purify-port i)
      (define tmp (build-path dest-dir (format "~a.download" file)))
      (call-with-output-file tmp #:exists 'truncate/replace
        (lambda (out) (copy-port i out)))
      (rename-file-or-directory tmp dest #t)
      (let ([sz (file-size dest)])
        (unless (= size sz)
          (eprintf "\n")
          (raise-user-error
           'get-libs "size of ~a is ~a; doesn't match expected size ~a"
           dest sz size)))
      (printf "done\n"))))

(define (same-content? f1 f2)
  ;; approximate:
  (and (file-exists? f1) (file-exists? f2) (= (file-size f1) (file-size f2))))

(define (install-file src dest)
  (if (regexp-match? #rx"[.]tgz" (path->string src))
    ;; Unpack tar file:
    (unpack-tgz src dest)
    ;; Plain copy:
    (unless (same-content? src dest)
      (printf "Updating ~a\n" dest)
      (when (file-exists? dest) (delete-file dest))
      (copy-file src dest))))

(define (unpack-tgz src* dest)
  (define src (path->string (path->complete-path src*)))
  (parameterize ([current-directory
                  (let-values ([(base name dir?) (split-path dest)]) base)])
    (define-values [p pout pin perr]
      (subprocess
       (current-output-port) (current-input-port) (current-error-port)
       (find-executable-path "tar") "zxf" src))
    (subprocess-wait p)))

(case mode
  [(download)
   (unless (directory-exists? dest-dir) (make-directory dest-dir))
   (for ([file+size (in-list needed-files+sizes)])
     (download-if-needed dest-dir (car file+size) (cadr file+size)))
   (when touch
     (define ok (build-path dest-dir touch))
     (when (file-exists? ok) (delete-file ok))
     (unless (file-exists? ok) (with-output-to-file ok void)))]
  [(install)
   (for ([file+size (in-list needed-files+sizes)])
     (define file (car file+size))
     (install-file (build-path src-dir "libs" file)
                   (build-path dest-dir file)))])
