#lang racket/base

(require racket/cmdline racket/tcp)

;; This program avoids racket/port and net/url, because it is loaded
;; without using bytecode.

(define url-host "download.racket-lang.org")
(define url-path "/libs/1/")
(define url-base (string-append "http://" url-host url-path))

(provide all-files+sizes)
(define all-files+sizes
  ;; alist mapping package to
  ;;   alist mapping architecture to
  ;;     a list of entries, each has filename and size
  ;;     and optionally a path that it would install to and the installed size
  `(;; Core Libraries
    [core
     [win32/i386
      ["iconv.dll" 892928]
      ["libeay32.dll" 1089536]
      ["ssleay32.dll" 237568]]
     [win32/x86_64
     ["libiconv-2.dll" 1378028]
     ["libeay32.dll" 1293824]
     ["ssleay32.dll" 260608]]]
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
      ["PSMTabBarControl.tgz" 89039 "PSMTabBarControl.framework" 247760]]
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
      ["PSMTabBarControl.tgz" 105765 "PSMTabBarControl.framework" 316512]]
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
      ["PSMTabBarControl.tgz" 95862 "PSMTabBarControl.framework" 229493]]
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
          '())]
     [win32/x86_64
      ["libjpeg-7.dll" 224768]
      ["libcairo-2.dll" 1266147]
      ["libpango-1.0-0.dll" 423199]
      ["libexpat-1.dll" 263006]
      ["libpng14-14.dll" 272473]
      ["zlib1.dll" 191825]
      ["libfreetype-6.dll" 633649]
      ["libintl-8.dll" 240862]
      ["libfontconfig-1.dll" 339943]
      ["libglib-2.0-0.dll" 1267577]
      ["libgobject-2.0-0.dll" 425888]
      ["libgmodule-2.0-0.dll" 119538]
      ["libpangocairo-1.0-0.dll" 185168]
      ["libpangowin32-1.0-0.dll" 192656]
      ["libpangoft2-1.0-0.dll" 1188615]
      ["libplplot.dll" 248832]
      ["libfit.dll" 69120]]]))

(define-values [package dest-dir]
  (command-line #:args [package [dest-dir (current-directory)]]
    (values (string->symbol package) dest-dir)))

(define (unixize p)
  (let-values ([(base name dir?) (split-path p)])
    (if (path? base)
      (string-append (unixize base) "/" (path->string name))
      (path->string name))))

(define architecture (string->symbol (unixize (system-library-subpath #f))))

(define (needed-files+sizes)
  (let ([files+sizes
         (cdr (or (assq package all-files+sizes)
                  (error 'get-libs "bad package: ~s, expecting one of ~s"
                         package (map car all-files+sizes))))])
    (cond [(assq architecture files+sizes) => cdr]
          [else '()])))

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

(define (download file size)
  (define src (format "~a~a/~a" url-path architecture file))
  (define-values [i o] (tcp-connect url-host 80))
  (fprintf o "GET ~a HTTP/1.0\r\nHost: ~a\r\n\r\n" src url-host)
  (flush-output o) (tcp-abandon-port o)
  (purify-port i)
  (define tmp (format "~a.download" file))
  (call-with-output-file tmp #:exists 'truncate/replace
    (lambda (out) (copy-port i out)))
  (rename-file-or-directory tmp file #t)
  (let ([sz (file-size file)])
    (unless (= size sz)
      (eprintf "\n")
      (raise-user-error 'get-libs
                        "size of ~a is ~a; doesn't match expected size ~a"
                        file sz size))))

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

(define (delete-path path)
  (cond [(directory-exists? path)
         (parameterize ([current-directory path])
           (for-each delete-path (directory-list)))
         (delete-directory path)]
        [(or (file-exists? path) (link-exists? path)) (delete-file path)]))

(define (directory-size dir)
  (parameterize ([current-directory dir])
    (for/fold ([sum 0]) ([path (in-list (directory-list))])
      (+ sum (path-size path)))))

(define (path-size path)
  (cond [(file-exists? path) (file-size path)]
        [(directory-exists? path) (directory-size path)]
        [else 0]))

(define got-path? ; approximate, using size
  (case-lambda [(path size unpacked-path unpacked-size)
                (got-path? unpacked-path unpacked-size)]
               [(path size)
                (equal? size (path-size path))]))

(unless (eq? package 'nothing)
  (unless (directory-exists? dest-dir) (make-directory dest-dir))
  (parameterize ([current-directory dest-dir])
    (define needed (needed-files+sizes))
    (define really-needed
      (filter (lambda (n) (not (apply got-path? n))) needed))
    (printf (if (null? needed)
              ">> No ~a libraries to download for ~a\n"
              ">> Getting ~a libraries for ~a\n")
            package architecture)
    (cond
      [(null? needed) (void)]
      [(null? really-needed)
       (printf ">> All files present, no downloads needed.\n")]
      [else
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
           (printf " already exists.\n")))])))
