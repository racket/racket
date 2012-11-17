;; This program is written in #%kernel and
;; dynamic-requires the real downloading,
;; because it is loaded without using bytecode.
(module get-libs '#%kernel
  (#%require '#%paramz (for-syntax '#%kernel))
  (#%provide all-files+sizes)

  (define-values (all-files+sizes)
    ;; alist mapping package to
    ;;   alist mapping architecture to
    ;;    a list of entries, each has filename and size
    ;;    and optionally a path that it would install to and the installed size
    (list
     ;; Core Libraries
     '[core
       [win32/i386
        ["iconv.dll" 892928]
        ["libeay32.dll" 1099776]
        ["ssleay32.dll" 239104]]
       [win32/x86_64
        ["libiconv-2.dll" 1378028]
        ["libeay32.dll" 1503232]
        ["ssleay32.dll" 309760]]]
     ;; Math Libraries
     '[math
       [i386-macosx
        ["libgmp.10.dylib" 399304]
        ["libmpfr.4.dylib" 398552]]
       [x86_64-macosx
        ["libgmp.10.dylib" 429684]
        ["libmpfr.4.dylib" 676320]]
       [ppc-macosx
        ["libgmp.10.dylib" 387588]
        ["libmpfr.4.dylib" 553892]]
       [win32/i386
        ["libgmp-10.dll" 394766]
        ["libmpfr-4.dll" 411662]]
       [win32/x86_64
        ["libgmp-10.dll" 386048]
        ["libmpfr-4.dll" 441344]]]
     ;; GUI Libraries
     [list
      'gui
      '[i386-macosx
        ["libcairo.2.dylib" 802620]
        ["libffi.5.dylib" 22424]
        ["libintl.8.dylib" 63084]
        ["libgio-2.0.0.dylib" 1511444]
        ["libjpeg.62.dylib" 412024]
        ["libglib-2.0.0.dylib" 1272192]
        ["libpango-1.0.0.dylib" 351672]
        ["libgmodule-2.0.0.dylib" 18820]
        ["libpangocairo-1.0.0.dylib" 83928]
        ["libgobject-2.0.0.dylib" 308304]
        ["libpixman-1.0.dylib" 526716]
        ["libgthread-2.0.0.dylib" 12708]
        ["libpng15.15.dylib" 200876]
        ["PSMTabBarControl.tgz" 94103 "PSMTabBarControl.framework" 251764]]
      '[x86_64-macosx
        ["libcairo.2.dylib" 926648]
        ["libffi.5.dylib" 23568]
        ["libintl.8.dylib" 63156]
        ["libgio-2.0.0.dylib" 2136056]
        ["libjpeg.62.dylib" 153360]
        ["libglib-2.0.0.dylib" 1689952]
        ["libpango-1.0.0.dylib" 392432]
        ["libgmodule-2.0.0.dylib" 19768]
        ["libpangocairo-1.0.0.dylib" 96640]
        ["libgobject-2.0.0.dylib" 438192]
        ["libpixman-1.0.dylib" 633368]
        ["libgthread-2.0.0.dylib" 8592]
        ["libpng15.15.dylib" 214836]
        ["PSMTabBarControl.tgz" 156265 "PSMTabBarControl.framework" 450751]]
      '[ppc-macosx
        ["libcairo.2.dylib" 2620616]
        ["libffi.5.dylib" 67920]
        ["libintl.8.dylib" 132252]
        ["libgio-2.0.0.dylib" 937488]
        ["libjpeg.62.dylib" 209688]
        ["libglib-2.0.0.dylib" 1242448]
        ["libpango-1.0.0.dylib" 760792]
        ["libgmodule-2.0.0.dylib" 19476]
        ["libpangocairo-1.0.0.dylib" 195372]
        ["libgobject-2.0.0.dylib" 352680]
        ["libpixman-1.0.dylib" 1626104]
        ["libgthread-2.0.0.dylib" 25068]
        ["libpng15.15.dylib" 570228]
        ["PSMTabBarControl.tgz" 96039 "PSMTabBarControl.framework" 229501]]
      (append
       '[win32/i386
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
         ["libpangoft2-1.0-0.dll" 679322]]
       (if (getenv "PLT_WIN_GTK")
           '(["libatk-1.0-0.dll" 153763]
             ["libgtk-win32-2.0-0.dll" 4740156]
             ["libgdk-win32-2.0-0.dll" 827670]
             ["libgdk_pixbuf-2.0-0.dll" 252150]
             ["libgio-2.0-0.dll" 669318]
             ["libwimp.dll" 69632]
             ["gtkrc" 1181])
           '()))
      '[win32/x86_64
        ["libjpeg-8.dll" 214016]
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
        ["libgthread-2.0-0.dll" 126615]
        ["libpangocairo-1.0-0.dll" 185168]
        ["libpangowin32-1.0-0.dll" 192656]
        ["libpangoft2-1.0-0.dll" 1188615]]]
     ;; Database libraries
     '[db
       [win32/i386
        ["sqlite3.dll" 570947]]
       [win32/x86_64
        ["sqlite3.dll" 617472]]]
     ;; COM libraries
     '[com
       [win32/i386
        ["myssink.dll" 92672]]
       [win32/x86_64
        ["myssink.dll" 108032]]]))

  (define-values [package dest-dir]
    (let-values ([(args) (vector->list (current-command-line-arguments))])
      (let-values
          ([(package) (if (null? args)
                          (error 'get-libs "missing \'package\' command-line argument")
                          (car args))])
        (let-values ([(dd)
                      (if (null? (cdr args)) (current-directory) (cadr args))])
          (values (string->symbol package) dd)))))

  (define-values (unixize)
    (lambda (p)
      (let-values ([(base name dir?) (split-path p)])
        (if (path? base)
            (string-append (unixize base) "/" (path->string name))
            (path->string name)))))

  (define-values (architecture)
    (string->symbol (unixize (system-library-subpath #f))))

  (define-values (needed-files+sizes)
    (lambda ()
      (define-values (l) (assq package all-files+sizes))
      (define-values (files+sizes)
        (cdr (if l
                 l
                 (error 'get-libs "bad package: ~s, expecting one of ~s"
                        package (map car all-files+sizes)))))
      (define-values (arch) (assq architecture files+sizes))
      (if arch
          (cdr arch)
          '())))

  (define-values (directory-size)
    (lambda (dir)
      (define-values (loop)
        (lambda (l)
          (if (null? l)
              0
              (+ (path-size (build-path dir (car l))) (loop (cdr l))))))
      (loop (directory-list dir))))

  (define-values (path-size)
    (lambda (path)
      (if (file-exists? path) (file-size path)
          (if (directory-exists? path)
              (directory-size path)
              0))))

  (define-values (path-size/show)
    (lambda (path)
      (let-values ([(sz) (path-size path)])
        (if (getenv "PLT_SHOW_PATH_SIZES")
            (printf "~s ~s\n" path sz)
            (void))
        sz)))

  (define-values (got-path?) ; approximate, using size
    (case-lambda [(path size unpacked-path unpacked-size)
                  (got-path? unpacked-path unpacked-size)]
                 [(path size)
                  (equal? size (path-size/show path))]))

  ;; not provided by #%kernel
  (define-values (filter)
    (lambda (f l)
      (if (null? l)
          l
          (if (f (car l))
              (cons (car l) (filter f (cdr l)))
              (filter f (cdr l))))))

  (define-syntaxes (here-dir)
    (λ (stx)
      (define-values (base name dir?) (split-path (syntax-source stx)))
      (datum->syntax (quote-syntax 'here) base)))

  (if (eq? package 'nothing)
      (void)
      (begin
        (if (directory-exists? dest-dir) (void) (make-directory dest-dir))
        (with-continuation-mark parameterization-key
          (extend-parameterization
           (continuation-mark-set-first #f parameterization-key)
           current-directory dest-dir)
          (let-values ()
            (define-values (needed) (needed-files+sizes))
            (define-values (really-needed)
              (filter (λ (n) (not (apply got-path? n))) needed))
            (printf (if (null? needed)
                        ">> No ~a libraries to download for ~a\n"
                        ">> Getting ~a libraries for ~a\n")
                    package architecture)
            (if (null? needed)
                (void)
                (if (null? really-needed)
                    (printf ">> All files present, no downloads needed.\n")
                    ((dynamic-require (build-path here-dir "download-libs.rkt") 'do-download)
                     needed really-needed architecture))))))))
