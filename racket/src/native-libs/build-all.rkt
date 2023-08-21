#lang racket/base
(require racket/system
         racket/format
         racket/runtime-path
         racket/list
         "cmdline.rkt")

(define (get-package-names win?)
  (append
   '("pkg-config")
   (cond
    [win?
     (append
      '("sed"
        "libiconv")
      (if aarch64?
          null
          '("longdouble")))]
    [else
     null])
   (cond
    [(or win? linux?)
     '("sqlite"
       "zlib")]
    [else
     null])
   (cond
     [(and win?
           aarch64?)
      '("openssl-3")]
     [else
      '("openssl-1")])
   '("expat"
     "gettext")
   (cond
    [linux?
     '("inputproto"
       "xproto"
       "xtrans"
       "kbproto"
       "xextproto"
       "renderproto"
       "libpthread-stubs"
       "libXau"
       "xcb-proto"
       "libxcb"
       "libX11"
       "libXext"
       "libXrender"
       "freefont")]
    [else null])
   (cond
     [win? null]
     [else '("libuuid")])
   '("libffi"
     "glib"
     "libpng"
     "freetype"
     "fontconfig"
     "pixman"
     "cairo"
     "harfbuzz"
     "fribidi"
     "pango"
     "gmp"
     "mpfr"
     "jpeg"
     "atk"
     "poppler")
   (cond
    [mac?
     '("libedit")]
    [else null])
   (cond
    [linux?
     '("gdk-pixbuf"
       "gtk+")]
    [else null])))

(define-runtime-path build-rkt "build.rkt")

(build-command-line)

(define package-names (get-package-names win?))

(for ([package-name (in-list package-names)])
  (printf "~a\n" (make-string 72 #\=))
  (cond
   [(file-exists? (build-path "dest" "stamps" package-name))
    (printf "Done already: ~a\n" package-name)]
   [else
    (printf "Building ~a\n" package-name)
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-command-line-arguments
                    (list->vector
                     (append
                      (list (if win? "--win" (if linux? "--linux" "--mac"))
                            (if m32?
                                (if ppc? "--mppc" "--m32")
                                (if aarch64? "--maarch64" "--m64")))
                      (cons "--archives"
                            (add-between (map ~a archives-dirs)
                                         "--archives"))
                      (list package-name)))])
      (dynamic-require build-rkt #f))]))
