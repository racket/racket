#lang racket/base
(require racket/system
         racket/format
         racket/runtime-path
         "cmdline.rkt")

(define (get-package-names win?)
  (append
   '("pkg-config")
   (if win?
       '("sed"
         "longdouble"
         "libiconv"
         "openssl"
         "zlib")
       null)
   '("expat"
     "gettext"
     "libffi"
     "glib"
     "libpng"
     "freetype"
     "fontconfig"
     "pixman"
     "cairo"
     "harfbuzz"
     "pango"
     "gmp"
     "mpfr"
     "jpeg"
     "poppler")))

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
                    (vector (if win? "--win" "--mac")
                            (if m32? (if ppc? "--mppc" "--m32") "--m64")
                            "--archives" (~a archives-dir)
                            package-name)])
      (dynamic-require build-rkt #f))]))
