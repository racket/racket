#lang at-exp racket/base
(require racket/system
         racket/file
         racket/format
         racket/runtime-path
         ds-store
         ds-store/alias)

(provide installer-dmg
         make-dmg)

(define hdiutil "/usr/bin/hdiutil")
(define codesign "/usr/bin/codesign")

(define-runtime-path bg-image "macosx-installer/racket-rising.png")

(define (system*/show . l)
  (displayln (apply ~a #:separator " " l))
  (flush-output)
  (unless (apply system* l)
    (error "failed")))

(define (make-dmg volname src-dir dmg bg readme sign-identity)
  (define tmp-dmg (make-temporary-file "~a.dmg"))
  (define work-dir
    (let-values ([(base name dir?) (split-path src-dir)])
      (build-path base "work")))
  (when (file-exists? dmg) (delete-file dmg))
  (delete-directory/files work-dir #:must-exist? #f)
  (make-directory* work-dir)
  (printf "Copying ~a\n" src-dir)
  (define dest-dir (build-path work-dir volname))
  (copy-directory/files src-dir dest-dir
                        #:keep-modify-seconds? #t)
  (when readme
    (call-with-output-file*
     (build-path work-dir volname "README.txt")
     #:exists 'truncate
     (lambda (o)
       (display readme o))))
  (when bg
    (copy-file bg (build-path work-dir ".bg.png")))
  (unless (string=? sign-identity "")
    (sign-executables dest-dir sign-identity))
  ;; The following command should work fine, but it looks like hdiutil in 10.4
  ;; is miscalculating the needed size, making it too big in our case (and too
  ;; small with >8GB images).  It seems that it works to first generate an
  ;; uncompressed image and then convert it to a compressed one.
  ;;   hdiutil create -format UDZO -imagekey zlib-level=9 -ov \
  ;;           -mode 555 -volname volname -srcfolder . dmg
  ;; So, first create an uncompressed image...
  (parameterize ([current-directory work-dir])
    (system*/show hdiutil
                  "create" "-format" "UDRW" "-ov"
                  "-mode" "755" "-volname" volname "-srcfolder" "."
                  tmp-dmg))
  ;; Then do the expected dmg layout...
  (when bg
    (dmg-layout tmp-dmg volname ".bg.png"))
  ;; And create the compressed image from the uncompressed image:
  (system*/show hdiutil
                "convert" "-format" "UDBZ" "-imagekey" "zlib-level=9" "-ov"
                tmp-dmg "-o" dmg)
  (delete-file tmp-dmg))

(define (sign-executables dest-dir sign-identity)
  ;; Sign any executable in "bin", top-level ".app", or either of those in "lib"
  (define (check-bins dir)
    (for ([f (in-list (directory-list dir #:build? #t))])
      (when (and (file-exists? f)
                 (member 'execute (file-or-directory-permissions f))
                 (member (call-with-input-file 
                          f
                          (lambda (i)
                            (define bstr (read-bytes 4 i))
                            (and (bytes? bstr)
                                 (= 4 (bytes-length bstr))
                                 (integer-bytes->integer bstr #f))))
                         '(#xFeedFace #xFeedFacf)))
        (system*/show codesign "-s" sign-identity f))))
  (define (check-apps dir)
    (for ([f (in-list (directory-list dir #:build? #t))])
      (when (and (directory-exists? f)
                 (regexp-match #rx#".app$" f))
        (define name (let-values ([(base name dir?) (split-path f)])
                       (path-replace-suffix name #"")))
        (define exe (build-path f "Contents" "MacOS" name))
        (when (file-exists? exe)
          (system*/show codesign "-s" sign-identity exe)))))
  (check-bins (build-path dest-dir "bin"))
  (check-bins (build-path dest-dir "lib"))
  (check-apps dest-dir)
  (check-apps (build-path dest-dir "lib")))

(define (dmg-layout dmg volname bg)
  (define-values (mnt del?)
    (let ([preferred (build-path "/Volumes/" volname)])
      (if (not (directory-exists? preferred))
          ;; Use the preferred path so that the alias is as
          ;; clean as possible:
          (values preferred #f)
          ;; fall back to using a temporary directory
          (values (make-temporary-file "~a-mnt" 'directory) #t))))
  (system*/show hdiutil
                "attach" "-readwrite" "-noverify" "-noautoopen"
                "-mountpoint" mnt dmg)
  (define alias (path->alias-bytes (build-path mnt bg)
                                   #:wrt mnt))
  (make-file-or-directory-link "/Applications" (build-path mnt "Applications"))
  (define (->path s) (string->path s))
  (write-ds-store (build-path mnt ".DS_Store")
                  (list
                   (ds 'same 'BKGD 'blob 
                       (bytes-append #"PctB"
                                     (integer->integer-bytes (bytes-length alias) 4 #t #t)
                                     (make-bytes 4 0)))
                   (ds 'same 'ICVO 'bool #t)
                   (ds 'same 'fwi0 'blob 
                       ;; Window location (size overridden below), sideview off:
                       (fwind 160 320 540 1000 'icnv #f))
                   (ds 'same 'fwsw 'long 135) ; window sideview width?
                   (ds 'same 'fwsh 'long 380) ; window sideview height?
                   (ds 'same 'icgo 'blob #"\0\0\0\0\0\0\0\4") ; ???
                   (ds 'same 'icvo 'blob
                       ;; folder view options:
                       #"icv4\0\200nonebotm\0\0\0\0\0\0\0\0\0\4\0\0")
                   (ds 'same 'icvt 'shor 16) ; icon label size
                   (ds 'same 'pict 'blob alias)
                   (ds (->path ".bg.png") 'Iloc 'blob (iloc 900 180)) ; file is hidden, anway
                   (ds (->path "Applications") 'Iloc 'blob (iloc 500 180))
                   (ds (->path volname) 'Iloc 'blob (iloc 170 180))))
  (system*/show hdiutil "detach" mnt)
  (when del?
    (delete-directory mnt)))

(define (installer-dmg human-name base-name dist-suffix readme sign-identity)
  (define dmg-name (format "bundle/~a-~a~a.dmg"
                           base-name
                           (system-library-subpath #f)
                           dist-suffix))
  (make-dmg human-name "bundle/racket" dmg-name bg-image readme sign-identity)
  dmg-name)
