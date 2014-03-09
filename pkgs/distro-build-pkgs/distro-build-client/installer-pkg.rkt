#lang at-exp racket/base
(require racket/system
         racket/file
         racket/format
         racket/runtime-path
         ds-store
         ds-store/alias
         xml)

(provide installer-pkg)

(define pkgbuild "/usr/bin/pkgbuild")
(define productbuild "/usr/bin/productbuild")

(define-runtime-path bg-image "macosx-installer/pkg-bg.png")

(define (system*/show . l)
  (displayln (apply ~a #:separator " " l))
  (flush-output)
  (unless (apply system* l)
    (error "failed")))

(define (gen-install-script install-dest)
  (~a "#!/bin/sh\n"
      "echo \"" (regexp-replace* #rx"\""
                                 install-dest 
                                 "\"'\"'\"")
      "\"/bin > /etc/paths.d/racket\n"))

(define (make-pkg human-name src-dir pkg-name readme sign-identity)
  (define install-dest (string-append "/Applications/" human-name))
  (define id (string-append "org.racket-lang." 
                            (regexp-replace* #rx" "
                                             human-name
                                             "-")))

  (define (make-rel dir-name)
    (let-values ([(base name dir?) (split-path src-dir)])
      (build-path base dir-name)))

  (define work-dir (make-rel "work"))
  (delete-directory/files work-dir #:must-exist? #f)
  (define scripts-dir (make-rel "scripts"))
  (delete-directory/files scripts-dir #:must-exist? #f)
  (define resources-dir (make-rel "resources"))
  (delete-directory/files resources-dir #:must-exist? #f)

  (printf "Creating ~a\n" scripts-dir)
  (make-directory* scripts-dir)
  (define postinstall (build-path scripts-dir "postinstall"))
  (call-with-output-file*
   postinstall
   (lambda (o)
     (write-string (gen-install-script install-dest) o)))
  (file-or-directory-permissions postinstall #o770)

  (printf "Creating ~a\n" resources-dir)
  (make-directory* resources-dir)
  (copy-file bg-image (build-path resources-dir "background.png"))

  (printf "Copying ~a\n" src-dir)
  (define dest-dir work-dir)
  (copy-directory/files src-dir dest-dir
                        #:keep-modify-seconds? #t)
  (when readme
    (call-with-output-file*
     (build-path dest-dir "README.txt")
     #:exists 'truncate
     (lambda (o)
       (display readme o))))
  (copy-file (build-path dest-dir "README.txt")
             (build-path resources-dir "README.txt"))

  (system*/show pkgbuild
                "--root" dest-dir
                "--install-location" install-dest
                "--scripts" scripts-dir
                "--identifier" id
                "--version" (version)
                (make-rel "racket.pkg"))
  (define pkg-xml (make-rel "racket.xml"))
  (system*/show productbuild
                "--synthesize" 
                "--package" (make-rel "racket.pkg")
                pkg-xml)
  (define synthesized (call-with-input-file*
                       pkg-xml
                       read-xml))
  (define updated
    (struct-copy document synthesized
                 [element (let ([e (document-element synthesized)])
                            (struct-copy element e
                                         [content
                                          (list*
                                           (element #f #f
                                                    'title
                                                    null
                                                    (list (pcdata #f #f human-name)))                                           
                                           (element #f #f
                                                    'readme
                                                    (list (attribute #f #f 'file "README.txt"))
                                                    null)
                                           (element #f #f
                                                    'background
                                                    (list (attribute #f #f 'file "background.png")
                                                          (attribute #f #f 'alignment "topleft")
                                                          (attribute #f #f 'scaling "none"))
                                                    null)
                                           (element-content e))]))]))
  (call-with-output-file*
   pkg-xml
   #:exists 'truncate
   (lambda (o)
     (write-xml updated o)))
  (apply system*/show
         productbuild
         (append
          (list "--distribution" pkg-xml
                "--package-path" (make-rel 'same)
                "--resources" resources-dir
                "--identifier" id
                "--version" (version))
          (if (string=? sign-identity "")
              null
              (list "--sign" sign-identity))
          (list pkg-name))))

(define (installer-pkg human-name base-name dist-suffix readme sign-identity)
  (define pkg-name (format "bundle/~a-~a~a.pkg"
                           base-name
                           (system-library-subpath #f)
                           dist-suffix))
  (make-pkg human-name "bundle/racket" pkg-name readme sign-identity)
  pkg-name)
