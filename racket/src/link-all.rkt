#lang racket/base
(require racket/cmdline
         racket/file
         racket/list
         racket/format
         racket/string
         pkg/lib)

(define only-platform? #f)

(define dirs
  (command-line
   #:once-each
   [("--platform") "Only packages whose names match the platform name"
    (set! only-platform? #t)]
   #:args
   dir
   dir))

(define found (make-hash))

(define rx:platform (regexp
                     (regexp-quote
                      (apply
                       ~a
                       #:separator "-"
                       (map path->string
                            (explode-path (system-library-subpath #f)))))))

;; Recur through directory tree, and treat each directory
;; that has an "info.rkt" file as a package (and don't recur
;; further into the package)
(for ([src-dir (in-list dirs)])
  (when (directory-exists? src-dir)
    (let loop ([src-dir src-dir])
      (for ([f (in-list (directory-list src-dir))])
        (define src-f (build-path src-dir f))
        (cond
         [(file-exists? (build-path src-f "info.rkt"))
          (when (or (not only-platform?)
                    (regexp-match? rx:platform f))
            (define f-name (path->string f))
            (when (hash-ref found f-name #f)
              (error 'pack-local 
                     "found packages multiple times: ~a and ~a"
                     (hash-ref found f)
                     src-f))
            (hash-set! found f-name src-f))]
         [(directory-exists? src-f)
          (loop src-f)])))))

;; Remove links that are no longer present or where the
;; directory shape has changed.
(let ([pkgs-exes (map explode-path (map path->complete-path dirs))])
  (for ([(name info) (in-hash (installed-pkg-table #:scope 'installation))])
    (when (eq? 'link (car (pkg-info-orig-pkg info)))
      (define dir (cadr (pkg-info-orig-pkg info)))
      (define ex (explode-path dir))
      (when (for/or ([pkgs-ex (in-list pkgs-exes)])
              (and ((length pkgs-ex) . < . (length ex))
                   (equal? pkgs-ex (take ex (length pkgs-ex)))))
        (when (or (not (hash-ref found name #f))
                  (not (equal?
                        (pkg-single-collection dir)
                        (and (sc-pkg-info? info)
                             (sc-pkg-info-collect info)))))
          (parameterize ([current-pkg-scope 'installation])
            (printf "Removing ~a\n" dir)
            (pkg-remove (list name)
                        #:force? #t)))))))

(void
 (parameterize ([current-pkg-scope 'installation])
   (define installed (installed-pkg-table #:scope 'installation))
   (pkg-install (for/list ([(name v) (in-hash found)]
                           #:when (not (hash-ref installed name #f)))
                  (printf "Adding ~a\n" v)
                  (pkg-desc (path->string v)
                            'link
                            #f
                            #f)))))
