#lang racket/base
(require racket/promise
         '#%utils
         (for-syntax racket/base))

;; ----------------------------------------
;; "config"

(define (find-config-dir)
  (find-main-config))

(provide find-config-dir)

;; ----------------------------------------
;; config: definitions

(define config-table
  (delay/sync
   (let ([d (find-config-dir)])
     (if d
         (let ([p (build-path d "config.rktd")])
           (if (file-exists? p)
               (call-with-input-file* 
                p
                (lambda (in) 
                  (call-with-default-reading-parameterization
                   (lambda ()
                     (read in)))))
               #hash()))
         #hash()))))

(define (to-path l)
  (cond [(string? l) (simplify-path (complete-path (string->path l)))]
        [(bytes? l) (simplify-path (complete-path (bytes->path l)))]
        [(list? l) (map to-path l)]
        [else l]))

(define (complete-path p)
  (cond [(complete-path? p) p]
        [else
         (path->complete-path
          p
          (find-main-collects))]))

(define-syntax-rule (define-config name key wrap)
  (define name (delay/sync
                 (wrap 
                  (hash-ref (force config-table) key #f)))))

(define-config config:collects-search-dirs 'collects-search-dirs to-path)
(define-config config:doc-dir 'doc-dir to-path)
(define-config config:doc-search-dirs 'doc-search-dirs to-path)
(define-config config:dll-dir 'dll-dir to-path)
(define-config config:lib-dir 'lib-dir to-path)
(define-config config:lib-search-dirs 'lib-search-dirs to-path)
(define-config config:share-dir 'share-dir to-path)
(define-config config:apps-dir 'apps-dir to-path)
(define-config config:include-dir 'include-dir to-path)
(define-config config:include-search-dirs 'include-search-dirs to-path)
(define-config config:bin-dir 'bin-dir to-path)
(define-config config:man-dir 'man-dir to-path)
(define-config config:links-file 'links-file to-path)
(define-config config:links-search-files 'links-search-files to-path)
(define-config config:pkgs-dir 'pkgs-dir to-path)
(define-config config:pkgs-search-dirs 'pkgs-search-dirs to-path)
(define-config config:cgc-suffix 'cgc-suffix values)
(define-config config:3m-suffix '3m-suffix values)
(define-config config:absolute-installation? 'absolute-installation? (lambda (x) (and x #t)))
(define-config config:doc-search-url 'doc-search-url values)
(define-config config:doc-open-url 'doc-open-url values)
(define-config config:installation-name 'installation-name values)
(define-config config:build-stamp 'build-stamp values)

(provide get-absolute-installation?
         get-cgc-suffix
         get-3m-suffix
         get-doc-search-url
         get-doc-open-url
         get-installation-name
         get-build-stamp)

(define (get-absolute-installation?) (force config:absolute-installation?))
(define (get-cgc-suffix) (force config:cgc-suffix))
(define (get-3m-suffix) (force config:3m-suffix))
(define (get-doc-search-url) (or (force config:doc-search-url)
                                 "http://docs.racket-lang.org/local-redirect/index.html"))
(define (get-doc-open-url) (force config:doc-open-url))
(define (get-installation-name) (or (force config:installation-name)
                                    (version)))
(define (get-build-stamp) (force config:build-stamp))

;; ----------------------------------------
;;  "collects"

(provide find-collects-dir
         get-main-collects-search-dirs
         find-user-collects-dir
         get-collects-search-dirs)
(define (find-collects-dir)
  (find-main-collects))
(define (get-main-collects-search-dirs)
  (combine-search (force config:collects-search-dirs)
                  (list (find-collects-dir))))
(define user-collects-dir
  (delay/sync (simplify-path (build-path (find-system-path 'addon-dir)
                                         (get-installation-name)
                                         "collects"))))
(define (find-user-collects-dir)
  (force user-collects-dir))
(define (get-collects-search-dirs)
  (current-library-collection-paths))

;; ----------------------------------------
;; Helpers

(define (single p) (if p (list p) null))
(define (extra a l) (if (and a (not (member (path->directory-path a)
                                            (map path->directory-path l))))
                        (append l (list a))
                        l))
(define (combine-search l default)
  ;; Replace #f in list with default path:
  (if l
      (let loop ([l l])
        (cond
         [(null? l) null]
         [(not (car l)) (append default (loop (cdr l)))]
         [else (cons (car l) (loop (cdr l)))]))
      default))
(define (cons-user u r)
  (if (and u (use-user-specific-search-paths))
      (cons u r)
      r))
(define (get-false) #f)
(define (chain-to f) f)

(define-syntax (define-finder stx)
  (syntax-case stx (get-false chain-to)
    [(_ provide config:id id user-id #:default user-default default)
     #'
     (begin
       (define-finder provide config:id id get-false default)
       (provide user-id)
       (define user-dir
         (delay/sync (simplify-path (build-path (find-system-path 'addon-dir)
                                                (get-installation-name)
                                                user-default))))
       (define (user-id)
         (force user-dir)))]
    [(_ provide config:id id user-id config:search-id search-id default)
     #'
     (begin
       (define-finder provide config:id id user-id default)
       (provide search-id)
       (define (search-id)
         (combine-search (force config:search-id)
                         (cons-user (user-id) (single (id))))))]
    [(_ provide config:id id user-id config:search-id search-id
        extra-search-dir default)
     #'
     (begin
       (define-finder provide config:id id user-id default)
       (provide search-id)
       (define (search-id)
         (combine-search (force config:search-id)
                         (extra (extra-search-dir)
                                (cons-user (user-id) (single (id)))))))]
    [(_ provide config:id id get-false (chain-to get-default))
     (with-syntax ([dir (generate-temporaries #'(id))])
       #'(begin
           (provide id)
           (define dir
             (delay/sync
               (or (force config:id) (get-default))))
           (define (id)
             (force dir))))]
    [(_ provide config:id id get-false default)
     (with-syntax ([dir (generate-temporaries #'(id))])
       #'(begin
           (provide id)
           (define dir
             (delay/sync
               (or (force config:id)
                   (let ([p (find-collects-dir)])
                     (and p (simplify-path (build-path p 'up default)))))))
           (define (id)
             (force dir))))]
    [(_ provide config:id id user-id default)
     #'(define-finder provide config:id id user-id #:default default default)]))

(define-syntax no-provide (syntax-rules () [(_ . rest) (begin)]))

(provide define-finder)

;; ----------------------------------------
;; "doc"

(define delayed-#f (delay/sync #f))

(provide find-doc-dir
         find-user-doc-dir
         get-doc-search-dirs)
(define-finder no-provide
  config:doc-dir
  find-doc-dir
  find-user-doc-dir
  delayed-#f
  get-new-doc-search-dirs
  "doc")
;; For now, include "doc" pseudo-collections in search path:
(define (get-doc-search-dirs)
  (combine-search (force config:doc-search-dirs)
                  (append (get-new-doc-search-dirs)
                          (map (lambda (p) (build-path p "doc"))
                               (current-library-collection-paths)))))

;; ----------------------------------------
;; "include"

(define-finder provide
  config:include-dir
  find-include-dir
  find-user-include-dir
  config:include-search-dirs
  get-include-search-dirs
  "include")

;; ----------------------------------------
;; "lib"

(define-finder provide
  config:lib-dir
  find-lib-dir
  find-user-lib-dir
  config:lib-search-dirs
  get-lib-search-dirs
  "lib")

;; ----------------------------------------
;; "share"

(define-finder provide
  config:share-dir
  find-share-dir
  find-user-share-dir
  "share")

;; ----------------------------------------
;; "apps"

(define-finder provide
  config:apps-dir
  find-apps-dir
  find-user-apps-dir #:default (build-path "share" "applications")
  (chain-to (lambda () (build-path (find-share-dir) "applications"))))

;; ----------------------------------------
;; "man"

(define-finder provide
  config:man-dir
  find-man-dir
  find-user-man-dir
  "man")

;; ----------------------------------------
;; Executables

;; `setup/dirs`

(provide config:bin-dir)

;; ----------------------------------------
;; DLLs

;; See `setup/dirs`

(provide config:dll-dir)

;; ----------------------------------------
;; Links files

(provide find-links-file
         get-links-search-files
         find-user-links-file)

(define (find-links-file)
  (or (force config:links-file)
      (build-path (find-share-dir) "links.rktd")))
(define (get-links-search-files)
  (combine-search (force config:links-search-files)
                  (list (find-links-file))))

(define (find-user-links-file [vers (get-installation-name)])
  (build-path (find-system-path 'addon-dir)
              vers
              "links.rktd"))

;; ----------------------------------------
;; Packages

(define-finder provide
  config:pkgs-dir
  find-pkgs-dir
  get-false
  config:pkgs-search-dirs
  get-pkgs-search-dirs
  (chain-to (lambda () (build-path (find-share-dir) "pkgs"))))

(provide find-user-pkgs-dir)
(define (find-user-pkgs-dir [vers (get-installation-name)])
  (build-path (find-system-path 'addon-dir)
              vers
              "pkgs"))
