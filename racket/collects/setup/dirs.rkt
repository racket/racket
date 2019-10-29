#lang racket/base
(require racket/promise
         racket/private/config
         compiler/private/winutf16
         compiler/private/mach-o
         setup/cross-system
         "private/dirs.rkt")

(provide (except-out (all-from-out "private/dirs.rkt")
                     config:dll-dir
                     config:bin-dir
                     config:config-tethered-console-bin-dir
                     config:config-tethered-gui-bin-dir
                     define-finder
                     get-config-table
                     to-path)
         find-cross-dll-dir
         find-dll-dir
         get-lib-search-dirs)

;; ----------------------------------------
;; Executables

(define-finder provide
  config:bin-dir
  find-console-bin-dir
  find-user-console-bin-dir
  (case (cross-system-type)
    [(windows) 'same]
    [(macosx unix) "bin"]))

(define-finder provide
  config:gui-bin-dir
  find-gui-bin-dir
  find-user-gui-bin-dir
  (case (cross-system-type)
    [(windows macosx) 'same]
    [(unix) "bin"]))

(provide find-config-tethered-console-bin-dir
         find-config-tethered-gui-bin-dir)

(define (find-config-tethered-console-bin-dir)
  (force config:config-tethered-console-bin-dir))

(define (find-config-tethered-gui-bin-dir)
  (force config:config-tethered-gui-bin-dir))

(provide find-addon-tethered-console-bin-dir
         find-addon-tethered-gui-bin-dir)

(define addon-bin-table
  (delay/sync
   (let ()
     (define f (build-path (find-system-path 'addon-dir)
                           "etc"
                           "config.rktd"))
     (and (file-exists? f)
          (call-with-input-file* 
           f
           (lambda (in) 
             (call-with-default-reading-parameterization
              (lambda ()
                (read in)))))))))

(define (find-addon-bin-dir key)
  (define t (force addon-bin-table))
  (and (hash? t)
       (let ([v (hash-ref t key #f)])
         (and (path-string? v)
              (simplify-path
               (path->complete-path
                v
                (build-path (find-system-path 'addon-dir)
                            "etc")))))))

(define (find-addon-tethered-console-bin-dir)
  (find-addon-bin-dir 'addon-tethered-console-bin-dir))

(define (find-addon-tethered-gui-bin-dir)
  (find-addon-bin-dir 'addon-tethered-gui-bin-dir))

;; ----------------------------------------
;; DLLs

(define (get-dll-dir get-system-type force-cross?)
  (delay/sync
    (case (get-system-type)
      [(windows)
       (if (and (eq? (system-type) 'windows)
                (not force-cross?))
           ;; Extract "lib" location from binary:
           (let ([exe (parameterize ([current-directory (find-system-path 'orig-dir)])
                        (find-executable-path (find-system-path 'exec-file)))])
             (and
              exe
              (with-input-from-file exe
                (lambda ()
                  (let ([m (regexp-match (byte-regexp
                                          (bytes-append
                                           (bytes->utf-16-bytes #"dLl dIRECTORy:")
                                           #"((?:..)*?)\0\0"))
                                         (current-input-port))])
                    (unless m
                      (error "cannot find \"dLl dIRECTORy\" tag in binary"))
                    (let-values ([(dir name dir?) (split-path exe)])
                      (if (regexp-match #rx#"^<" (cadr m))
                          ;; no DLL dir in binary
                          #f
                          ;; resolve relative directory:
                          (let ([p (bytes->path (utf-16-bytes->bytes (cadr m)))])
                            (path->complete-path p dir)))))))))
           ;; Cross-compile: assume it's "lib"
           (find-lib-dir))]
      [(macosx)
       (if (and (eq? (system-type) 'macosx)
                (not force-cross?))
           (let* ([exe (parameterize ([current-directory (find-system-path 'orig-dir)])
                         (let loop ([p (find-executable-path
                                        (find-system-path 'exec-file))])
                           (and 
                            p
                            (if (link-exists? p)
                                (loop (let-values ([(r) (resolve-path p)]
                                                   [(dir name dir?) (split-path p)])
                                        (if (and (path? dir)
                                                 (relative-path? r))
                                            (build-path dir r)
                                            r)))
                                p))))]
                  [rel (and exe
                            (let ([l (get/set-dylib-path exe "Racket" #f)])
                              (if (null? l)
                                  #f
                                  (car l))))])
             (cond
              [(not rel) #f] ; no framework reference found!?
              [(regexp-match
                #rx#"^(@executable_path/)?(.*?)G?Racket.framework"
                rel)
               => (lambda (m)
                    (let ([b (caddr m)])
                      (if (and (not (cadr m)) (bytes=? b #""))
                          #f ; no path in exe
                          (simplify-path
                           (path->complete-path
                            (if (not (cadr m))
                                (bytes->path b)
                                (let-values ([(dir name dir?) (split-path exe)])
                                  (if (bytes=? b #"")
                                      dir
                                      (build-path dir (bytes->path b)))))
                            (find-system-path 'orig-dir))))))]
              [else (find-lib-dir)]))
           ;; Cross-compile: assume it's "lib"
           (find-lib-dir))]
      [else
       (if (eq? 'shared (cross-system-type 'link))
           (or (force config:dll-dir) (find-lib-dir))
           #f)])))

(define cross-dll-dir
  (get-dll-dir cross-system-type
               (eq? (system-type 'cross) 'force)))
(define host-dll-dir
  (get-dll-dir system-type
               #f))

(define (find-cross-dll-dir)
  (force cross-dll-dir))

(define (find-dll-dir)
  (force host-dll-dir))

;; ----------------------------------------

(define (get-lib-search-dirs)
  (cond
   [(and (eq? (cross-system-type) (system-type))
         (eq? (system-type 'cross) 'infer))
    (get-cross-lib-search-dirs)]
   [else
    (force host-lib-search-dirs)]))

(define host-config
  (get-config-table
   (lambda () (exe-relative-path->complete-path (find-system-path 'host-config-dir)))))

(define host-lib-search-dirs
  (delay/sync
   (combine-search
    (to-path (hash-ref (force host-config) 'lib-search-dirs #f))
    (list (find-user-lib-dir)
          (let ([coll-dir (exe-relative-path->complete-path
                           (find-system-path 'host-collects-dir))])
            (or (let ([p (hash-ref (force host-config) 'lib-dir #f)])
                  (and p
                       (path->complete-path p coll-dir)))
                (build-path coll-dir 'up "lib")))))))
