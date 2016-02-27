#lang racket/base
(require racket/promise
         compiler/private/winutf16
         compiler/private/mach-o
         setup/cross-system
         "private/dirs.rkt")

(provide (except-out (all-from-out "private/dirs.rkt")
                     config:dll-dir
                     config:bin-dir
                     define-finder)
         find-dll-dir)

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
  config:bin-dir
  find-gui-bin-dir
  find-user-gui-bin-dir
  (case (cross-system-type)
    [(windows macosx) 'same]
    [(unix) "bin"]))

;; ----------------------------------------
;; DLLs

(provide find-dll-dir)
(define dll-dir
  (delay/sync
    (case (cross-system-type)
      [(windows)
       (if (eq? (system-type) 'windows)
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
       (if (eq? (system-type) 'macosx)
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
(define (find-dll-dir)
  (force dll-dir))

