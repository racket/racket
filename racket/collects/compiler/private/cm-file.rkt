#lang racket/base
(require racket/file
         racket/path
         file/sha1
         "cm-util.rkt"
         "cm-path.rkt"
         "cm-security.rkt"
         "cm-log.rkt")

(provide (all-defined-out))

(define (try-file-time p)
  (let ([s (file-or-directory-modify-seconds p #f (lambda () #f))])
    (and s
         (if (eq? (use-compiled-file-check) 'modify-seconds)
             s
             0))))

(define (touch path)
  (when (eq? 'modify-seconds (use-compiled-file-check))
    (with-compiler-security-guard
     (file-or-directory-modify-seconds 
      path
      (current-seconds)
      (lambda ()
        (close-output-port (open-output-file path #:exists 'append)))))))

(define (try-delete-file path [noisy? #t])
  ;; Attempt to delete, but give up if it doesn't work:
  (with-handlers ([exn:fail:filesystem? void])
    (when noisy? (trace-printf "deleting ~a" path))
    (with-compiler-security-guard (delete-file* path))))

(define (delete-file* path)
  (if (eq? 'windows (system-type))
      ;; Using `delete-directory/files` tries deleting by first moving
      ;; to the temporary folder:
      (delete-directory/files path #:must-exist? #f)
      (delete-file path)))

(define (actual-source-path path)
  (if (file-exists? path) 
      path
      (let ([alt-path (rkt->ss path)])
        (if (file-exists? alt-path)
            alt-path
            path))))

;; with-compile-output : path (output-port path -> alpha) -> alpha
(define (with-compile-output path proc)
  (call-with-atomic-output-file 
   path
   #:security-guard (pick-security-guard)
   proc
   ;; On Windows, if some other process/place is reading the file, then
   ;; an atomic move cannot succeed. Pause and try again, up to a point,
   ;; then give up on atomicity.
   #:rename-fail-handler (let ([amt 0.01])
                           (lambda (exn tmp-path)
                             (cond
                              [(and amt
                                    (eq? 'windows (system-type))
                                    (exn:fail:filesystem:errno? exn)
                                    (let ([errno (exn:fail:filesystem:errno-errno exn)])
                                      (and (eq? 'windows (cdr errno))
                                           (eqv? (car errno) 5)))) ; ERROR_ACCESS_DENIED
                               (cond
                                [(< amt 0.5)
                                 (sleep amt)
                                 (set! amt (* 2 amt))]
                                [else
                                 ;; Give up an atomicity
                                 (try-delete-file path)
                                 ;; And give up on trying to handle errors
                                 (set! amt #f)])]
                              [else (raise exn)])))))

(define (get-source-sha1 p)
  (with-handlers ([exn:fail:filesystem? (lambda (exn)
                                          (and (path-has-extension? p #".rkt")
                                               (get-source-sha1 (path-replace-extension p #".ss"))))])
    (call-with-input-file* p sha1)))

(define (verify-times ss-name zo-name)
  (when (eq? 'modify-seconds (use-compiled-file-check))
    (define ss-sec (file-or-directory-modify-seconds ss-name))
    (define zo-sec (try-file-time zo-name))
    (cond [(not ss-sec) (error 'compile-zo "internal error")]
          [(not zo-sec) (error 'compile-zo "failed to create .zo file (~a) for ~a"
                               zo-name ss-name)]
          [(< zo-sec ss-sec) (error 'compile-zo
                                    "date for newly created .zo file (~a @ ~a) ~
                                     is before source-file date (~a @ ~a)~a"
                                    zo-name (format-time zo-sec)
                                    ss-name (format-time ss-sec)
                                    (if (> ss-sec (current-seconds))
                                        ", which appears to be in the future"
                                        ""))])))
