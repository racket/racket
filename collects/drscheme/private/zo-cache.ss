#lang scheme/base
(require scheme/path
         scheme/file
         compiler/cm)

#|

All of this code runs on the user's parameterization/thread

|#

(provide build-and-load-zo-file)

(define (build-and-load-zo-file original-load/use-compiled-handler path mod)
  (cond
    [(or (not (filename-extension path))
         (already-a-compiled-file? path))
     ;; if there is no extension, just give up.
     ;; if there is a compiled file that look up to date
     ;;    in the usual place, use it.
     (original-load/use-compiled-handler path mod)]
    [else
     ;; otherwise do some managed compilation
     (parameterize ([manager-skip-file-handler (λ (x)
                                                 (printf "considering ~s\n" x)
                                                 #f)])
       (managed-compile-zo path))
     (original-load/use-compiled-handler path mod)]))

(define (exists-and-is-newer? orig-path candidate-path)
  (and (file-exists? candidate-path)
       (< (file-or-directory-modify-seconds orig-path)
          (file-or-directory-modify-seconds candidate-path))))

(define (already-a-compiled-file? path) 
  (let* ([filename (file-name-from-path path)]
         [base (path-only path)]
         [file-zo-name (and filename (compiled-name filename #".zo"))]
         [fm (file-or-directory-modify-seconds path)]
         [newer-exists?
          (λ (pot-path)
            (and (file-exists? pot-path)
                 (< fm (file-or-directory-modify-seconds pot-path))))])
    (and file-zo-name
         (ormap 
          (λ (c-f-p)
            (or (newer-exists? (build-path base c-f-p file-zo-name))
                (newer-exists? (build-path base 
                                           c-f-p 
                                           "native"
                                           (system-library-subpath)
                                           (compiled-name filename (system-type 'so-suffix))))))
          (use-compiled-file-paths)))))


;; compiled-name : path [bytes] -> path or #f
;; returns #f if the path does not have an extension.
;; otherwise, returns an appropriately modified filename, extended with new-extension
(define (compiled-name path new-extension)
  (let* ([extension (filename-extension path)]
         [basename (and extension
                        (let ([pbs (path->bytes path)])
                          (subbytes pbs
                                    0 
                                    (- (bytes-length pbs)
                                       (bytes-length extension)
                                       1 ;; extra one for '.' in there
                                       ))))])
    (and basename
         (bytes->path
          (bytes-append basename #"_" extension new-extension)))))

