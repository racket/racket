#lang scheme/base

(require scheme/class scheme/file scheme/path)

(provide make-indirect-renderer-mixin)

(define (dotless bytes) (regexp-replace #rx#"[.]" bytes #""))

(define ((make-indirect-renderer-mixin
          base-renderer base-suffix target-suffix convert)
         %renderer)
  (class (base-renderer %renderer)
    (define tmp-dest-dir #f)
    (define/override (get-dest-directory create?)
      (or tmp-dest-dir (super get-dest-directory create?)))
    (define/override (get-suffix) target-suffix)
    (define/override (render srcs dests ri)
      (define tmp-dir
        (make-temporary-file
         (format "scribble-~a-to-~a-~~a"
                 (dotless base-suffix) (dotless target-suffix))
         'directory))
      (define (cleanup)
        (when (directory-exists? tmp-dir) (delete-directory/files tmp-dir)))
      (with-handlers ([void (lambda (e) (cleanup) (raise e))])
        (define tmp-dests
          (map (lambda (dest)
                 (build-path tmp-dir
                             (path-replace-suffix (file-name-from-path dest)
                                                  base-suffix)))
               dests))
        (set! tmp-dest-dir tmp-dir)
        ;; it would be better if it's ok to change current-directory for this
        (super render srcs tmp-dests ri)
        (for ([tmp tmp-dests] [dst dests])
          (parameterize ([current-directory tmp-dir])
            (convert (file-name-from-path tmp)))
          (copy-file (build-path tmp-dir(file-name-from-path dst)) dst))
        (cleanup)))
    (super-new)))
