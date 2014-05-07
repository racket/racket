#lang racket
(require racket/system
         "config.rkt"
         "archive.rkt"
         "path-utils.rkt"
         "dirstruct.rkt")

(define (make-archive rev)
  (define archive-path (revision-archive rev))
  (if (file-exists? archive-path)
    (begin (printf "r~a is already archived\n" rev)
           #t)
    (begin (local [(define tmp-path (make-temporary-file))]
                  (printf "Archiving r~a\n" rev)
                  (safely-delete-directory (revision-trunk.tgz rev))
                  (safely-delete-directory (revision-trunk.tar.7z rev))
                  (create-archive tmp-path (revision-dir rev))
                  (rename-file-or-directory tmp-path archive-path)
                  (safely-delete-directory (revision-log-dir rev))
                  (safely-delete-directory (revision-analyze-dir rev)))
           #f)))

(provide make-archive)
