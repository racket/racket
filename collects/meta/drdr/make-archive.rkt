#lang scheme
(require scheme/system
         "config.ss"
         "archive.ss"
         "path-utils.ss"
         "dirstruct.ss")

(define (archive-directory pth)
  (define tmp (path-add-suffix pth #".bak"))
  (system* (find-executable-path "tar")
           "czf"
           (path->string (path-add-suffix pth #".tgz"))
           (path->string pth))
  (rename-file-or-directory pth tmp)
  (safely-delete-directory tmp))

(define (make-archive rev)
  (define archive-path (revision-archive rev))
  (if (file-exists? archive-path)
      (printf "r~a is already archived\n" rev)
      (local [(define tmp-path (make-temporary-file))]
        (printf "Archiving r~a\n" rev)
        (create-archive tmp-path (revision-dir rev))
        (rename-file-or-directory tmp-path archive-path)
        (archive-directory (revision-log-dir rev))
        (archive-directory (revision-analyze-dir rev)))))

(define mode (make-parameter 'single))

(init-revisions!)

(command-line #:program "make-archive"
              #:once-any
              ["--single" "Archive a single revision" (mode 'single)]
              ["--many" "Archive many revisions" (mode 'many)]
              #:args (ns)
              (local [(define n (string->number ns))]
                (case (mode)
                  [(many)
                   (local [(define all-revisions
                             (sort revisions >=))]
                     (for ([rev (in-list (list-tail all-revisions n))])
                       (make-archive rev)))]
                  [(single)
                   (make-archive n)])))
