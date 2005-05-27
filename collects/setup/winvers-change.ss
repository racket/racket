;; This thing will crawl over a given tree and/or files (given on the
;; command-line, defaults to the current directory) , and looks for "xxxxxxx"
;; things to replace by the version number, which should be done on a Windows
;; binary tree before packing a distribution.  It will rename files with
;; "xxxxxxx" in their name by a "NNN_NNN" version number, and will also do this
;; rewrite in all files.  (Actually looking for a few known string templates,
;; to be safe.)  Note that this is done *in-place*, so it will not work from a
;; running MzScheme.exe on Windows -- "winvers.ss" uses a trick of making a
;; copy of the binary and restarting that copy for the actual change.

(module winvers-change mzscheme

  (define verbose? #t)
  (define binary-extensions '(#"exe" #"dll" #"lib" #"so" #"def"
                              ;; #"obj" #"o"
                              ))
  (define xxxs #"xxxxxxx")
  (define xxxs-re
    (bytes-append #"(?:lib(?:mzsch|mzgc|mred)(?:|3m)|"
                  #"[Pp][Ll][Tt][Gg][Dd][Ii]_|"
                  #"[Uu][Nn][Ii][Pp][Ll][Tt]_|"
                  #"(?:[Ll][Ii][Bb]|[Ss][Ss][Ll])[Ee][Aa][Yy]32)(" xxxs #")"))
  (define renaming
    (byte-regexp (bytes-append #"^" xxxs-re #"[.](?:dll|lib)$")))
  (define substitutions
    (map (lambda (s) (regexp-replace #rx#"~a" s xxxs-re))
         ;; pdb not needed, but this way we can expect no
         ;; `xxxxxxx's when we finish.
         '(#"~a[.](?:dll|lib|pdb)\0"
           #"~a_NULL_THUNK_DATA\0"
           #"__IMPORT_DESCRIPTOR_~a\0"
           #"__head_~a_lib\0"
           #"__~a_lib_iname\0")))

  (require (lib "filename-version.ss" "dynext"))
  (define version-bytes (string->bytes/utf-8 filename-version-part))

  (define bytes-downcase
    (let* ([a* (char->integer #\A)]
           [z* (char->integer #\Z)]
           [d* (- (char->integer #\a) a*)])
      (define (byte-downcase b) (if (<= a* b z*) (+ b d*) b))
      (lambda (bstr)
        (list->bytes (map byte-downcase (bytes->list bstr))))))

  (define (binary-file? filename)
    (cond
     [(regexp-match #rx#"[.]([^.]+)$" filename) =>
      (lambda (m)
        (member (bytes-downcase (cadr m)) binary-extensions))]
     [else #f]))

  (define (do-file file)
    (define (path) (bytes->path file))
    (when (binary-file? file)
      (let ([dfile (bytes-downcase file)])
        (cond [(regexp-match-positions renaming dfile) =>
               (lambda (m)
                 (let ([new (bytes-append (subbytes dfile 0 (caadr m))
                                          version-bytes
                                          (subbytes dfile (cdadr m)))])
                   (when verbose?
                     (printf "Renaming: ~a/~a -> ~a\n"
                             (current-directory) file new))
                   (rename-file-or-directory (path) (bytes->path new))
                   (set! file new)))]
              [(regexp-match-positions xxxs dfile)
               (fprintf (current-error-port) "Warning: ~a/~a was not renamed!\n"
                        (current-directory) file)]))
      (let-values ([(i o)    (open-input-output-file (path) 'update)]
                   [(print?) verbose?])
        (for-each (lambda (subst)
                    (file-position i 0)
                    (let loop ([pos 0])
                      (cond [(regexp-match-positions subst i) =>
                             (lambda (m)
                               (when print?
                                 (printf "Changing: ~a/~a\n"
                                         (current-directory) file)
                                 (set! print? #f))
                               (file-position o (+ pos (caadr m)))
                               (display version-bytes o)
                               (flush-output o)
                               (file-position i (+ pos (cdar m)))
                               (loop (+ pos (cdar m))))])))
                  substitutions)
        (file-position i 0)
        (when (regexp-match-positions xxxs i)
          (fprintf (current-error-port) "Warning: ~a/~a still has \"~a\"!\n"
                   (current-directory) file xxxs))
        (close-input-port i)
        (close-output-port o))))

  (let loop ([files (if (zero? (vector-length (current-command-line-arguments)))
                      '(#".")
                      (map string->bytes/utf-8
                           (vector->list (current-command-line-arguments))))])
    (when (pair? files)
      (let ([path (bytes->path (car files))])
        (cond [(file-exists? path) (do-file (car files))]
              [(directory-exists? path)
               (parameterize ([current-directory path])
                 (loop (map path->bytes (directory-list))))]))
      (loop (cdr files))))

)
