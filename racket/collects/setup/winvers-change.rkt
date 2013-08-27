;; This thing will crawl over a given tree and/or files (given on the
;; command-line, defaults to the current directory) , and looks for "xxxxxxx"
;; things to replace by the version number, which should be done on a Windows
;; binary tree before packing a distribution.  It will rename files with
;; "xxxxxxx" in their name by a "NNN_NNN" version number, and will also do this
;; rewrite in all files.  (Actually looking for a few known string templates,
;; to be safe.)  Note that this is done *in-place*, so it will not work from a
;; running Racket.exe on Windows -- "winvers.rkt" uses a trick of making a copy
;; of the binary and restarting that copy for the actual change.

#lang racket/base

(define verbose? #t)
(define binary-extensions '("exe" "dll" "lib" "so" "def" "exp" #|"obj" "o"|#))
(define xxxs #"xxxxxxx")
(define xxxs-re
  (bytes-append #"(?:lib(?:g?racket|mzgc)(?:|3m))(" xxxs #")"))
(define renaming (regexp (format "^~a[.](?:dll|lib|exp)$" xxxs-re)))
(define substitutions
  (map (lambda (s) (byte-regexp (regexp-replace #rx#"~a" s xxxs-re)))
       ;; pdb not needed, but this way we can expect no
       ;; `xxxxxxx's when we finish.
       '(#"~a[.](?:dll|lib|exp|pdb)"
         #"~a_NULL_THUNK_DATA\0"
         #"__IMPORT_DESCRIPTOR_~a\0"
         #"__head_~a_lib\0"
         #"__~a_lib_iname\0")))

(require dynext/filename-version)
(define version-bytes (string->bytes/utf-8 filename-version-part))

(define (binary-file? filename)
  (cond [(regexp-match #rx"[.]([^.]+)$" filename) =>
         (lambda (m) (member (string-downcase (cadr m)) binary-extensions))]
        [else #f]))

(define (do-file file)
  (define (full-path) ; proc since `file' can change
    (build-path (current-directory) file))
  (let ([dfile (string-downcase file)])
    (cond [(regexp-match-positions renaming dfile) =>
           (lambda (m)
             (let ([new (string-append (substring dfile 0 (caadr m))
                                       filename-version-part
                                       (substring dfile (cdadr m)))])
               (when verbose? (printf "Renaming: ~a -> ~a\n" (full-path) new))
               (rename-file-or-directory file new)
               (set! file new)))]
          [(regexp-match-positions xxxs dfile)
           (eprintf "Warning: ~a was not renamed!\n" (full-path))]))
  (let-values ([(i o) (open-input-output-file file #:exists 'update)])
    (define print? verbose?)
    (for ([subst (in-list substitutions)])
      (file-position i 0)
      (let loop ([pos 0])
        (cond [(regexp-match-positions subst i) =>
               (lambda (m)
                 (when print?
                   (printf "Changing: ~a\n" (full-path))
                   (set! print? #f))
                 (file-position o (+ pos (caadr m)))
                 (write-bytes version-bytes o)
                 (flush-output o)
                 (file-position i (+ pos (cdar m)))
                 (loop (+ pos (cdar m))))])))
    (file-position i 0)
    (when (regexp-match-positions xxxs i)
      (eprintf "Warning: ~a still has \"~a\"!\n" (full-path) xxxs))
    (close-input-port i)
    (close-output-port o)))

(let loop ([paths (if (zero? (vector-length (current-command-line-arguments)))
                    '(".")
                    (vector->list (current-command-line-arguments)))])
  (for ([path (in-list paths)])
    (cond [(file-exists? path)
           (when (binary-file? path) (do-file path))]
          [(directory-exists? path)
           (parameterize ([current-directory path])
             (loop (map path->string (directory-list))))])))
