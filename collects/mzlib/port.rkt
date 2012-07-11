#lang racket/base

;; deprecated library, see `racket/port`

(require racket/port)
(provide ;; these are the functions that used to be defined in
         ;; `mzlib/port` but are now defined in `racket/port`
         open-output-nowhere
         make-pipe-with-specials
         make-input-port/read-to-peek
         peeking-input-port
         relocate-input-port
         transplant-input-port
         filter-read-input-port
         special-filter-input-port
         relocate-output-port
         transplant-output-port
         merge-input
         copy-port
         input-port-append
         convert-stream
         make-limited-input-port
         reencode-input-port
         reencode-output-port
         dup-input-port
         dup-output-port

         read-bytes-avail!-evt
         peek-bytes-avail!-evt
         read-bytes!-evt
         peek-bytes!-evt
         read-bytes-evt
         peek-bytes-evt
         read-string!-evt
         peek-string!-evt 
         read-string-evt
         peek-string-evt
         regexp-match-evt
         read-bytes-line-evt
         read-line-evt
         eof-evt

         ;; defined here and not in racket/port
         strip-shell-command-start)

;; ----------------------------------------

(define (strip-shell-command-start in)
  (when (regexp-match-peek #rx#"^#![^\r\n]*" in)
    (let loop ([s (read-line in)])
      (when (regexp-match #rx#"\\\\$" s)
        (loop (read-line in))))))
