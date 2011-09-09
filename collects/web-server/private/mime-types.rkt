#lang racket/base
(require racket/contract
         racket/match
         racket/promise)
(require "util.rkt"
         web-server/http)
(provide/contract
 [read-mime-types (path-string? . -> . (hash/c symbol? bytes?))]
 [make-path->mime-type (path-string? . -> . (path? . -> . (or/c false/c bytes?)))])

; read-mime-types : path? -> hash-table?
(define (read-mime-types a-path)
  (define MIME-TYPE-TABLE (make-hasheq))
  (with-input-from-file a-path
    (lambda ()
      (let loop ()
        (match (read-line (current-input-port) 'any)
          [(? eof-object?)
           (void)]
          [(regexp #rx#"^([^\t ]+)[\t ]+(.+)$" (list _ type exts))
           (for ([ext (in-list (regexp-split #" " exts))])
             (hash-set! MIME-TYPE-TABLE (lowercase-symbol! ext) type))
           (loop)]
          [_
           (loop)]))))
  MIME-TYPE-TABLE)

;; make-get-mime-type : path? -> path? -> bytes?
;; determine the mime type based on the filename's suffix
;;
;; Notes (GregP):
;; 1. Can we determine the mime type based on file contents?
;; 2. Assuming that 7-bit ASCII is correct for mime-type
(define (make-path->mime-type a-path)
  (define MIME-TYPE-TABLE
    (delay/sync
     (read-mime-types a-path)))
  (lambda (path)
    (match (path->bytes path)
      [(regexp #rx#".*\\.([^\\.]*$)" (list _ sffx))
       (hash-ref (force MIME-TYPE-TABLE)
                 (lowercase-symbol! sffx)
                 #f)]
      [_ #f])))
