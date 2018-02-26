#lang racket/base
(require "../print/custom-write.rkt"
         "../port/string-output.rkt"
         "../locale/string.rkt")

(provide (struct-out path)
         is-path?
         path-for-some-system?
         path-string?
         string-no-nuls?
         string->path
         string->path-bytes
         ->path)

(struct path (bytes convention)
        #:property prop:custom-write
        (lambda (p port mode)
          (when mode
            (write-string "#<path:" port))
          (write-string (bytes->string/locale (path-bytes p)) port)
          (when mode
            (write-string ">" port)))
        #:property prop:equal+hash
        (list
         (lambda (p1 p2 eql?)
           (eql? (path-bytes p1) (path-bytes p2)))
         (lambda (p hc)
           (hc (path-bytes p)))
         (lambda (p hc)
           (hc (path-bytes p)))))

(define is-path?
  (let ([path? (lambda (p)
                 (and (path? p)
                      (eq? (path-convention p)
                           (system-path-convention-type))))])
    path?))

(define (path-for-some-system? p)
  (path? p))

(define (path-string? p)
  (or (is-path? p)
      (and (string? p)
           (positive? (string-length p))
           (string-no-nuls? p))))

(define (string-no-nuls? s)
  (and (string? s)
       (for/and ([c (in-string s)])
         (not (char=? c #\nul)))))

(define (string->path s)
  (path (string->path-bytes s)
        (system-path-convention-type)))

(define (string->path-bytes s)
  (string->bytes/locale s (char->integer #\?)))

(define (->path p)
  (if (string? p)
      (string->path p)
      p))
