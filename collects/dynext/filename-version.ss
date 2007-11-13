#lang scheme/base

;; This module provides the string that should replace xxxxxxx's in file names.
;; The version number is compacted representing alternate sets of digits by
;; letters, and then dropping "."s:
;;    3.99.1.5 => 3.jj.1.f => 3jj1f

(provide filename-version-part)

(define filename-version-part
  (let* ([l (map string->number (regexp-split #rx"[.]" (version)))]
         [s (apply
             string-append
             (let loop ([l l][alpha? #f])
               (cond
                [(null? l) null]
                [else (let ([s (number->string (car l))])
                        (cons (if alpha?
                                  (list->string
                                   (map (lambda (c)
                                          (integer->char
                                           (+ (char->integer c)
                                              (- (char->integer #\a)
                                                 (char->integer #\0)))))
                                        (string->list s)))
                                  s)
                              (loop (cdr l) (not alpha?))))])))])
    (string-append (make-string (max 0 (- 7 (string-length s)))
                                #\_)
                   s)))
