#lang racket/base
(require "../locale/string.rkt"
         (rename-in "path.rkt"
                    [string->path raw:string->path])
         "check.rkt"
         "sep.rkt"
         "build.rkt"
         "string.rkt"
         "split.rkt"
         "protect.rkt"
         "relativity.rkt"
         "cleanse.rkt"
         "simplify.rkt"
         "directory-path.rkt"
         "system.rkt"
         "api.rkt"
         "ffi.rkt")

(provide (rename-out [is-path? path?])
         path-for-some-system?
         
         string->path
         path->string
         bytes->path
         path->bytes

         string->path-element
         bytes->path-element
         path-element->string
         path-element->bytes
         
         path<?
         
         path-convention-type

         build-path
         build-path/convention-type
         
         split-path
         explode-path
         
         absolute-path?
         relative-path?
         complete-path?

         current-directory
         current-directory-for-user
         current-load-relative-directory
         current-drive

         path->complete-path
         path->directory-path
         
         cleanse-path
         simplify-path

         find-system-path
         set-exec-file!
         set-run-file!
         set-collects-dir!
         set-config-dir!

         _path)


(define/who (bytes->path bstr [convention (system-path-convention-type)])
  (check who bytes? bstr)
  (check-convention who convention)
  (check-path-bytes who bstr)
  (path (bytes->immutable-bytes bstr) convention))

(define/who (path->bytes p)
  (check who path? #:contract "path-for-some-system?" p)
  (bytes-copy (path-bytes p)))

(define/who (string->path-element s)
  (check who string? s)
  (check-path-string who s)
  (do-bytes->path-element (string->path-bytes s)
                          (system-path-convention-type)
                          who
                          s))

(define/who (bytes->path-element bstr [convention (system-path-convention-type)])
  (check who bytes? bstr)
  (check-convention who convention)
  (check-path-bytes who bstr)
  (do-bytes->path-element bstr convention who bstr))

(define (path-element? p)
  (cond
   [(path? p)
    (define bstr (path-bytes p))
    (define convention (path-convention p))
    (and
     ;; Quick pre-check: any separators?
     (or (not (eq? convention 'unix))
         (not (for/or ([c (in-bytes bstr)]
                       [i (in-naturals)])
                (and (is-sep? c convention)
                     i))))
     (let-values ([(base name dir?) (split-path p)])
       (and (symbol? base)
            (path? name))))]
   [else #f]))

(define (do-bytes->path-element bstr convention who orig-arg)
  (define (bad-element)
    (raise-arguments-error who
                           "cannot be converted to a path element"
                           "path" orig-arg
                           "explanation" "path can be split, is not relative, or names a special element"))
  (when (eq? 'windows convention)
    ;; Make sure we don't call `protect-path-element` on a
    ;; byte string that contains a "\":
    (when (for/or ([b (in-bytes bstr)])
            (eqv? b (char->integer #\\)))
      (bad-element)))
  (define len (bytes-length bstr))
  (define p (path (protect-path-element (bytes->immutable-bytes bstr) convention)
                  convention))
  (unless (path-element? p)
    (bad-element))
  p)

(define/who (path-element->string p)
  (check who path-element? p)
  (bytes->string/locale (path-bytes p) #\?))

(define/who (path-element->bytes p)
  (check who path-element? p)
  (bytes-copy (path-bytes p)))

(define/who path<?
  (case-lambda
    [(p)
     (check who is-path? #:contract "path?" p)
     #t]
    [(p1 p2)
     (check who is-path? #:contract "path?" p1)
     (check who is-path? #:contract "path?" p2)
     (bytes<? (path-bytes p1) (path-bytes p2))]
    [(p . ps)
     (check who is-path? #:contract "path?" p)
     (let loop ([bstr (path-bytes p)] [ps ps])
       (cond
        [(null? ps) #t]
        [else
         (define p (car ps))
         (check who is-path? #:contract "path?" p)
         (define bstr2 (path-bytes p))
         (and (bytes<? bstr bstr2)
              (loop bstr2 (cdr ps)))]))]))

(define/who (path-convention-type p)
  (check who path-for-some-system? p)
  (path-convention p))
