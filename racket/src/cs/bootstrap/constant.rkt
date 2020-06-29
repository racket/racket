#lang racket/base
(require racket/match
         "scheme-readtable.rkt"
         "config.rkt")

;; Extract constants that we need to get started by reading
;; "cmacros.ss" and the machine ".def" file (without trying to run or
;; expand the files)

(define ht (make-hasheq))

(define (read-constants i)
  (parameterize ([current-readtable scheme-readtable])
    (let loop ()
      (define e (read i))
      (unless (eof-object? e)
        (match e
          [`(define-constant ,id2 (case (constant ,id1)
                                    [(,v1) ,rv1]
                                    [(,v2) ,rv2]
                                    . ,_))
           (define v (hash-ref ht id1))
           (hash-set! ht id2
                      (cond
                        [(eqv? v v1) rv1]
                        [(eqv? v v2) rv2]
                        [else (error "unknown")]))]
          [`(define-constant ,id ,v)
           (when (exact-integer? v)
             (hash-set! ht id v))]
          [_ (void)])
        (loop)))))

(when scheme-dir
  (call-with-input-file
   (build-path scheme-dir "s" (string-append target-machine ".def"))
   read-constants)
  
  (call-with-input-file
   (build-path scheme-dir "s" "cmacros.ss")
   read-constants))

(define-syntax-rule (define-constant id ...)
  (begin
    (provide id ...)
    (define id (hash-ref ht 'id #f)) ...))

(hash-set! ht 'ptr-bytes (/ (hash-ref ht 'ptr-bits 64) 8))

(define-constant
  ptr-bytes
  fixnum-bits
  max-float-alignment
  annotation-debug
  annotation-profile
  visit-tag
  revisit-tag
  prelex-is-flags-offset
  prelex-was-flags-offset
  prelex-sticky-mask
  prelex-is-mask
  scheme-version)

(provide record-ptr-offset)
(define record-ptr-offset 1)
