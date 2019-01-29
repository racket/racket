#lang racket/base
(require "version-bytes.rkt"
         "linklet.rkt"
         "../host/linklet.rkt"
         "correlated-linklet.rkt")

(provide read-linklet-bundle-or-directory)

(define (read-linklet-bundle-or-directory in)
  (define (read-linklet-or-directory initial?)
    ;; `#~` has already been read
    (define start-pos (- (file-position in) 2))
    (define vers-len (min 63 (read-byte in)))
    (define vers (read-bytes vers-len in))
    (unless (equal? vers version-bytes)
      (raise-read-error 'read-compiled-linklet
                        "version mismatch"
                        "expected" (version)
                        "found" (bytes->string/utf-8 vers #\?)
                        "in" (let ([n (object-name in)])
                               (if (path? n)
                                   (unquoted-printing-string
                                    (path->string n))
                                   in))))
    (define vm-len (min 63 (read-byte in)))
    (define vm (read-bytes vm-len in))
    (define as-correlated-linklet? (equal? vm correlated-linklet-vm-bytes))
    (unless (or as-correlated-linklet?
                (equal? vm vm-bytes))
      (raise-read-error 'read-compiled-linklet
                        "virtual-machine mismatch"
                        "expected" (bytes->string/utf-8 vm-bytes)
                        "found" (bytes->string/utf-8 vm #\?)
                        "in" (let ([n (object-name in)])
                               (if (path? n)
                                   (unquoted-printing-string
                                    (path->string n))
                                   in))))
    (define tag (read-byte in))
    (cond
      [(eqv? tag (char->integer #\B))
       (define sha-1 (read-bytes 20 in))
       (define b-ht (if as-correlated-linklet?
                        (read-correlated-linklet-bundle-hash in)
                        (read-linklet-bundle-hash in)))
       (unless (hash? b-ht)
         (raise-read-error 'read-linklet-bundle-hash
                           "bad read result"
                           "expected" "hash/c"
                           "found" (format "~s" b-ht)
                           "in" (let ([n (object-name in)])
                                  (if (path? n)
                                      (path->string n)
                                      in))))

       (hash->linklet-bundle
        (add-hash-code (if initial?
                           (strip-submodule-references b-ht)
                           b-ht)
                       sha-1))]
      [(eqv? tag (char->integer #\D))
       (unless initial?
         (raise-read-error 'read-compiled-linklet
                           "expected a linklet bundle"))
       (read-bundle-directory start-pos)]
      [else
       (raise-read-error 'read-compiled-linklet
                         "expected a `B` or `D`")]))
  
  (define (read-bundle-directory pos)
    (define count (read-int in))
    (define position-to-name
      (let loop ([count count] [accum (hasheqv)])
        (cond
          [(zero? count) accum]
          [else
           (define bstr (read-bytes (read-int in) in))
           (define offset (read-int in))
           (define len (read-int in))
           (read-int in) ; left
           (read-int in) ; right
           (loop (sub1 count)
                 (hash-set accum offset bstr))])))
    
    (let loop ([count count] [accum '()])
      (cond
        [(zero? count)
         (list->bundle-directory accum hash->linklet-directory)]
        [else
         (define name (hash-ref position-to-name (- (file-position in) pos) #f))
         (unless name
           (raise-read-error 'read-compiled-linklet
                             "bundle not at an expected file position"))
         (define bstr (read-bytes 2 in))
         (define bundle
           (cond
             [(equal? #"#~" bstr)
              (read-linklet-or-directory #f)]
             [(equal? #"#f" bstr)
              #f]
             [else
              (raise-read-error 'read-compiled-linklet
                                "expected a `#~` or `#f` for a bundle")]))
         (loop (sub1 count)
               (cons (cons (decode-name name 0) bundle) accum))])))

   (read-linklet-or-directory #t))


(define (read-int in)
  (integer-bytes->integer (read-bytes 4 in) #f #f))

(define (decode-name bstr pos)
  (define blen (bytes-length bstr))
  (define (bad-bundle)
    (raise-read-error 'read-compiled-linklet
                      "malformed bundle"))
  (cond
    [(= pos blen)
     '()]
    [(> pos blen) (bad-bundle)]
    [else
     (define len (bytes-ref bstr pos))
     (when (> (+ pos len 1) blen) (bad-bundle))
     (cond
       [(= len 255)
        (define len (integer-bytes->integer bstr #f #f (add1 pos) (+ pos 5)))
        (when (> (+ pos len 1) blen) (bad-bundle))
        (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (+ pos 5) (+ pos 5 len)) #\?))
              (decode-name bstr (+ pos 5 len)))]
       [else
        (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (add1 pos) (+ pos 1 len)) #\?))
              (decode-name bstr (+ pos 1 len)))])]))

;; Convert a post-order list into a tree
(define (list->bundle-directory l hash->linklet-directory)
  ;; The bundles list is in post-order, so we can build directories
  ;; bottom-up
  (let loop ([l l] [prev-len 0] [stack '()] [accum (hasheq)])
    (when (null? l)
      (raise-read-error 'read-compiled-linklet
                        "invalid bundle sequence"))
    (let* ([p (car l)]
           [path (car p)]
           [v (cdr p)]
           [len (length path)])
      (when (< len prev-len)
        (raise-read-error 'read-compiled-linklet
                          "invalid bundle sequence"))
      (let sloop ([prev-len prev-len] [stack stack] [accum accum])
        (cond
          [(> len (add1 prev-len))
           (sloop (add1 prev-len)
                  (cons accum stack)
                  (hasheq))]
          [else
           (let ([path (list-tail path (max 0 (sub1 prev-len)))])
             (cond
               [(= len prev-len)
                (let ([accum (if v
                                 (hash-set accum #f v)
                                 accum)])
                  (if (zero? len)
                      (hash->linklet-directory accum)
                      (loop (cdr l)
                            (sub1 prev-len)
                            (cdr stack)
                            (hash-set (car stack) (car path) (hash->linklet-directory accum)))))]
               [else
                (let ([path (if (positive? prev-len)
                                (cdr path)
                                path)])
                  (loop (cdr l)
                        prev-len
                        stack
                        (hash-set accum
                                  (car path)
                                  (hash->linklet-directory (if v
                                                               (hasheq #f v)
                                                               (hasheq))))))]))])))))

;; When a bundle is loaded by itself, remove any 'pre and 'post
;; submodule descriptions:
(define (strip-submodule-references b-ht)
  (hash-remove (hash-remove b-ht 'pre) 'post))

;; If the bundle has a non-zero hash code, record it with the
;; 'hash-code key to enable module caching
(define (add-hash-code b-ht sha-1)
  (if (bytes=? sha-1 #"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0")
      b-ht
      (hash-set b-ht 'hash-code sha-1)))

(define (raise-read-error who msg . details)
  (raise
   (exn:fail:read
    (apply
     string-append
     (format "~a: ~a" who msg)
     (let loop ([details details])
       (cond
         [(null? details) null]
         [else
          (list*
           "  " (car details) ": " (format "~v" (cadr details))
           (loop (cddr details)))])))
    (current-continuation-marks)
    null)))
