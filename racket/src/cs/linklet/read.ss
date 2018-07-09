(define (read-compiled-linklet in)
  (performance-region
   'read-bundle
   (read-compiled-linklet-or-directory in #t)))

(define (read-compiled-linklet-or-directory in initial?)
  ;; `#~` has already been read
  (let* ([start-pos (- (file-position in) 2)]
         [vers-len (min 63 (read-byte in))]
         [vers (read-bytes vers-len in)])
    (unless (equal? vers (string->bytes/utf-8 (version)))
      (raise-arguments-error 'read-compiled-linklet
                             "version mismatch"
                             "expected" (version)
                             "found" (bytes->string/utf-8 vers #\?)
                             "in" (let ([n (object-name in)])
                                    (if (path? n)
                                        (unquoted-printing-string
                                         (path->string n))
                                        in))))
    (let ([tag (read-byte in)])
      (cond
       [(equal? tag (char->integer #\B))
        (let ([sha-1 (read-bytes 20 in)])
          (let ([len (read-int in)])
            (let ([bstr (read-bytes len in)])
              (let ([b (fasl-read (open-bytevector-input-port bstr))])
                (add-hash-code (adjust-linklet-bundle-laziness
                                (if initial?
                                    (strip-submodule-references b)
                                    b))
                               sha-1)))))]
       [(equal? tag (char->integer #\D))
        (unless initial?
          (raise-argument-error 'read-compiled-linklet
                                "expected a linklet bundle"))
        (read-bundle-directory in start-pos)]
       [else
        (raise-arguments-error 'read-compiled-linklet
                               "expected a `B` or `D`")]))))

(define (read-int in)
  (integer-bytes->integer (read-bytes 4 in) #f #f))

(define (read-bundle-directory in pos)
  (let ([count (read-int in)])
    (let ([position-to-name
           (let loop ([count count] [accum (hasheqv)])
             (cond
              [(zero? count) accum]
              [else
               (let ([bstr (read-bytes (read-int in) in)])
                 (let* ([offset (read-int in)]
                        [len (read-int in)])
                   (read-int in) ; left
                   (read-int in) ; right
                   (loop (fx1- count)
                         (hash-set accum offset bstr))))]))])
      (let loop ([count count] [accum '()])
        (cond
         [(zero? count)
          (list->bundle-directory accum)]
         [else
          (let ([name (hash-ref position-to-name (- (file-position in) pos) #f)])
            (unless name
              (raise-arguments-error 'read-compiled-linklet
                                     "bundle not at an expected file position"))
            (let ([bstr (read-bytes 2 in)])
              (let ([bundle
                     (cond
                      [(equal? '#vu8(35 126) bstr)
                       (read-compiled-linklet in)]
                      [(equal? '#vu8(35 102) bstr)
                       #f]
                      [else
                       (raise-arguments-error 'read-compiled-linklet
                                              "expected a `#~` or `#f` for a bundle")])])
                (loop (fx1- count)
                      (cons (cons (decode-name name 0) bundle) accum)))))])))))

(define (decode-name bstr pos)
  (let ([blen (bytes-length bstr)]
        [bad-bundle (lambda ()
                      (raise-arguments-error 'read-compiled-linklet
                                             "malformed bundle"))])
    (cond
     [(= pos blen)
      '()]
     [(> pos blen) (bad-bundle)]
     [else
      (let ([len (bytes-ref bstr pos)])
        (when (> (+ pos len 1) blen) (bad-bundle))
        (if (= len 255)
            (let ([len (integer-bytes->integer bstr #f #f (fx1+ pos) (fx+ pos 5))])
              (when (> (+ pos len 1) blen) (bad-bundle))
              (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (fx+ pos 5) (+ pos 5 len)) #\?))
                    (decode-name bstr (+ pos 5 len))))
            (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (fx1+ pos) (+ pos 1 len)) #\?))
                  (decode-name bstr (+ pos 1 len)))))])))

;; Convert a post-order list into a tree
(define (list->bundle-directory l)
  ;; The bundles list is in post-order, so we can build directories
  ;; bottom-up
  (let loop ([l l] [prev-len 0] [stack '()] [accum (hasheq)])
    (when (null? l)
      (raise-arguments-error 'read-compiled-linklet
                             "invalid bundle sequence"))
    (let* ([p (car l)]
           [path (car p)]
           [v (cdr p)]
           [len (length path)])
      (when (< len prev-len)
        (raise-arguments-error 'read-compiled-linklet
                               "invalid bundle sequence"))
      (let sloop ([prev-len prev-len] [stack stack] [accum accum])
        (cond
         [(> len (fx1+ prev-len))
          (sloop (fx1+ prev-len)
                 (cons accum stack)
                 (hasheq))]
         [else
          (let ([path (list-tail path (fxmax 0 (fx1- prev-len)))])
            (cond
             [(= len prev-len)
              (let ([accum (if v
                               (hash-set accum #f v)
                               accum)])
                (if (zero? len)
                    (make-linklet-directory accum)
                    (loop (cdr l)
                          (fx1- prev-len)
                          (cdr stack)
                          (hash-set (car stack) (car path) (make-linklet-directory accum)))))]
             [else
              (let ([path (if (positive? prev-len)
                              (cdr path)
                              path)])
                (loop (cdr l)
                      prev-len
                      stack
                      (hash-set accum
                                (car path)
                                (make-linklet-directory (if v
                                                            (hasheq #f v)
                                                            (hasheq))))))]))])))))

;; When a bundle is loaded by itself, remove any 'pre and 'post
;; submodule descriptions:
(define (strip-submodule-references b)
  (make-linklet-bundle (hash-remove (hash-remove (linklet-bundle-hash b) 'pre) 'post)))

;; If the bundle has a non-zero hash code, record it with the
;; 'hash-code key to enable module caching
(define (add-hash-code b sha-1)
  (if (bytevector=? sha-1 '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      b
      (make-linklet-bundle (hash-set (linklet-bundle-hash b) 'hash-code sha-1))))

(define read-on-demand-source
  (make-parameter #f
                  (lambda (v)
                    (unless (or (eq? v #t) (eq? v #f) (and (path? v)
                                                           (complete-path? v)))
                      (raise-argument-error 'read-on-demand-source
                                            "(or/c #f #t (and/c path? complete-path?))"
                                            v))
                    v)))

(define (adjust-linklet-bundle-laziness b)
  (make-linklet-bundle
   (let ([ht (linklet-bundle-hash b)])
     (let loop ([i (hash-iterate-first ht)])
       (cond
        [(not i) (hasheq)]
        [else
         (let-values ([(key val) (hash-iterate-key+value ht i)])
           (hash-set (loop (hash-iterate-next ht i))
                     key
                     (if (linklet? val)
                         (adjust-linklet-laziness val)
                         val)))])))))

(define (adjust-linklet-laziness linklet)
  (set-linklet-code linklet
                    (linklet-code linklet)
                    (if (|#%app| read-on-demand-source)
                        'faslable
                        'faslable-strict)))

