(define (write-linklet-bundle b port mode)
  ;; Various tools expect a particular header:
  ;;   "#~"
  ;;   length of version byte string (< 64) as one byte
  ;;   version byte string
  ;;   "B"
  ;;   20 bytes of SHA-1 hash
  (write-bytes '#vu8(35 126) port)
  (let ([vers (string->bytes/utf-8 (version))])
    (write-bytes (bytes (bytes-length vers)) port)
    (write-bytes vers port))
  (write-bytes '#vu8(66) port)
  (write-bytes (make-bytes 20 0) port)
  ;; The rest is whatever we want. We'll simply fasl the bundle.
  (let-values ([(o get) (open-bytevector-output-port)])
    (fasl-write* b o)
    (let ([bstr (get)])
      (write-int (bytes-length bstr) port)
      (write-bytes bstr port))))

(define (linklet-bundle->bytes b)
  (let ([o (open-output-bytes)])
    (write-linklet-bundle b o #t)
    (get-output-bytes o)))

(define (write-linklet-directory ld port mode)
  ;; Various tools expect a particular header:
  ;;   "#~"
  ;;   length of version byte string (< 64) as one byte
  ;;   version byte string
  ;;   "D"
  ;;   bundle count as 4-byte integer
  ;;   binary tree:
  ;;     bundle-name length as 4-byte integer
  ;;     bundle name [encoding decribed below]
  ;;     bundle offset as 4-byte integer
  ;;     bundle size as 4-byte integer
  ;;     left-branch offset as 4-byte integer
  ;;     right-branch offset as 4-byte integer
  ;; A bundle name corresponds to a list of symbols. Each symbol in the list is
  ;; prefixed with either: its length as a byte if less than 255; 255 followed by
  ;; a 4-byte integer for the length.
  (write-bytes '#vu8(35 126) port)
  (let ([vers (string->bytes/utf-8 (version))])
    (write-bytes (bytes (bytes-length vers)) port)
    (write-bytes vers port)
    (write-bytes '#vu8(68) port)
    ;; Flatten a directory of bundles into a vector of pairs, where
    ;; each pair has the encoded bundle name and the bundle bytes
    (let* ([bundles (list->vector (flatten-linklet-directory ld '() '()))]
           [len (vector-length bundles)]
           [initial-offset (+ 2 ; "#~"
                              1 ; version length
                              (bytes-length vers)
                              1 ; D
                              4)]) ; bundle count
      (write-int len port) ; bundle count
      (chez:vector-sort! (lambda (a b) (bytes<? (car a) (car b))) bundles)
      ;; Compute bundle offsets
      (let* ([btree-size (compute-btree-size bundles len)]
             [node-offsets (compute-btree-node-offsets bundles len initial-offset)]
             [bundle-offsets (compute-bundle-offsets bundles len (+ initial-offset btree-size))])
        (write-directory-btree bundles node-offsets bundle-offsets len port)
        ;; Write the bundles
        (let loop ([i 0])
          (unless (fx= i len)
            (write-bytes (cdr (vector-ref bundles i)) port)
            (loop (fx1+ i))))))))

;; Flatten a tree into a list of `(cons _name-bstr _bundle-bstr)`
(define (flatten-linklet-directory ld rev-name-prefix accum)
  (let ([ht (linklet-directory-hash ld)])
    (let loop ([i (hash-iterate-first ht)] [accum accum] [saw-bundle? #f])
      (cond
       [(not i)
        (if saw-bundle?
            accum
            (cons (cons (encode-name rev-name-prefix)
                        '#vu8(35 102))
                  accum))]
       [else
        (let-values ([(key value) (hash-iterate-key+value ht i)])
          (cond
           [(eq? key #f)
            (loop (hash-iterate-next ht i)
                  (cons (cons (encode-name rev-name-prefix)
                              (linklet-bundle->bytes value))
                        accum)
                  #t)]
           [else
            (loop (hash-iterate-next ht i)
                  (flatten-linklet-directory value (cons key rev-name-prefix) accum)
                  saw-bundle?)]))]))))

;; Encode a bundle name (as a reversed list of symbols) as a single
;; byte string
(define (encode-name rev-name)
  (define (encode-symbol s)
    (let* ([bstr (string->bytes/utf-8 (symbol->string s))]
           [len (bytes-length bstr)])
      (if (< len 255)
          (list (bytes len) bstr)
          (list (bytes 255) (integer->integer-bytes len 4 #f #f) bstr))))
  (let loop ([rev-name rev-name] [accum '()])
    (cond
     [(null? rev-name) (apply bytes-append accum)]
     [else
      (loop (cdr rev-name) (append (encode-symbol (car rev-name))
                                   accum))])))

;; Figure out how big the binary tree will be, which depends
;; on the size of bundle-name byte strings
(define (compute-btree-size bundles len)
  (let loop ([i 0] [size 0])
    (if (= i len)
        size
        (let ([nlen (bytes-length (car (vector-ref bundles i)))])
          ;; 5 numbers: name length, bundle offset, bundles size, lef, and right
          (loop (fx1+ i) (+ size nlen (* 5 4)))))))

;; Compute the offset where each node in the binary tree will reside
;; relative to the start of the bundle directory's "#~"
(define (compute-btree-node-offsets bundles len initial-offset)
  (let ([node-offsets (make-vector len)])
    (let loop ([lo 0] [hi len] [offset initial-offset])
      (cond
       [(= lo hi) offset]
       [else
        (let* ([mid (quotient (+ lo hi) 2)])
          (vector-set! node-offsets mid offset)
          (let* ([nlen (bytes-length (car (vector-ref bundles mid)))]
                 [offset (+ offset 4 nlen 4 4 4 4)])
            (let ([offset (loop lo mid offset)])
              (loop (add1 mid) hi offset))))]))
    node-offsets))

;; Compute the offset where each bundle will reside relative
;; to the start of the bundle directory's "#~"
(define (compute-bundle-offsets bundles len offset)
  (let ([bundle-offsets (make-vector len)])
    (let loop ([i 0] [offset offset])
      (unless (= i len)
        (vector-set! bundle-offsets i offset)
        (loop (fx1+ i) (+ offset (bytes-length (cdr (vector-ref bundles i)))))))
    bundle-offsets))

;; Write the binary tree for the directory:
(define (write-directory-btree bundles node-offsets bundle-offsets len port)
  (let loop ([lo 0] [hi len])
    (cond
     [(= lo hi) (void)]
     [else
      (let* ([mid (quotient (+ lo hi) 2)]
             [p (vector-ref bundles mid)]
             [nlen (bytes-length (car p))])
        (write-int nlen port)
        (write-bytes (car p) port)
        (write-int (vector-ref bundle-offsets mid) port)
        (write-int (bytes-length (cdr p)) port)
        (cond
         [(> mid lo)
          (let ([left (quotient (+ lo mid) 2)])
            (write-int (vector-ref node-offsets left) port))]
         [else
          (write-int 0 port)])
        (cond
         [(< (fx1+ mid) hi)
          (let ([right (quotient (+ (fx1+ mid) hi) 2)])
            (write-int (vector-ref node-offsets right) port))]
         [else
          (write-int 0 port)])
        (loop lo mid)
        (loop (fx1+ mid) hi))])))

(define (write-int n port)
  (write-bytes (integer->integer-bytes n 4 #f #f) port))
