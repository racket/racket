#lang racket/base
(require "../host/linklet.rkt"
         "version-bytes.rkt"
         "correlated-linklet.rkt")

(provide write-linklet-bundle
         write-linklet-directory)

(define (write-linklet-bundle b as-correlated-linklet? linklet-bundle->hash port)
  ;; Various tools expect a particular header:
  ;;   "#~"
  ;;   length of version byte string (< 64) as one byte
  ;;   version byte string
  ;;   "B"
  ;;   20 bytes of SHA-1 hash
  (write-bytes #"#~" port)
  (write-bytes (bytes (bytes-length version-bytes)) port)
  (write-bytes version-bytes port)
  (let ([vm-bytes (if as-correlated-linklet?
                      correlated-linklet-vm-bytes
                      vm-bytes)])
    (write-bytes (bytes (bytes-length vm-bytes)) port)
    (write-bytes vm-bytes port))
  (write-bytes #"B" port)
  (write-bytes (make-bytes 20 0) port)
  ;; The rest is whatever the VM wants
  (if as-correlated-linklet?
      (write-correlated-linklet-bundle-hash (linklet-bundle->hash b) port)
      (write-linklet-bundle-hash (linklet-bundle->hash b) port)))

(define (linklet-bundle->bytes b as-correlated-linklet? linklet-bundle->hash)
  (define o (open-output-bytes))
  (write-linklet-bundle b as-correlated-linklet? linklet-bundle->hash o)
  (get-output-bytes o))

(define (write-linklet-directory ld as-correlated-linklet? linklet-directory->hash linklet-bundle->hash port)
  ;; Various tools expect a particular header:
  ;;   "#~"
  ;;   length of version byte string (< 64) as one byte
  ;;   version byte string
  ;;   length of virtual machine byte string (< 64) as one byte
  ;;   virtual machine byte string
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
  (let ([vm-bytes (if as-correlated-linklet?
                      correlated-linklet-vm-bytes
                      vm-bytes)])
    (write-bytes #"#~" port)
    (write-byte (bytes-length version-bytes) port)
    (write-bytes version-bytes port)
    (write-byte (bytes-length vm-bytes) port)
    (write-bytes vm-bytes port)
    (write-bytes #"D" port)
    ;; Flatten a directory of bundles into a vector of pairs, where
    ;; each pair has the encoded bundle name and the bundle bytes  
    (define (flatten-linklet-directory ld rev-name-prefix accum)
      (define-values (new-accum saw-bundle?)
        (for/fold ([accum accum] [saw-bundle? #f]) ([(key value) (in-hash (linklet-directory->hash ld))])
          (cond
            [(eq? key #f)
             (values (cons (cons (encode-name rev-name-prefix)
                                 (linklet-bundle->bytes value as-correlated-linklet? linklet-bundle->hash))
                           accum)
                     #t)]
            [else
             (values (flatten-linklet-directory value (cons key rev-name-prefix) accum)
                     saw-bundle?)])))
      (cond
        [saw-bundle? new-accum]
        [else (cons (cons (encode-name rev-name-prefix)
                          #"#f")
                    new-accum)]))
    (define bundles (list->vector
                     (sort (flatten-linklet-directory ld '() '())
                           (lambda (a b) (bytes<? (car a) (car b))))))
    (define len (vector-length bundles))
    (define initial-offset (+ 2 ; "#~"
                              1 ; version length
                              (bytes-length version-bytes)
                              1 ; vm length
                              (bytes-length vm-bytes)
                              1 ; D
                              4)) ; bundle count
    (write-int len port) ; bundle count
    ;; Compute bundle offsets
    (define btree-size (compute-btree-size bundles len))
    (define node-offsets (compute-btree-node-offsets bundles len initial-offset))
    (define bundle-offsets (compute-bundle-offsets bundles len (+ initial-offset btree-size)))
    (write-directory-btree bundles node-offsets bundle-offsets len port)
    ;; Write the bundles
    (for ([i (in-range len)])
      (write-bytes (cdr (vector-ref bundles i)) port))))

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
  (for/sum ([i (in-range len)])
    (define nlen (bytes-length (car (vector-ref bundles i))))
    ;; 5 numbers: name length, bundle offset, bundles size, lef, and right
    (+ nlen (* 5 4))))

;; Compute the offset where each node in the binary tree will reside
;; relative to the start of the bundle directory's "#~"
(define (compute-btree-node-offsets bundles len initial-offset)
  (define node-offsets (make-vector len))
  (let loop ([lo 0] [hi len] [offset initial-offset])
    (cond
      [(= lo hi) offset]
      [else
       (define mid (quotient (+ lo hi) 2))
       (vector-set! node-offsets mid offset)
       (define nlen (bytes-length (car (vector-ref bundles mid))))
       (let* ([offset (+ offset 4 nlen 4 4 4 4)]
              [offset (loop lo mid offset)])
         (loop (add1 mid) hi offset))]))
  node-offsets)

;; Compute the offset where each bundle will reside relative
;; to the start of the bundle directory's "#~"
(define (compute-bundle-offsets bundles len offset)
  (define bundle-offsets (make-vector len))
  (let loop ([i 0] [offset offset])
    (unless (= i len)
      (vector-set! bundle-offsets i offset)
      (loop (add1 i) (+ offset (bytes-length (cdr (vector-ref bundles i)))))))
  bundle-offsets)

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
         [(< (add1 mid) hi)
          (let ([right (quotient (+ (add1 mid) hi) 2)])
            (write-int (vector-ref node-offsets right) port))]
         [else
          (write-int 0 port)])
        (loop lo mid)
        (loop (add1 mid) hi))])))

(define (write-int n port)
  (write-bytes (integer->integer-bytes n 4 #f #f) port))
