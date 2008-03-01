;;; sort.scm  --  Jens Axel Søgaard

;;; PURPOSE

; This file contains routines for sorting files larger than main memory.

;;; DEPENDCIES

; Relies on vector-sort! from srfi-32 available from v35x.

;;; USAGE

; The file format is simply
;    <record><record><record>...

; The user provides read-record, which reads a <record>, and
; write-record that writes a record. 

; The main function is:

;  (disk-sort in-file out-file block-size read-record write-record less?)

; This will take the records in in-file, sort them according to less?, 
; and write them to out-file.
; The block-size indicates the number of records that can be sorted in-memory.
; It is okay to let in-file and out-file be the same.

;;; IMPLEMENTATION

(module sort mzscheme
  (provide disk-sort)
  
  (require (only (lib "43.ss" "srfi")
                 vector-copy vector-for-each)
           (only (lib "32.ss" "srfi")
                 vector-sort!)
           (lib "42.ss" "srfi"))
  
  (define (read-block-from-file filename position block-size read-record)
    (let ([port (open-input-file filename)])
      (begin0
        (read-block port position block-size read-record)
        (close-input-port port))))
  
  (define (read-block port position block-size read-record)
    ; Read size records from port starting from the file-position position
    ; Return the records as a vector. If there are less than size records 
    ; left in the file, the vector returned will have a length smaller than size.
    (let ([v (make-vector block-size)])
      (file-position port position)
      (let loop ([n 0])
        (cond
          [(= n block-size) v]
          [else             (let ([r (read-record port)])
                              (if (and (eof-object? r) (< n block-size))
                                  ; n < size   records read
                                  (set! v (vector-copy v 0 n))
                                  ; store read record, and read the rest
                                  (begin
                                    (vector-set! v n r)
                                    (loop (+ n 1)))))]))
      (if (any?-ec (: x v) (eof-object? x))
          (error))
      v))
      
  
  (define (merge-vectors v1 v2 less?)
    (let* ([s1 (vector-length v1)]
           [s2 (vector-length v2)]
           [v  (make-vector (+ s1 s2))])
      (let loop ([n1 0] [n2 0] [n 0])
        (cond
          [(and (= n1 s1) (= n2 s2))    v]
          [(= n1 s1)                    (begin
                                          (vector-set! v n (vector-ref v2 n2))
                                          (loop n1 (+ n2 1) (+ n 1)))]
          [(= n2 s2)                    (begin
                                          (vector-set! v n (vector-ref v1 n1))
                                          (loop (+ n1 1) n2 (+ n 1)))]
          [(less? (vector-ref v1 n1)
                  (vector-ref v2 n2))   (begin
                                          (vector-set! v n (vector-ref v1 n1))
                                          (loop (+ n1 1) n2 (+ n 1)))]
          [else                         (begin
                                          (vector-set! v n (vector-ref v2 n2))
                                          (loop n1 (+ n2 1) (+ n 1)))]))))
  
  
  (define (write-block vector write-record port)
    (vector-for-each (lambda (i r) (write-record r port))
                     vector))
  
  
  (define (merge-blocks-in-file-once in-file out-file block-size read-record write-record less?)
    ; in-file consists of a series of blocks. Each block contains block-size records,
    ; in sorted order according to less?. 
    
    ; out-file will consist of a series of blocks of size 2*block_size. Each sorted
    ; according to less?
    
    ; in-file and out-file can be the same file
    (let ([in  (open-input-file in-file)]
          [out (open-output-file out-file 'update)])
      
      (define (read-next-block)
        (read-block in (file-position in) block-size read-record))
      
      (begin0
        (let loop ([b1 (read-next-block)] 
                   [b2 (read-next-block)])
          (if (zero? (+ (vector-length b1) (vector-length b2)))
              (void)
              (begin
                (write-block (merge-vectors b1 b2 less?) write-record out)
                (loop (read-next-block) (read-next-block)))))
        (close-input-port in)
        (close-output-port out))))
  
  (define (filename->number-of-records filename read-record)
    (let ([in (open-input-file filename)])
      (begin0
        (let loop ([n 0])
          (if (eof-object? (read-record in))
              n
              (loop (+ n 1))))
        (close-input-port in))))
  
  (define (number-of-merges-needed size block-size)
    ; how many merges is neccessary to sort a file consisting of size records,
    ; the file consists of blocks of block-size sorted records
    (if (>= block-size size)
        0
        (+ 1 (number-of-merges-needed size (* 2 block-size)))))
  
  
  (define (merge-blocks-in-file file size block-size read-record write-record less?)
    (if (zero? (number-of-merges-needed size block-size))
        (void)
        (begin
          (merge-blocks-in-file-once file file block-size read-record write-record less?)
          (merge-blocks-in-file file size (* 2 block-size) read-record write-record less?))))
  
  
  (define (disk-sort in-file out-file block-size read-record write-record less?)
    ; TODO: Remember size from the first pass
    
    ; sort the records in in-filename.
    ; block-size records can be sorted in-memory
    (let ([in  (open-input-file in-file)]
          [out (open-output-file out-file 'update)])
      (define (read-next-block)
        (read-block in (file-position in) block-size read-record))
      ; Initial pass reads blocks and sort them in-memory
      (let loop ([b (read-next-block)])
        (if (zero? (vector-length b))
            (begin
              (close-input-port in)
              (close-output-port out))
            (begin
              (vector-sort! less? b)
              (write-block b write-record out)
              (loop (read-next-block)))))
      ; Finish off by repeated merging
      (let ([size (filename->number-of-records out-file read-record)])
        (merge-blocks-in-file out-file size block-size read-record write-record less?))))
  
  )