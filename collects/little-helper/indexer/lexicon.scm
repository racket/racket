;;; lexicon.scm

; This file contains the disk-based lexicon.
; It is used for lookups only - i.e. it is not
; used under index generation.

;;; IDEAS OF IMPROVEMENT:
;    - replace generic read with a my-read-string
;      that simply reads a string "..." -- and measure 
;      whether it is faster.
;    - cache some memory blocks

;;; TERMINOLOGY

; A LEXICON is a collection of term/term-number pairs.
; A TERM is a string. A TERM-NUMBER is a natural number.

; Given a term, it is possible to determine whether
; the term is present in the lexicon, and if so, to
; find the associated term-number.

;;; INTENDED USAGE

; This implementation assume that lexicon creation happens once, 
; and that lookups happen often.
; That is: Lookups should be fast and require very little memory.

; For the search engine this lexicon is used by the web server,
; when queries are made. Another lexicon is used during the
; indexing proces.

;;; FILE FORMAT 

; All term/term-number pairs follow each other with no seperation.
; The term is enclosed in quotation marks. The term number
; is written (for now) as a normal integer.

;    "term1"num1"term2"num2...

; Rationale: 
;    The total length of all terms in  current lexicon 
;    is 1.700.000 for PLT Source Search, so the total 
;    lexicon size ought to be around 3-4 megabytes, which 
;    is fine for the web-server.

;;; PREAMBLE

;(print-struct #t)

(module lexicon mzscheme
  (provide create-disk-index 
           for-each-term-in-disk-lexicon
           lookup)
  
  (require (lib "42.ss" "srfi")
           (only (lib "43.ss" "srfi") vector-copy)
           "planet/file.scm") ; (planet "io.ss" ("dherman" "io.plt"))
  
  ; Just in case, the representation is going to change
  (define read-term read)
  (define read-number read)
  (define read-pos read)
  (define write-term write)
  (define write-number write)
  (define write-pos write)
  
  ;;; DISK INDICES
  
  ; A disk index contains term/file-position pairs in the
  ; same format a lexicon is stored in. It is used to
  ; find relevant blocks in a lexicon.
  
  ; create-disk-index : filename filename natural ->
  ;   Construct a disk index with term/file-position pairs
  ;   for lexicon-file in index-file. 
  (define (create-disk-index lexicon-file index-file block-size)
    (call-with-input-file lexicon-file
      (lambda (in)
        (call-with-output-file index-file
          (lambda (out)
            (let loop ([n 0])
              (let ([pos    (file-position in)]
                    [term   (read-term in)]
                    [number (read-number in)])
                (unless (or (eof-object? term)
                            (eof-object? number))
                  (when (zero? (modulo n block-size))
                    (write-term term out)
                    (write-pos pos out))
                  (loop (+ n 1))))))
          'truncate))))
  
  
  ;;; MEMORY INDICES
  
  ; A memory index is meant to be kept in-memory.
  ; It is used to find appropriate blocks in the
  ; lexicon file.
  
  (define-struct memory-index (terms numbers) (make-inspector))
  
  ; where  terms is a vector of strings in ascending order,
  ; and    numbers is a vector of numbers.
  ; It represents a mapping from terms to numbers.
  
  ; memory-index-lookup : memory-index term -> natural
  ;   lookup term in the given memory-index:
  ;   return 
  ;            numbers[i]
  ;   where i is an index such that
  ;         term[i] <= term < term[i+1]
  (define (memory-index-lookup mi term)
    (let ([terms   (memory-index-terms mi)]
          [numbers (memory-index-numbers mi)])
      ; binary search to find index
      (let loop ([from 0]  
                 [to   (vector-length terms)])
        (if (= (- to from) 1)
            (vector-ref numbers from)
            (let ([mid (+ from (quotient (- to from) 2))])
              (if (string>=? term (vector-ref terms mid))
                  (loop mid to)
                  (loop from mid)))))))
  
  ; read-disk-index-size : filename -> natural
  ;   return the number of terms in the disk index
  ; TODO:  consider putting the size in the beginning of the index file.
  (define (read-disk-index-size filename)
    (call-with-input-file filename
      (lambda (in)
        (let loop ([n 0])
          (let ([term   (read-term in)]
                [number (read-number in)])
            (if (or (eof-object? term)
                    (eof-object? number))
                n
                (loop (+ n 1))))))))
  
  ; read-disk-index-block : filename natural natural -> memory-index
  ;   read the block of size block-size situated at the file position pos
  (define (read-disk-index-block index-file block-size pos)
    (call-with-input-file index-file
      (lambda (port)
        ; go to start of block
        (file-position port pos)   
        ; read block
        (let ([terms   (make-vector block-size)]
              [numbers (make-vector block-size)])
          (let loop ([n  0])
            (if (= n block-size)
                'normal
                (let ([term   (read-term port)]
                      [number (read-number port)])
                  (if (or (eof-object? term)
                          (eof-object? number))
                      ; early file ending => truncate terms and numbers
                      (begin
                        (set! terms (vector-copy terms 0 n))
                        (set! numbers (vector-copy numbers 0 n)))
                      ; continue
                      (begin
                        (vector-set! terms n term)
                        (vector-set! numbers n number)
                        (loop (+ n 1)))))))
          ; return block
          (make-memory-index terms numbers)))))
  
  ; read-disk-index : filename -> memory-index-block
  ;   read a whole disk-index into a memory-index-block
  (define (read-disk-index file)
    (read-disk-index-block file (read-disk-index-size file) 0)) 
  
  
  ;;; MEMORY LEXICON BLOCKS
  
  ; A memory lexicon block is an in-memory representation
  ; of a block of the lexicon file.
  
  (define-struct memory-lexicon-block (terms numbers) (make-inspector))
  ; where terms is vector of strings 
  ; and   numbers is a vector of numbers.
  
  ; memory-lexicon-block-lookup : memory-lexicon-block term -> number or #f
  ;   lookup term in the given memory-lexicon-block:
  ;   returns 
  ;      numbers[i],   
  ;   where i is an index such that
  ;      term[i] = term  
  ;   or #f if no such index exists.
  (define (memory-lexicon-block-lookup mlb term)
    ;(display "memory-lexicon-block-lookup: ") (display mlb) (newline)
    (let ([terms   (memory-lexicon-block-terms mlb)]
          [numbers (memory-lexicon-block-numbers mlb)])
      ; binary search to find index
      (let loop ([from 0]  
                 [to   (vector-length terms)])
        (if (= (- to from) 1)
            (if (string=? term (vector-ref terms from))
                (vector-ref numbers from)
                #f)
            (let ([mid (+ from (quotient (- to from) 2))])
              (if (string>=? term (vector-ref terms mid))
                  (loop mid to)
                  (loop from mid)))))))
  
  
  ; read-lexicon-block : filename natural natural -> memory-lexicon-block
  ;   read the block of size block-size situated at the file position pos
  (define (read-lexicon-block file block-size pos)
    (call-with-input-file file
      (lambda (port)
        ; go to start of block
        (file-position port pos)   
        ; read block
        (let ([terms   (make-vector block-size)]
              [numbers (make-vector block-size)])
          (let loop ([n  0])
            (if (= n block-size)
                'normal
                (let ([term   (read port)]
                      [number (read port)])
                  (if (or (eof-object? term)
                          (eof-object? number))
                      ; early file ending => truncate terms and numbers
                      (begin
                        (set! terms (vector-copy terms 0 n))
                        (set! numbers (vector-copy numbers 0 n)))
                      ; continue
                      (begin
                        (vector-set! terms n term)
                        (vector-set! numbers n number)
                        (loop (+ n 1)))))))
          ; return block
          (make-memory-lexicon-block terms numbers)))))
  
  
  ; write-memory-lexicon-block : memory-lexicon-block port ->
  ;   write the lexicon block to port
  (define (write-memory-lexicon-block mlb port)
    (let ([terms   (memory-lexicon-block-terms mlb)]
          [numbers (memory-lexicon-block-numbers mlb)])
      (do-ec (:parallel 
              (: t terms)
              (: n numbers))
             (begin
               (write t port)
               (write n port)))))
  
  ;;; LOOKUP
  
  (define (lookup term lexicon-file index-file block-size)
    (let* ([mem-index         (get-disk-index index-file)]
           [block-pos         (memory-index-lookup mem-index term)]
           [mem-lexicon-block (get-lexicon-block lexicon-file block-size block-pos)])
      ;(display (list mem-index block-pos mem-lexicon-block)) (newline)
      (memory-lexicon-block-lookup mem-lexicon-block term)))
  
  ; get-disk-index : index-file-name -> memory-index-block
  ;   cached version of read-disk-index
  (define get-disk-index
    (let ([cache (make-hash-table 'equal)])
      (lambda (index-file)
        (hash-table-get cache index-file
                        (lambda ()
                          (let ([mem-index (read-disk-index index-file)])
                            (hash-table-put! cache index-file mem-index)
                            mem-index))))))
  
  (define get-lexicon-block
    (let ([cache (make-hash-table 'equal 'weak)])
      (lambda (lexicon-file block-size block-pos)
        (let ([key (list lexicon-file block-size block-pos)])
          (hash-table-get cache key
                          (lambda ()
                            (let ([mlb (read-lexicon-block lexicon-file block-size block-pos)])
                              (hash-table-put! cache key mlb)
                              mlb)))))))
  
  
  ;;; TODO TODO    CACHING
  ;;;  - cache memory index
  ;;;  - cache lexicon blocks  -  even one would be an improvement
  
  ;;; SPECIAL PURPOSE
  
  (require "config.scm")
  
  (define (for-each-term-in-disk-lexicon index-name f)
    (call-with-input-file (lexicon-path index-name)
      (lambda (port)
        (let loop ()
          (let ([term (read-term port)]
                [number (read-number port)])
            (if (or (eof-object? term)
                    (eof-object? number))
                (void)
                (begin
                  (f term number)
                  (loop))))))))
  
  ; NOTE: Not used in current design
  ;       If for-each-term-in-disk-lexicon is too slow,
  ;       then something to try is flat-lexicon + grep
  (define (create-flat-lexicon index-name)
    (call-with-output-file (flat-lexicon-path index-name)
      (lambda (out)
        (for-each-term-in-disk-lexicon 
         index-name
         (lambda (term number)
           (display term out)
           (newline out))))))
  
  ;;;
  ;;; TEST
  ;;;
  
  (define (test-memory-index-lookup)
    (define mi 
      (make-memory-index 
       (vector "a" "b" "c" "d" "e")
       (vector 0   1   2   3   4)))
    (equal? (map (lambda (t) (memory-index-lookup mi t))
                 (list "a" "b" "c" "d" "e" "f"
                       "aa" "bb" "cc" "dd" "ee" "ff"))
            (list 0 1 2 3 4 4
                  0 1 2 3 4 4)))
  
  (define (test-lexicon-block-lookup)
    (define mlb
      (make-memory-lexicon-block
       (vector "a" "b" "c" "d" "e")
       (vector 0   1   2   3   4)))
    (equal? (map (lambda (t) (memory-lexicon-block-lookup mlb t))
                 (list "a" "b" "c" "d" "e" "f"
                       "aa" "bb" "cc" "dd" "ee" "ff"))
            (list 0 1 2 3 4 #f
                  #f #f #f #f #f #f)))
  
  (define (test-read-lexicon-block)
    (define mlb 
      (make-memory-lexicon-block
       (vector "a" "b" "c" "d" "e")
       (vector 0   1   2   3   4)))
    
    (with-temporary-file file ()
      (call-with-output-file file
        (lambda (port)
          (write-memory-lexicon-block mlb port))
        'truncate)
      
      (let ([pos 0]
            [size (vector-length
                   (memory-lexicon-block-terms mlb))])
        (equal? (read-lexicon-block file size pos)
                mlb))))
  
  
  (define (test)
    (and (test-memory-index-lookup)
         (test-lexicon-block-lookup)
         (test-read-lexicon-block)))
  
  
  ;;; GENERATING TEST INPUT
  
  (define (write-alphabet-lexicon file)
    ;"a"0"b"1"c"2 ... 
    (call-with-output-file file
      (lambda (out)
        (do-ec (: i (char->integer (string-ref "a" 0)) (+ 1 (char->integer (string-ref "z" 0))))
               (begin
                 (write (list->string
                         (list
                          (integer->char i))) 
                        out)
                 (write (- i (char->integer (string-ref "a" 0)))
                        out))))
      'truncate))
  
  #;(begin
    (write-alphabet-lexicon "alphabet-lexicon.txt")
    (create-disk-index "alphabet-lexicon.txt" "alphabet-disk-index.txt" 5)
    
    ; (lookup term lexicon-file index-file block-size)
    (list-ec (: i 
                (char->integer (string-ref "a" 0))
                (+ 1 (char->integer (string-ref "z" 0))))
             (lookup (string (integer->char i))
                     "alphabet-lexicon.txt" "alphabet-disk-index.txt" 5))
    )
  )