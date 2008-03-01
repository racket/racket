;;; indexer.scm 

(module indexer mzscheme

  (provide (struct index (name documents lexicon positions))
           (struct lexicon (terms term-numbers))
           index-files
           delete-index
           load-index
           save-index
           
           (struct document (path weight))
           lookup-document-path
           
           term-number->term
           read-frequency
           number-of-documents
           number-of-terms
           
           read-inverted-list
           
           token-case-sensitive)
  
  ;;; 
  ;;; INDEXER - SORT BASED - WITH COMPRESSION - GAPS VERSION
  ;;;         
  
  ; This version stores the term numbers as gaps. This implies
  ; that we need to write the initial temporary file in sorted
  ; blocks. The tokenizing and the in-memory sorting phases are
  ; therefore combined.
  
  ;; The sort-based algorithm:
  
  ; 1.  For each term in each document a record is made in
  ;     a temporary file. 
  
  ;     A record consists of a term number, a document number
  ;     and the frequency of the term in the document.
  
  ; 2.  Internal sorting is done with MAX-RECORDS-IN-MEMORY
  ;     records at a time. Two records are compared first 
  ;     by term number, then by document number.
  
  ; 3.  Pairwise merging is used to get the whole file sorted.
  
  ; 4.  The inverted file is output.
  
  (require 
   (lib "match.ss")
   (lib "file.ss")
   (lib "list.ss")
   (lib "serialize.ss")
   ; (all-except (planet "42.ss" ("soegaard" "srfi.plt" 1 2)) index)
   (all-except (file "planet/srfi.plt/1/2/42.ss") index)
   (rename (file "planet/srfi.plt/1/2/42.ss") Index index)
   "planet/bit-io.scm"   ; (planet "bit-io.scm" ("soegaard" "bit-io.plt" 2 0))
   "planet/dotimes.scm"  ; (planet "control.scm" ("soegaard" "control.plt")) ; dotimes
   "config.scm"
   "compression.scm"
   "lexer.scm")
  
  ;;;
  ;;; INDEX
  ;;;
  
  ; An INDEX is a 
  (define-serializable-struct index (name documents lexicon positions) (make-inspector))
  ; where  name       is unused,
  ; and    documents  is a hash-table from document numbers to document structures,
  ; and    lexicon    is lexicon structure (the in-memory kind while constructing kind, see below), 
  ; and    positions  is a vector, which associates a term-number with its bit-position 
  ;                   in the inverted file.
  ; NOTE: 
  
  (define (save-index path i . options)
    (apply with-output-to-file path
           (λ () (write (serialize i)))
           options))
  
  (define (delete-index path)
    (when (file-exists? path)
      (delete-file path)))
  
  (define (load-index path)
    (cond [(file-exists? path)  ; TODO for debug
           (with-input-from-file path
             (λ () (deserialize (read))))]
          [else
           (error (format "the index file '~a' doesn't exist\n" path))]))
  
  
  (define (number-of-terms index)
    (lexicon-size (index-lexicon index)))
  
  ;;;
  ;;; DOCUMENTS
  ;;;
  
  ; A DOCUMENT is a 
  (define-serializable-struct document (path weight) (make-inspector))
  ; where 
  ;   path   is the file path 
  ;                        n   2
  ;   weight is W = sqrt( sum w   ) , where w  = 1 + ln( f  )
  ;              d        t=1  d,t           d,t          d,t
  
  (define (make-empty-documents)
    (make-hash-table 'equal))
  
  (define (number-of-documents index)
    (hash-table-count (index-documents index)))
  
  (define (lookup-document-path index d)
    (let ([doc (hash-table-get (index-documents index) d)])
      (if doc
          (document-path doc)
          #f)))
  
  ;;;
  ;;; LEXICON
  ;;;
  
  ; Each term is associated with a term number (id).
  ; During tokenizing we need term->term-number lookups. 
  ; In later phases we need term-number->term lookups.
  
  ; A LEXICON is a 
  (define-serializable-struct lexicon (terms term-numbers) (make-inspector))
  ; where  terms         is a hash table associating byte strings with term numbers,
  ; and    term-numbers  is an vector of terms (byte strings).
  
  (define (make-empty-lexicon)
    (make-lexicon (make-hash-table 'equal)
                  #f))
  
  (define (lexicon-size lexicon)
    (hash-table-count (lexicon-terms lexicon)))
  
  (define (lexicon-insert! lexicon term)
    ; return the term-number of term, if term is not
    ; present insert it first
    (let ([terms (lexicon-terms lexicon)])
      (hash-table-get terms term
                      (λ ()
                        (let ([id (add1 (lexicon-size lexicon))])
                          (hash-table-put! terms term id)
                          id)))))
  
  ; lookup-term : lexicon bytes -> (union term-number #f)
  (define (lookup-term lexicon term)
    (hash-table-get (lexicon-terms lexicon) term (λ () #f)))
 
  ; term-number->term : index natural -> term
  ;   return the term with term-number t
  (define (term-number->term index t)
    (vector-ref (lexicon-term-numbers (index-lexicon index)) t))
  
  
  ;;;
  ;;; RECORDS
  ;;;
  
  (define (make-record term-number doc-number freq)
    (list term-number doc-number freq))
  
  (define write-term-gap write-number/unary)
  (define read-term-gap  read-number/unary)
  
  (define write-document-number write-number/delta)
  (define read-document-number read-number/delta)
  
  (define write-frequency write-number/gamma)
  (define read-frequency  read-number/gamma)
  
  (define write-record
    (case-lambda
      [(term-number+doc-number+freq)
       (write-record term-number+doc-number+freq (current-output-bit-port))]
      [(term-number+doc-number+freq out-bit-port)
       (match term-number+doc-number+freq
         [(term-number doc-number freq)
          (write-term-gap term-number out-bit-port)
          (write-document-number doc-number out-bit-port)
          (write-frequency freq out-bit-port)])]))
  
  (define read-record
    (case-lambda
      [()            (read-record (current-input-bit-port))]
      [(in-bit-port) (list (read-term-gap in-bit-port)
                           (read-document-number in-bit-port)
                           (read-frequency in-bit-port))]))
  
  (define (skip-records n port)
    (do-ec (:repeat n)
           (read-record port)))
  
  (define (record<? r1 r2)
    (or (< (car r1) (car r2))
        (and (= (car r1) (car r2))
             (< (cadr r1) (cadr r2)))))
  
  (define (write-sorted-records rs)
    (do-ec (:let prev-t 1)
           (:list r rs)
           (:match (t d f) r)
           (:let tg (- t prev-t))
           (begin
             (write-record (list tg d f))
             (set! prev-t t))))
   
  ;;;
  ;;; UTIL
  ;;;
  
  (define-syntax swap!
    (syntax-rules ()
      [(swap! x y)
       (let ([t x])
         (set! x y)
         (set! y t))]))
  
  ;;;
  ;;; INDEXING
  ;;;
  
  (define (merge-two-blocks size1 in1 size2 in2 out)
    (let ([prev-t1 1]
          [prev-t2 1]
          [prev-t  1])
      (define (read1)
        (match (read-record in1)
          [(tg d f) 
           (set! prev-t1 (+ prev-t1 tg))
           (list prev-t1 d f)]))
      (define (read2)
        (match (read-record in2)
          [(tg d f) 
           (set! prev-t2 (+ prev-t2 tg))
           (list prev-t2 d f)]))
      (define (write r)
        (match r
          [(t d f) 
           (write-record (list (- t prev-t) d f) out)
           (set! prev-t t)]))
      
      (skip-records size1 in2)
      (let loop ([r1 (read1)]
                 [r2 (read2)]
                 [s1 (sub1 size1)]
                 [s2 (sub1 size2)])
        (cond
          [(record<? r1 r2) (write r1)
                            (if (> s1 0)
                                (loop (read1) r2
                                      (sub1 s1) s2)
                                (begin
                                  (write r2)
                                  (dotimes (_ s2)
                                           (write (read2)))))]
          [else             (write r2)
                            (if (> s2 0)
                                (loop r1 (read2)
                                      s1 (sub1 s2))
                                (begin
                                  (write r1)
                                  (dotimes (_ s1)
                                           (write (read1)))))]))
      (skip-records size2 in1)))
  
  ; 
  (define (external-merge-sort number-of-records block-size 
                               in-file out-file . options)
    (cond
      [(>= block-size number-of-records)
       (values in-file out-file)]
      [else
       (display "*")
       (let ([number-of-block-pairs 
              (quotient number-of-records (* 2 block-size))])
         (let* ([in1 (open-input-bit-file in-file)]
                [in2 (open-input-bit-file in-file)]
                [out (apply open-output-bit-file out-file options)])
           (dotimes (_ number-of-block-pairs)
                    (merge-two-blocks block-size in1 block-size in2 out))
           (let ([left (- number-of-records
                          (* 2 number-of-block-pairs block-size))])
             (if (> left block-size)
                 (merge-two-blocks block-size in1 (- left block-size) in2 out)
                 (do-ec (:repeat left)
                        (write-record (read-record in1) out))))
           (close-input-bit-port in1)
           (close-input-bit-port in2)
           (close-output-bit-port out)
           (apply external-merge-sort 
                  number-of-records (* 2 block-size)
                  out-file in-file options)))]))
  
  
  (define (index-files files-to-index result-path name lexer)
    ; 1. Initialization
    (display "INITIALIZATION\n")
    (let ([documents  (make-empty-documents)]
          [lexicon    (make-empty-lexicon)]
          [tmp        (make-temporary-file "tmp1-~a.data" #f "/tmp/")]
          [tmp2       (make-temporary-file "tmp2-~a.data" #f "/tmp/")])
      ; 2. Process text and write records to a temporary file
      (display "TOKENIZING\n")
      (let ([number-of-records
             (with-output-to-bit-file tmp
               (λ ()
                 (letrec ([records       '()]
                          [len           0]
                          [add-record!   (λ(r) 
                                           (set! len (+ len 1))
                                           (set! records (cons r records)))]
                          [sort-and-save (λ ()
                                           (write-sorted-records (quicksort records record<?))
                                           (set! records '()))])
                   (begin0
                     (sum-ec (:parallel 
                              ;(:repeat 10)  ; early stopping for test runs
                              (: file-path (Index doc-number) files-to-index))
                             (begin (hash-table-put! documents doc-number (make-document file-path 0)))
                             ; (if (indexable-file? file-path))
                             ;(begin (hash-table-put! documents doc-number (make-document file-path 0)))
                             (: f (calculate-term-frequencies lexicon file-path lexer))
                             (:match (term-number . freq) f)
                             (begin 
                               (add-record! (make-record term-number doc-number freq))
                               (if (zero? (remainder len MAX-RECORDS-IN-MEMORY))
                                   (sort-and-save))
                               1))
                     (sort-and-save))))
               'replace)])
        ; 3. Internal sorting of blocks of size MAX-RECORDS-IN-MEMORY
        ; NOTE: In this version of the indexer, this phase was put into the previous phase.
        ;       - see the book.
        ; (display "INTERNAL SORT\n")
        ; (internal-sort number-of-records MAX-RECORDS-IN-MEMORY
        ;               read-record write-record
        ;               tmp tmp2 'update)
        ; (swap! tmp tmp2)
        
        ; 4. Merging
        (display "MERGING\n")
        (let-values ([(sorted-file temporary-file)
                      (external-merge-sort number-of-records MAX-RECORDS-IN-MEMORY 
                                           tmp tmp2 'replace)])
          (delete-file temporary-file)
          
          ; 5. i) Output inverted file
          ;         Format for each entry:
          ;           t n g1 f1 ... gn gn
          ;         where t is the term number, gi are gaps between document numbers, i.e.
          ;         d1=g1, d2=g1+g2, etc,  and fi are frequencies.
          ;   ii) Record the bit position of each term in the inverted file.
          (let* ([number-of-terms (lexicon-size lexicon)]
                 [positions       (make-vector (add1 number-of-terms) #f)]
                 [ns (make-vector (add1 number-of-terms) 0)])
            ; pass 1: for each t find n
            (with-input-from-bit-file sorted-file
              (λ ()
                (do-ec (:let t 1)
                       (:repeat number-of-records)
                       (:match (tg d f) (read-record))
                       (begin
                         (set! t (+ t tg))
                         (vector-set! ns t (add1 (vector-ref ns t)))))))
            ; pass 2: output the index
            (with-input-from-bit-file sorted-file
              (λ ()
                (with-output-to-bit-file result-path
                  (λ ()
                    (do-ec (:let t 1)
                           (: k 1 (add1 number-of-terms))
                           ; first record
                           (:let n (vector-ref ns k))
                           (:let pos (bit-file-position (current-output-bit-port)))
                           (:match (tg d f) (read-record))
                           (begin (set! t (+ t tg)))
                           (:let dg d)
                           (begin
                             (vector-set! positions t pos)
                             (write-number t)
                             (write-number n)
                             (write-number dg)
                             (write-frequency f))
                           ; remaining n-1 records
                           (:let prev-d d)
                           (:repeat (- n 1))
                           (:match (tg d f) (read-record))
                           (begin (set! t (+ t tg)))
                           (:let dg (- d prev-d))
                           (begin
                             (write-number dg)
                             (write-frequency f)
                             (set! prev-d d))))
                  'replace)))
            (delete-file sorted-file)
            ; 5. Finish up
            (let ([term-numbers (make-vector (add1 (lexicon-size lexicon)) #"")])
              ; run through the hash-table and make the term-number to term association
              (hash-table-for-each (lexicon-terms lexicon)
                                   (λ (term term-number)
                                     (vector-set! term-numbers term-number term)))
              (set-lexicon-term-numbers! lexicon term-numbers)
              (calculate-document-weights (make-index name documents lexicon positions) result-path)))))))
  
  
  
  ; calculate-term-frequencies : lexicon path -> (list (cons term-number frequecy))
  ;   return a list of term-number/frequecy pairs
  ;   terms in file will be added to the lexicon
  (define (calculate-term-frequencies lexicon file lexer)
    (let ([frequencies (make-hash-table 'equal)])
      (define (increase-frequency! term-number)
        (let ([f (hash-table-get frequencies term-number
                                 (λ ()
                                   (hash-table-put! frequencies term-number 0)
                                   0))])
          (hash-table-put! frequencies term-number (add1 f))))
      (define (insert-and-increase! term)
        (let ([term-number (lexicon-insert! lexicon term)])
          (increase-frequency! term-number)))
      ; for each token in the file:
      (lexer file (λ (term pos) (insert-and-increase! term)))
      (let ([fs '()])
        (hash-table-for-each frequencies 
                             (λ (term freq)
                               (set! fs (cons (cons term freq) fs))))
        fs)))
  
  (define (read-inverted-list)
      ; Format for each entry:
      ;   t n g1 f1 ... gn gn
      ; where t is the term number, gi are gaps between
      ; document numbers, i.e.
      ;   d1=g1, d2=g1+g2, ...
      ; and fi are frequencies.
      (let* ([t (read-number (current-input-bit-port))]
             [n (read-number)])
        (list t n
              (list-ec (:let d 0)
                       (: _ n)
                       (:let dg (read-number))
                       (:let f  (read-frequency))
                       (begin 
                         (set! d (+ d dg))
                         (list d f))))))
  
  (define (calculate-document-weights an-index path)
    (let ([ws (make-vector (add1 (number-of-documents an-index)) 0)]
          [N  (number-of-documents an-index)])
      ; sum the wdt^2 = log(1 + N/fdt)^2
      (match an-index
        [($ index ignored-path documents lexicon positions)
         (with-input-from-bit-file path
           (λ ()
             (do-ec (:repeat (number-of-terms an-index))
                    (:let il (read-inverted-list))
                    (:match (t n dfs) il)
                    (:list df dfs)
                    (:let d (car df))
                    (:let f (cadr df))
                    (vector-set! ws d (+ (vector-ref ws d)
                                         (expt (log (+ 1.0 (/ N f))) 2))))))])
      ; take square roots
      (do-ec (:vector w (Index i) ws)
             (vector-set! ws i (sqrt w)))
      ; store the results
      (hash-table-map 
       (index-documents an-index)
       (λ (d doc)
         (set-document-weight! doc (vector-ref ws d))
         doc))
      ; return index
      an-index))
  
  )
