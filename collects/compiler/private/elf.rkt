#lang racket/base

(provide add-racket-section
         adjust-racket-section-size)

(define 32BIT 1)
(define 64BIT 2)

(define LITTLEEND 1)
(define BIGEND 2)

(define SECTION-ALIGN 16) ; conservative?

(define SHT_PROGBITS 1)

(struct elf (ph-offset ph-esize ph-ecount
                       sh-offset sh-esize sh-ecount
                       class format sh-str-index)
  #:transparent)
(struct section (name-offset offset size)
  #:transparent)
(struct program (offset size)
  #:transparent)

(define (copy-port-bytes amt in out)
  (let ([get (min amt 4096)])
    (let ([s (read-bytes get in)])
      (unless (and (bytes? s)
                   (= get (bytes-length s)))
        (error 'add-elf-section "file data copy failed"))
      (write-bytes s out))
    (unless (= get amt)
      (copy-port-bytes (- amt get) in out))))

(define (write-n-bytes amt out)
  (let ([put (min amt 4096)])
    (write-bytes (make-bytes put 0) out)
    (unless (= put amt)
      (write-n-bytes (- amt put) out))))

(define (round-up v align)
  (let ([d (modulo v align)])
    (if (zero? d)
        v
        (+ v (- align d)))))

(define (read-elf p fail-k k #:dump? [dump? #f])
  (define (stop) (raise "unexpected input"))
  (define (expect b)
    (eq? b (read-byte p)))
  (define (skip n)
    (for ([i (in-range n)]) 
      (when (eof-object? (read-byte p))
        (stop))))
  (define (read-a-byte)
    (let ([v (read-byte p)])
      (when (eof-object? v)
        (stop))
      v))
  (define (skip-half) (skip 2))
  (define (skip-word) (skip 4))
  (define (show v) (displayln v) v)
  ;; Read ELF identification ---------------
  (if (not (and
            (expect #x7F)
            (expect (char->integer #\E))
            (expect (char->integer #\L))
            (expect (char->integer #\F))))
      ;; Not an ELF binary
      (fail-k)
      ;; Is an ELF binary:
      (let ([class (read-byte p)])
	(unless (or (= class 32BIT)
		    (= class 64BIT))
	  (stop))
        (let ([format (read-byte p)])
          (unless (or (= format LITTLEEND)
                      (= format BIGEND))
            (stop))
          ;; Set up multi-byte reading ---------------
          (let* ([read-word
		  (lambda ()
		    (let ([a (read-a-byte)]
			  [b (read-a-byte)]
			  [c (read-a-byte)]
			  [d (read-a-byte)])
		      (cond
		       [(= format LITTLEEND)
			(bitwise-ior a
				     (arithmetic-shift b 8)
				     (arithmetic-shift c 16)
				     (arithmetic-shift d 24))]
		       [else
			(bitwise-ior d
				     (arithmetic-shift c 8)
				     (arithmetic-shift b 16)
				     (arithmetic-shift a 24))])))]
		 [read-xword
		  (lambda ()
		    (if (= class 32BIT)
			(read-word)
			(let ([b (read-bytes 8 p)])
			  (if (and (bytes? b) (= 8 (bytes-length b)))
			      (integer-bytes->integer b #f (= format BIGEND))
			      (stop)))))]
		 [read-half
		  (lambda ()
		    (let ([a (read-a-byte)]
			  [b (read-a-byte)])
		      (cond
		       [(= format LITTLEEND)
			(bitwise-ior a
				     (arithmetic-shift b 8))]
		       [else
			(bitwise-ior b
				     (arithmetic-shift a 8))])))]
		 [skip-addr (lambda ()
			      (skip (if (= class 32BIT) 4 8)))]
		 [read-addr (lambda () (read-xword))]
		 [read-off (lambda () (read-xword))])
	    (skip 1) ; version
	    (skip 9) ; padding
	    (skip-half) ; type
	    (skip-half) ; machine
	    (skip-word) ; version
	    (skip-addr) ; entry
            ;; Read rest of ELF header -----------------
            (let ([ph-offset (read-off)]
                  [sh-offset (read-off)]
                  [flags (read-word)]
                  [eh-size (read-half)]
                  [ph-esize (read-half)]
                  [ph-ecount (read-half)]
                  [sh-esize (read-half)]
                  [sh-ecount (read-half)]
                  [sh-str-index (read-half)])
              ;; Read sections ------------------------
              (let ([sections
                     (for/list ([i (in-range sh-ecount)])
                       (file-position p (+ sh-offset (* i sh-esize)))
                       (let ([name-offset (read-word)]
                             [type (read-word)]
                             [flags (read-xword)]
                             [addr (read-addr)]
                             [offset (read-off)]
                             [size (read-xword)]
                             [link (read-word)]
                             [info (read-word)]
                             [align (read-xword)]
                             [esize (read-xword)])
                         (section name-offset offset size)))])
                ;; Read program headers ------------------------
                (let ([programs
                       (for/list ([i (in-range ph-ecount)])
                         (file-position p (+ ph-offset (* i ph-esize)))
                         (let ([type (read-word)]
			       [flags (if (= class 32BIT)
					  0
					  (read-word))]
                               [offset (read-off)]
                               [vaddr (read-addr)]
                               [paddr (read-addr)]
                               [file-size (read-xword)])
                           (program offset file-size)))])
                  ;; Load strings from string section ------------------------
                  (let* ([str-section (list-ref sections sh-str-index)]
                         [strs (begin
                                 (file-position p (section-offset str-section))
                                 (read-bytes (section-size str-section) p))])
                    (when dump?
                      (for ([s (in-list sections)])
                        (printf "~s ~x ~x\n"
                                (regexp-match #rx#"[^\0]*" strs (min (section-name-offset s)
                                                                     (bytes-length strs)))
                                (section-offset s)
                                (section-size s))))
                    (k (elf ph-offset ph-esize ph-ecount
                            sh-offset sh-esize sh-ecount
                            class format sh-str-index)
                       sections programs 
                       str-section strs))))))))))

(define (add-racket-section src-file dest-file section-name get-data)
  (call-with-input-file* 
   src-file 
   (lambda (in)
     (read-elf
      in
      (lambda () (values #f #f #f #f))
      (lambda (elf sections programs str-section strs)
        (let ([new-sec-pos (+ (elf-sh-offset elf)
                              (* (elf-sh-esize elf) (elf-sh-ecount elf)))]
              [new-sec-delta (round-up (elf-sh-esize elf) SECTION-ALIGN)]
              [new-str-pos (+ (section-offset str-section)
                              (section-size str-section))]
              [new-str-delta (round-up (add1 (bytes-length section-name))
                                       SECTION-ALIGN)]
              [total-size (file-size src-file)]
              [class (elf-class elf)]
              [format (elf-format elf)])
          (let-values ([(a-pos a-delta b-pos b-delta)
                        (if (new-sec-pos . < . new-str-pos)
                            (values new-sec-pos new-sec-delta
                                    new-str-pos new-str-delta)
                            (values new-str-pos new-str-delta
                                    new-sec-pos new-sec-delta))]
		       [(data decl-len mid) (get-data (+ total-size new-str-delta new-sec-delta))])
            (call-with-output-file*
             dest-file
             #:exists 'truncate
             (lambda (out)
               (let* ([write-word
                       (lambda (v)
                         (if (= format LITTLEEND)
                             (begin
                               (write-byte (bitwise-and v #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -8) #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -16) #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -24) #xFF) out))
                             (begin
                               (write-byte (bitwise-and (arithmetic-shift v -24) #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -16) #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -8) #xFF) out)
                               (write-byte (bitwise-and v #xFF) out))))]
		      [write-xword 
		       (lambda (v)
			 (if (= class 32BIT)
			     (write-word v)
			     (display (integer->integer-bytes v 8 #f (= format BIGEND)) 
				      out)))]
		      [write-addr (lambda (v) (write-xword v))]
		      [write-off (lambda (v) (write-xword v))]
                      [write-half
                       (lambda (v)
                         (if (= format LITTLEEND)
                             (begin
                               (write-byte (bitwise-and v #xFF) out)
                               (write-byte (bitwise-and (arithmetic-shift v -8) #xFF) out))
                             (begin
                               (write-byte (bitwise-and (arithmetic-shift v -8) #xFF) out)
                               (write-byte (bitwise-and v #xFF) out))))]
                      [adjust (lambda (offset)
                                (if (offset . >= . a-pos)
                                    (if (offset . >= . b-pos)
                                        (+ offset a-delta b-delta)
                                        (+ offset a-delta))
                                    offset))]
                      [adjust* (lambda (offset)
                                 (add1 (adjust (sub1 offset))))]
		      [at-class (lambda (a b) (if (= class 32BIT) a b))])
                 
                 (file-position in 0)
                 (copy-port-bytes a-pos in out)
                 (write-n-bytes a-delta out)
                 (copy-port-bytes (- b-pos a-pos) in out)
                 (write-n-bytes b-delta out)
                 (copy-port-bytes (- total-size b-pos) in out)
                 
                 ;; Write new section:
                 (file-position out (adjust* new-sec-pos))
                 (write-word (section-size str-section))
                 (write-word SHT_PROGBITS)
                 (write-xword 0) ; flags
                 (write-addr 0) ; addr
                 (write-off (+ total-size new-sec-delta new-str-delta))
                 (write-xword (bytes-length data))
                 (write-word 0) ; link
                 (write-word 0) ; info
                 (write-xword 1) ; align
                 (write-xword 0) ; esize

                 ;; Write new string:
                 (file-position out (adjust* new-str-pos))
                 (write-bytes section-name out)

                 ;; Fix section-header and program-header offsets:
                 (file-position out (at-class 28 32))
                 (write-off (adjust (elf-ph-offset elf)))
                 (write-off (adjust (elf-sh-offset elf)))

                 ;; Increment section count:
                 (file-position out (at-class 48 60))
		 (write-half (add1 (length sections)))

                 ;; Increment string section size:
                 (file-position out (adjust (+ (elf-sh-offset elf)
                                               (* (elf-sh-str-index elf)
                                                  (elf-sh-esize elf))
                                               (at-class 20 32))))
                 (write-xword (+ (section-size str-section) new-str-delta))

                 ;; Fix up section offsets:
                 (for ([s (in-list sections)]
                       [i (in-naturals)])
                   (let ([offset (section-offset s)])
                     (when (offset . > . a-pos)
                       (file-position out (adjust (+ (elf-sh-offset elf)
                                                     (* i (elf-sh-esize elf))
                                                     (at-class 16 24))))
                       (write-off (adjust offset)))))

                 ;; Fix up program offsets:
                 (for ([p (in-list programs)]
                       [i (in-naturals)])
                   (let ([offset (program-offset p)])
                     (when (offset . > . a-pos)
                       (file-position out (adjust (+ (elf-ph-offset elf)
                                                     (* i (elf-ph-esize elf))
                                                     (at-class 4 8))))
                       (write-off (adjust offset)))))
                 
                 ;; Write new section data:
                 (let ([dest (+ total-size new-sec-delta new-str-delta)])
                   (file-position out dest)
                   (write-bytes data out)
                   
                   (values dest (+ dest (bytes-length data)) decl-len mid))))))))))))

(define (adjust-racket-section-size src-file name-regexp new-size)
  (define fixup
    (call-with-input-file*
     src-file
     (lambda (in)
       (read-elf
        in
        (lambda () (values #f #f #f #f))
        (lambda (elf sections programs str-section strs)
          (and elf
               (for/or ([s (in-list sections)]
                        [i (in-naturals)])
                 (and (regexp-match? name-regexp
                                     strs
                                     (min (section-name-offset s)
                                          (bytes-length strs)))
                      (lambda (out)
                        (let ([class (elf-class elf)]
                              [format (elf-format elf)])
                          (define-values (word-size xoff-size xword-size addr-size)
                            (if (= class 32BIT)
                                (values 4 4 4 4)
                                (values 4 8 8 8)))
                          ;; Go to section record, and specifically to
                          ;; the size field:
                          (file-position out (+ (elf-sh-offset elf)
                                                (* i (elf-sh-esize elf))
                                                (* 2 word-size)
                                                xword-size
                                                addr-size
                                                xoff-size))
                          ;; Write the new size:
                          (display (integer->integer-bytes new-size
                                                           xword-size
                                                           #f
                                                           (= format BIGEND)) 
                                   out)))))))))))
  (when fixup
    (call-with-output-file*
     src-file
     #:exists 'update
     fixup)))
