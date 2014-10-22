#lang racket/base

(provide add-racket-section
         adjust-racket-section-size
         get-rpath
         set-rpath)

(define 32BIT 1)
(define 64BIT 2)

(define LITTLEEND 1)
(define BIGEND 2)

(define SECTION-TABLE-ALIGN 16) ; conservative enough?
(define SECTION-ALIGN 16)       ; conservative enough?

(define SHT_PROGBITS 1)
(define SHT_NOBITS 8)

(define DT_HASH 4)
(define DT_STRTAB 5)
(define DT_SYMTAB 6)
(define DT_RELA 7)
(define DT_STRSZ 10)
(define DT_INIT 12)
(define DT_FINI 13)
(define DT_RPATH 15)
(define DT_REL 17)
(define DT_JMPREL 23)
(define DT_GNU_HASH #x6ffffef5)
(define	DT_VERNEED #x6ffffffe)
(define DT_VERSYM #x6ffffff0)

(define dynamic-adjusts
  (list DT_HASH
        DT_STRTAB
        DT_SYMTAB
        DT_RELA
        DT_REL
        DT_INIT
        DT_FINI
        DT_JMPREL
        DT_GNU_HASH
        DT_VERNEED
        DT_VERSYM))

(struct elf (ph-offset ph-esize ph-ecount
                       sh-offset sh-esize sh-ecount
                       class format sh-str-index)
  #:transparent)
(struct section (name-offset addr offset size type)
  #:transparent)
(struct program (offset vaddr paddr size)
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

(define (stop) (raise "unexpected input"))

(define (do-read-a-byte p)
  (let ([v (read-byte p)])
    (when (eof-object? v)
      (stop))
    v))

(define (do-read-word format p)
  (let ([a (do-read-a-byte p)]
        [b (do-read-a-byte p)]
        [c (do-read-a-byte p)]
        [d (do-read-a-byte p)])
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
                   (arithmetic-shift a 24))])))

(define (do-read-xword class format p)
  (if (= class 32BIT)
      (do-read-word format p)
      (let ([b (read-bytes 8 p)])
        (if (and (bytes? b) (= 8 (bytes-length b)))
            (integer-bytes->integer b #f (= format BIGEND))
            (stop)))))


(define (do-write-word v format out)
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
        (write-byte (bitwise-and v #xFF) out))))

(define (do-write-xword v class format out)
  (if (= class 32BIT)
      (do-write-word v format out)
      (display (integer->integer-bytes v 8 #f (= format BIGEND)) 
               out)))

(define (read-elf p fail-k k #:dump? [dump? #f])
  (define (expect b)
    (eq? b (read-byte p)))
  (define (skip n)
    (for ([i (in-range n)]) 
      (when (eof-object? (read-byte p))
        (stop))))
  (define (read-a-byte) (do-read-a-byte p))
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
		  (lambda () (do-read-word format p))]
		 [read-xword
		  (lambda () (do-read-xword class format p))]
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
                         (section name-offset addr offset size type)))])
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
                           (program offset vaddr paddr file-size)))])
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

(define (find-section name sections strs)
  (define len (bytes-length strs))
  (define name-len (bytes-length name))
  (for/or ([s (in-list sections)])
    (and (bytes=? name
                  (subbytes strs
                            (min (section-name-offset s)
                                 len)
                            (min (+ (section-name-offset s)
                                    name-len)
                                 len)))
         s)))

(define (find-section-by-offset offset sections)
  (for/or ([s (in-list sections)])
    (and (offset . >= . (section-offset s))
         (offset . < . (+ (section-offset s)
                          (section-size s)))
         s)))

(define (add-racket-section src-file dest-file section-name get-data)
  (call-with-input-file* 
   src-file 
   (lambda (in)
     (read-elf
      in
      (lambda () (values #f #f #f #f))
      (lambda (elf sections programs str-section strs)
        (let ([total-size (file-size src-file)])
          (expand-elf in dest-file
                      elf sections programs str-section strs total-size
                      section-name
                      get-data
                      null
                      void)))))))

(define (expand-elf in dest-file
                    ;; Current state parted from `in`:
                    elf sections programs str-section strs total-size
                    ;; New state:
                    section-name ; #f or name of new section
                    get-data     ; get data for new section (if any)
                    ;; Expansions must be after section headers, must
                    ;; not be at the version beginning of a section,
                    ;; and beware of extending sections that need
                    ;; ".dynamic" support (such as the ".dynstr" section,
                    ;; which is currently supported here); each
		    ;; expansion element has the form
		    ;;  (list <offset> <expand-amt>)
                    expansions    ; list of (list position delta)
		    ;; If `vaddr-moves` is not supplied, then
		    ;; virtual-address adjustments are derived from
		    ;; `expansions` based on the way it overlaps with
		    ;; section offets. If those adjustments would move
		    ;; any SHT_PROGBITS sections, though, then the
		    ;; expanded section(s) must be moved, instead,
		    ;; as described by `vaddr-moves`. Each element
		    ;; has the form
		    ;;  (list <old-vaddr> <length> <new-vaddr>)
		    #:vaddr-moves [vaddr-moves #f]
                    finish)
  (define num-new-sections (if section-name 1 0))
  (let ([new-sec-pos (+ (* num-new-sections (elf-sh-offset elf))
                        (* (elf-sh-esize elf) (elf-sh-ecount elf)))]
        [new-sec-delta (round-up (* num-new-sections (elf-sh-esize elf))
                                 SECTION-TABLE-ALIGN)]
        [new-str-pos (+ (section-offset str-section)
                        (section-size str-section))]
        [new-str-delta (if section-name
                           (round-up (add1 (bytes-length section-name))
                                     SECTION-TABLE-ALIGN)
                           0)]
        [class (elf-class elf)]
        [format (elf-format elf)])
    (let-values ([(expansions)
                  ;; Make expansions a sorted list of (list pos delta),
                  ;; including a 0-length expansion at the end:
                  (sort
                   (list*
                    (list total-size 0)
                    (list new-sec-pos new-sec-delta)
                    (list new-str-pos new-str-delta)
                    expansions)
                   <
                   #:key car)]
                 [(data decl-len mid)
                  ;; The `decl-len` and `mid` returns are ignored
                  ;; and returned as-s
                  (if get-data
                      (get-data (+ total-size new-str-delta new-sec-delta))
                      (values #"" 0 0))])
      (define vm-expansions
	(if vaddr-moves
	    (apply
	     append
	     (for/list ([move (in-list vaddr-moves)])
	       (list
		;; shift at start of region:
		(list (car move)
		      (- (caddr move) (car move)))
		;; unshift at end of region:
		(list (+ (car move) (cadr move))
		      (- (car move) (caddr move))))))
	    (filter
	     values
	     (for/list ([expansion (in-list expansions)])
	       (define s (find-section-by-offset (car expansion) sections))
	       (and s (list (+ (section-addr s)
			       (- (car expansion) (section-offset s)))
			    (cadr expansion)))))))
      (call-with-output-file*
       dest-file
       #:exists 'truncate
       (lambda (out)
         (let* ([write-word
                 (lambda (v) (do-write-word v format out))]
                [write-xword 
                 (lambda (v) (do-write-xword v class format out))]
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
                [read-word
                 (lambda () (do-read-word format in))]
                [read-xword
                 (lambda () (do-read-xword class format in))]
                [xword-size (if (= class 32BIT) 4 8)]
                [adjust/cmp (lambda (offset expansions >?)
                              (+ offset
                                 (for/fold ([delta 0]) ([expansion (in-list expansions)])
                                   (if (offset . >? . (car expansion))
                                       (+ delta (cadr expansion))
                                       delta))))]
                [adjust (lambda (offset) (adjust/cmp offset expansions >=))]
                [adjust* (lambda (offset) (adjust/cmp offset expansions >))]
                [vm-adjust (lambda (offset) (adjust/cmp offset vm-expansions >=))]
                [at-class (lambda (a b) (if (= class 32BIT) a b))])
           
           (file-position in 0)
           (for/fold ([pos 0]) ([expansion (in-list expansions)])
             (copy-port-bytes (- (car expansion) pos) in out)
             (write-n-bytes (cadr expansion) out)
             (car expansion))
           
           (when section-name
             ;; Write new section header:
             (file-position out (adjust* new-sec-pos))
             (write-word (section-size str-section))
             (write-word SHT_PROGBITS)
             (write-xword 0) ; flags
             (write-addr 0) ; addr
             (write-off (adjust total-size)) ; at end
             (write-xword (bytes-length data))
             (write-word 0) ; link
             (write-word 0) ; info
             (write-xword 1) ; align
             (write-xword 0) ; esize

             ;; Write new string for section name:
             (file-position out (adjust* new-str-pos))
             (write-bytes section-name out))

           ;; Fix section-header and program-header offsets:
           (file-position out (at-class 28 32))
           (write-off (adjust (elf-ph-offset elf)))
           (write-off (adjust (elf-sh-offset elf)))

           (when section-name
             ;; Increment section count:
             (file-position out (at-class 48 60))
             (write-half (add1 (length sections)))

             ;; Increment string section size:
             (file-position out (adjust (+ (elf-sh-offset elf)
                                           (* (elf-sh-str-index elf)
                                              (elf-sh-esize elf))
                                           (at-class 20 32))))
             (write-xword (+ (section-size str-section) new-str-delta)))

           ;; Fix up section offsets and sizes:
           (define (new-section-size s)
             (- (adjust* (+ (section-offset s) (section-size s)))
                (adjust (section-offset s))))
           (for ([s (in-list sections)]
                 [i (in-naturals)])
             (let ([addr (section-addr s)]
                   [offset (section-offset s)])
               (file-position out (adjust (+ (elf-sh-offset elf)
                                             (* i (elf-sh-esize elf))
                                             (at-class 14 16))))

	       (define new-addr (vm-adjust addr))
	       (unless (or (= new-addr addr)
			   (not (= SHT_PROGBITS (section-type s))))
		 (error 'expand-elf "cannot move SHT_PROGBITS section"))
               (write-addr new-addr)
               (write-off (adjust offset))

               (unless (= SHT_NOBITS (section-type s))
                 (define new-size (new-section-size s))
                 (unless (= new-size (section-size s))
                   (write-xword new-size)))))

           ;; Fix up program offsets:
           (for ([p (in-list programs)]
                 [i (in-naturals)])
             (let ([offset (program-offset p)]
                   [vaddr (program-vaddr p)]
                   [paddr (program-paddr p)])
               (file-position out (adjust (+ (elf-ph-offset elf)
                                             (* i (elf-ph-esize elf))
                                             (at-class 4 8))))
               (write-off (adjust offset))
               (write-off (vm-adjust vaddr))
               (write-off (vm-adjust paddr))))
           
           ;; Fix up ".dynamic", if any:
           (define dynamic (find-section #".dynamic\0" sections strs))
           (when dynamic
             (file-position in (section-offset dynamic))
             (for ([i (in-range 0 (section-size dynamic) (* 2 xword-size))])
               (define tag (read-xword))
               (define val (read-xword))
               (define dest-pos (adjust (+ i (section-offset dynamic) xword-size)))
               (cond
                [(memq tag dynamic-adjusts) ; DT_HASH, DT_STRTAB, etc.
                 (define new-val (vm-adjust val))
                 (unless (= val new-val)
                   (file-position out dest-pos)
                   (write-xword new-val))]
                [(= tag DT_STRSZ)
                 (define s (find-section #".dynstr\0" sections strs))
                 (unless s (error "could not find .dynstr"))
                 (define new-size (new-section-size s))
                 (unless (= new-size (section-size s))
                   (file-position out dest-pos)
                   (write-xword new-size))])))
           
           (begin0
            ;; Write new section data, producing result:
            (let ([dest (adjust total-size)])
              (file-position out dest)
              (write-bytes data out)

              (values dest (+ dest (bytes-length data)) decl-len mid))

            ;; Any final writes:
            (finish out adjust adjust*))))))))

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

(define (get-rpath src-file)
  (call-with-input-file*
   src-file
   (lambda (in)
     (read-elf
      in
      (lambda () #f)
      (lambda (elf sections programs str-section strs)
        (and elf
             (let ([s (find-section #".dynamic\0" sections strs)]
                   [s2 (find-section #".dynstr\0" sections strs)])
               (and s
                    s2
                    (let ()
                      (define-values (rpath rpath-pos)
                        (find-rpath in elf s))
                      (and rpath
                           (let ()
                             (file-position in (section-offset s2))
                             (define m (regexp-match #rx#"^[^\0]*" in rpath))
                             (and m (car m)))))))))))))

(define (find-rpath in elf s)
  (define class (elf-class elf))
  (define format (elf-format elf))
  (define xword-size (if (= class 32BIT) 4 8))
  (file-position in (section-offset s))
  (for/fold ([rpath #f] [rpath-pos #f])
            ([i (in-range 0 (section-size s) (* 2 xword-size))])
    (define tag (do-read-xword class format in))
    (define val (do-read-xword class format in))
    (cond
     [(= tag DT_RPATH)
      (values val (+ i xword-size (section-offset s)))]
     [else
      (values rpath rpath-pos)])))

(define (set-rpath src-file dest-file rpath)
  (define (fail why) (error 'set-rpath "could not set RT_RPATH; ~a" why))
  (call-with-input-file*
   src-file
   (lambda (in)
     (read-elf
      in
      (lambda () #f)
      (lambda (elf sections programs str-section strs)
        (unless elf (fail "not ELF"))
        (let ([dynamic (find-section #".dynamic\0" sections strs)]
              [dynstr (find-section #".dynstr\0" sections strs)])
          (unless dynamic (fail "no .dynamic"))
          (unless dynstr (fail "no .dynstr"))
          (define-values (rpath-index rpath-pos)
            (find-rpath in elf dynamic))
          (define strtab (section-offset dynstr))
          
          ;; Easy case is when there is enough space counting 0s after
          ;; the current setting, which happens when we (or `chrpath`)
          ;; shrink an rpath:
          (file-position in (+ strtab rpath-index))
          (define avail-len (bytes-length (car (regexp-match #rx#"^[^\0]*\0*" in))))
          (cond
           [(avail-len . > . (bytes-length rpath))
            ;; Copy src to dest, then fixup
            (copy-file src-file dest-file #t)
            (call-with-output-file*
             dest-file
             #:exists 'update
             (lambda (out)
               (file-position out (+ strtab rpath-index))
               (write-bytes rpath out)
               ;; zero out remaining space:
               (write-bytes (make-bytes (- avail-len (bytes-length rpath))) out)
               (void)))]
           [else
            ;; Hard case, where we need to add a new string:
            (define new-str-pos
              ;; New pos is just before last nul byte, to make sure it's
              ;; within the section:
              (sub1 (+ (section-offset dynstr) (section-size dynstr))))
	    (define new-vm-addr
	      (for/fold ([addr (section-addr dynstr)]) ([s (in-list sections)])
		(max addr (+ (section-addr s) (section-size s)))))
            (expand-elf in dest-file
                        elf sections programs str-section strs (file-size src-file)
                        ;; No new section:
                        #f #f
                        ;; Add rpath at end of dynstrs, use virtual address
			;; past all allocated:
                        (list
                         (list new-str-pos
			       (round-up (+ (bytes-length rpath) 1) SECTION-ALIGN)))
			#:vaddr-moves (list
				       (list (section-addr dynstr)
					     (section-size dynstr)
					     (round-up new-vm-addr SECTION-ALIGN)))
                        (lambda (out adjust adjust*)
                          ;; Write new dynstr:
                          (file-position out (adjust* new-str-pos))
                          (write-byte 0 out)
                          (write-bytes rpath out)

                          ;; Update RT_RPATH
                          (file-position out (adjust rpath-pos))
                          (do-write-xword (section-size dynstr) 
                                          (elf-class elf) (elf-format elf)
                                          out)))])))))))
