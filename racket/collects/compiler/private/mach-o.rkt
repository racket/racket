#lang racket/base
(require racket/promise)

(provide add-plt-segment
         remove-signature
         add-ad-hoc-signature
         get/set-dylib-path)

(define (check-exe-id exe-id)
  (unless (memv exe-id '(#xFeedFacf #xFeedFace))
    (error 'mach-o "unrecognized #x~x" exe-id)))

(define aarch64-machine-type #x0100000C)

(define (read-ulong p)
  (integer-bytes->integer (read-bytes 4 p) #f))

(define (read-xulong p)
  (integer-bytes->integer (read-bytes 8 p) #f))

(define (write-ulong v out)
  (display (integer->integer-bytes v 4 #f) out))

(define (write-xulong v out)
  (display (integer->integer-bytes v 8 #f) out))

(define (write-be-ulong v out)
  (display (integer->integer-bytes v 4 #f #t) out))

(define (check-same a b)
  (unless (= a b)
    (error 'check-same "not: ~e ~e" a b)))

(define (round-up-page v machine-type)
  (if (eqv? machine-type aarch64-machine-type)
      (bitwise-and #xFFFFC000 (+ v #x3FFF))
      (bitwise-and #xFFFFF000 (+ v #xFFF))))

(define (mult-of-8 n)
  (bitwise-and (+ n 7) (bitwise-not #x7)))

(define (mult-of-16 n)
  (bitwise-and (+ n 15) (bitwise-not #xF)))

(define move-link-edit? #t)

;; To add a segment to a Mach-O executable, we need to add a load command.
;; To support code signing of the resulting executable, we need to place the
;; new segment before the linkedit segment, because code signing wants the
;; linkedit segment last.
;;
;; Also, we need to strip away any existing signature. We expect any
;; existing signature to be at the end of the linkedit segment. The
;; signature is 16-byte aligned, which means that some padding may
;; have been added to the linkedit segment, and we have to undo that
;; when removing the signature. In other words, we have to recognize
;; everything that contributes to the linkedit segment and find the
;; contibution that is otherwise last; that's the job of
;; `linkedit-limit-offset`, below.
;;
;; Since the new segment is written before the linkedit segment, we
;; need to shift all the offsets that refer to file positions of
;; things within the linkedit segment. The `...-pos` variables
;; generally retain the location in a file of an offset that needs to
;; be updated.
;;
(define (add-plt-segment file
                         segdata ; if #f, just strips a signature, if any
                         #:name [segment-name #"__PLTSCHEME"])
  (let-values ([(p out) (open-input-output-file file #:exists 'update)])
    (dynamic-wind
        void
        (lambda ()
          (file-stream-buffer-mode out 'none)
          (define exe-id (read-ulong p))
          (check-exe-id exe-id)
          (define machine-id (read-ulong p))
          (read-ulong p)
          (check-same #x2 (read-ulong p))
          (let* ([total-cnt (read-ulong p)]
                 [cmdssz (read-ulong p)]
                 [min-used (round-up-page cmdssz machine-id)]
                 [sym-tab-pos #f]
                 [dysym-pos #f]
                 [hints-pos #f]
                 [link-edit-64? #f]
                 [link-edit-pos #f]
                 [link-edit-addr 0]
                 [link-edit-offset 0]
                 [link-edit-len 0]
                 [link-edit-vmlen 0]
                 [dyld-info-pos #f]
                 [dyld-info-offs #f]
                 [function-starts-pos #f]
                 [function-starts-offset #f]
                 [data-in-code-pos #f]
                 [data-in-code-offset #f]
                 [exports-trie-pos #f]
                 [exports-trie-offset #f]
                 [chained-fixups-pos #f]
                 [chained-fixups-offset #f]
                 [code-signature-pos #f]
                 [code-signature-size 0]
                 [code-signature-lc-sz 0]
                 [code-sign-drs-pos #f]
                 [code-sign-drs-offset 0]
                 [linkedit-limit-offset 0])
            ;; (printf "~a cmds, length 0x~x\n" cnt cmdssz)
            (read-ulong p) ; flags
            (when (equal? exe-id #xFeedFacf)
              (read-ulong p)) ; extra reserved word for 64-bit header
            (let loop ([cnt total-cnt])
              (unless (zero? cnt)
                (let ([pos (file-position p)]
                      [cmd (read-ulong p)]
                      [sz (read-ulong p)])
                  ;; (printf "~x (~a)\n" cmd sz)
                  (case cmd
                    [(1 #x19) ; #x19 is 64-bit variant
                     ;; Segment
                     (let ([64? (equal? cmd #x19)])
                       (let ([segname (read-bytes 16 p)]
                             [vmaddr ((if 64? read-xulong read-ulong) p)]
                             [vmlen ((if 64? read-xulong read-ulong) p)]
                             [offset ((if 64? read-xulong read-ulong) p)]
                             [len ((if 64? read-xulong read-ulong) p)])
                         ;; (printf "~s\n" segname)
                         (when (equal? segname #"__LINKEDIT\0\0\0\0\0\0")
                           (set! link-edit-64? 64?)
                           (set! link-edit-pos pos)
                           (set! link-edit-addr vmaddr)
                           (set! link-edit-vmlen vmlen)
                           (set! link-edit-offset offset)
                           (set! link-edit-len len)
                           (when (link-edit-len . < . 0)
                             (error "bad LINKEDIT length")))
                         ;; (printf "  0x~x 0x~x -> 0x~x 0x~x\n" offset len vmaddr vmlen)
                         (read-ulong p)
                         (read-ulong p)
                         (let ([nsects (read-ulong p)])
                           (read-ulong p)
                           (let loop ([nsects nsects])
                             (unless (zero? nsects)
                               (let ([sect (read-bytes 16 p)]
                                     [seg (read-bytes 16 p)]
                                     [vmaddr ((if 64? read-xulong read-ulong) p)]
                                     [vmsz ((if 64? read-xulong read-ulong) p)]
                                     [offset (read-ulong p)]
                                     [align (read-ulong p)]
                                     [reloff (read-ulong p)]
                                     [nreloc (read-ulong p)]
                                     [flags (read-ulong p)])
                                 (when ((+ offset vmsz) . > . (+ cmdssz (if (equal? exe-id #xFeedFacf) 32 28)))
                                   (when (and (positive? offset)
                                              (offset . < . min-used))
                                     ;; (printf "   new min!\n")
                                     (set! min-used offset)))
                                 ;; (printf "    ~s,~s 0x~x 0x~x\n" seg sect offset vmsz)
                                 (read-ulong p) 
                                 (read-ulong p)
                                 (when 64? (read-ulong p)))
                               (loop (sub1 nsects)))))))]
                    [(2)
                     ;; LC_SYMTAB, symbol table
                     (set! sym-tab-pos pos)
                     (let ([symoffset (read-ulong p)]
                           [nsyms (read-ulong p)]
                           [stroffset (read-ulong p)]
                           [strsize (read-ulong p)]
                           [symsize (if link-edit-64? 16 12)])
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ symoffset (* nsyms symsize))))
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ stroffset strsize))))]
                    [(#xB)
                     ;; LC_DYSYMTAB, Dysym
                     (set! dysym-pos pos)
                     ;; Skip over counts:
                     (for ([i 6]) (read-ulong p))
                     ;; Check that unhandled counts are zero; we could handle
                     ;; more of these, and it's just a matter of working out
                     ;; the size to multiply each count
                     (for ([i 3])
                       (read-ulong p)
                       (unless (zero? (read-ulong p))
                         (error 'check-header "unhandled LC_DYSYMTAB count is not 0")))
                     ;; indirect syms
                     (let ([offset (read-ulong p)]
                           [count (read-ulong p)])
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset (* count 4)))))
                     ;; Two more:
                     (for ([i 2])
                       (read-ulong p)
                       (unless (zero? (read-ulong p))
                         (error 'check-header "unhandled LC_DYSYMTAB count is not 0")))]
                    [(#x16)
                     ;; 2-level hints table
                     (set! hints-pos pos)]
                    [(#x22 #x80000022)
                     ;; LC_DYLD_INFO
                     (let ([rebaseoff (read-ulong p)]
                           [rebasesize (read-ulong p)]
                           [bindoff (read-ulong p)]
                           [bindsize (read-ulong p)]
                           [weakbindoff (read-ulong p)]
                           [weakbindsize (read-ulong p)]
                           [lazybindoff (read-ulong p)]
                           [lazybindsize (read-ulong p)]
                           [exportbindoff (read-ulong p)]
                           [exportbindsize (read-ulong p)])
                       (set! dyld-info-pos pos)
                       (set! dyld-info-offs (vector rebaseoff bindoff weakbindoff lazybindoff exportbindoff))
                       (set! linkedit-limit-offset (max linkedit-limit-offset
                                                        (+ rebaseoff rebasesize)
                                                        (+ bindoff bindsize)
                                                        (+ weakbindoff weakbindsize)
                                                        (+ lazybindoff lazybindsize)
                                                        (+ exportbindoff exportbindsize))))]
                    [(#x80000033)
                     ;; LC_DYLD_EXPORTS_TRIE
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! exports-trie-pos pos)
                       (set! exports-trie-offset offset)
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [(#x80000034)
                     ;; LC_DYLD_CHAINED_FIXUPS
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! chained-fixups-pos pos)
                       (set! chained-fixups-offset offset)
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [(#x26)
                     ;; LC_FUNCTION_STARTS
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! function-starts-offset offset)
                       (set! function-starts-pos pos)
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [(#x29)
                     ;; LC_DATA_IN_CODE
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! data-in-code-offset offset)
                       (set! data-in-code-pos pos)
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [(#x1D)
                     ;; LC_CODE_SIGNATURE
                     (if (= cnt 1)
                         (let ([offset (read-ulong p)]
                               [size (read-ulong p)])
                           (file-position p (+ offset size))
                           (if (eof-object? (read-byte p))
                               (begin
                                 (unless ((abs (- offset linkedit-limit-offset)) . < . 16)
                                   (error 'check-header
                                          "code signature does not line up with end of other linkedit blocks: ~s vs. ~s"
                                          offset linkedit-limit-offset))
                                 (set! code-signature-pos pos)
                                 (set! code-signature-lc-sz sz)
                                 ;; Claim a larger size to account for padding:
                                 (set! code-signature-size (- link-edit-len (- linkedit-limit-offset link-edit-offset))))
                               (log-warning "WARNING: code signature is not at end of file")))
                         (log-warning "WARNING: code signature is not last load command"))]
                    [(#x2B)
                     ;; LC_DYLIB_CODE_SIGN_DRS
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! code-sign-drs-offset offset)
                       (set! code-sign-drs-pos pos)
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [(#x1E #x2E)
                     ;; LC_SEGMENT_SPLIT_INFO or LC_LINKER_OPTIMIZATION_HINT
                     (let ([offset (read-ulong p)]
                           [size (read-ulong p)])
                       (set! linkedit-limit-offset (max linkedit-limit-offset (+ offset size))))]
                    [else
                     (void)])
                  (file-position p (+ pos sz))
                  (loop (sub1 cnt)))))
            (when (or segdata
                      code-signature-pos)
              ;; (printf "Start offset: 0x~x\n" min-used)
              (let ([end-cmd (+ cmdssz 
                                (if (equal? exe-id #xFeedFacf) 32 28)
                                (- code-signature-lc-sz))]
                    [new-cmd-sz (if segdata
                                    (if link-edit-64? 72 56)
                                    0)]
                    [outlen (if segdata
                                (round-up-page (bytes-length segdata) machine-id)
                                0)]
                    [out-offset (if move-link-edit?
                                    link-edit-offset
                                    (+ link-edit-offset (round-up-page link-edit-len machine-id)))]
                    [out-addr (if move-link-edit?
                                  link-edit-addr
                                  (+ link-edit-addr (round-up-page link-edit-vmlen machine-id)))])
                (unless ((+ end-cmd new-cmd-sz) . < . min-used)
                  (error 'check-header 
                         "no room for a new section load command (current end is ~a; min used is ~a; need ~a)"
                         end-cmd min-used new-cmd-sz))
                ;; Shift commands starting with link-edit command:
                (unless link-edit-pos (error "LINKEDIT not found"))
                (file-position p link-edit-pos)
                (let ([s (read-bytes (- end-cmd link-edit-pos) p)])
                  (file-position out (+ link-edit-pos new-cmd-sz))
                  (display s out))
                (file-position out 16)
                ;; Adjust the number of load commands:
                (write-ulong (+ total-cnt (if segdata 1 0) (if code-signature-pos -1 0)) out)
                (write-ulong (+ cmdssz (- code-signature-lc-sz) new-cmd-sz) out)
                (cond
                  [segdata
                   ;; Write the new command:
                   (file-position out link-edit-pos)
                   (write-ulong (if link-edit-64? #x19 1) out) ; LC_SEGMENT[_64]
                   (write-ulong new-cmd-sz out)
                   (display (pad-segment-name segment-name) out)
                   ((if link-edit-64? write-xulong write-ulong) out-addr out)
                   ((if link-edit-64? write-xulong write-ulong) outlen out)
                   ((if link-edit-64? write-xulong write-ulong) out-offset out)
                   ((if link-edit-64? write-xulong write-ulong) outlen out)
                   (write-ulong 0 out) ; maxprot
                   (write-ulong 0 out) ; minprot
                   (write-ulong 0 out)
                   (write-ulong 4 out) ; 4 means SG_NORELOC
                   ;; Shift command positions
                   (unless sym-tab-pos
                     (error 'mach-o "symtab position not found"))
                   (when (sym-tab-pos . > . link-edit-pos)
                     (set! sym-tab-pos (+ sym-tab-pos new-cmd-sz)))
                   (unless dysym-pos
                     (error 'mach-o "dysym position not found"))
                   (when (dysym-pos . > . link-edit-pos)
                     (set! dysym-pos (+ dysym-pos new-cmd-sz)))
                   (define-syntax-rule (shift-pos! pos)
                     (when pos
                       (when (pos . > . link-edit-pos)
                         (set! pos (+ pos new-cmd-sz)))))
                   (shift-pos! hints-pos)
                   (shift-pos! function-starts-pos)
                   (shift-pos! data-in-code-pos)
                   (shift-pos! code-sign-drs-pos)
                   (shift-pos! exports-trie-pos)
                   (shift-pos! chained-fixups-pos)
                   (set! link-edit-pos (+ link-edit-pos new-cmd-sz))
                   (when move-link-edit?
                     ;; Update link-edit segment entry:
                     (file-position out (+ link-edit-pos 24))
                     ((if link-edit-64? write-xulong write-ulong) (+ link-edit-addr outlen) out)
                     ((if link-edit-64? write-xulong write-ulong) link-edit-vmlen out)
                     ;; (printf "Update to ~a\n" (+ out-offset outlen))
                     ((if link-edit-64? write-xulong write-ulong) (+ out-offset outlen) out)
                     ((if link-edit-64? write-xulong write-ulong) (- link-edit-len code-signature-size) out)
                     ;; Read link-edit segment:
                     (file-position p link-edit-offset)
                     (let ([link-edit (read-bytes (- link-edit-len code-signature-size) p)])
                       ;; Write link-edit data in new location:
                       (file-position out (+ link-edit-offset outlen))
                       (display link-edit out))
                     ;; Shift symbol-table pointer:
                     (file-position p (+ sym-tab-pos 8))
                     (let ([symtab-offset (read-ulong p)]
                           [_ (read-ulong p)]
                           [symstr-offset (read-ulong p)])
                       (file-position out (+ sym-tab-pos 8))
                       (write-ulong (+ symtab-offset outlen) out)
                       (file-position out (+ sym-tab-pos 16))
                       (write-ulong (+ symstr-offset outlen) out))
                     ;; Shift dysym pointers:
                     (for-each (lambda (delta)
                                 (file-position p (+ dysym-pos delta))
                                 (let ([offset (read-ulong p)])
                                   (unless (zero? offset)
                                     (file-position out (+ dysym-pos delta))
                                     (write-ulong (+ offset outlen) out))))
                               '(32 40 48 56 64 72))
                     ;; Shift hints pointer:
                     (when hints-pos
                       (file-position p (+ hints-pos 8))
                       (let ([hints-offset (read-ulong p)])
                         (file-position out (+ hints-pos 8))
                         (write-ulong (+ hints-offset outlen) out)))
                     ;; Shift function starts:
                     (when function-starts-pos
                       (file-position p (+ function-starts-pos 8))
                       (write-ulong (+ function-starts-offset outlen) out))
                     ;; Shift data-in-code:
                     (when data-in-code-pos
                       (file-position p (+ data-in-code-pos 8))
                       (write-ulong (+ data-in-code-offset outlen) out))
                     ;; Shift code-sign drs:
                     (when code-sign-drs-pos
                       (file-position p (+ code-sign-drs-pos 8))
                       (write-ulong (+ code-sign-drs-offset outlen) out))
                     ;; Shift dyld-info offs
                     (when dyld-info-pos
                       (let ([update (lambda (n)
                                       (unless (< (vector-ref dyld-info-offs n) out-offset)
                                         (file-position out (+ dyld-info-pos new-cmd-sz 8 (* n 8)))
                                         (write-ulong (+ (vector-ref dyld-info-offs n) outlen) out)))])
                         (update 0)
                         (update 1)
                         (update 2)
                         (update 3)
                         (update 4)))
                     ;; Shift export-trie drs:
                     (when exports-trie-pos
                       (file-position p (+ exports-trie-pos 8))
                       (write-ulong (+ exports-trie-offset outlen) out))
                     ;; Shift chained-fixups drs:
                     (when chained-fixups-pos
                       (file-position p (+ chained-fixups-pos 8))
                       (write-ulong (+ chained-fixups-offset outlen) out)))
                   ;; Write segdata to former link-data offset:
                   (file-position out out-offset)
                   (display segdata out)
                   (display (make-bytes (- outlen (bytes-length segdata)) 0) out)
                   ;; Adjust file size 
                   (file-truncate out (+ link-edit-offset link-edit-len (- code-signature-size) outlen))]
                  [code-signature-pos
                   ;; Shrink linkedit size, since signature is going away
                   (let* ([file-pos (+ link-edit-pos 8 16 (* 1 (if link-edit-64? 8 4)))]
                          [link-edit-len (- linkedit-limit-offset link-edit-offset)]
                          [link-edit-vm-len (round-up-page link-edit-len machine-id)])
                     (file-position out file-pos)
                     ((if link-edit-64? write-xulong write-ulong) link-edit-vmlen out)
                     (file-position out (+ file-pos (* 2 (if link-edit-64? 8 4))))
                     ((if link-edit-64? write-xulong write-ulong) link-edit-len out)
                     ;; Adjust file size 
                     (file-truncate out (+ link-edit-offset link-edit-len)))])
                ;; Result is offset where data was written:
                out-offset))))
        (lambda ()
          (close-input-port p)
          (close-output-port out)))))

(define (pad-segment-name bs)
  (bytes-append bs (make-bytes (- 16 (bytes-length bs)))))

(define (fix-offset p pos out d base delta)
  (when (and out (not (zero? delta)))
    (file-position p (+ pos d))
    (let ([offset (read-ulong p)])
      (when (offset . > . base)
        (file-position out (+ pos d))
        (write-ulong (+ offset delta) out)
        (flush-output out)))))

(define (remove-signature file)
  (add-plt-segment file #f))

;; requires that a signature is not already present
(define (add-ad-hoc-signature file)
  (let-values ([(p out) (open-input-output-file file #:exists 'update)])
    (dynamic-wind
     void
     (lambda ()
       (file-stream-buffer-mode out 'none)
       (define exe-id (read-ulong p))
       (check-exe-id exe-id)
       (define machine-id (read-ulong p))
       (cond
         [(eqv? machine-id aarch64-machine-type)
          (define orig-size (file-size file))
          (define file-identity (let-values ([(base name dir?) (split-path file)])
                                  (bytes-append (path->bytes name) #"\0")))
          (read-ulong p)
          (check-same #x2 (read-ulong p))
          (let* ([total-cnt (read-ulong p)]
                 [cmdssz (read-ulong p)]
                 [min-used (round-up-page cmdssz machine-id)]
                 [link-edit-64? #f]
                 [link-edit-pos #f]
                 [link-edit-len 0])
            (read-ulong p) ; flags
            (when (equal? exe-id #xFeedFacf)
              (read-ulong p)) ; extra reserved word for 64-bit header
            (let loop ([cnt total-cnt])
              (unless (zero? cnt)
                (let ([pos (file-position p)]
                      [cmd (read-ulong p)]
                      [sz (read-ulong p)])
                  (case cmd
                    [(1 #x19) ; #x19 is 64-bit variant
                     ;; Segment
                     (let ([64? (equal? cmd #x19)])
                       (let ([segname (read-bytes 16 p)]
                             [vmaddr ((if 64? read-xulong read-ulong) p)]
                             [vmlen ((if 64? read-xulong read-ulong) p)]
                             [offset ((if 64? read-xulong read-ulong) p)]
                             [len ((if 64? read-xulong read-ulong) p)])
                         ;; (printf "~s\n" segname)
                         (when (equal? segname #"__LINKEDIT\0\0\0\0\0\0")
                           (set! link-edit-64? 64?)
                           (set! link-edit-pos pos)
                           (set! link-edit-len len))))]
                    [(#x1D)
                     ;; LC_CODE_SIGNATURE
                     (error 'add-ad-hoc-signature "file already has a signature")])
                  (file-position p (+ pos sz))
                  (loop (sub1 cnt)))))
            (let* ([end-cmd (+ cmdssz 
                               (if (equal? exe-id #xFeedFacf) 32 28))]
                   [new-cmd-sz 16]
                   [log-page-size 12]
                   [page-size (expt 2 log-page-size)]
                   [hash-code-size 32]
                   [padded-size (mult-of-16 orig-size)]
                   [num-slots (quotient (+ padded-size (sub1 page-size)) page-size)]
                   [data-size (+ 20
                                 88
                                 (bytes-length file-identity)
                                 (* num-slots hash-code-size))])
              (unless ((+ end-cmd new-cmd-sz) . < . min-used)
                (error 'check-header 
                       "no room for a new section load command (current end is ~a; min used is ~a; need ~a)"
                       end-cmd min-used new-cmd-sz))
              (unless link-edit-pos
                (error 'add-ad-hoc-signature
                       "did not find linkedit section"))
              (file-position out 16)
              ;; Adjust the number of load commands:
              (write-ulong (+ total-cnt 1) out)
              (write-ulong (+ cmdssz new-cmd-sz) out)
              ;; Write the new command:
              (file-position out end-cmd)
              (write-ulong #x1D out) ;; LC_CODE_SIGNATURE
              (write-ulong new-cmd-sz out)
              (write-ulong padded-size out) ; data offset
              (write-ulong data-size out)
              ;; Update LINKEDIT length:
              (let ([file-pos (+ link-edit-pos 8 16 (* 1 (if link-edit-64? 8 4)))]
                    [len (+ link-edit-len data-size (- padded-size orig-size))])
                (file-position out file-pos)
                ((if link-edit-64? write-xulong write-ulong) (round-up-page len machine-id) out) ; vm-len
                (file-position out (+ file-pos (* 2 (if link-edit-64? 8 4))))
                ((if link-edit-64? write-xulong write-ulong) len out))
              ;; Add padding:
              (file-position out orig-size)
              (write-bytes (make-bytes (- padded-size orig-size) 0) out)

              ;; Hash file content
              (flush-output out)
              (file-position p 0)
              (define hash-codes
                (let loop ([pos 0])
                  (if (pos . >= . padded-size)
                      '()
                      (cons (sha256-bytes (read-bytes (min page-size (- padded-size pos)) p))
                            (loop (+ pos page-size))))))

              ;; Write signature at end

              (file-position out padded-size)
              (write-be-ulong #xfade0cc0 out) ; CSMAGIC_EMBEDDED_SIGNATURE
              (write-be-ulong data-size out)
              (write-be-ulong 1 out) ; count
              (write-be-ulong 0 out) ; type
              (write-be-ulong 20 out) ; offset

              (write-be-ulong #xfade0c02 out) ; CSMAGIC_CODEDIRECTORY
              (write-be-ulong (- data-size 20) out) ; length (remaining)
              (write-be-ulong #x20400 out) ; version
              (write-be-ulong #x20002 out) ; flags = CS_ADHOC #x0000002 + ???
              (write-be-ulong (+ 88 (bytes-length file-identity)) out) ; hash array offset
              (write-be-ulong 88 out) ; identity offset
              (write-be-ulong 0 out) ; special slots
              (write-be-ulong num-slots out) ; special slots
              (write-be-ulong padded-size out) ; limit (i.e., original file size plus padding)
              (write-byte hash-code-size out)
              (write-byte 2 out) ; SHA-256
              (write-byte 0 out) ; spare
              (write-byte log-page-size out)
              ;; etc.:
              (write-bytes #"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" out)
              (write-bytes #"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\1" out)

              (write-bytes file-identity out)
              (for ([hash-code (in-list hash-codes)])
                (write-bytes hash-code out))))]
         [else
          ;; no signing
          (void)]))
     (lambda ()
       (close-input-port p)
       (close-output-port out)))))

(define (get/set-dylib-path file rx new-path)
  (let-values ([(p out) (if new-path
                            (open-input-output-file file #:exists 'update)
                            (values (open-input-file file)
                                    #f))])
    (dynamic-wind
        void
        (lambda ()
          (define exe-id (read-ulong p))
          (check-exe-id exe-id)
          (read-ulong p)
          (read-ulong p)
          (read-ulong p) ; 2 is executable, etc.
          (let* ([cnt (read-ulong p)]
                 [cmdssz (read-ulong p)])
            (read-ulong p)
            (when (equal? exe-id #xFeedFacf)
              (read-ulong p))
            (let loop ([cnt cnt] [base 0] [delta 0] [result null])
              (if (zero? cnt)
                  result
                  (let ([pos (file-position p)]
                        [cmd (read-ulong p)]
                        [sz (read-ulong p)])
                    (case cmd
                      [(#xC)
                       ;; LC_LOAD_DYLIB
                       (let ([offset (read-ulong p)])
                         (file-position p (+ pos offset))
                         (let* ([namelen (- sz offset)]
                                [segname (read-bytes namelen p)]
                                [segname (car (regexp-match #rx#"^[^\0]*" segname))])
                           (if (regexp-match rx segname)
                               (let* ([newnamelen (and out
                                                       (mult-of-8 (+ 1 (bytes-length new-path))))]
                                      [delta (if out
                                                 (-  newnamelen namelen)
                                                 0)])
                                 (when out
                                   (unless (zero? delta)
                                     ;; We assume that there's enough header room to
                                     ;; extend this load command, because the binary
                                     ;; was linked with -headerpad_max_install_names
                                     (file-position out (+ pos 4))
                                     (write-ulong (+ sz delta) out)
                                     (flush-output out)
                                     ;; Shift rest of load commands by delta
                                     (let ([end (+ cmdssz 56)])
                                       (file-position p (+ pos sz))
                                       (let ([s (read-bytes (- end (+ pos sz)) p)])
                                         (file-position out (+ pos sz delta))
                                         (write-bytes s out)
                                         (when (negative? delta)
                                           ;; zero-out now-unneeded space:
                                           (write-bytes (make-bytes (- delta) 0) out))
                                         (flush-output out))
                                       ;; Change load-commands size in header:
                                       (file-position out 20)
                                       (write-ulong (+ cmdssz delta) out)
                                       (flush-output out)))
                                   (file-position out (+ pos offset))
                                   (write-bytes new-path out)
                                   (write-bytes (make-bytes (- newnamelen (bytes-length new-path)) 0) out)
                                   (flush-output out))
                                 (file-position p (+ pos sz delta))
                                 (loop (sub1 cnt) pos delta (cons segname result)))
                               (begin
                                 (file-position p (+ pos sz))
                                 (loop (sub1 cnt) base delta result)))))]
                      [else
                       (file-position p (+ pos sz))
                       (loop (sub1 cnt) base delta result)]))))))
        (lambda ()
          (close-input-port p)
          (when out
            (close-output-port out))))))
