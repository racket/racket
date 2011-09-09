#lang racket/base

(provide add-plt-segment
         get/set-dylib-path)

(define exe-id
  (if (equal? (path->bytes (system-library-subpath #f)) #"x86_64-macosx")
      #xFeedFacf
      #xFeedFace))

(define (read-ulong p)
  (integer-bytes->integer (read-bytes 4 p) #f))

(define (read-xulong p)
  (integer-bytes->integer (read-bytes 8 p) #f))

(define (write-ulong v out)
  (display (integer->integer-bytes v 4 #f) out))

(define (write-xulong v out)
  (display (integer->integer-bytes v 8 #f) out))

(define (check-same a b)
  (unless (= a b)
    (error 'check-same "not: ~e ~e" a b)))

(define (round-up-page v)
  (bitwise-and #xFFFFF000 (+ v #xFFF)))

(define (mult-of-4 n)
  (let ([m (modulo n 4)])
    (if (zero? m)
        n
        (+ n (- 4 m)))))

(define move-link-edit? #t)

(define (add-plt-segment file segdata)
  (let-values ([(p out) (open-input-output-file file #:exists 'update)])
    (dynamic-wind
        void
        (lambda ()
          (file-stream-buffer-mode out 'none)
          (check-same exe-id (read-ulong p))
          (read-ulong p)
          (read-ulong p)
          (check-same #x2 (read-ulong p))
          (let* ([cnt (read-ulong p)]
                 [cmdssz (read-ulong p)]
                 [min-used (round-up-page cmdssz)]
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
                 [dyld-info-offs #f])
            ;; (printf "~a cmds, length 0x~x\n" cnt cmdssz)
            (read-ulong p) ; flags
            (when (equal? exe-id #xFeedFacf)
              (read-ulong p)) ; extra reserved word for 64-bit header
            (let loop ([cnt cnt])
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
                                   (when (offset . < . min-used)
                                     ;; (printf "   new min!\n")
                                     (set! min-used offset)))
                                 ;; (printf "    ~s,~s 0x~x 0x~x\n" seg sect offset vmsz)
                                 (read-ulong p) 
                                 (read-ulong p)
                                 (when 64? (read-ulong p)))
                               (loop (sub1 nsects)))))))]
                    [(2)
                     ;; Symbol table
                     (set! sym-tab-pos pos)]
                    [(#xB)
                     ;; Dysym
                     (set! dysym-pos pos)]
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
                       (set! dyld-info-offs (vector rebaseoff bindoff weakbindoff lazybindoff exportbindoff)))]
                    [else
                     (void)])
                  (file-position p (+ pos sz))
                  (loop (sub1 cnt)))))
            ;; (printf "Start offset: 0x~x\n" min-used)
            (let ([end-cmd (+ cmdssz (if (equal? exe-id #xFeedFacf) 32 28))]
                  [new-cmd-sz (if link-edit-64? 72 56)]
                  [outlen (round-up-page (bytes-length segdata))]
                  [out-offset (if move-link-edit?
                                  link-edit-offset
                                  (+ link-edit-offset (round-up-page link-edit-len)))]
                  [out-addr (+ link-edit-addr (round-up-page link-edit-vmlen))])
              (unless ((+ end-cmd new-cmd-sz) . < . min-used)
                (error 'check-header 
                       "no room for a new section load command (current end is ~a; min used is ~a)"
                       end-cmd min-used))
              ;; Shift commands starting with link-edit command:
              (unless link-edit-pos (error "LINKEDIT not found"))
              (file-position p link-edit-pos)
              (let ([s (read-bytes (- end-cmd link-edit-pos) p)])
                (file-position out (+ link-edit-pos new-cmd-sz))
                (display s out))
              (file-position out 16)
              ;; Increment the number of load commands:
              (write-ulong (+ cnt 1) out)
              (write-ulong (+ cmdssz new-cmd-sz) out)
              ;; Write the new command:
              (file-position out link-edit-pos)
              (write-ulong (if link-edit-64? #x19 1) out) ; LC_SEGMENT[_64]
              (write-ulong new-cmd-sz out)
              (display #"__PLTSCHEME\0\0\0\0\0" out)
              ((if link-edit-64? write-xulong write-ulong) out-addr out)
              ((if link-edit-64? write-xulong write-ulong) outlen out)
              ((if link-edit-64? write-xulong write-ulong) out-offset out)
              ((if link-edit-64? write-xulong write-ulong) outlen out)
              (write-ulong 0 out)
              (write-ulong 0 out)
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
              (when hints-pos
                (when (hints-pos . > . link-edit-pos)
                  (set! hints-pos (+ hints-pos new-cmd-sz))))
              (set! link-edit-pos (+ link-edit-pos new-cmd-sz))
              (when move-link-edit?
                ;; Update link-edit segment entry:
                (file-position out (+ link-edit-pos (if link-edit-64? 40 32)))
                ;; (printf "Update to ~a\n" (+ out-offset outlen))
                ((if link-edit-64? write-xulong write-ulong) (+ out-offset outlen) out)
                ;; Read link-edit segment:
                (file-position p link-edit-offset)
                (let ([link-edit (read-bytes link-edit-len p)])
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
                    (update 4))))
              ;; Write segdata to former link-data offset:
              (file-position out out-offset)
              (display segdata out)
              (display (make-bytes (- outlen (bytes-length segdata)) 0) out)
              ;; Result is offset where data was written:
              out-offset)))
        (lambda ()
          (close-input-port p)
          (close-output-port out)))))

(define (fix-offset p pos out d base delta)
  (when (and out (not (zero? delta)))
    (file-position p (+ pos d))
    (let ([offset (read-ulong p)])
      (when (offset . > . base)
        (file-position out (+ pos d))
        (write-ulong (+ offset delta) out)
        (flush-output out)))))

(define (get/set-dylib-path file rx new-path)
  (let-values ([(p out) (if new-path
                            (open-input-output-file file #:exists 'update)
                            (values (open-input-file file)
                                    #f))])
    (dynamic-wind
        void
        (lambda ()
          (check-same exe-id (read-ulong p))
          (read-ulong p)
          (read-ulong p)
          (check-same #x2 (read-ulong p))
          (let* ([cnt (read-ulong p)]
                 [cmdssz (read-ulong p)])
            (read-ulong p)
            (when (equal? exe-id #xFeedFacf)
              (read-ulong p))
            (let loop ([cnt cnt][base 0][delta 0][result #f])
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
                                                       (mult-of-4 (+ 1 (bytes-length new-path))))]
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
                                 (loop (sub1 cnt) pos delta segname))
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
