#lang racket/base
(require racket/contract
         racket/pretty)

(provide read-pe+resources
         update-resources

         resource-ref
         resource-ref/path
         resource-set
         resource-remove)

(define (byte->integer p)
  (read-byte p))
(define (word->integer p)
  (integer-bytes->integer (read-bytes 2 p) #f #f))
(define (dword->integer p)
  (integer-bytes->integer (read-bytes 4 p) #f #f))
(define (xword->integer p)
  (integer-bytes->integer (read-bytes 8 p) #f #f))

(define (integer->word i p)
  (display (integer->integer-bytes i 2 #f #f) p))
(define (integer->dword i p)
  (display (integer->integer-bytes i 4 #f #f) p))
(define (integer->3/2word i p)
  (display (subbytes (integer->integer-bytes i 4 #f #f) 0 3) p))

(define (flag v)
  (positive? (bitwise-and #x80000000 v)))
(define (value v)
  (bitwise-and #x7FFFFFFF v))
(define (add-flag v)
  (bitwise-ior #x80000000 v))

(define-logger pe-rsrc)

(define (skip-to-image-headers-after-signature p)
  ;; p is expected to be a file port
  (define dos-sig (word->integer p))
  (unless (= #x5A4D dos-sig)
    (error 'pe-rsrc "bad DOS signature ~x" dos-sig))
  (file-position p 60)
  (let ([pos (dword->integer p)])
    ;; pos points to IMAGE_NT_HEADERS
    ;; (log-error "at ~s" pos)
    (file-position p pos)
    (define sig (dword->integer p))
    (unless (= #x4550 sig) ; = #"PE\0\0"
      (error 'pe-rsrc "bad PE signature ~x" sig))
    pos))

(struct pe (sections section-alignment file-alignment
                     image-size-pos section-start-pos rsrc-offset rsrc-virtual-addr rsrc-size))
(struct section (name virtual-size virtual-addr
                      file-length file-position
                      characteristics)
        #:prefab)

(define (read-pe p)
  (let ([pos (skip-to-image-headers-after-signature p)])
    (word->integer p) ; skip machine
    (let ([num-sections (word->integer p)]
          [_ (begin (dword->integer p) ; date time stamp
                    (dword->integer p) ; symbol table - 0 for modern exes
                    (dword->integer p))] ; symbol count - 0 for modern exes
          [size (word->integer p)]) ; size of optional headers
      (file-position p (+ pos 4 20))
      (define image-type
        (case (word->integer p)
          [(#x10B) 'pe32]
          [(#x20B) 'pe32+]
          [else (error "unrecognized image type")]))
      (file-position p (+ pos 4 20 32))
      (define section-alignment (dword->integer p))
      (define file-alignment (dword->integer p))
      (log-pe-rsrc-debug "alignment ~x ~x" section-alignment file-alignment)
      (define image-size-pos (+ pos 4 20 56))
      (file-position p image-size-pos)
      (log-pe-rsrc-debug "image size ~x" (dword->integer p))
      ;;(file-position p (+ pos 4 20 64))
      ;;(log-error "checksum ~x at ~a" (dword->integer p) (+ pos 4 20 64))
      ;;(file-position p (+ pos 4 20 (if (eq? image-type 'pe32) 92 108)))
      ;;(log-error "extra entries ~a in ~a" (dword->integer p) size)
      (define rsrc-offset (+ pos 4 20 (if (eq? image-type 'pe32) 112 128)))
      (file-position p rsrc-offset)
      (define rsrc-virtual-addr (dword->integer p))
      (define rsrc-size (dword->integer p))
      (define section-start-pos (+ pos
                                   4       ; Signature : DWORD
                                   20      ; FileHeader: IMAGE_FILE_HEADER
                                   size))  ; "optional" header
      (define (z v) (unless (zero? v) (error "expected zero")))
      (pe (let sloop ([i 0] [section-pos section-start-pos])
            (if (= i num-sections)
                null
                (begin
                  (file-position p section-pos)
                  ;; p points to an IMAGE_SECTION_HEADER
                  (cons (section (read-bytes 8 p)   ; name
                                 (dword->integer p) ; virtual size
                                 (dword->integer p) ; virtual address
                                 (dword->integer p) ; length
                                 (dword->integer p) ; file pos
                                 (begin
                                   (z (dword->integer p)) ; relocations (zero)
                                   (z (dword->integer p)) ; line numbers (zero)
                                   (z (word->integer p))  ; num relocations (zero)
                                   (z (word->integer p))  ; num line numbers (zero)
                                   (dword->integer p))) ; characteristics
                        (sloop (add1 i) (+ section-pos 40))))))
          section-alignment
          file-alignment
          image-size-pos
          section-start-pos
          rsrc-offset
          rsrc-virtual-addr
          rsrc-size))))

(define (show-sections sections)
  (for/fold ([prev-end 0] [prev-full-end 0]) ([s (in-list sections)])
    (log-pe-rsrc-debug "~s ~x [+~x/+~x] @ ~x  ~x [-> ~x] $~x"
                       (section-name s)
                       (section-virtual-addr s)
                       (- (section-virtual-addr s) prev-end)
                       (- (section-virtual-addr s) prev-full-end)
                       (section-file-position s)
                       (section-file-length s)
                       (section-virtual-size s)
                       (section-characteristics s))
    (values (+ (section-virtual-addr s)
               (section-virtual-size s))
            (+ (section-virtual-addr s)
               (section-file-length s))))
  (void))

(define (show-resources rsrcs [indent ""])
  (cond
   [(directory? rsrcs)
    (log-pe-rsrc-debug "~a~s" indent (entry-name rsrcs))
    (for ([e (in-list (directory-content rsrcs))])
      (show-resources e (string-append indent "  ")))]
   [else
    (log-pe-rsrc-debug "~a~s ~x ~s at ~x" indent (entry-name rsrcs)
                       (bytes-length (resource-content rsrcs))
                       (resource-codepage rsrcs)
                       (resource-file-pos rsrcs))]))

(struct entry (name) #:prefab)
(struct directory entry (timestamp major-version minor-version content) #:prefab)
(struct resource entry (content codepage file-pos) #:prefab)

(define (read-rsrcs p rsrc-pos rsrc-virtual-addr)
  (let loop ([dir-name #f] [dir-pos 0] [depth 0])
    (file-position p (+ rsrc-pos dir-pos 4))
    (let ([timestamp (dword->integer p)]
          [major-version (word->integer p)]
          [minor-version (word->integer p)]
          [num-named (word->integer p)]
          [num-ided (word->integer p)])
      ;;(log-error "dir at ~x[~a]: ~a+~a" dir-pos depth num-named num-ided)
      (directory
       dir-name
       timestamp
       major-version
       minor-version
       (let iloop ([i 0])
         (if (= i (+ num-ided num-named))
             null
             (let ([name-delta (dword->integer p)]
                   [data-delta (dword->integer p)]
                   [next (file-position p)])
               (cons
                (let ([name (if (i . < . num-named)
                                (begin
                                  ;;(log-error "name at ~x = ~x" (value name-delta) (+ rsrc-pos (value name-delta)))
                                  (file-position p (+ rsrc-pos (value name-delta)))
                                  (let* ([len (word->integer p)])
                                    ;; len is in unicode chars...
                                    (let ([unistr (read-bytes (* 2 len) p)])
                                      ;; Assume it fits into ASCII...
                                      (regexp-replace* "\0" 
                                                       (bytes->string/latin-1 unistr)
                                                       ""))))
                                name-delta)])
                  (if (flag data-delta)
                      ;; Directory:
                      (loop name (value data-delta) (add1 depth))
                      ;; Entry:
                      (begin
                        (file-position p (+ rsrc-pos (value data-delta)))
                        (let ([rva (dword->integer p)]
                              [size (dword->integer p)]
                              [codepage (dword->integer p)])
                          ;;(log-error "resource at ~x ~x" (value data-delta) size)
                          (define file-pos (+ rva
                                              (- rsrc-pos
                                                 rsrc-virtual-addr)))
                          (file-position p file-pos)
                          (resource name
                                    (read-bytes size p)
                                    codepage
                                    file-pos)))))
                (begin
                  (file-position p next)
                  (iloop (add1 i)))))))))))

(define (write-rsrcs rsrcs p virtual-addr)
  (define (sorted-directory-content e)
    (sort (directory-content e)
          #:key entry-name
          (lambda (a b)
            (cond
             [(string? a)
              (if (string? a)
                  (string<? a b)
                  #f)]
             [(string? b) #f]
             [else
              (< a b)]))))
  ;; Compute offsets ----------------------------------------
  (define (directory-size e)
    (+ 16 (* 8 (length (directory-content e)))))
  (define-values (dir-end-pos rev-entries dir-pos-ht)
    (let loop ([rsrcs rsrcs]
               [pos (directory-size rsrcs)]
               [rev-entries (list rsrcs)]
               [dir-pos-ht (hasheq)])
      (cond
       [(resource? rsrcs) (values pos (cons rsrcs rev-entries) dir-pos-ht)]
       [else
        (define content (sorted-directory-content rsrcs))
        (define-values (new-pos new-rev-entries new-dir-pos-ht)
          (let dloop ([content content]
                      [pos pos]
                      [rev-entries rev-entries]
                      [dir-pos-ht dir-pos-ht])
            (cond
             [(null? content) (values pos rev-entries dir-pos-ht)]
             [else
              (define e (car content))
              (cond
               [(directory? e)
                (dloop (cdr content)
                       (+ pos (directory-size e))
                       (cons e rev-entries)
                       (hash-set dir-pos-ht e pos))]
               [else
                (dloop (cdr content) pos rev-entries dir-pos-ht)])])))
        (let dloop ([content content]
                    [pos new-pos]
                    [rev-entries new-rev-entries]
                    [dir-pos-ht new-dir-pos-ht])
          (cond
           [(null? content) (values pos rev-entries dir-pos-ht)]
           [else
            (define-values (new-pos new-rev-entries new-dir-pos-ht)
              (loop (car content) pos rev-entries dir-pos-ht))
            (dloop (cdr content) new-pos new-rev-entries new-dir-pos-ht)]))])))
  (define entries (reverse rev-entries))
  (define align-strings? #f)
  (define (str-size s)
    (let ([n (+ 2 (* 2 (string-length s)))])
      (if align-strings?
          (if (zero? (bitwise-and n 3))
              n
              (+ n (- 4 (bitwise-and n 3))))
          n)))
  (define str-length
    (for/sum ([e (in-list entries)])
      (if (string? (entry-name e))
          (str-size (entry-name e))
          0)))
  (define aligned-str-length
    (same-alignment 4 str-length))
  (define res-start dir-end-pos)
  (define res-length (* 16 (for/sum ([e (in-list entries)])
                             (if (resource? e) 1 0))))
  (define str-start (+ res-start res-length))
  (define data-start (+ str-start aligned-str-length))
  (define (align-data-size s)
    (same-alignment 2 s))
  
  ;; Write out directory ----------------------------------------
  (for/fold ([str-pos str-start] [res-pos res-start]) ([e (in-list entries)])
    (cond
     [(resource? e) (values str-pos res-pos)]
     [(directory? e)
      (define content (sorted-directory-content e))
      (integer->dword 0 p)
      (integer->dword (directory-timestamp e) p)
      (integer->word (directory-major-version e) p)
      (integer->word (directory-minor-version e) p)
      (define num-strs (let loop ([cs content])
                         (cond
                          [(null? cs) 0]
                          [(number? (entry-name (car cs))) 0]
                          [else (+ 1 (loop (cdr cs)))])))
      (integer->word num-strs p)
      (integer->word (- (length content) num-strs) p)
      (for/fold ([str-pos str-pos] [res-pos res-pos]) ([e (in-list content)])
        (values (cond
                 [(string? (entry-name e))
                  (integer->dword (add-flag str-pos) p)
                  (+ str-pos (str-size (entry-name e)))]
                 [else
                  (integer->dword (entry-name e) p)
                  str-pos])
                (cond
                 [(resource? e)
                  (integer->dword res-pos p)
                  (+ res-pos 16)]
                 [else
                  (integer->dword (add-flag (hash-ref dir-pos-ht e)) p)
                  res-pos])))]))
  
  ;; Write out resource data entries
  (for/fold ([data-pos data-start]) ([e (in-list entries)])
    (cond
     [(resource? e)
      (define len (align-data-size (bytes-length (resource-content e))))
      (integer->dword (+ data-pos virtual-addr) p)
      (integer->dword len p)
      (integer->dword (resource-codepage e) p)
      (integer->dword 0 p)
      (+ data-pos len)]
     [else
      data-pos]))
  
  ;; Write out strings
  (for ([e (in-list entries)])
    (define n (entry-name e))
    (when (string? n)
      (integer->word (string-length n) p)
      (for ([c (in-string n)])
        (integer->word (char->integer c) p))
      (when align-strings?
        (when (even? (string-length n))
          (integer->word 0 p)))))
  (write-bytes (make-bytes (- aligned-str-length str-length)) p)
  
  ;; Write out resource content
  (for ([e (in-list entries)])
    (when (resource? e)
      (define bstr (resource-content e))
      (write-bytes bstr p)
      (define aligned-size (align-data-size (bytes-length bstr)))
      (write-bytes (make-bytes (- aligned-size (bytes-length bstr))) p)))
  
  (void))


(define (find-section sections find-name)
  (let loop ([sections sections])
    (cond
     [(null? sections)
      (error 'find-section "can't find section: ~e" find-name)]
     [else
      (define s (car sections))
      (if (bytes=? find-name (section-name s))
          s
          (loop (cdr sections)))])))

(define rsrc-section-name #".rsrc\0\0\0")

(define (read-pe+resources i)
  (define pe (read-pe i))
  (define s (find-section (pe-sections pe) rsrc-section-name))
  
  (log-pe-rsrc-debug "sections at ~x" (pe-section-start-pos pe))
  (show-sections (pe-sections pe))
  (log-pe-rsrc-debug "rsrc at ~x ~x" (pe-rsrc-virtual-addr pe) (pe-rsrc-size pe))
  
  (unless (and (= (section-virtual-addr s) (pe-rsrc-virtual-addr pe))
               (>= (section-virtual-size s) (pe-rsrc-size pe)))
    (error 'pe-rsrc
           "sections and resource information do not line up in the typical way"))
  
  (define rsrcs (read-rsrcs i (section-file-position s) (section-virtual-addr s)))
  
  (show-resources rsrcs)
  
  (values pe rsrcs))

(define (same-alignment orig new)
  (cond
   [(bitwise-bit-set? orig 1)
    new]
   [(bitwise-bit-set? new 1)
    (same-alignment orig (add1 new))]
   [else
    (arithmetic-shift (same-alignment
                       (arithmetic-shift orig -1)
                       (arithmetic-shift new -1))
                      1)]))

(define (update-sections pe new-sections o)
  (file-position o (pe-section-start-pos pe))
  (for ([s (in-list new-sections)])
    (write-bytes (section-name s) o)
    (integer->dword (section-virtual-size s) o)
    (integer->dword (section-virtual-addr s) o)
    (integer->dword (section-file-length s) o)
    (integer->dword (section-file-position s) o)
    (integer->dword 0 o)
    (integer->dword 0 o)
    (integer->word 0 o)
    (integer->word 0 o)
    (integer->dword (section-characteristics s) o)))

(define (update-resources src pe rsrcs)
  (define o (open-output-bytes))
  (write-rsrcs rsrcs o (pe-rsrc-virtual-addr pe))
  (define bstr (get-output-bytes o))
  (define len (bytes-length bstr))

  (define s (find-section (pe-sections pe) rsrc-section-name))
  (cond
   [(len . <= . (section-file-length s))
    (log-pe-rsrc-debug "new content fits in place of old content")
    (call-with-output-file
     src
     #:exists 'update
     (lambda (o)
       (file-position o (section-file-position s))
       (write-bytes bstr o)
       (write-bytes (make-bytes (- (section-file-length s) len)) o)
       
       (file-position o (pe-rsrc-offset pe))
       (integer->dword (pe-rsrc-virtual-addr pe) o)
       (integer->dword len o)))]
   [else
    (log-pe-rsrc-debug "moving resources to end")
    (define new-virtual-addr
      (same-alignment
       (section-virtual-addr s)
       (for/fold ([pos 0]) ([s2 (in-list (pe-sections pe))]
                            #:unless (eq? s s2))
         (max pos
              (+ (section-virtual-addr s2)
                 (section-virtual-size s2))))))

    (define o (open-output-bytes))
    (write-rsrcs rsrcs o new-virtual-addr)
    (define bstr (get-output-bytes o))
    (define len (bytes-length bstr))
    
    (define new-virtual-size len)
    (define new-file-size (same-alignment (pe-section-alignment pe) len))
    (define new-position
      (let ([fs (file-size src)])
        (cond
         [(= fs (+ (section-file-position s)
                   (section-file-length s)))
          ;; Section was already at end, so overwrite is ok
          (section-file-position s)]
         [else
          (same-alignment (pe-file-alignment pe) fs)])))
    (log-pe-rsrc-debug "moving to ~x ~x at ~x"
                       new-virtual-addr
                       new-virtual-size
                       new-position)
    
    (define new-sections
      (sort (for/list ([s2 (in-list (pe-sections pe))])
              (if (eq? s s2)
                  (section (section-name s)
                           new-virtual-size new-virtual-addr
                           new-file-size new-position
                           (section-characteristics s))
                  s2))
            <
            #:key section-virtual-addr))

    (call-with-output-file
     src
     #:exists 'update
     (lambda (o)
       (define exe-size (+ new-position new-file-size))
       
       (update-sections pe new-sections o)
       
       (file-position o new-position)
       (write-bytes bstr o)
       (write-bytes (make-bytes (- new-file-size len)) o)
    
       (file-position o (pe-rsrc-offset pe))
       (integer->dword new-virtual-addr o)
       (integer->dword new-virtual-size o)
       
       (file-position o (pe-image-size-pos pe))
       (integer->dword (+ new-virtual-addr new-file-size) o)
       
       (file-truncate o exe-size)))]))

;; ----------------------------------------

(define (get-match dir n)
  (and dir
       (for/or ([e (in-list (directory-content dir))])
         (and (or (not n)
                  (equal? n (entry-name e)))
              e))))

(define (set-match dir v
                   #:name [name (entry-name v)]
                   #:can-remove? [can-remove? #f])
  (define new-content
    (let loop ([c (directory-content dir)])
      (cond
       [(null? c) (if v
                      (list v)
                      null)]
       [(or (not name)
            (equal? (entry-name (car c)) name))
        (if v
            (cons v (cdr c))
            (cdr c))]
       [else (cons (car c) (loop (cdr c)))])))
  (if (and can-remove? (null? new-content))
      #f
      (struct-copy directory dir
                   [content new-content])))

(define (resource-ref/path rsrcs type name language)
  (define t (get-match rsrcs type))
  (define n (get-match t name))
  (define r (get-match n language))
  (values (and t (entry-name t))
          (and n (entry-name n))
          (and r (entry-name r))
          (and r (resource-content r))
          (and r (resource-file-pos r))))

(define (resource-ref rsrcs type name language)
  (define-values (t n l v p) (resource-ref/path rsrcs type name language))
  v)

(define (resource-set rsrcs type name language v)
  (define (mk name what)
    (directory (or name
                   (error 'resource-set
                          "cannot infer ~a"
                          what))
               0 ; timestamp
               0 ; major-version
               0 ; minor-version
               null))
  (define t (or (get-match rsrcs type) (mk type 'type)))
  (define n (or (get-match t name) (mk name 'name)))
  (define l (get-match n language))
  (set-match rsrcs #:name type
             (set-match t
                        #:name name
                        (set-match n
                                   #:name language
                                   (resource (or language
                                                 (and l (entry-name l))
                                                 (error 'resource-set
                                                        "cannot infer language"))
                                             v
                                             1252
                                             0)))))

(define (resource-remove rsrcs type name language)
  (define t (get-match rsrcs type))
  (define n (get-match t name))
  (if (get-match n language)
      (set-match rsrcs #:name type
                 (set-match t
                            #:name name
                            #:can-remove? #t
                            (set-match n
                                       #:name language
                                       #:can-remove? #t
                                       #f)))
      rsrcs))

#;
(module+ test
  (define-syntax-rule (check a b ...)
    (let ([got (call-with-values (lambda () a) list)]
          [expect (list b ...)])
      (unless (equal? got expect)
        (error 'test "failed: ~.s\n  got: ~e" 'a got))))
  (define (d name l)
    (directory name 0 0 0 l))
  (check (resource-ref (d #f null) #f #f #f)
         #f)
  (define hi-d (d #f (list (d 1 (list (d "hi" (list (resource 1033 #"ok" 1252 0))))))))
  (define hi2-d (d #f (list (d 1 (list (d "hi" (list (resource 1033 #"yep" 1252 0))))))))
  (check (resource-set (d #f null) 1 "hi" 1033 #"ok")
         hi-d)
  (check (resource-remove hi-d #f "no-hi" #f)
         hi-d)
  (check (resource-remove hi-d #f "hi" #f)
         (d #f null))
  (check (resource-set hi-d #f "hi" #f #"yep")
         hi2-d)
  (define bye-d (d #f (list (d 1 (list (d "hi" (list (resource 1033 #"ok" 1252 0)))
                                       (d "bye" (list (resource 1033 #"ok" 1252 0))))))))
  (check (resource-set hi-d 1 "bye" 1033 #"ok")
         bye-d)
  (check (resource-remove bye-d 1 "bye" #f)
         hi-d))

