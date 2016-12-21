#lang racket/base
(require file/gzip racket/file)

(define tar-block-size 512)
(define tar-name-length 100)
(define tar-prefix-length 155)
(define tar-link-name-length 100)

(define 0-block (make-bytes tar-block-size 0)) ; used for fast block zeroing

(define (new-block) (bytes-copy 0-block))
(define (zero-block! buf) (bytes-copy! buf 0 0-block))

(define path->name-bytes
  (case (system-type)
    [(windows) (lambda (p) (regexp-replace* #rx"\\\\" (path->bytes p) "/"))]
    [else path->bytes]))

(define sep-char (char->integer #\/))

(define (split-tar-name path)
  (let* ([bts (path->name-bytes path)]
         [len (bytes-length bts)])
    (if (< len tar-name-length)
        (values bts #f #t)
        (let loop ([n 1]) ; search for a split point
          (cond [(<= (sub1 len) n)
                 ;; Doesn't fit, so we'll use an extension record:
                 (values (subbytes bts 0 (sub1 tar-name-length)) #f #f)]
                [(and (eq? sep-char (bytes-ref bts n))
                      (< n tar-prefix-length)
                      (< (- len (+ n 1)) tar-name-length))
                 ;; Fits after splitting:
                 (values (subbytes bts (add1 n)) (subbytes bts 0 n) #t)]
                [else (loop (add1 n))])))))

;; see also the same function name in "zip.rkt"
(define (path-attributes path dir?)
  (apply bitwise-ior
         (map (lambda (p)
                (case p
                  [(read)    #o444]
                  [(write)   #o200] ; mask out write bits
                  [(execute) #o111]))
              (file-or-directory-permissions path))))

(define 0-byte (char->integer #\0))

(define ((tar-one-entry buf prefix get-timestamp follow-links? format) path)
  (let* ([link?   (and (not follow-links?) (link-exists? path))]
         [dir?    (and (not link?) (directory-exists? path))]
         [size    (if (or dir? link?) 0 (file-size path))]
         [p       0] ; write pointer
         [cksum   0]
         [cksum-p #f])
    (define full-file-name (if prefix
                               (build-path prefix path)
                               path))
    (define-values (file-name file-prefix file-name-fits?)
      (split-tar-name full-file-name))
    (define-syntax advance (syntax-rules () [(_ l) (set! p (+ p l))]))
    (define (write-block* len bts) ; no padding required
      (when bts
        (define wlen (min (bytes-length bts) len))
        (bytes-copy! buf p bts 0 wlen)
        (for ([i (in-range wlen)])
          (set! cksum (+ cksum (bytes-ref bts i)))))
      (advance len))
    (define (write-block len bts) ; len includes one nul padding
      (when (and bts (<= len (bytes-length bts)))
        (error 'tar "entry too long, should fit in ~a bytes: ~e"
               (sub1 len) bts))
      (write-block* len bts))
    (define (write-octal len int) ; int should take all space -1 nul-padding
      (let loop ([q (+ p len -2)] [n int])
        (if (< q p)
          (when (< 0 n)
            (error 'tar "integer too big, should fit in ~a bytes: ~e"
                   int (sub1 len)))
          (let ([d (+ 0-byte (modulo n 8))])
            (bytes-set! buf q d)
            (set! cksum (+ cksum d))
            (loop (sub1 q) (quotient n 8)))))
      (advance len))
    (define (write-bytes/fill-block bstr) ; assumes that buf is zero'd
      (write-bytes bstr)
      (define len (bytes-length bstr))
      (unless (zero? (modulo len tar-block-size))
        (write-bytes buf (current-output-port) (modulo len tar-block-size))))
    (define attrib-path
      (if link?
          ;; For a link, use attributes of the containing directory:
          (let-values ([(base name dir?) (split-path path)])
            (or (and (path? base)
                     base)
                (current-directory)))
          path))
    (define link-path-bytes (and link?
                                 (path->bytes (resolve-path path))))
    ;; see http://www.mkssoftware.com/docs/man4/tar.4.asp for format spec
    (define (write-a-block file-name-bytes file-prefix size type link-path-bytes)
      (set! p 0)
      (set! cksum 0)
      (set! cksum-p #f)
      (write-block tar-name-length file-name-bytes)
      (write-octal   8 (path-attributes attrib-path dir?))
      (write-octal   8 0)          ; always root (uid)
      (write-octal   8 0)          ; always root (gid)
      (write-octal  12 size)
      (write-octal  12 (get-timestamp attrib-path))
      ;; set checksum later, consider it "all blanks" for cksum
      (set! cksum-p p) (set! cksum (+ cksum (* 8 32))) (advance 8)
      (write-block*  1 type) ; type-flag
      (if link-path-bytes
          (let ([len (min (sub1 tar-link-name-length) (bytes-length link-path-bytes))])
            (write-block* len link-path-bytes)   ; link-name (possibly truncated)
            (advance (- tar-link-name-length len)))
          (advance tar-link-name-length))        ; no link-name
      (write-block   6 #"ustar")   ; magic
      (write-block*  2 #"00")      ; version
      (write-block  32 #"root")    ; always root (user-name)
      (write-block  32 #"root")    ; always root (group-name)
      (write-octal   8 0)          ; device-major
      (write-octal   8 0)          ; device-minor
      (write-block tar-prefix-length file-prefix)
      (set! p cksum-p)
      (write-octal   8 cksum)      ; patch checksum
      (write-bytes buf))
    ;; If the file name is too long, then we need to write an
    ;; extension block, first (GNU format):
    (when (and (not file-name-fits?)
               (eq? format 'gnu))
      (define full-file-bytes (path->bytes full-file-name))
      (write-a-block #"././@LongLink" #"" (bytes-length full-file-bytes) #"L" #f)
      (zero-block! buf)
      (write-bytes/fill-block full-file-bytes))
    ;; Ditto for a long link target (GNU format):
    (define long-target-link? (and link-path-bytes
                                   ((bytes-length link-path-bytes) . >= . tar-link-name-length)))
    (when (and long-target-link?
               (eq? format 'gnu))
      (write-a-block #"././@LongLink" #"" (bytes-length link-path-bytes) #"K" #f)
      (zero-block! buf)
      (write-bytes/fill-block link-path-bytes))
    ;; Or this way (pax format):
    (when (and (or (not file-name-fits?)
                   long-target-link?)
               (eq? format 'pax))
      (define full-file-bytes (path->bytes full-file-name))
      (define pax-bytes
        (let* ([ht #hash()]
               [ht (if (not file-name-fits?) (hash-set ht 'path full-file-bytes) ht)]
               [ht (if long-target-link? (hash-set ht 'linkpath link-path-bytes) ht)])
          (pax->bytes ht)))
      (define pax-header-path-bytes (let ([bstr (bytes-append #"PaxHeader/" full-file-bytes)])
                                      (if ((bytes-length bstr) . >= . tar-name-length)
                                          (subbytes bstr 0 (sub1 tar-name-length))
                                          bstr)))
      (write-a-block pax-header-path-bytes #"" (bytes-length pax-bytes) #"x" #f)
      (zero-block! buf)
      (write-bytes/fill-block pax-bytes))
    ;; If plain 'ustar, report an error for long paths
    (when (eq? format 'ustar)
      (when (not file-name-fits?)
        (error 'tar "path too long for ustar, must fit in ~a bytes: ~e"
               (sub1 tar-name-length)
               full-file-name))
      (when long-target-link?
        (error 'tar "symbolic-link target too long for ustar, must fit in ~a bytes: ~e"
               (sub1 tar-link-name-length)
               link-path-bytes)))
    ;; Write the data block
    (write-a-block file-name file-prefix size (if link? #"2" (if dir? #"5" #"0")) link-path-bytes)
    ;; Write the file data (if any)
    (if (or dir? link?)
      (zero-block! buf) ; must clean buffer for re-use
      ;; write the file
      (with-input-from-file path
        (lambda ()
          (let loop ([n size])
            (let ([l (read-bytes! buf)])
              (cond
                [(eq? l tar-block-size) (write-bytes buf) (loop (- n l))]
                [(number? l) ; shouldn't happen
                 (write-bytes buf (current-output-port) 0 l) (loop (- n l))]
                [(not (eq? eof l)) (error 'tar "internal error")]
                [(not (zero? n))
                 (error 'tar "file changed while packing: ~e" path)]
                [else (zero-block! buf) ; must clean buffer for re-use
                      (let ([l (modulo size tar-block-size)])
                        (unless (zero? l)
                          ;; complete block (buf is now zeroed)
                          (write-bytes buf (current-output-port)
                                       0 (- tar-block-size l))))]))))))))

;; tar-write : (listof relative-path) ->
;; writes a tar file to current-output-port
(provide tar->output)
(define (tar->output files [out (current-output-port)]
                     #:get-timestamp [get-timestamp file-or-directory-modify-seconds]
                     #:path-prefix [prefix #f]
                     #:follow-links? [follow-links? #f]
                     #:format [format 'pax])
  (check-format 'tar->output format)
  (parameterize ([current-output-port out])
    (let* ([buf (new-block)] [entry (tar-one-entry buf prefix get-timestamp follow-links? format)])
      (for-each entry files)
      ;; two null blocks end-marker
      (write-bytes buf) (write-bytes buf))))

;; tar : output-file paths ->
(provide tar)
(define (tar tar-file
             #:exists-ok? [exists-ok? #f]
             #:path-prefix [prefix #f]
             #:path-filter [path-filter #f]
             #:follow-links? [follow-links? #f]
             #:get-timestamp [get-timestamp file-or-directory-modify-seconds]
             #:format [format 'pax]
             . paths)
  (check-format 'tar format)
  (when (null? paths) (error 'tar "no paths specified"))
  (with-output-to-file tar-file
    #:exists (if exists-ok? 'truncate/replace 'error)
    (lambda () (tar->output (pathlist-closure paths
                                         #:follow-links? follow-links?
                                         #:path-filter path-filter)
                       #:get-timestamp get-timestamp
                       #:path-prefix prefix
                       #:follow-links? follow-links?
                       #:format format))))

;; tar-gzip : output-file paths ->
(provide tar-gzip)
(define (tar-gzip tgz-file
                  #:exists-ok? [exists-ok? #f]
                  #:path-prefix [prefix #f]
                  #:path-filter [path-filter #f]
                  #:follow-links? [follow-links? #f]
                  #:get-timestamp [get-timestamp file-or-directory-modify-seconds]
                  #:format [format 'pax]
                  . paths)
  (check-format 'tar-gzip format)
  (when (null? paths) (error 'tar-gzip "no paths specified"))
  (with-output-to-file tgz-file
    #:exists (if exists-ok? 'truncate/replace 'error)
    (lambda ()
      (let-values ([(i o) (make-pipe (* 1024 1024 32))])
        (define tar-exn #f)
        (thread (lambda ()
                  (with-handlers [((lambda (exn) #t) (lambda (exn) (set! tar-exn exn)))]
                    (tar->output (pathlist-closure paths
                                                   #:follow-links? follow-links?
                                                   #:path-filter path-filter)
                                 o
                                 #:path-prefix prefix
                                 #:follow-links? follow-links?
                                 #:get-timestamp get-timestamp
                                 #:format format))
                  (close-output-port o)))
        (gzip-through-ports
         i (current-output-port)
         (cond [(regexp-match #rx"^(.*[.])(?:tar[.]gz|tgz)$"
                              (if (path? tgz-file)
                                (path->string tgz-file) tgz-file))
                => (lambda (m) (string-append (car m) "tar"))]
               [else #f])
         (current-seconds))
        (when tar-exn (raise tar-exn))))))

;; ----------------------------------------

(define (check-format who format)
  (case format
    [(ustar gnu pax) (void)]
    [else (raise-argument-error who "(or/c 'pax 'gnu 'ustar)" format)]))

(define (pax->bytes ht)
  (define (number->bytes n) (string->bytes/utf-8 (format "~a" n)))
  (apply
   bytes-append
   (for/list ([(k v) (in-hash ht)])
     (define k-bytes (string->bytes/utf-8 (symbol->string k)))
     (define len
       ;; Weird: we have to figure out the line length *including* the bytes
       ;; that describe the line length.
       (let loop ([guess-len (+ 1 (bytes-length k-bytes) 1 (bytes-length v) 1)])
         (define n-bytes (number->bytes guess-len))
         (define encoding-size (+ (bytes-length n-bytes) 1 (bytes-length k-bytes) 1 (bytes-length v) 1))
         (cond
          [(= guess-len encoding-size)
           guess-len]
          [(guess-len . < . encoding-size)
           (loop (add1 guess-len))]
          [else
           (error 'pax->bytes "internal error: cannot figure out encoding line length!")])))
     (bytes-append (number->bytes len)
                   #" "
                   k-bytes
                   #"="
                   v
                   #"\n"))))
