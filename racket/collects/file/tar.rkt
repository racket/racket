#lang racket/base
(require file/gzip racket/file)

(define tar-block-size 512)
(define tar-name-length 100)
(define tar-prefix-length 155)

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
      (values bts #f)
      (let loop ([n 1]) ; search for a split point
        (cond [(<= (sub1 len) n)
               (error 'tar "path too long for USTAR: ~a" path)]
              [(and (eq? sep-char (bytes-ref bts n))
                    (< n tar-prefix-length)
                    (< (- len (+ n 1)) tar-name-length))
               (values (subbytes bts (add1 n)) (subbytes bts 0 n))]
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

(define ((tar-one-entry buf prefix get-timestamp) path)
  (let* ([link?   (link-exists? path)]
         [dir?    (and (not link?) (directory-exists? path))]
         [size    (if (or dir? link?) 0 (file-size path))]
         [p       0] ; write pointer
         [cksum   0]
         [cksum-p #f])
    (define-values (file-name file-prefix) (split-tar-name (if prefix
                                                               (build-path prefix path)
                                                               path)))
    (define-syntax advance (syntax-rules () [(_ l) (set! p (+ p l))]))
    (define (write-block* len bts) ; no padding required
      (when bts
        (bytes-copy! buf p bts)
        (for ([i (in-range (bytes-length bts))])
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
    ;; see http://www.mkssoftware.com/docs/man4/tar.4.asp for format spec
    (write-block tar-name-length file-name)
    (write-octal   8 (path-attributes path dir?))
    (write-octal   8 0)          ; always root (uid)
    (write-octal   8 0)          ; always root (gid)
    (write-octal  12 size)
    (write-octal  12 (get-timestamp path))
    ;; set checksum later, consider it "all blanks" for cksum
    (set! cksum-p p) (set! cksum (+ cksum (* 8 32))) (advance 8)
    (write-block*  1 (if link? #"2" (if dir? #"5" #"0"))) ; type-flag: dir/file (no symlinks)
    (if link?
        (let* ([p (path->bytes (resolve-path path))]
               [len (bytes-length p)])
          (if (len . < . 100)
              (begin
                (write-block* len p)
                (advance (- 100 len)))
              (error 'tar "soft-link target too long")))
        (advance     100))       ; no link-name
    (write-block   6 #"ustar")   ; magic
    (write-block*  2 #"00")      ; version
    (write-block  32 #"root")    ; always root (user-name)
    (write-block  32 #"root")    ; always root (group-name)
    (write-octal   8 0)          ; device-major
    (write-octal   8 0)          ; device-minor
    (write-block tar-prefix-length file-prefix)
    (set! p cksum-p)
    (write-octal   8 cksum)      ; patch checksum
    (write-bytes buf)
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
                     #:path-prefix [prefix #f])
  (parameterize ([current-output-port out])
    (let* ([buf (new-block)] [entry (tar-one-entry buf prefix get-timestamp)])
      (for-each entry files)
      ;; two null blocks end-marker
      (write-bytes buf) (write-bytes buf))))

;; tar : output-file paths ->
(provide tar)
(define (tar tar-file
             #:path-prefix [prefix #f]
             #:get-timestamp [get-timestamp file-or-directory-modify-seconds]
             . paths)
  (when (null? paths) (error 'tar "no paths specified"))
  (with-output-to-file tar-file
    (lambda () (tar->output (pathlist-closure paths #:follow-links? #f)
                            #:get-timestamp get-timestamp
                            #:path-prefix prefix))))

;; tar-gzip : output-file paths ->
(provide tar-gzip)
(define (tar-gzip tgz-file
                  #:path-prefix [prefix #f]
                  #:get-timestamp [get-timestamp file-or-directory-modify-seconds]
                  . paths)
  (when (null? paths) (error 'tar-gzip "no paths specified"))
  (with-output-to-file tgz-file
    (lambda ()
      (let-values ([(i o) (make-pipe (* 1024 1024 32))])
        (thread (lambda ()
                  (tar->output (pathlist-closure paths #:follow-links? #f) o
                               #:path-prefix prefix
                               #:get-timestamp get-timestamp)
                  (close-output-port o)))
        (gzip-through-ports
         i (current-output-port)
         (cond [(regexp-match #rx"^(.*[.])(?:tar[.]gz|tgz)$"
                              (if (path? tgz-file)
                                (path->string tgz-file) tgz-file))
                => (lambda (m) (string-append (car m) "tar"))]
               [else #f])
         (current-seconds))))))
