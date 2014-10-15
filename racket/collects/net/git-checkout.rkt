#lang racket/base
(require racket/tcp
         racket/format
         racket/file
         racket/port
         racket/string
         file/gunzip
         openssl/sha1
         net/url)

;; Stefan Saasen's "Reimplementing 'git clone' in Haskell from the bottom up"
;;  http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/
;; provided many helpful hints for this implementation.

(provide git-checkout)

(define-logger git-checkout)

;; Like `git clone`, but producing just the checkout
(define (git-checkout host
                      repo
                      #:dest-dir dest-dir
                      #:transport [transport 'git]
                      #:ref [ref "master"]
                      #:depth [given-depth 1]
                      #:quiet? [quiet? #f]
                      #:tmp-dir [given-tmp-dir #f]
                      #:clean-tmp-dir? [clean-tmp-dir? (not given-tmp-dir)]
                      #:port [port (case transport
                                     [(git) 9418]
                                     [(http) 80]
                                     [(https) 443])])
  (let retry-loop ([given-depth given-depth])
    (define tmp-dir (or given-tmp-dir
                        (make-temporary-file "git~a" 'directory)))
    
    (define (status fmt . args)
      (define msg (apply format fmt args))
      (unless quiet?
        (displayln msg)
        (flush-output))
      (log-git-checkout-info msg))
    
    (status "Contacting ~a" host)
    (define-values (i o) (initial-connect transport host port repo))
    ((let/ec esc
       (dynamic-wind
        void
        (lambda ()
          (status "Getting refs")
          (write-pkt o
                     "git-upload-pack " "/" repo "\0"
                     "host=" host "\0")
          (define pkts (read-pkts i))
          (unless (pair? pkts)
            (error 'git-checkout "no initial pkts from the server"))

          ;; Parse server's initial reply
          (define server-capabilities (parse-server-capabilities (car pkts)))
          (define refs ; (list (list <name> <ID>) ...)
            (parse-initial-refs pkts))
          
          ;; Find the commits needed for `ref`:
          (define-values (ref-commit    ; #f or an ID string
                          want-commits) ; list of ID string
            (select-commits ref refs status))

          (define depth (and given-depth
                             ref-commit
                             (cond
                              [(member "shallow" server-capabilities)
                               given-depth]
                              [else
                               (status "Server does not support `shallow`")
                               #f])))

          ;; Tell the server which commits we need
          (set!-values (i o) (want-step transport host port repo i o))
          (for ([want-commit (in-list want-commits)]
                [pos (in-naturals)])
            (write-pkt o "want " want-commit (if (zero? pos) " " "") "\n"))
          (when depth
            (write-pkt o "deepen " depth "\n"))
          (write-pkt o)

          ;; Tell the server that we're ready for the objects
          (write-pkt o "done\n")
          (set!-values (i o) (done-step transport host port repo i o))

          (when depth
            ;; If we wrote `deepen`, then the server replies with `shallow`s.
            ;; Note that these were available before writing `done` in the
            ;; case of the 'git transport, but it works here for all transports.
            (let loop ()
              (define r (read-pkt i))
              (cond
               [(eof-object? r)
                (void)]
               [(regexp-match? #rx"^shallow " r)
                (loop)]
               [else
                (error 'git-checkout "expected shallow, got ~s" r)])))

          ;; Tell the server that we're ready for the objects
          (define nak (read-pkt i))
          (unless (equal? #"NAK\n" nak)
            (error 'git-checkout "expected NAK, got ~s" nak))

          (make-directory* tmp-dir)
          (define tmp (make-tmp-info tmp-dir #:fresh? #t))
          
          (dynamic-wind
           void
           (lambda ()
             ;; Read packfile pbjects, which are written into
             ;; `tmp-dir`. If `depth` gives the server trouble,
             ;; we might get an EOF, in which case we'll try again:
             (define objs (read-packfile i tmp status
                                         (and depth
                                              (lambda ()
                                                (esc (lambda ()
                                                       (status "Unexpected EOF; retrying without depth")
                                                       (retry-loop #f)))))))

             (unless clean-tmp-dir?
               (call-with-output-file*
                (build-path tmp-dir "objs")
                #:exists 'truncate
                (lambda (o) (write objs o) (newline o))))
             
             ;; Convert deltas into full objects withing `tmp`:
             (status "Applying deltas")
             (define obj-ids (rewrite-deltas objs tmp))
             
             (unless clean-tmp-dir?
               (call-with-output-file*
                (build-path tmp-dir "all-objs")
                #:exists 'truncate
                (lambda (o) (write obj-ids o) (newline o))))
             
             (define commit
               (or ref-commit
                   (find-commit-as-reference ref obj-ids)))
             
             ;; Extract the tree from the packfile objects:
             (status "Extracting tree to ~a" dest-dir)
             (extract-commit-tree (hex-string->bytes commit)
                                  obj-ids tmp dest-dir)
             
             ;; Done
             void)
           (lambda ()
             (status "Cleaning up")
             (close-tmp-info tmp)
             (when clean-tmp-dir?
               (delete-directory/files tmp-dir)))))
        (lambda ()
          (close-input-port i)
          (close-output-port o)))))))

;; ----------------------------------------
;; Transports: git, http, and https

;; initial-connect: transport-sym string natural string
;;                  -> (values input-port output-port)
;;  Contacts the server and returns an output port for writing
;;  the request (ignored if not needed for the the transport)
;;  and an input port from reading the available references
(define (initial-connect transport host port repo)
  (case transport
    [(git)
     (tcp-connect host port)]
    [(http https)
     (define i
       (get-pure-port
        (string->url
         (~a transport "://" host ":" port "/" repo
             "/info/refs?service=git-upload-pack"))))
     (define ok? #f)
     (dynamic-wind
      void
      (lambda ()
        (unless (regexp-match-peek #px#"^[0-9a-f]{4}#" i)
          (error 'git-checkout (~a "response is not consistent with the Git protocol\n"
                                   "  initial portion: ~s")
                 (read-bytes 64 i)))
        (define pkt (read-pkt i))
        (define term-pkt (read-pkt i))
        (unless (eof-object? term-pkt)
          (error 'git-checkout (~a "expected a null packet, received something else\n"
                                   "  packet: ~s")
                 term-pkt))
        (set! ok? #t)
        (values i (open-output-nowhere)))
      (lambda ()
        (unless ok? (close-input-port i))))]
    [else
     (error 'git-checkout "unrecognized transport\n  given: ~e" transport)]))

;; want-step: transport-sym string natural string input-port output-port
;;            -> (values input-port output-port)
;;  Replaces the connection, if appropriate to the transport, for
;;  writing the wanted references.
(define (want-step transport host port repo i o)
  (case transport
    [(git) (values i o)]
    [(http https)
     (close-input-port i)
     (values (open-input-bytes #"") (open-output-bytes))]))

;; done-step: transport-sym string natural string input-port output-port
;;            -> (values input-port output-port)
;;  Replaces the connection, if appropriate to the transport, after
;;  writing the wanted references and before reading the server's
;;  response.
(define (done-step transport host port repo i o)
  (case transport
    [(git) (values i o)]
    [(http https)
     (define s (get-output-bytes o))
     (define i
       (post-pure-port
        (string->url
         (~a transport "://" host ":" port "/" repo
             "/git-upload-pack"))
        s
        (list "Content-Type: application/x-git-upload-pack-request")))
     (values i (open-output-nowhere))]))

;; ----------------------------------------

;; parse-server-capabilities : bytes -> (listof string)
(define (parse-server-capabilities first-pkt)
  (let ([m (regexp-match #rx#"\0(.*)\n$" first-pkt)])
    (cond
     [m (string-split (bytes->string/utf-8 (cadr m)))]
     [else null])))

;; parse-initial-refs : (listof bytes) -> (listof (list bytes string))
;;  In each element of the returned list, first item is
;;  the branch or tag name, second is the ID
(define (parse-initial-refs pkts)
  (filter
   values
   (for/list ([pkt (in-list pkts)])
     (define m (regexp-match #px#"^([0-9a-fA-F]{40}) (.+)[\0\n]" pkt))  
     (unless m (error 'git-checkout "count not parse ref pkt\n  pkt: ~a"
                      pkt))
     (define name (caddr m))
     (define id (bytes->string/utf-8 (cadr m)))
     (cond
      [(regexp-match? #rx#"\\^{}$" name)
       ;; drop parent references
       #f]
      [else (list name id)]))))

;; select-commits : string (listof (list bytes string))
;;                  -> (values string-or-#f (listof string))
;;  Convert the user's request `ref`, which is a branch or tag or ID,
;;  into a specific ID --- if we can determine it from the server's
;;  initial response. If we can, the list of requested IDs will be
;;  just that one. Otherwise, we'll have to return a list of all
;;  IDs, and then we'll look for the reference later.
(define (select-commits ref refs status)
  (define ref-looks-like-id? (regexp-match? #rx"^[0-9a-fA-F]+$" ref))

  (define ref-rx (byte-regexp (bytes-append
                               #"^refs/(?:heads|tags)/"
                               (regexp-quote (string->bytes/utf-8 ref))
                               #"$")))
  (define ref-commit
    (or
     ;; Search list of branch and tag names:
     (for/or ([ref (in-list refs)])
       (and (regexp-match? ref-rx (car ref))
            (cadr ref)))
     ;; Try matching the references as a commit/tag ID of a branch or tag:
     (let ([rx (id-ref->regexp ref)])
       (for/or ([a-ref (in-list refs)])
         (and (regexp-match? rx (cadr a-ref))
              (begin
                (status "Commit id ~s matches ~a" ref (car a-ref))
                (cadr a-ref)))))))
  
  (define want-commits
    (cond
     [ref-commit (list ref-commit)]
     [ref-looks-like-id?
      (status "Requested reference looks like commit id; getting all commits")
      (for/list ([ref (in-list refs)])
        (cadr ref))]
     [else
      (error 'git "could not find requested reference\n  reference: ~a" ref)]))
  
  (values ref-commit want-commits))

;; ----------------------------------------
;; A "pkt" is the basic unit of communiation in many parts
;; of the git protocol. The first four bytes specify the
;; length of the package (including those bytes).

;; write-pkt : output-port any ... -> void
;;  `display`s each argument to create a package
(define (write-pkt o . args)
  (define s (open-output-bytes))
  (for ([arg (in-list args)])
    (display arg s))
  (define msg (get-output-bytes s))
  (define len (bytes-length msg))
  (define full-msg
    (cond
     [(zero? len) #"0000"]
     [else
      (define len-bstr (string->bytes/utf-8 (format "~x" (+ 4 len))))
      (bytes-append (make-bytes (- 4 (bytes-length len-bstr))
                                (char->integer #\0))
                    len-bstr
                    msg)]))
  (write-bytes full-msg o)
  (flush-output o))

;; read-pkg : input-port -> bstr-or-eof
;;  Reads one pkt, returning eof of the special "null" pkt
(define (read-pkt i)
  (define len-bstr (read-bytes 4 i))
  (cond
   [(eof-object? len-bstr) eof]
   [else
    (unless (and (bytes? len-bstr)
                 (= 4 (bytes-length len-bstr)))
      (error 'git-checkout "error getting pkt length"))
    (define len (string->number (bytes->string/utf-8 len-bstr #\?) 16))
    (unless len
      (error 'git-checkout "error getting pkt length\n  length string: ~e" len-bstr))
    (cond
     [(= len 0) eof] ; flush pkt
     [else
      (define payload-len (- len 4))
      (unless (payload-len . >= . 0)
        (error 'git-checkout "pkt length makes no sense\n  length: ~a" len))
      (read-bytes-exactly 'payload payload-len i)])]))

;; read a list of pkts until an empty packet is found
(define (read-pkts i)
  (define pkt (read-pkt i))
  (if (or (eof-object? pkt)
          (equal? #"" pkt))
      null
      (cons pkt (read-pkts i))))

;; ----------------------------------------
;; Packfile objects

(struct object (location  ; filename within tmp or position in small-object file
                type      ; 'blob, 'commit, etc.; see `type-num->sym`
                type-info ; #f or id
                id)       ; sha1 as bytes
  #:prefab)

;; read-packfile : input-port tmp-info status-proc (or/c #f (-> any)) -> (listof object)
;;  The `initial-eof-handler` argument should escape, if it's not #f
(define (read-packfile i tmp status initial-eof-handler)
  (define pack-bstr (read-bytes 4 i))
  (unless (equal? pack-bstr #"PACK")
    (when (and (eof-object? pack-bstr)
               initial-eof-handler)
      (initial-eof-handler))
    (error 'git-checkout "header error\n  bytes: ~s" pack-bstr))
  (define vers (read-bytes 4 i))
  (unless (equal? vers #"\0\0\0\2")
    (error 'git-checkout "only version 2 supported"))
  (define count-bstr (read-bytes-exactly 'count 4 i))
  (define count (integer-bytes->integer count-bstr #t #t))
  (status "Getting ~a objects" count)
  (for/list ([pos (in-range count)])
    (read-object i pos tmp)))

(define OBJ_COMMIT 1)
(define OBJ_TREE 2)
(define OBJ_BLOB 3)
(define OBJ_TAG 4)
(define OBJ_OFS_DELTA 6)
(define OBJ_REF_DELTA 7)

(define type-num->sym (hash OBJ_COMMIT 'commit
                            OBJ_TREE 'tree
                            OBJ_BLOB 'blob
                            OBJ_TAG 'tag
                            OBJ_OFS_DELTA 'ofs-delta
                            OBJ_REF_DELTA 'ref-delta))

;; read-object : input-port integer tmp-info -> object
(define (read-object i pos tmp)
  (define c (read-byte-only 'type-and-size i))
  (define type (bitwise-and (arithmetic-shift c -4) #x7))
  (when (zero? type) (error 'git-checkout "bad packfile type"))
  (define init-len (bitwise-and c #xF))
  (define len
    (if (msb-set? c)
        (+ init-len (arithmetic-shift (read-integer i) 4))
        init-len))
  (define type-sym (hash-ref type-num->sym type))
  (define type-info
    (case type-sym
     [(ref-delta)
      (read-bytes-exactly 'referenced-id 20 i)]
     [(ofs-delta)
      (error 'gi-checkout "got OFS delta when we didn't claim to support it")]
     [else #f]))
  (define filename (~a (case type-sym
                        [(ref-delta) "delta"]
                        [else "obj"])
                       pos))
  (define location
    (call-with-output-object
     tmp
     filename
     len
     (lambda (o)
       (zlib-inflate i o))))
  (construct-object location type-sym type-info len tmp))

;; To build an `object`, we need to construct a SHA-1 from the object
;; content, which is in a file in `tmp`
(define (construct-object filename type-sym type-info size tmp)
  (object filename type-sym type-info
          (call-with-input-object
           tmp
           filename
           (lambda (i)
             (define prefix (~a type-sym " " size "\0"))
             (sha1-bytes (input-port-append #f
                                            (open-input-string prefix)
                                            i))))))

;; rewrite-deltas : (listof object) -> tmp
;; Given a mapping from ids to objects, combine each "delta" file with
;; a referenced object to create a new object file. The deltas,
;; referenced objects, and generated objects all are in `tmp`. The
;; result is an id-to-object mapping that includes all the given
;; objects plus the generated ones.
(define (rewrite-deltas objs tmp)
  (define ids (hash-copy
               (for/hash ([obj (in-list objs)])
                 (values (object-id obj) obj))))
  (for ([obj (in-list objs)]
        [pos (in-naturals)])
    (when (eq? (object-type obj) 'ref-delta)
      (define base-obj (hash-ref ids (object-type-info obj)))
      (define new-filename (~a "obj" pos))
      (call-with-input-object
       tmp
       ;; the delta file:
       (object-location obj)
       (lambda (i)
         (call-with-input-object
          tmp
          ;; apply delta to this base object:
          (object-location base-obj)
          (lambda (src-in)
            (define src-len (read-integer i))
            (define dest-len (read-integer i))
            (define location
              (call-with-output-object
               tmp
               ;; write to this new object:
               new-filename
               dest-len
               (lambda (o)
                 ;; Each delta command is either "copy" or "insert"
                 (let loop ()
                   (define c (read-byte i))
                   (cond
                    [(eof-object? c)
                     (void)]
                    [(msb-set? c)
                     ;; Copy
                     (define src-offset (read-number-by-bits i (bitwise-and c #xF)))
                     (define raw-src-len (read-number-by-bits i (bitwise-and (arithmetic-shift c -4)
                                                                             #x7)))
                     (define src-len (if (zero? raw-src-len) #x10000 raw-src-len))
                     (file-position src-in src-offset)
                     (copy-port-n src-in o src-len)
                     (loop)]
                    [else
                     ;; Insert
                     (copy-port-n i o c)
                     (loop)])))))
            ;; Add the geemrated object to our table:
            (define new-obj (construct-object location (object-type base-obj) #f
                                              dest-len tmp))
            (hash-set! ids (object-id new-obj) new-obj)))))))
  ids)

;; ----------------------------------------
;; Finding a commit id

(define (find-commit-as-reference ref obj-ids)
  (define rx (id-ref->regexp ref))
  (define matches
    (for/list ([(id obj) (in-hash obj-ids)]
               #:when (eq? 'commit (object-type obj))
               #:when (regexp-match? rx (bytes->hex-string id)))
      (bytes->hex-string id)))
  (cond
   [(= 1 (length matches)) (car matches)]
   [(null? matches)
    (error 'git-checkout "no commit found matching id: ~a" ref)]
   [else
    (error 'git-checkout "found multiple commits matching id: ~a" ref)]))

(define (id-ref->regexp ref)
  (regexp (~a "^" (regexp-quote (string-downcase ref)))))

;; ----------------------------------------
;; Extract a checkout tree

;; extract-commit-tree : bytes (hash/c bytes object) tmp-info path -> void
;;  Extract the designated commit to `dest-dir`, using objects from `tmp`
(define (extract-commit-tree obj-id obj-ids tmp dest-dir)
  (define obj (hash-ref obj-ids obj-id))
  (case (object-type obj)
    [(commit)
     (define tree-id-bstr
       (call-with-input-object
        tmp
        (object-location obj)
        (lambda (i)
          (define m (regexp-try-match #px"^tree ([0-9a-fA-F]{40})" i))
          (unless m
            (error 'git-checkout "cannot extract tree from commit file for ~s"
                   (bytes->hex-string obj-id)))
          (cadr m))))
     (define tree-id (hex-string->bytes (bytes->string/utf-8 tree-id-bstr)))
     (extract-tree tree-id obj-ids tmp dest-dir)]
    [(tag)
     (define commit-id-bstr
       (call-with-input-object
        tmp
        (object-location obj)
        (lambda (i)
          (define m (regexp-try-match #px"^object ([0-9a-fA-F]{40})" i))
          (unless m
            (error 'git-checkout "cannot extract commit from tag file for ~s"
                   (bytes->hex-string obj-id)))
          (cadr m))))
     (define commit-id (hex-string->bytes (bytes->string/utf-8 commit-id-bstr)))
     (extract-commit-tree commit-id obj-ids tmp dest-dir)]
    [(tree)
     (extract-tree obj-id obj-ids tmp dest-dir)]
    [else
     (error 'git-checkout "cannot extract tree from ~a: ~s"
            (object-type obj)
            (bytes->hex-string obj-id))]))

;; extract-commit-tree : bytes (hash/c bytes object) tmp-info path -> void
;;  Extract the designated tree to `dest-dir`, using objects from `tmp`
(define (extract-tree tree-id obj-ids tmp dest-dir)
  (make-directory* dest-dir)
  (define tree-obj (hash-ref obj-ids tree-id))
  (call-with-input-object
   tmp
   (object-location tree-obj)
   (lambda (i)
     (let loop ()
       (define m (regexp-try-match #px"^([0-7]{3,6}) ([^\0]+)\0" i))
       (when m
         (define id (read-bytes-exactly 'id 20 i))
         (define mode (cadr m))
         (define fn (bytes->path-element (caddr m)))
         (define (this-object-location)
           (object-location (hash-ref obj-ids id)))
         (case (datum-intern-literal mode)
          [(#"100644" #"644"
            #"100755" #"755")
           (copy-object tmp
                        (this-object-location)
                        (build-path dest-dir fn))]
          [(#"40000" #"040000")
           (extract-tree id obj-ids tmp (build-path dest-dir fn))]
          [(#"120000")
           (make-file-or-directory-link (build-path dest-dir fn)
                                        (object->bytes tmp (this-object-location)))]
          [(#"160000")
           ;; submodule; just make a directory placeholder
           (make-directory* (build-path dest-dir fn))]
          [else
           (error 'extract-tree "unknown mode: ~s" mode)])
         (loop))))))

;; ----------------------------------------
;; Temporary directory & database

(struct tmp-info (dir small-i small-o [pos #:mutable] [flush? #:mutable]))

;; make-tmp-info : path -> tmp-info
(define (make-tmp-info tmp-dir #:fresh? [fresh? #f])
  (define-values (i o) (open-input-output-file
                        (build-path tmp-dir "objs-small")
                        #:exists (if fresh? 'truncate 'update)))
  (file-stream-buffer-mode i 'none)
  (tmp-info tmp-dir i o 0 #f))

;; close-tmp-info : tmp-info -> void
(define (close-tmp-info tmp)
  (close-input-port (tmp-info-small-i tmp))
  (close-output-port (tmp-info-small-o tmp)))

;; call-with-output-object : tmp-info string natural (output-port -> any) -> location
(define (call-with-output-object tmp filename len proc)
  (define (check-len got-len)
    (unless (= len got-len)
      (error 'git-checkout "size mismatch\n  expected: ~a\n  received: ~a"
             len
             got-len)))
  (cond
   [(len . < . 256)
    (define location (tmp-info-pos tmp))
    (define s (open-output-bytes))
    (proc s)
    (let ([bstr (get-output-bytes s)])
      (check-len (bytes-length bstr))
      (define o (tmp-info-small-o tmp))
      (file-position o location)
      (write-bytes bstr o)
      (set-tmp-info-pos! tmp (+ len (tmp-info-pos tmp)))
      (set-tmp-info-flush?! tmp #t))
    (cons location len)]
   [else
    (define path (build-path (tmp-info-dir tmp) filename))
    (call-with-output-file* path proc #:exists 'truncate)
    (check-len (file-size path))
    filename]))

;; call-with-input-object : tmp-info location (input-port -> X) -> X
(define (call-with-input-object tmp location proc)
  (cond
   [(pair? location)
    (define bstr (object->bytes tmp location))
    (proc (open-input-bytes bstr))]
   [else
    (call-with-input-file* (build-path (tmp-info-dir tmp) location) proc)]))

;; copy-object : tmp-info location path -> void
(define (copy-object tmp location dest-file)
  (cond
   [(pair? location)
    (define bstr (object->bytes tmp location))
    (call-with-output-file*
     dest-file
     #:exists 'truncate
     (lambda (o) (write-bytes bstr o)))]
   [else
    (copy-file (build-path (tmp-info-dir tmp) location)
               dest-file
               #t)]))

;; object->bytes : tmp-info location -> bytes
(define (object->bytes tmp location)
  (cond
   [(pair? location)
    (when (tmp-info-flush? tmp)
      (flush-output (tmp-info-small-o tmp))
      (set-tmp-info-flush?! tmp #f))
    (define i (tmp-info-small-i tmp))
    (file-position i (car location))
    (read-bytes (cdr location) i)]
   [else
    (file->bytes (build-path (tmp-info-dir tmp) location))]))

;; ----------------------------------------
;; Utils

(define (read-bytes-exactly what len i)
  (define bstr (read-bytes len i))
  (unless (and (bytes? bstr)
               (= (bytes-length bstr) len))
    (error 'git-checkout (~a "error getting bytes for ~a\n"
                             "  expected length: ~a\n"
                             "  got length: ~a")
           what
           len
           (if (eof-object? bstr)
               eof
               (bytes-length bstr))))
  bstr)

(define (read-byte-only what i)
  (define c (read-byte i))
  (unless (byte? c)
    (error 'git-checkout "expected to get a byte for ~a, got enf-of-file"
           what))
  c)

;; copy-port-n : input-port output-port natural -> void
(define (copy-port-n i o n)
  (cond
   [(n . <= . 4096)
    (define bstr (read-bytes n i))
    (unless (and (bytes? bstr)
                 (= (bytes-length bstr) n))
      (error 'git-checkout "not enough bytes during copy"))
    (write-bytes bstr o)]
   [else
    (copy-port-n i o 4096)
    (copy-port-n i o (- n 4096))]))

(define (msb-set? c)
  (bitwise-bit-set? c 7))

;; A common integer encoding is a sequence of 7-bit
;; piecs of the number, where the most-significant bit
;; indicates whether the number continues
(define (read-integer i)
  (let loop ([amt 0] [shift 0])
    (define c (read-byte i))
    (cond
     [(eof-object? c) amt]
     [else
      (define new-amt (+ amt
                         (arithmetic-shift (bitwise-and c #x7F) shift)))
      (if (msb-set? c)
          (loop new-amt (+ shift 7))
          new-amt)])))

;; Another number format, where a bitmap `n` indicates
;; when to read a byte
(define (read-number-by-bits i n)
  (cond
   [(zero? n) 0]
   [else
    (+ (if (bitwise-bit-set? n 0)
           (read-byte i)
           0)
       (arithmetic-shift (read-number-by-bits i (arithmetic-shift n -1))
                         8))]))

;; zlib-inflate : input-port output-port
;;  Reads compressed data from `i`, writes uncompressed to `o`
(define (zlib-inflate i o)
  (define cmf (read-byte-only 'zlib-cmf i))
  (define flg (read-byte-only 'zlib-flag i))
  (unless (= 8 (bitwise-and cmf #xF))
    (error 'git-checkout "compression is not `deflate`"))
  (when (bitwise-bit-set? flg 5)
    ;; read dictid
    (read-bytes-exactly 'dictid 4 i))
  (inflate i o)
  ;; Verify checksum?
  (read-bytes-exactly 'adler-checksum 4 i)
  (void))

;; ----------------------------------------

(module+ main
  (require racket/cmdline)

  (define depth 1)
  (define ref "master")
  (define tmp-dir #f)
  (define transport 'git)

  (define-values (host repo dest)
    (command-line
     #:once-any
     [("--git") "Use the Git transport (the default)"
      (set! transport 'git)]
     [("--http") "Use the \"smart\" HTTP transport"
      (set! transport 'http)]
     [("--https") "Use the \"smart\" HTTPS transport"
      (set! transport 'https)]
     #:once-each
     [("--depth") d "Commit depth of <d> (default is 1, 0 means \"all\")"
      (set! depth (string->number d))
      (unless (exact-nonnegative-integer? depth)
        (raise-user-error 'git-checkout "bad depth: ~a" d))]
     [("--ref") branch/tag/commit "Checkout specified commit"
      (set! ref branch/tag/commit)]
     [("--tmp") dir "Write temporary files to <dir>"
      (set! tmp-dir dir)]
     #:args (host repo dest)
     (values host repo dest)))

  (git-checkout host repo
                #:transport transport
                #:dest-dir dest
                #:tmp-dir tmp-dir
                #:ref ref
                #:depth (if (eq? 0 depth) #f depth)))
