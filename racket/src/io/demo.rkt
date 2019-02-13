#lang racket/base
(require "bootstrap-main.rkt"
         (only-in racket/base
                  [string->bytes/utf-8 host:string->bytes/utf-8]
                  [bytes->string/utf-8 host:bytes->string/utf-8]
                  [open-input-file host:open-input-file]
                  [close-input-port host:close-input-port]
                  [read-line host:read-line]
                  [read-byte host:read-byte]
                  [file-stream-buffer-mode host:file-stream-buffer-mode]
                  [port-count-lines! host:port-count-lines!]
                  [current-directory host:current-directory]
                  [path->string host:path->string]))

(path->string (current-directory))
(set-string->number?! string->number)

(define-syntax-rule (test expect rhs)
  (let ([e expect]
        [v rhs])
    (unless (equal? e v)
      (error 'failed "~s: ~e not ~e" 'rhs v e))))

(test #f (bytes-utf-8-ref #"\364\220\200\200" 0))

(test #"\205\327\305\377@:\276r\337[\212'\b\202\36\343<\320\274\316" (sha1-bytes #"abcdefghijklmn"))
(test #"\340\373\262\1m\341\6V\352$IR\311}\350x7\337d\263\320\243\247\350\342\31R " (sha224-bytes #"abcdefghijklmn"))
(test #"\6S\307\351\222\327\252\324\f\262cW8\270p\344\301T\257\263F4\r\2\307\227\324\220\335R\325\371" (sha256-bytes #"abcdefghijklmn"))
(test #"\205\327\305\377@:\276r\337[\212'\b\202\36\343<\320\274\316" (sha1-bytes (open-input-bytes #"abcdefghijklmn")))
(test #"\205\327\305\377@:\276r\337[\212'\b\202\36\343<\320\274\316" (sha1-bytes (open-input-bytes #"__abcdefghijklmn__") 2 16))

(test #t (file-exists? "demo.rkt"))
(test #f (file-exists? "compiled"))
(test #f (file-exists? "compiled/demo-file"))

(test #t (directory-exists? "compiled"))
(test #f (directory-exists? "compiled/demo-dir"))

(test #f (link-exists? "compiled"))
(test #f (link-exists? "compiled/demo-dir"))

(call-with-output-file "compiled/demo-file" void)
(call-with-output-file "compiled/demo-file" void 'replace)
(let ([now (current-seconds)]
      [f-now (file-or-directory-modify-seconds "compiled/demo-file")])
  (test #t (<= (- now 10) f-now now))
  (file-or-directory-modify-seconds "compiled/demo-file" (- now 5))
  (test (- now 5) (file-or-directory-modify-seconds "compiled/demo-file")))
(rename-file-or-directory "compiled/demo-file" "compiled/demo-file2")
(delete-file "compiled/demo-file2")

(test 88 (file-or-directory-modify-seconds "compiled/bad" #f (lambda () 88)))
(test 89 (file-or-directory-modify-seconds "compiled/bad" (current-seconds) (lambda () 89)))

(test #t (and (memq 'read (file-or-directory-permissions "demo.rkt")) #t))
(test #t (and (memq 'read (file-or-directory-permissions "compiled")) #t))

(printf "~s\n" (filesystem-root-list))
(printf "~s\n" (directory-list))
(make-directory "compiled/demo-dir")
(delete-directory "compiled/demo-dir")

(printf "demo.rkt = ~s\n" (file-or-directory-identity "demo.rkt"))
(test (file-or-directory-identity "demo.rkt") (file-or-directory-identity "demo.rkt"))
(test #f (= (file-or-directory-identity "compiled") (file-or-directory-identity "demo.rkt")))

(test (call-with-input-file "demo.rkt"
        (lambda (i)
          (let loop ([n 0])
            (if (eof-object? (read-byte i))
                n
                (loop (add1 n))))))
      (file-size "demo.rkt"))

(copy-file "demo.rkt" "compiled/demo-copy" #t)
(test (file-size "demo.rkt")
      (file-size "compiled/demo-copy"))
(test (file-or-directory-permissions "demo.rkt" 'bits)
      (file-or-directory-permissions "compiled/demo-copy" 'bits))
(delete-file "compiled/demo-copy")

(make-file-or-directory-link "../demo.rkt" "compiled/also-demo.rkt")
(test #t (link-exists? "compiled/also-demo.rkt"))
(test (string->path "../demo.rkt") (resolve-path "compiled/also-demo.rkt"))
(delete-file "compiled/also-demo.rkt")
(test #f (link-exists? "compiled/also-demo.rkt"))

(printf "~s\n" (expand-user-path "~/at-home"))

(struct animal (name weight)
  #:property prop:custom-write (lambda (v o mode)
                                 (fprintf o "<~a>" (animal-name v))))

(test "apple" (format "~a" 'apple))
(test "apple" (format "~a" "apple"))
(test "apple" (format "~a" #"apple"))
(test "#:apple" (format "~a" '#:apple))
(test "17.5" (format "~a" 17.5))

(test "apple" (format "~s" 'apple))
(test "\"apple\"" (format "~s" "apple"))
(test "#\"apple\"" (format "~s" #"apple"))
(test "#:apple" (format "~s" '#:apple))
(test "17.5" (format "~s" 17.5))

(test "1\n\rx0!\"hi\"" (format "1~%~  \n  \rx~ ~o~c~s" 0 #\! "hi"))

(test "*(1 2 3 apple\t\u0001 end <spot> file 1\"2\"3 #hash((a . 1) (b . 2)))*"
      (format "*~a*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) ,(string->path "file") #"1\"2\"3" #hash((b . 2) (a . 1)))))
(test "*'(1 2 3 \"apple\\t\\u0001\" end <spot> #\"1\\\"2\\\"3\\t\\0010\")*"
      (format "*~.v*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) #"1\"2\"3\t\0010")))

(fprintf (current-output-port) "*~v*" '!!!)
(newline)

(test "no: hi 10"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s" 10)))

(test "error: format string requires 1 arguments, given 3"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s" 1 2 3)))
(test "error: format string requires 2 arguments, given 1"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s ~s" 8)))

(define infinite-ones 
  (make-input-port 'ones
                   (lambda (s) 
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   #f
                   void))

(test 49 (read-byte infinite-ones))
(test #\1 (read-char infinite-ones))
(test #"11111" (read-bytes 5 infinite-ones))
(test #"11111" (peek-bytes 5 3 infinite-ones))
(test #"11111" (read-bytes 5 infinite-ones))
(test "11111" (read-string 5 infinite-ones))

(define fancy-infinite-ones
  (make-input-port 'fancy-ones
                   (lambda (s)
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   (lambda (s skip progress-evt)
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   (lambda () (void))
                   (lambda () (make-semaphore))
                   (lambda (amt evt ext-evt) (make-bytes amt (char->integer #\1)))
                   (lambda () (values 7 42 1024))
                   (lambda () (void))
                   (lambda () 99)
                   (case-lambda
                     [() 'block]
                     [(m) (void)])))
(test #"11111" (read-bytes 5 fancy-infinite-ones))
(test #t (evt? (port-progress-evt fancy-infinite-ones)))
(test #t (port-commit-peeked 5 (port-progress-evt fancy-infinite-ones) always-evt fancy-infinite-ones))
(test '(#f #f 99) (call-with-values (lambda () (port-next-location fancy-infinite-ones)) list))
(port-count-lines! fancy-infinite-ones)
(test '(7 42 1024) (call-with-values (lambda () (port-next-location fancy-infinite-ones)) list))
(test 98 (file-position fancy-infinite-ones))
(test 'block (file-stream-buffer-mode fancy-infinite-ones))
(test (void) (file-stream-buffer-mode fancy-infinite-ones 'none))

(define mod3-peeked? #f)
(define mod3-cycle/one-thread
  (let* ([n 2]
	 [mod! (lambda (s delta)
		 (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
                 1)])
    (make-input-port
     'mod3-cycle/not-thread-safe
     (lambda (s)
       (set! n (modulo (add1 n) 3))
       (mod! s 0))
     (lambda (s skip progress-evt)
       (set! mod3-peeked? #t)
       (mod! s (add1 skip)))
     void)))
(test "01201" (read-string 5 mod3-cycle/one-thread))
(test #f mod3-peeked?)
(test "01201" (peek-string 5 (expt 2 5000) mod3-cycle/one-thread))

(let-values ([(r w) (make-pipe)])
  (write-byte 200 w)
  (test #t (byte-ready? r))
  (test #f (char-ready? r)))

(let ()
  (define-values (r w) (make-pipe))
  (define ch (make-channel))
  (display "hi" w)
  (peek-byte r)
  (let ([t (thread (lambda ()
		     (port-commit-peeked 1 (port-progress-evt r) ch r)))])
    (sync (system-idle-evt))
    (let ([t2
	   (thread (lambda ()
		     (port-commit-peeked 1 (port-progress-evt r) ch r)))])
      (sync (system-idle-evt))
      (test #t (thread-running? t))
      (test #t (thread-running? t2))
      (thread-suspend t2)
      (break-thread t2)
      (kill-thread t)
      (thread-resume t2)
      (sleep)))
  (test (char->integer #\h) (peek-byte r)))

(let ()
  (define i (open-input-bytes #"apple"))
  (test (char->integer #\a) (peek-byte i))
  (define threads
    (for/list ([n (in-range 100)])
      (thread (lambda () (test #f (port-commit-peeked 1 (port-progress-evt i) (make-semaphore) i))))))
  (sync (system-idle-evt))
  (test #t (andmap thread-running? threads))
  (test (char->integer #\a) (read-byte i))
  (sync (system-idle-evt))
  (test #f (andmap thread-running? threads)))

(define accum-list '())
(define accum-sema (make-semaphore 1))
(define (accum-ready?) (and (sync/timeout 0 (semaphore-peek-evt accum-sema)) #t))
(define (maybe-accum-evt)
  (if (zero? (random 2))
      (wrap-evt (semaphore-peek-evt accum-sema) (lambda (v) #f))
      #f))
(define accum-o
  (make-output-port 'accum
                    (semaphore-peek-evt accum-sema)
                    (lambda (bstr start end no-buffer/block? enable-break?)
                      (cond
                        [(accum-ready?)
                         (set! accum-list (cons (subbytes bstr start end) accum-list))
                         (- end start)]
                        [else
                         (maybe-accum-evt)]))
                    void
                    (lambda (v no-buffer/block? enable-break?)
                      (cond
                        [(accum-ready?)
                         (set! accum-list (cons v accum-list))
                         #t]
                        [else
                         (maybe-accum-evt)]))
                    (lambda (bstr start end)
                      (wrap-evt (semaphore-peek-evt accum-sema)
                                (lambda (a)
                                  (set! accum-list (cons (subbytes bstr start end) accum-list))
                                  (- end start))))
                    (lambda (v)
                      (wrap-evt (semaphore-peek-evt accum-sema)
                                (lambda (a)
                                  (set! accum-list (cons v accum-list))
                                  #t)))))

(test 5 (write-bytes #"hello" accum-o))
(test '(#"hello") accum-list)
(test 0 (write-bytes #"" accum-o))
(test '(#"hello") accum-list)
(test (void) (flush-output accum-o))
(test '(#"" #"hello") accum-list)
(test 4 (sync (write-bytes-avail-evt #"hola!!" accum-o 0 4)))
(test '(#"hola" #"" #"hello") accum-list)
(test #t (port-writes-special? accum-o))
(test #t (write-special 'howdy accum-o))
(test '(howdy #"hola" #"" #"hello") accum-list)

(set! accum-list '())
(semaphore-wait accum-sema)
(test #f (sync/timeout 0 accum-o))
(test 0 (write-bytes-avail* #"hello" accum-o))
(test accum-list '())
(semaphore-post accum-sema)
(test accum-o (sync/timeout 0 accum-o))
(test 5 (write-bytes-avail* #"hello" accum-o))
(test accum-list '(#"hello"))

(define specialist
  (let ([special
          (lambda (source line col pos)
            (list 'special source line col pos))])
    (make-input-port 'ones
                     (lambda (s) special)
                     (lambda (bstr skip-k p-evt) special)
                     void)))
(port-count-lines! specialist)

(test '(special #f 1 0 1) (read-byte-or-special specialist))
(test '#&(special src 1 1 2) (read-byte-or-special specialist box 'src))
(test '(special #f 1 2 3) (peek-byte-or-special specialist))
(test '#&(special src 1 2 3) (peek-byte-or-special specialist 0 #f box 'src))
(test 'special (peek-byte-or-special specialist 0 #f 'special 'src))
(test 'special (peek-char-or-special specialist 0 'special 'src))

(let-values ([(i o) (make-pipe)])
  (struct my-i (i) #:property prop:input-port 0)
  (struct my-o (o) #:property prop:output-port 0)
  (define c-i (let ([i (my-i i)])
                (make-input-port 'c-i i i void)))
  (define c-o (let ([o (my-o o)])
                (make-output-port 'c-o o o void)))
  (write-bytes #"hello" c-o)
  (test #"hello" (read-bytes 5 c-i)))

(test "apλple" (bytes->string/utf-8 (string->bytes/utf-8 "!!ap\u3BBple__" #f 2) #f 0 7))
(test "ap?ple" (bytes->string/latin-1 (string->bytes/latin-1 "ap\u3BBple" (char->integer #\?))))
(test "apλp\uF7F8\U00101234le" (bytes->string/utf-8 (string->bytes/utf-8 "ap\u3BBp\uF7F8\U101234le")))
(test (string (integer->char #x10400)) (bytes->string/utf-8 #"\360\220\220\200"))

(define apple (string->bytes/utf-8 "ap\u3BBple"))
(define elppa (list->bytes (reverse (bytes->list (string->bytes/utf-8 "ap\u3BBple")))))

(let ()
  (define-values (i o) (make-pipe))
  (for ([n 3])
    (test 4096 (write-bytes (make-bytes 4096 (char->integer #\a)) o))
    (for ([j (in-range 4096)])
      (read-byte i))
    (unless (zero? (pipe-content-length i))
      (error "pipe loop failed\n"))))

(define p (open-input-bytes apple))
(define-values (i o) (make-pipe))

(void (write-bytes #"x" o))
(test
 256
 (let loop ([x 1] [content '(#"x")] [accum null])
   (cond
     [(= x 256) x]
     [(null? content)
      (loop x (reverse accum) null)]
     [else
      (define bstr (list->bytes
                    (for/list ([j (in-range x)])
                      (modulo j 256))))
      (write-bytes bstr o)
      (write-bytes bstr o)
      (unless (equal? (read-bytes (bytes-length (car content)) i)
                      (car content))
        (error))
      (loop (add1 x) (cdr content) (list* bstr bstr accum))])))

(let ()
  (define path (build-path "compiled" "demo-out"))
  (define o (open-output-file path 'truncate))
  ;; We expect this to be buffered:
  (test 12 (write-bytes #"abcdefghijkl" o))
  (test 12 (file-position o))
  (test (void) (file-position o 6))
  (test 3 (write-bytes #"xyz" o))
  (test (void) (file-position o eof))
  (test 1 (write-bytes #"!" o))
  (close-output-port o)

  (test 13 (file-size path))

  (define i (open-input-file path))
  (test #"abcdefxyzjkl!" (read-bytes 20 i))
  (test (void) (file-position i 0))
  (test #"abcdef" (read-bytes 6 i))
  (test (void) (file-position i 9))
  (test #"jkl!" (read-bytes 6 i))
  (close-input-port i))

(let ()
  (define in (open-input-bytes #"hello"))
  (test 0 (file-position in))
  (test #"hel" (read-bytes 3 in))
  (test 3 (file-position in))
  (test (void) (file-position in 2))
  (test #"llo" (read-bytes 3 in))
  (test 5 (file-position in))
  (test eof (read-bytes 3 in))
  (test 5 (file-position in))
  (test (void) (file-position in eof))
  (test 5 (file-position in))
  (test (void) (file-position in 100))
  (test 100 (file-position in)))

(let ()
  (define out (open-output-bytes))
  (test 0 (file-position out))
  (write-bytes #"hello" out)
  (test 5 (file-position out))
  (test (void) (file-position out 1))
  (test 1 (file-position out))
  (write-bytes #"ola" out)
  (test 4 (file-position out))
  (test #"holao" (get-output-bytes out))
  (write-bytes #"!!" out)
  (test 6 (file-position out))
  (test #"hola!!" (get-output-bytes out))
  (test (void) (file-position out 10))
  (test #"hola!!\0\0\0\0" (get-output-bytes out)))

(log-error "start")
(let ()
  (define-values (i o) (make-pipe))
  (port-count-lines! i)
  (port-count-lines! o)
  (define (next-location p)
    (define-values (line col pos) (port-next-location p))
    (list line col pos))
  (test '(1 0 1) (next-location i))
  (test '(1 0 1) (next-location o))

  (write-bytes #"a\n b" o)
  (test '(2 2 5) (next-location o))

  (test #"a" (read-bytes 1 i))
  (test '(1 1 2) (next-location i))
  (test #"\n" (read-bytes 1 i))
  (test '(2 0 3) (next-location i))
  (test #" b" (read-bytes 2 i))
  (test '(2 2 5) (next-location i))

  (write-bytes #"x\r" o)
  (test '(3 0 7) (next-location o))
  (write-bytes #"\n" o)
  (test '(3 0 7) (next-location o))
  (write-bytes #"!" o)
  (test '(3 1 8) (next-location o))

(log-error "here")
  (test #"x\r" (read-bytes 2 i))
  (test '(3 0 7) (next-location i))
  (test #"\n!" (read-bytes 2 i))
  (test '(3 1 8) (next-location i)))

;; ----------------------------------------

(let ([c (bytes-open-converter "latin1" "UTF-8")])
  (test '(#"A\302\200" 2 complete)
        (call-with-values (lambda () (bytes-convert c #"A\200")) list))
  (define bstr (make-bytes 3))
  (test '(3 2 complete)
        (call-with-values (lambda () (bytes-convert c #"A\200" 0 2 bstr)) list))
  (test #"A\302\200" bstr)
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\200" 0 2 #f 0 2)) list))
  (test '(#"A\302\200" 2 complete)
        (call-with-values (lambda () (bytes-convert c #"A\200" 0 2 #f 0 3)) list))
  (test '(#"A" 1 complete)
        (call-with-values (lambda () (bytes-convert c #"A\200" 0 1 #f 0 2)) list))
  (test (void) (bytes-close-converter c)))

(let ([c (bytes-open-converter "UTF-8" "latin1")])
  (test '(#"A\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200")) list))
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 1)) list))
  (test '(#"A\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 2)) list))
  (test '(#"A" 1 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 1 #f 0 2)) list))
  (test '(#"A" 1 aborts)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 2 #f 0 2)) list))
  (test (void) (bytes-close-converter c)))

(let ([c (bytes-open-converter "UTF-8" "UTF-8")])
  (test '(#"A\302\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200")) list))
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 1)) list))
  (test '(#"A\302\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 3)) list))
  (test '(#"A" 1 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 1 #f 0 2)) list))
  (test '(#"A" 1 aborts)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 2 #f 0 2)) list))
  (test '(#"A" 1 error)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 3)) list))
  (test '(#"A" 1 error)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 2)) list))
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 1)) list))
  (test '(#"\360\220\220\200" 4 complete)
        (call-with-values (lambda () (bytes-convert c #"\360\220\220\200")) list))
  (test (void) (bytes-close-converter c)))

(let ([c (bytes-open-converter "UTF-8-permissive" "UTF-8")])
  (test '(#"A\302\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200")) list))
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 1)) list))
  (test '(#"A\302\200" 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 3 #f 0 3)) list))
  (test '(#"A" 1 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 1 #f 0 2)) list))
  (test '(#"A" 1 aborts)
        (call-with-values (lambda () (bytes-convert c #"A\302\200" 0 2 #f 0 2)) list))
  (test '(#"A" 1 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 3)) list))
  (test '(#"A\357\277\275" 2 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 4)) list))
  (test '(#"A\357\277\275" 2 aborts)
        (call-with-values (lambda () (bytes-convert c #"A\302\302" 0 3 #f 0 5)) list))
  (test '(#"A\357\277\275" 2 continues)
        (call-with-values (lambda () (bytes-convert c #"A\302x" 0 3 #f 0 4)) list))
  (test (void) (bytes-close-converter c)))

(define (reorder little)
  (if (system-big-endian?)
      (let* ([len (bytes-length little)]
             [bstr (make-bytes len)])
        (for ([i (in-range len)])
          (bytes-set! bstr i (bytes-ref little (bitwise-xor i 1)))))
      little))

(let ([c (bytes-open-converter "platform-UTF-8" "platform-UTF-16")])
  (test `(,(reorder #"A\0\200\0") 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200")) list))
  (test `(,(reorder #"A\0") 1 error)
        (call-with-values (lambda () (bytes-convert c #"A\200")) list))
  ;; unpaired high surrogate
  (test `(#"" 0 error)
        (call-with-values (lambda () (bytes-convert c #"\355\240\200")) list))
  ;; unpaired low surrogate
  (test `(#"" 0 error)
        (call-with-values (lambda () (bytes-convert c #"\355\260\201")) list))
  (test `(,(reorder #"\1\330\0\334") 4 complete)
        (call-with-values (lambda () (bytes-convert c #"\360\220\220\200")) list))
  (test (void) (bytes-close-converter c)))

(let ([c (bytes-open-converter "UTF-8-ish" "UTF-16-ish")])
  (test `(,(reorder #"A\0\200\0") 3 complete)
        (call-with-values (lambda () (bytes-convert c #"A\302\200")) list))
  (test `(,(reorder #"A\0") 1 error)
        (call-with-values (lambda () (bytes-convert c #"A\200")) list))
  ;; unpaired high surrogate
  (test `(,(reorder #"\0\330") 3 complete)
        (call-with-values (lambda () (bytes-convert c #"\355\240\200")) list))
  ;; unpaired low surrogate
  (test `(,(reorder #"\1\334") 3 complete)
        (call-with-values (lambda () (bytes-convert c #"\355\260\201")) list))
  ;; surrogate pair where each is separately encoded
  (test `(,(reorder #"\0\330\1\334") 6 complete)
        (call-with-values (lambda () (bytes-convert c #"\355\240\200\355\260\201")) list))
  (test `(,(reorder #"\1\330\0\334") 4 complete)
        (call-with-values (lambda () (bytes-convert c #"\360\220\220\200")) list))
  (test (void) (bytes-close-converter c)))

(let ([c (bytes-open-converter "UTF-16-ish" "UTF-8-ish")])
  (test `(#"A\302\200" 4 complete)
        (call-with-values (lambda () (bytes-convert c (reorder #"A\0\200\0"))) list))
  ;; unpaired high surrogate
  (test `(#"" 0 aborts)
        (call-with-values (lambda () (bytes-convert c (reorder #"\0\330"))) list))
  (test `(#"\355\240\200X" 4 complete)
        (call-with-values (lambda () (bytes-convert c (reorder #"\0\330X\0"))) list))
  ;; unpaired low surrogate
  (test `(#"\355\260\201" 2 complete)
        (call-with-values (lambda () (bytes-convert c (reorder #"\1\334"))) list))
  (test `(#"\355\260\201X" 4 complete)
        (call-with-values (lambda () (bytes-convert c (reorder #"\1\334X\0"))) list))
  ;; surrogate pair
  (test `(#"\360\220\200\201" 4 complete)
        (call-with-values (lambda () (bytes-convert c (reorder #"\0\330\1\334"))) list))
  (test (void) (bytes-close-converter c)))

;; ----------------------------------------

(parameterize ([current-locale "C"])
  (test #"A*Z" (string->bytes/locale "A\u3BBZ" 42)))

;; Latin-1
(parameterize ([current-locale "en_US.ISO8859-1"])
  (test #"!\xD6!" (string->bytes/locale "!\uD6!"))
  (test "!\uD6!" (bytes->string/locale #"!\xD6!")))

(parameterize ([current-locale "en_US.UTF-8"])
  (test #f (string<? "Éric" "Dric")))
(when (eq? 'unix (system-type))
  (parameterize ([current-locale "fr_FR.ISO8859-1"])
    (test #t (string-locale<? "Éric" "Dric"))))

(test #t (string-locale<? "apple" "applex"))
(test #f (string-locale=? "apple" "applex"))
(test #f (string-locale>? "apple" "applex"))

(test #t (string-locale<? "apple\0x" "apple\0y"))
(test #f (string-locale=? "apple\0x" "apple\0y"))
(test #f (string-locale>? "apple\0x" "apple\0y"))

(test #t (string-locale-ci=? "apple" "AppLE"))
(test #f (string-locale-ci=? "apple" "AppLEx"))

(test #t (boolean? (string-locale<? "Apple" "apple")))
(test #f (string-locale-ci<? "Apple" "apple"))

(test #t (and (member (string-locale-downcase "Éric")
                      '("éric" "Éric"))
              #t))
(when (eq? 'unix (system-type))
  (parameterize ([current-locale "en_US.ISO8859-1"])
    (test "Éric" (string-locale-downcase "Éric"))))


;; ----------------------------------------

(define (print-test v expect #:print [print print])
  (define o (open-output-string))
  (parameterize ([current-output-port o])
    (print v))
  (test expect (get-output-string o)))

(let ([b (box #f)])
  (set-box! b b)
  (print-test b "#0='#&#0#"))

(let ([b (vector #f #f)])
  (struct p (x y) #:transparent)
  (struct c (x y) #:prefab)
  (vector-set! b 0 b)
  (vector-set! b 1 b)
  (print-test b "#0='#(#0# #0#)")
  (print-test '(1) "'(1)")
  (print-test (cons 1 (cons 2 3)) "'(1 2 . 3)")
  (print-test (cons 1 (mcons 3 4)) "(cons 1 (mcons 3 4))")
  (print-test (cons 1 (cons 2 (mcons 3 4))) "(list* 1 2 (mcons 3 4))")
  (print-test (cons 1 (cons (mcons 3 4) null)) "(list 1 (mcons 3 4))")
  (print-test '('a) "'('a)")
  (print-test '(4 . 'a) "'(4 . 'a)")
  (print-test '(4 unquote a) "'(4 . ,a)")
  (print-test '(4 unquote @a) "'(4 . , @a)")
  (print-test '#(4 unquote a) "'#(4 unquote a)")
  (print-test '((quote a b)) "'((quote a b))")
  (print-test (p 1 2) "(p 1 2)")
  (print-test (box (p 1 2)) "(box (p 1 2))")
  (print-test (hasheq 1 (p 1 2) 2 'other) "(hasheq 1 (p 1 2) 2 'other)")
  (print-test (arity-at-least 1) "(arity-at-least 1)")
  (let ([v (make-placeholder #f)])
    (placeholder-set! v (list (p 1 2) v))
    (print-test (make-reader-graph v) "#0=(list (p 1 2) #0#)"))
  (let ([v (make-placeholder #f)])
    (placeholder-set! v (c (p 1 2) v))
    (print-test (make-reader-graph v) "#0=(c (p 1 2) #0#)")))

(let ([b (make-hash)])
  (hash-set! b 'self b)
  (print-test b "#0='#hash((self . #0#))"))

(let ()
  (struct a (x) #:mutable #:transparent)
  (let ([an-a (a #f)])
    (set-a-x! an-a an-a)
    (print-test an-a "#0=(a #0#)")))

(let ()
  (struct a (x) #:mutable #:prefab)
  (let ([an-a (a #f)])
    (set-a-x! an-a an-a)
    (print-test an-a "#0='#s((a #(0)) #0#)")))

(let ()
  (define p1 (cons 1 2))
  (define p2 (cons p1 p1))
  (print-test p2 "'((1 . 2) 1 . 2)")
  (parameterize ([print-graph #t])
    (print-test p2 "'(#0=(1 . 2) . #0#)")))

(let ()
  (define p1 (mcons 1 2))
  (define p2 (mcons p1 p1))
  (print-test p2 "(mcons (mcons 1 2) (mcons 1 2))")
  (print-test p2 #:print write "{{1 . 2} 1 . 2}")
  (parameterize ([print-graph #t])
    (print-test p2 "(mcons #0=(mcons 1 2) #0#)"))
  (print-test (mcons 1 null) "(mcons 1 '())")
  (print-test (mcons 1 (mcons 2 null)) "(mcons 1 (mcons 2 '()))")
  (print-test (mcons 1 null) "{1}" #:print write)
  (print-test (mcons 1 (mcons 2 null)) "{1 2}" #:print write))

(print-test '|hello world| "'|hello world|")
(print-test '|1.0| "'|1.0|")
(print-test '1\|2 "'1\\|2")
(print-test '#:apple "'#:apple")
(print-test '#:|apple pie| "'#:|apple pie|")
(print-test '#:1.0 "'#:1.0")
(print-test 1.0 "1.0")

;; ----------------------------------------

(define l (tcp-listen 59078 5 #t))
(test #t (tcp-listener? l))
(test #t (evt? l))

(define-values (ti to) (tcp-connect "localhost" 59078))
(test l (sync l))
(define-values (tai tao) (tcp-accept l))

(test #f (file-stream-port? i))
(test #f (file-stream-port? o))

(test 6 (write-string "hello\n" to))
(flush-output to)
(test "hello" (read-line tai))

(test 9 (write-string "goodbyte\n" tao))
(flush-output tao)
(test "goodbyte" (read-line ti))

(close-output-port to)
(close-output-port tao)
(close-input-port ti)
(close-input-port tai)

(tcp-close l)

;; ----------------------------------------

(define u1 (udp-open-socket))
(test (void) (udp-bind! u1 #f 10768))

(define u2 (udp-open-socket))
(test (void) (udp-send-to u2 "localhost" 10768 #"hello"))
(let* ([bstr (make-bytes 10)]
       [l (call-with-values (lambda () (udp-receive! u1 bstr)) list)])
  (test 5 (car l))
  (test #"hello" (subbytes bstr 0 5)))
(test '(#f #f #f) (call-with-values (lambda () (udp-receive!* u1 (make-bytes 1))) list))

;; ----------------------------------------

(let ()
  (define-values (sp o i e)
    (subprocess (current-output-port)
                (current-input-port)
                (current-error-port)
                "/bin/cat"))
  (sleep 0.1)
  (subprocess-kill sp #f)
  (test sp (sync sp))
  (test #t (positive? (subprocess-status sp))))

(let ()
  (define-values (sp o i e)
    (subprocess (current-output-port)
                (current-input-port)
                (current-error-port)
                "/bin/ls"))
  (test sp (sync sp))
  (test #t (zero? (subprocess-status sp))))

(let ()
  (define-values (sp o i e)
    (subprocess #f
                #f
                (current-error-port)
                "/bin/cat"))
  (display "hello\n" i)
  (flush-output i)
  (test "hello" (read-line o))
  (close-output-port i)
  (test eof (read-line o))
  (test (void) (subprocess-wait sp))
  (test #t (zero? (subprocess-status sp))))

;; ----------------------------------------

'read-string
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (port-count-lines! p)
       (let loop ()
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(define read-byte-buffer-mode 'block)
(define count-lines? #t)

'read-byte/host
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (host:open-input-file "compiled/io.rktl"))
       (host:file-stream-buffer-mode p read-byte-buffer-mode)
       (when count-lines? (host:port-count-lines! p))
       (let loop ()
         (unless (eof-object? (host:read-byte p))
           (loop)))
       (host:close-input-port p)
       (loop (sub1 j))))))

'read-byte
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (file-stream-buffer-mode p read-byte-buffer-mode)
       (when count-lines? (port-count-lines! p))
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'read-line/host
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (host:open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (host:read-line p))
           (loop)))
       (host:close-input-port p)
       (loop (sub1 j))))))

'read-line
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (read-line p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'encoding
(time
 (for/fold ([v #f]) ([i (in-range 1000000)])
   (bytes->string/utf-8 (string->bytes/utf-8 "ap\u3BBple"))))
(time
 (for/fold ([v #f]) ([i (in-range 1000000)])
   (host:bytes->string/utf-8 (host:string->bytes/utf-8 "ap\u3BBple"))))

(test "a" (read-line (open-input-string "a")))
(test "a" (read-line (open-input-string "a\nb")))
(test "a" (read-line (open-input-string "a\r\nb") 'any))
(test "a" (read-line (open-input-string "a\rb") 'any))

(test #\l (bytes-utf-8-ref #"apple" 3))
(test #\λ (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 2))
(test #\p (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 3))
(test #\l (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 3 #\? 1))
(test #f (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 6))

(test 3 (bytes-utf-8-index #"apple" 3))
(test 4 (bytes-utf-8-index (string->bytes/utf-8 "apλple") 3))
