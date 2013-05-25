#!r6rs

(library (tests r6rs io ports)
  (export run-io-ports-tests)
  (import (rnrs)
          (rnrs mutable-strings (6))
          (tests r6rs test))

  (define-syntax test-transcoders
    (syntax-rules ()
      [(_ bytevector->string string->bytevector)
       (begin
         (test (bytevector->string #vu8(97 112 112 206 187 101)
                                   (make-transcoder (utf-8-codec)))
               "app\x03BB;e")
         (test (bytevector->string #vu8(97 112 112 206 187 101)
                                   (make-transcoder (latin-1-codec)))
               "app\xCE;\xBB;e")
         (test (bytevector->string #vu8(#xFE #xFF 0 97 0 112 0 112 #x3 #xBB 0 101)
                                   (make-transcoder (utf-16-codec)))
               "app\x03BB;e")
         (test (bytevector->string #vu8(97 10 98 13 99 13 10 100 #o302 #o205 101 
                                           #o342 #o200 #o250 102 13 #o302 #o205 103)
                                   (make-transcoder (utf-8-codec) 'none))
               "a\nb\rc\r\nd\x85;e\x2028;f\r\x85;g")
         (test (bytevector->string #vu8(97 10 98 13 99 13 10 100 #o302 #o205 101 #o342 
                                           #o200 #o250 102 13 #o302 #o205 103)
                                   (make-transcoder (utf-8-codec) 'lf))
               "a\nb\nc\nd\ne\nf\ng")
         (test/exn (bytevector->string #vu8(97 112 112 206 101)
                                       (make-transcoder (utf-8-codec) 'lf 'raise))
                   &i/o-decoding)

         (test (string->bytevector "app\x03BB;e"
                                   (make-transcoder (utf-8-codec)))
               #vu8(97 112 112 206 187 101))
         (test (string->bytevector "apple\x85;"
                                   (make-transcoder (latin-1-codec)))
               #vu8(97 112 112 108 101 #x85))
         (test/alts (string->bytevector "app\x03BB;e"
                                        (make-transcoder (utf-16-codec)))
                    ;; Could be LE or BE (where BE is with or without BOM):
                    #vu8(#xFF #xFE 97 0 112 0 112 0 #xBB #x3 101 0)
                    #vu8(#xFE #xFF 0 97 0 112 0 112 #x3 #xBB 0 101)
                    #vu8(0 97 0 112 0 112 #x3 #xBB 0 101))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'lf))
               #vu8(97 10 98))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'cr))
               #vu8(97 13 98))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'crlf))
               #vu8(97 13 10 98))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'nel))
               #vu8(97 #o302 #o205 98))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'ls))
               #vu8(97 #o342 #o200 #o250 98))
         (test (string->bytevector "a\nb"
                                   (make-transcoder (utf-8-codec) 'crnel))
               #vu8(97 13 #o302 #o205 98))
         (test/exn (string->bytevector "a\x185;b" (make-transcoder (latin-1-codec) 'lf 'raise))
                   &i/o-encoding))]))

  (define-syntax test-positions
    (syntax-rules ()
      [(_ make)
       (begin
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     (lambda () 0)
                     #f
                     (lambda () 'ok))])
           (test (port-has-port-position? p) #t)
           (test (port-has-set-port-position!? p) #f)
           (test (port-position p) 0)
           (test/unspec (close-port p)))
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     #f
                     (lambda (pos) 'ok)
                     (lambda () 'ok))])
           (test (port-has-port-position? p) #f)
           (test (port-has-set-port-position!? p) #t)
           (test/unspec (set-port-position! p 0))
           (test/unspec (close-port p)))
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     #f
                     #f
                     (lambda () 'ok))])
           (test (port-has-port-position? p) #f)
           (test (port-has-set-port-position!? p) #f)
           (test/unspec (close-port p))))]))

  (define-syntax test-rw
    (syntax-rules ()
      [(_ v)
       (test (let ([p (open-string-input-port
                       (call-with-string-output-port
                        (lambda (p) (put-datum p v))))])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda () (get-datum p))
                   (lambda () (close-port p))))
             v)]))

  ;; ----------------------------------------

  (define (run-io-ports-tests)

    (test (enum-set->list (file-options)) '())
    (test (enum-set-member? 'no-create (file-options)) #f)
    (test (enum-set-member? 'no-create (file-options no-create)) #t)
    (test (enum-set-member? 'no-create (file-options no-fail)) #f)
    (test (enum-set-member? 'no-fail (file-options no-fail)) #t)
    (test (enum-set-member? 'no-truncate (file-options no-truncate)) #t)
    (test (enum-set-member? 'no-truncate (file-options no-create no-fail no-truncate)) #t)
    (test (enum-set-member? 'no-fail (file-options no-create no-fail no-truncate)) #t)
    (test (enum-set-member? 'no-create (file-options no-create no-fail no-truncate)) #t)

    (test (buffer-mode none) 'none)
    (test (buffer-mode line) 'line)
    (test (buffer-mode block) 'block)
    (test (buffer-mode? 'none) #t)
    (test (buffer-mode? 'line) #t)
    (test (buffer-mode? 'block) #t)
    (test (buffer-mode? 'point) #f)

    (test/unspec (latin-1-codec))
    (test/unspec (utf-8-codec))
    (test/unspec (utf-16-codec))

    (test (eol-style lf) 'lf)
    (test (eol-style cr) 'cr)
    (test (eol-style crlf) 'crlf)
    (test (eol-style nel) 'nel)
    (test (eol-style crnel) 'crnel)
    (test (eol-style ls) 'ls)
    (test (eol-style none) 'none)
    (test (symbol? (native-eol-style)) #t)

    (test (error-handling-mode ignore) 'ignore)
    (test (error-handling-mode raise) 'raise)
    (test (error-handling-mode replace) 'replace)

    (test (transcoder-codec (make-transcoder (latin-1-codec))) (latin-1-codec))
    (test (transcoder-codec (make-transcoder (utf-8-codec))) (utf-8-codec))
    (test (transcoder-codec (make-transcoder (utf-16-codec))) (utf-16-codec))
    (test (transcoder-eol-style (make-transcoder (utf-16-codec))) (native-eol-style))
    (test (transcoder-error-handling-mode (make-transcoder (utf-16-codec))) 'replace)

    (test (transcoder-codec (make-transcoder (utf-8-codec) 'nel)) (utf-8-codec))
    (test (transcoder-eol-style (make-transcoder (utf-8-codec) 'nel)) 'nel)
    (test (transcoder-error-handling-mode (make-transcoder (utf-8-codec) 'nel)) 'replace)
    (test (transcoder-codec (make-transcoder (utf-8-codec) 'nel 'raise)) (utf-8-codec))
    (test (transcoder-eol-style (make-transcoder (utf-8-codec) 'nel 'raise)) 'nel)
    (test (transcoder-error-handling-mode (make-transcoder (utf-8-codec) 'nel 'raise)) 'raise)

    (test/unspec (native-transcoder))

    (test-transcoders bytevector->string
                      string->bytevector)

    (test (eqv? (eof-object) (eof-object)) #t)
    (test (eq? (eof-object) (eof-object)) #t)

    ;; ----------------------------------------
    ;; Check file creation and truncation:

    (test/unspec
     (if (file-exists? "io-tmp1")
         (delete-file "io-tmp1")))

    ;; Don't create if 'no-create:
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-fail))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-truncate))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-fail no-truncate))
              &i/o-file-does-not-exist)

    ;; Create:
    (let ([p (open-file-output-port "io-tmp1")])
      (test (file-exists? "io-tmp1") #t)
      (test (port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test (output-port? p) #t)
      (test (input-port? p) #f)
      (test/unspec (flush-output-port p))
      (test/unspec (close-port p)))

    ;; Don't re-create:
    (test/exn (open-file-output-port "io-tmp1")
              &i/o-file-already-exists)
    (test/exn (open-file-output-port "io-tmp1" (file-options no-truncate))
              &i/o-file-already-exists)

    ;; Re-open if 'no-create is specified:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create))])
      (test/unspec (close-port p)))
    
    ;; Re-open if 'no-fail is specified:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (close-port p)))

    ;; Create if 'no-fail is specified and it doesn't exist:
    (test/unspec (delete-file "io-tmp1"))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail no-truncate))])
      (test/unspec (close-port p)))
    (test/unspec (delete-file "io-tmp1"))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (put-bytevector p #vu8(99 101 98 100)))
      (test/unspec (close-port p)))

    ;; Check content:
    (let ([p (open-file-input-port "io-tmp1")])
      (test (port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test (input-port? p) #t)
      (test (output-port? p) #f)
      (test (get-bytevector-n p 5) #vu8(99 101 98 100))
      (test (port-eof? p) #t)
      (test/unspec (close-port p)))

    ;; Check that 'no-truncate doesn't truncate:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail no-truncate))])
      (test/unspec (put-bytevector p #vu8(97)))
      (test/unspec (close-port p)))
    (let ([p (open-file-input-port "io-tmp1")])
      (test (get-bytevector-n p 5) #vu8(97 101 98 100))
      (test/unspec (close-port p)))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create no-truncate))])
      (test/unspec (put-bytevector p #vu8(96)))
      (test/unspec (close-port p)))
    (let ([p (open-file-input-port "io-tmp1")])
      (test (get-bytevector-n p 5) #vu8(96 101 98 100))
      (test/unspec (close-port p)))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create no-truncate))])
      (test (port-has-port-position? p) #t)
      (test (port-has-set-port-position!? p) #t)
      (test (port-position p) 0)
      (test/unspec (set-port-position! p 6))
      (test (port-position p) 6)
      (test/unspec (put-bytevector p #vu8(102)))
      (test/unspec (close-port p)))
    (let ([p (open-file-input-port "io-tmp1")])
      (test (get-bytevector-n p 4) #vu8(96 101 98 100))
      (test/unspec (get-bytevector-n p 2))
      (test (get-bytevector-n p 2) #vu8(102))
      (test/unspec (close-port p)))

    ;; Otherwise, truncate:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (close-port p)))
    (let ([p (open-file-input-port "io-tmp1")])
      (test (port-eof? p) #t)
      (test/unspec (close-port p)))

    ;; ----------------------------------------
    ;; Check buffer modes? Just make sure they're accepted:

    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'line)])
      (test (output-port-buffer-mode p) 'line)
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'block)])
      (test (output-port-buffer-mode p) 'block)
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'none)])
      (test (output-port-buffer-mode p) 'none)
      (close-port p))

    (let ([p (open-file-input-port "io-tmp1" (file-options) 'line)])
      (close-port p))
    (let ([p (open-file-input-port "io-tmp1" (file-options) 'block)])
      (close-port p))
    (let ([p (open-file-input-port "io-tmp1" (file-options) 'none)])
      (close-port p))
    
    ;; ----------------------------------------
    ;; Transcoders

    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (latin-1-codec)))])
      (when (port-has-port-position? p)
        (test/unspec (port-position p))
        (when (port-has-set-port-position!? p)
          (let ([pos (port-position p)])
            (test/unspec (set-port-position! p pos)))))
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test/unspec (put-string p "apple"))
      (test/unspec (put-string p "berry" 3))
      (test/unspec (put-string p "berry" 1 1))
      (close-port p))

    (let ([p (open-file-input-port "io-tmp1" (file-options)
                                   'block (make-transcoder (latin-1-codec)))])
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test (lookahead-char p) #\a)
      (test (get-char p) #\a)
      (test (get-string-n p 20) "pplerye")
      (test (lookahead-char p) (eof-object))
      (test (get-char p) (eof-object))
      (close-port p))

    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (utf-8-codec)))])
      (test/unspec (put-string p "app\x3BB;e"))
      (close-port p))
    (let ([p (open-file-input-port "io-tmp1" (file-options)
                                   'block (make-transcoder (latin-1-codec)))])
      (test (get-string-n p 20) "app\xCE;\xBB;e")
      (close-port p))
    
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (utf-16-codec)))])
      (test/unspec (put-string p "app\x3BB;e"))
      (close-port p))
    (let ([p (open-file-input-port "io-tmp1" (file-options)
                                   'block (make-transcoder (utf-16-codec)))])
      (test (get-string-n p 20) "app\x3BB;e")
      (close-port p))
    (let ([p (open-file-input-port "io-tmp1")])
      (let ([b1 (get-u8 p)])
        (cond
        [(equal? b1 #xFE)
         (test (get-u8 p) #xFF)
         (test (get-u8 p) 0)
         (test (get-u8 p) 97)]
        [(equal? b1 #xFF)
         (test (get-u8 p) #xFE)
         (test (get-u8 p) 97)
         (test (get-u8 p) 0)]
        [else
         ;; Must be big-endian
         (test b1 0)
         (test (get-u8 p) 97)]))
      (test/unspec (close-port p)))

    (let ([bytevector->string-via-file
           (lambda (bv tr)
             (let ([p (open-file-output-port "io-tmp1" (file-options no-create))])
               (put-bytevector p bv)
               (close-port p))
             (let ([p (open-file-input-port "io-tmp1" (file-options) 'block tr)])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda () (get-string-all p))
                   (lambda () (close-port p)))))]
          [string->bytevector-via-file
           (lambda (str tr)
             (let ([p (open-file-output-port "io-tmp1" (file-options no-create)
                                             'block tr)])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda () (put-string p str) (flush-output-port p))
                   (lambda () (close-port p))))
             (let ([p (open-file-input-port "io-tmp1")])
               (let ([v (get-bytevector-all p)])
                 (close-port p)
                 v)))])
      (test-transcoders bytevector->string-via-file
                        string->bytevector-via-file))

    (let ((port (open-bytevector-input-port #vu8())))
      (test (eof-object? (get-bytevector-all port)) #t)
      (test (eof-object? (get-bytevector-n port 10)) #t))

    (let ([test-i+o
           (lambda (buf)
             (let ([p (open-file-input/output-port "io-tmp1"
                                                   (file-options no-fail)
                                                   buf)])
               (if (and (port-has-port-position? p)
                        (port-has-set-port-position!? p))
                   (begin
                     (port-position p)
                     (test (port-position p) 0)
                     (test/unspec (put-bytevector p #vu8(7 9 11)))
                     (unless (eq? buf 'none)
                       (test/unspec (flush-output-port p)))
                     (test (port-position p) 3)
                     (test/unspec (set-port-position! p 0))
                     (test (get-bytevector-n p 2) #vu8(7 9))
                     (test/unspec (put-bytevector p #vu8(13 15 17)))
                     (unless (eq? buf 'none)
                       (test/unspec (flush-output-port p)))
                     (test/unspec (set-port-position! p 3))
                     (test (get-bytevector-n p 2) #vu8(15 17)))
                   (begin
                     (test/unspec (put-bytevector p #vu8(7 9 11)))
                     (test (get-u8 p) (eof-object))))
               (test/unspec (close-port p))))])
      (test-i+o 'line)
      (test-i+o 'block)
      (test-i+o 'none))

    (let ([p (open-file-input/output-port "io-tmp1"
                                          (file-options no-fail)
                                          'none
                                          (make-transcoder (latin-1-codec)))])
      (test/unspec (put-string p "berry"))
      (test/unspec (close-port p)))
    (let ([p (open-file-input/output-port "io-tmp1"
                                          (file-options no-fail no-truncate)
                                          'none
                                          (make-transcoder (latin-1-codec)))])
      (test (get-string-n p 4) "berr")
      (test/unspec (put-string p "apple"))
      (test/unspec (close-port p)))
    (let ([p (open-file-input/output-port "io-tmp1"
                                          (file-options no-fail no-truncate)
                                          'none
                                          (make-transcoder (latin-1-codec)))])
      (test (get-string-n p 10) "berrapple")
      (test/unspec (close-port p)))

    (test/unspec (delete-file "io-tmp1"))
    
    ;; ----------------------------------------
    ;; bytevector ports

    (let ([p (open-bytevector-input-port #vu8(0 1 2 3))])
      (test (input-port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test (get-u8 p) 0)
      (test (lookahead-u8 p) 1)
      (test (get-u8 p) 1)
      (let ([bv (make-bytevector 10 0)])
        (test (get-bytevector-n! p bv 1 7) 2)
        (test bv #vu8(0 2 3 0 0 0 0 0 0 0)))
      (test (get-bytevector-some p) (eof-object))
      (close-port p))

    (let-values ([(p get) (open-bytevector-output-port)])
      (test (output-port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test/unspec (put-u8 p 10))
      (test/unspec (put-bytevector p #vu8(11 12 13)))
      (test/unspec (put-bytevector p #vu8(14 15 16 17 18) 4))
      (test/unspec (put-bytevector p #vu8(14 15 16 17 18) 2 1))
      (test (get) #vu8(10 11 12 13 18 16))
      (test (get) #vu8())
      (close-port p))

    (test (call-with-bytevector-output-port
           (lambda (p)
             (put-bytevector p #vu8(1 2 3))))
          #vu8(1 2 3))

    (test (call-with-bytevector-output-port
           (lambda (p)
             (put-string p "app\x3BB;e"))
           (make-transcoder (utf-8-codec)))
          #vu8(97 112 112 206 187 101))

    (let ([bytevector->string-via-port
           (lambda (bv tr)
             (let ([p (open-bytevector-input-port bv tr)])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda () (get-string-all p))
                   (lambda () (close-port p)))))]
          [string->bytevector-via-port
           (lambda (str tr)
             (let-values ([(p get) (open-bytevector-output-port tr)])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda ()
                     (put-string p str)
                     (get))
                   (lambda ()
                     (close-port p)))))])
      (test-transcoders bytevector->string-via-port
                        string->bytevector-via-port))

    ;; ----------------------------------------
    ;; string ports

    (let ([p (open-string-input-port "app\x3BB;e\r\nban")])
      (test (input-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test (get-char p) #\a)
      (test (lookahead-char p) #\p)
      (test (get-line p) "pp\x3BB;e\r")
      (let ([s (make-string 10 #\_)])
        (test (get-string-n! p s 1 9) 3)
        (test s "_ban______")))

    (let ([p (open-string-input-port "(1 2 3) 4")])
      (test (get-datum p) '(1 2 3))
      (close-port p))

    (let-values ([(p get) (open-string-output-port)])
      (test/unspec (put-string p "app\x3BB;e"))
      (test (get) "app\x3BB;e")
      (test (get) "")
      (close-port p))

    (test (call-with-string-output-port
           (lambda (p)
             (test/unspec (put-string p "app\x3BB;y"))))
          "app\x3BB;y")
    
    ;; ----------------------------------------
    ;; custom ports

    (let* ([pos 0]
           [p (make-custom-binary-input-port
               "custom in"
               (lambda (bv start count)
                 (if (= pos 16)
                     0
                     (begin
                       (set! pos (+ 1 pos))
                       (bytevector-u8-set! bv start pos)
                       1)))
               (lambda () pos)
               (lambda (p) (set! pos p))
               (lambda () 'ok))])
      (test (port-has-port-position? p) #t)
      (test (port-has-set-port-position!? p) #t)
      (test (port-position p) 0)
      (test (get-bytevector-n p 3) #vu8(1 2 3))
      (test (port-position p) 3)
      (test (lookahead-u8 p) 4)
      (test (lookahead-u8 p) 4)
      (test (port-position p) 3)
      (test/unspec (set-port-position! p 10))
      (get-bytevector-n p 2)
      (test (get-bytevector-n p 2) #vu8(13 14))
      (test (get-bytevector-n p 2) #vu8(15 16))
      (test (get-bytevector-n p 2) (eof-object))
      (test/unspec (set-port-position! p 2))
      (test (get-bytevector-n p 3) #vu8(3 4 5))
      (test/unspec (close-port p)))

    (test-positions make-custom-binary-input-port)

    (let* ([pos 0]
           [p (make-custom-textual-input-port
               "custom in"
               (lambda (bv start count)
                 (if (= pos 16)
                     0
                     (begin
                       (set! pos (+ 1 pos))
                       (string-set! bv start (integer->char (+ 96 pos)))
                       1)))
               (lambda () pos)
               (lambda (p) (set! pos p))
               (lambda () 'ok))])
      (test/unspec (port-position p))
      (test (get-string-n p 3) "abc")
      (test (lookahead-char p) #\d)
      (test (lookahead-char p) #\d)
      (test (get-string-n p 7) "defghij")
      (get-string-n p 2)
      (test (get-string-n p 2) "mn")
      (test (get-string-n p 2) "op")
      (test (get-string-n p 2) (eof-object))
      (test/unspec (close-port p)))

    ;; textual port positions are hopelessly broken in R6RS
    #;(test-positions make-custom-textual-input-port)
    
    (let* ([accum '()]
           [p (make-custom-binary-output-port
               "custom out"
               (lambda (bv start count)
                 (let ([bv2 (make-bytevector count)])
                   (bytevector-copy! bv start bv2 0 count)
                   (set! accum (append
                                (reverse (bytevector->u8-list bv2))
                                accum))
                   count))
               (lambda () (length accum))
               (lambda (pos) (set! accum (list-tail accum (- (length accum) pos))))
               (lambda () 'ok))])
      (test (port-has-port-position? p) #t)
      (test (port-has-set-port-position!? p) #t)
      (test (port-position p) 0)
      (test/unspec (put-bytevector p #vu8(2 4 6)))
      (flush-output-port p)
      (test accum '(6 4 2))
      (test (port-position p) 3)
      (test/unspec (set-port-position! p 2))
      (test (port-position p) 2)
      (test accum '(4 2))
      (test/unspec (put-bytevector p #vu8(3 7 9 11) 2 1))
      (flush-output-port p)
      (test accum '(9 4 2))
      (test/unspec (close-port p)))

    (test-positions make-custom-binary-output-port)

    (let* ([accum '()]
           [p (make-custom-textual-output-port
               "custom out"
               (lambda (str start count)
                 (let ([str (substring str start count)])
                   (set! accum (append
                                (reverse (string->list str))
                                accum))
                   count))
               (lambda () (length accum))
               (lambda (pos) (set! accum (list-tail accum (- (length accum) pos))))
               (lambda () 'ok))])
      (test (port-has-port-position? p) #t)
      (test (port-has-set-port-position!? p) #t)
      (test (port-position p) 0)
      (test/unspec (put-string p "ab"))
      (test (port-position p) 2)
      (test/unspec (put-string p "c"))
      (flush-output-port p)
      (test accum '(#\c #\b #\a))
      (test (port-position p) 3)
      (test/unspec (set-port-position! p 2))
      (test (port-position p) 2)
      (test accum '(#\b #\a))
      (test/unspec (put-string p "xyzw" 2 1))
      (flush-output-port p)
      (test accum '(#\z #\b #\a))
      (test/unspec (close-port p)))

    ;; textual port positions are hopelessly broken in R6RS
    #;(test-positions make-custom-textual-output-port)

    (let* ([save #f]
           [p (make-custom-binary-input/output-port
               "custom in"
               (lambda (bv start end)
                 (bytevector-u8-set! bv start 7)
                 1)
               (lambda (bv start end)
                 (set! save (bytevector-u8-ref bv start))
                 1)
               #f #f #f)])
      (test/unspec (put-u8 p 10))
      (flush-output-port p)
      (test save 10)
      (test (get-u8 p) 7)
      (close-port p))
    
    (test-positions (lambda (id r/w get set close)
                      (make-custom-binary-input/output-port
                       id r/w r/w get set close)))

    (let* ([save #f]
           [p (make-custom-textual-input/output-port
               "custom in"
               (lambda (str start end)
                 (string-set! str start #\!)
                 1)
               (lambda (str start end)
                 (set! save (string-ref str start))
                 1)
               #f #f #f)])
      (test/unspec (put-char p #\q))
      (flush-output-port p)
      (test save #\q)
      (test (get-char p) #\!)
      (close-port p))
    
    ;; textual port positions are hopelessly broken in R6RS
    #;(test-positions (lambda (id r/w get set close)
                        (make-custom-textual-input/output-port
                         id r/w r/w get set close)))

    ;; ----------------------------------------
    ;; stdin, stderr, stdout

    (let ([p (standard-input-port)])
      (test (input-port? p) #t)
      (test (output-port? p) #f)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test/unspec (close-port p)))

    (let ([p (standard-output-port)])
      (test (input-port? p) #f)
      (test (output-port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test/unspec (close-port p)))

    (let ([p (standard-error-port)])
      (test (input-port? p) #f)
      (test (output-port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test/unspec (close-port p)))

    (test (input-port? (current-input-port)) #t)
    (test (output-port? (current-input-port)) #f)
    (test (binary-port? (current-input-port)) #f)
    (test (textual-port? (current-input-port)) #t)

    (test (input-port? (current-output-port)) #f)
    (test (output-port? (current-output-port)) #t)
    (test (binary-port? (current-output-port)) #f)
    (test (textual-port? (current-output-port)) #t)

    ;; ----------------------------------------

    (test-rw 10)
    (test-rw 10.0)
    (test-rw 1/2)
    (test-rw 1+2i)
    (test-rw 1+2.0i)
    (test-rw #t)
    (test-rw #f)
    (test-rw 'apple)
    (test-rw (string->number "app\x3BB;e"))
    (test-rw (string->symbol " "))
    (test-rw (string->symbol "+"))
    (test-rw (string->symbol "0"))
    (test-rw (string->symbol "app\x1678;e"))
    (test-rw 'a1)
    (test-rw '->)
    (test-rw '...)
    (test-rw "apple")
    (test-rw "app\x3BB;e")
    (test-rw "app\x1678;e")
    (test-rw "\r\n")
    (test-rw #\a)
    (test-rw #\x3BB)
    (test-rw #\nul)
    (test-rw #\alarm)
    (test-rw #\backspace)
    (test-rw #\tab)
    (test-rw #\linefeed)
    (test-rw #\newline)
    (test-rw #\vtab)
    (test-rw #\page)
    (test-rw #\return)
    (test-rw #\esc)
    (test-rw #\space)
    (test-rw #\delete)
    (test-rw #\xFF)
    (test-rw #\x00006587)
    (test-rw #\x10FFFF)
    (test-rw #\x1678)
    (test-rw #vu8())
    (test-rw #vu8(1 2 3))
    (test-rw '#(a))
    (test-rw '#())
    (test-rw '#(a 1/2 "str" #vu8(1 2 7)))

    ;; ----------------------------------------
    
    ;;
    ))
