#!r6rs

(library (tests r6rs io ports)
  (export run-io-ports-tests)
  (import (rnrs)
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
         (test (let ([v (string->bytevector "app\x03BB;e"
                                            (make-transcoder (utf-16-codec)))])
                 ;; Could be LE or BE:
                 (if (= (bytevector-u8-ref v 0) #xFE)
                     v
                     (if (equal? v #vu8(#xFF #xFE 97 0 112 0 112 0 #xBB #x3 101 0))
                         #vu8(#xFE #xFF 0 97 0 112 0 112 #x3 #xBB 0 101)
                         v)))
               #vu8(#xFE #xFF 0 97 0 112 0 112 #x3 #xBB 0 101))
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
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'block)])
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'none)])
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
        (if (= b1 #xFE)
            (begin
              (test (get-u8 p) #xFF)
              (test (get-u8 p) 0)
              (test (get-u8 p) 97))
            (begin
              (test b1 #xFF)
              (test (get-u8 p) #xFE)
              (test (get-u8 p) 97)
              (test (get-u8 p) 0)))))

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
               (put-string p str)
               (close-port p))
             (let ([p (open-file-input-port "io-tmp1")])
               (dynamic-wind
                   (lambda () 'ok)
                   (lambda () (get-bytevector-all p))
                   (lambda () (close-port p)))))])
      (test-transcoders bytevector->string-via-file
                        string->bytevector-via-file))
    
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
        (test/unspec (get-bytevector-n! p bv 1 7))
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
      

    ;; ----------------------------------------
    
    ;;
    )

  #;(run-io-ports-tests)
  #;(report-test-results)
  )

