#!r6rs

(library (tests r6rs io simple)
  (export run-io-simple-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-io-simple-tests)

    (test/unspec
     (when (file-exists? "io-tmp2")
       (delete-file "io-tmp2")))

    (test/values (call-with-output-file "io-tmp2"
                   (lambda (p)
                     (test (output-port? p) #t)
                     (test (binary-port? p) #f)
                     (test (textual-port? p) #t)
                     (test/unspec (write-char #\q p))
                     (test/unspec (newline p))
                     (test/unspec (display "more" p))
                     (test/unspec (write "last" p))
                     (values 3 4)))
                 3 4)

    (test/values (call-with-input-file "io-tmp2"
                   (lambda (p)
                     (test (input-port? p) #t)
                     (test (binary-port? p) #f)
                     (test (textual-port? p) #t)
                     (test (peek-char p) #\q)
                     (test (read-char p) #\q)
                     (test (read-char p) #\newline)
                     (test (read-char p) #\m)
                     (test (read-char p) #\o)
                     (test (peek-char p) #\r)
                     (test (read-char p) #\r)
                     (test (read-char p) #\e)
                     (test (read p) "last")
                     (test (read p) (eof-object))
                     (values 7 8 9)))
                 7 8 9)

    (test/unspec (delete-file "io-tmp2"))

    (let ([p (open-output-file "io-tmp2")])
      (test (output-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test/unspec (write-char #\! p))
      (test/unspec (close-output-port p)))

    (let ([p (open-input-file "io-tmp2")])
      (test (input-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test (read-char p) #\!)
      (test/unspec (close-input-port p)))

    (test/unspec (delete-file "io-tmp2"))

    (test/values (with-output-to-file "io-tmp2"
                   (lambda ()
                     (test/unspec (write-char #\z))
                     (test/unspec (newline))
                     (test/unspec (display "a"))
                     (test/unspec (write "a"))
                     (values 30 40)))
                 30 40)

    (test/values (with-input-from-file "io-tmp2"
                   (lambda ()
                     (test (peek-char) #\z)
                     (test (read-char) #\z)
                     (test (read) 'a)
                     (test (read) "a")
                     (test (read) (eof-object))
                     (values 70 80 90)))
                 70 80 90)

    (test/unspec
     (when (file-exists? "io-tmp2")
       (delete-file "io-tmp2")))
    
    (test (input-port? (current-input-port)) #t)
    (test (binary-port? (current-input-port)) #f)
    (test (textual-port? (current-input-port)) #t)

    (test (output-port? (current-output-port)) #t)
    (test (binary-port? (current-output-port)) #f)
    (test (textual-port? (current-output-port)) #t)

    (test (output-port? (current-error-port)) #t)
    (test (binary-port? (current-error-port)) #f)
    (test (textual-port? (current-error-port)) #t)

    ;;
    ))
