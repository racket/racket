#lang at-exp racket

(define (write-to-file input)
  (define file (make-temporary-file))
  (with-output-to-file file
                       #:mode 'text
                       #:exists 'truncate
                       (lambda () (printf input)))
  file)
  
(define (execute-racket file)
  (match-define [list output input id error-port status]
                (process (format "racket ~a" file)))
  (status 'wait)
  (when (not (= 0 (status 'exit-code)))
    (printf "Error: ~a\n" (read-string 1024 error-port))
    (error 'run "couldn't run racket. error code ~a" (status 'exit-code)))
  (define result (read-string 4096 output))
  (close-input-port output)
  (close-input-port error-port)
  (close-output-port input)
  (delete-file file)
  result)

(define (run-honu input)
  (define file (write-to-file input))
  (with-handlers ([exn? (lambda (e)
                          (when (file-exists? file)
                            (delete-file file))
                          (raise e))])
    (execute-racket file)))

(define (same? actual expected)
  ;; (printf "Expected \n'~a'\n\ngot \n'~a'\n\n" expected actual)
  (string=? actual expected))

(define (output . stuff)
  ;; (printf "output '~a'\n" stuff)
  (apply string-append "" (append stuff (list "\n"))))

(define (test input output)
  (same? (run-honu input) output))

(define (input . stuff)
  (apply string-append "#lang honu\n" stuff))

(test
  @input{
  5
  6
  }

  @output{5
  6
  })

(test
  @input{
  1 + 1
  }

  @output{2
  })

(test
  @input{
  foo(x){
    x * 2
  }
  foo(5);
  }

  @output{10
  })

(test
  @input{
  var n = 5;
  cond
    n < 10: 'x1,
    n > 10: 'x2;
  }

  @output{'x1
  })

(test
  @input{
  if 2 > 1 then
    1
  else
    0
  }

  @output{1
  })
