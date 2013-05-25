#lang scheme/base

(require (prefix-in r6rs: rnrs/io/ports-6))

(provide (rename-out [r6rs:eof-object eof-object])
         eof-object?
         (rename-out [r6rs:call-with-input-file call-with-input-file]
                     [r6rs:call-with-output-file call-with-output-file])
         input-port?
         output-port?
         (rename-out [r6rs:current-input-port current-input-port]
                     [r6rs:current-output-port current-output-port]
                     [r6rs:current-error-port current-error-port]
                     [r6rs:with-input-from-file with-input-from-file]
                     [r6rs:with-output-to-file with-output-to-file]
                     [r6rs:open-input-file open-input-file]
                     [r6rs:open-output-file open-output-file])
         close-input-port
         close-output-port
         (rename-out [r6rs:read-char read-char]
                     [r6rs:peek-char peek-char]
                     [r6rs:read read]
                     [r6rs:write-char write-char]
                     [r6rs:newline newline]
                     [r6rs:display display]
                     [r6rs:write write]))

(define (r6rs:call-with-input-file file proc)
  (r6rs:call-with-port
   (r6rs:open-input-file file)
   proc))

(define (r6rs:call-with-output-file file proc)
  (r6rs:call-with-port
   (r6rs:open-output-file file)
   proc))

(define (r6rs:with-input-from-file file proc)
  (let ([p (r6rs:open-input-file file)])
    (begin0
     (parameterize ([current-input-port p])
       (proc))
     (close-input-port p))))

(define (r6rs:with-output-to-file file proc)
  (let ([p (r6rs:open-output-file file)])
    (begin0
     (parameterize ([current-output-port p])
       (proc))
     (close-output-port p))))

(define (r6rs:open-input-file file)
  (r6rs:transcoded-port (r6rs:open-file-input-port file) (r6rs:native-transcoder)))

(define (r6rs:open-output-file file)
  (r6rs:transcoded-port (r6rs:open-file-output-port file) (r6rs:native-transcoder)))

(define (r6rs:read-char [in (r6rs:current-input-port)])
  (r6rs:get-char in))

(define (r6rs:peek-char [in (r6rs:current-input-port)])
  (r6rs:lookahead-char in))

(define (r6rs:read [in (r6rs:current-input-port)])
  (r6rs:get-datum in))

(define (r6rs:write-char ch [out (r6rs:current-output-port)])
  (r6rs:put-char out ch))

(define (r6rs:newline [out (r6rs:current-output-port)])
  (r6rs:put-char out #\newline))

(define (r6rs:display v [out (r6rs:current-output-port)])
  (unless (r6rs:textual-port? out)
    (raise-type-error 'display "textual port" out))
  ;; Should we make mpairs print with parens?
  (display v out))

(define (r6rs:write v [out (r6rs:current-output-port)])
  (r6rs:put-datum out v))
