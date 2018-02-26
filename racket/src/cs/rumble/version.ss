
(define-syntax (extract-version-string stx)
  (chez:call-with-input-file
   "../racket/src/schvers.h"
   (lambda (i)
     (let ([to-find "#define MZSCHEME_VERSION \""])
       (let loop ([pos 0])
         (cond
          [(= pos (string-length to-find))
           (list->string
            (let loop ()
              (let ([ch (chez:read-char i)])
                (if (char=? ch #\")
                    '()
                    (cons ch (loop))))))]
          [else
           (let ([ch (chez:read-char i)])
             (cond
              [(char=? ch (string-ref to-find pos))
               (loop (add1 pos))]
              [else
               (loop 0)]))]))))))

(define (version) (extract-version-string))
(define (banner) (string-append "Welcome to Racket " (version) "\n"))
