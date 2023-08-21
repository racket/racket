
(define-syntax (extract-version-string stx)
  (#%call-with-input-file
   (ormap (lambda (dir)
            (let ([p (path-build dir "../version/racket_version.h")])
              (and (#%file-exists? p)
                   p)))
          (source-directories))
   (lambda (i)
     (let ([to-find "#define MZSCHEME_VERSION_"])
       (let numbers-loop ([numbers '()])
         (cond
          [(= 4 (length numbers))
           (cond
            [(not (zero? (car numbers)))
             (#%apply #%format "~a.~a.~a.~a" (reverse numbers))]
            [(not (zero? (cadr numbers)))
             (#%apply #%format "~a.~a.~a" (reverse (cdr numbers)))]
            [else
             (#%apply #%format "~a.~a" (reverse (cddr numbers)))])]
          [else
           (let loop ([pos 0])
             (cond
              [(= pos (string-length to-find))
               (#%read-char i) ; discard "X", "Y", "Z", or "W"
               (#%read-char i) ; discard space
               (let digit-loop ([n 0])
                 (let ([ch (#%read-char i)])
                   (if (char-numeric? ch)
                       (digit-loop (+ (* n 10) (- (char->integer ch) (char->integer #\0))))
                       (numbers-loop (cons n numbers)))))]
              [else
               (let ([ch (#%read-char i)])
                 (cond
                  [(char=? ch (string-ref to-find pos))
                   (loop (add1 pos))]
                  [else
                   (loop 0)]))]))]))))))

(define (version) (extract-version-string))
(define (banner) (string-append "Welcome to Racket v" (version) " [cs].\n"))
