#lang scheme/base
(require (for-syntax mzlib/inflate
                     scheme/base))

(provide decode)

(define-syntax (decode stx)
  (syntax-case stx ()
    [(_ arg ...)
     (andmap identifier? (syntax->list (syntax (arg ...))))
     (let ()
       (define (decode-sexp str)
         (let* ([loc 
                 (let loop ([chars (string->list str)])
                   (cond
                     [(null? chars) '()]
                     [(null? (cdr chars)) (error 'to-sexp "missing digit somewhere")]
                     [else (let ([fst (to-digit (car chars))]
                                 [snd (to-digit (cadr chars))])
                             (cons
                              (+ (* fst 16) snd)
                              (loop (cddr chars))))]))])
           (let-values ([(p-in p-out) (make-pipe)])
             (inflate (open-input-bytes (apply bytes loc)) p-out)
             (read p-in))))
       
       (define (to-digit char)
         (cond
           [(char<=? #\0 char #\9) 
            (- (char->integer char)
               (char->integer #\0))]
           [(char<=? #\a char #\f) 
            (+ 10 (- (char->integer char)
                     (char->integer #\a)))]))
       
       (define decoded
         (decode-sexp 
          (apply 
           string-append
           (map (λ (x) (symbol->string (syntax-e x)))
                (syntax->list (syntax (arg ...)))))))
       
       (datum->syntax stx decoded stx))]))
