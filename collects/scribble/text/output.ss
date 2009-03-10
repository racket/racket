#lang scheme/base

(require scheme/promise)

(provide output verbatim unverbatim prefix)

(define (output x [p (current-output-port)])
  (define (getcol) (let-values ([(line col pos) (port-next-location p)]) col))
  (port-count-lines! p)
  ;; pfx can be a column number, or a byte-string, or #f for nothing at all
  (let loop ([x x] [pfx (getcol)])
    ;; new can be a new target column number or an additional prefix to add (a
    ;; string or a byte string)
    (define (combine-pfx pfx new)
      (and pfx new
           (if (number? pfx)
             (if (number? new)
               ;; new target column
               (max pfx new)
               ;; add a prefix to existing column
               (bytes-append (make-spaces pfx)
                             (if (string? new) (string->bytes/utf-8 new) new)))
             (if (number? new)
               ;; add spaces to get to the target column after
               (let ([cur (bytes-length pfx)])
                 (if (new . > . cur)
                   (bytes-append pfx (make-spaces (- new cur)))
                   pfx))
               ;; append prefixes
               (bytes-append pfx (if (string? new)
                                   (string->bytes/utf-8 new)
                                   new))))))
    ;; used to output strings and byte strings, where each internal newline
    ;; should be followed by the prefix
    (define (do-string write get-length nl-rx)
      (define len (get-length x))
      (define ms (and pfx (or (bytes? pfx) (pfx . > . 0)) (len . > . 0)
                      (regexp-match-positions* nl-rx x)))
      (if (pair? ms)
        (let ([pfx (if (bytes? pfx) pfx (make-spaces pfx))])
          (let loop ([start 0] [ms ms])
            (let ([i (cdar ms)])
              (write x p start i)
              (when (< i len)
                (write-bytes pfx p)
                (if (null? (cdr ms))
                  (write x p i)
                  (loop i (cdr ms)))))))
        (write x p)))
    (cond
      ;; no output for these
      [(or (void? x) (not x) (null? x)) (void)]
      ;; for lists and pairs the indentation at the beginning is used, then
      ;; output the contents recursively
      [(pair? x) (let ([pfx (combine-pfx pfx (getcol))])
                   (if (list? x)
                     (for ([x (in-list x)]) (loop x pfx))
                     (begin (loop (car x) pfx) (loop (cdr x) pfx))))]
      ;; delayed values
      [(and (procedure? x) (procedure-arity-includes? x 0)) (loop (x) pfx)]
      [(promise? x) (loop (force x) pfx)]
      ;; special output wrappers
      [(special? x)
       (let ([c (special-contents x)])
         (case (special-flag x)
           [(verbatim)   (loop c #f)]
           [(unverbatim) (loop c (getcol))]
           [(prefix)
            (let ([pfx (combine-pfx (combine-pfx pfx (getcol)) (car c))])
              ;; could also do: (loop (cdr c) pfx), but save time
              (for ([x (in-list (cdr c))]) (loop x pfx)))]
           [else (error 'output "unknown special value flag: ~e"
                        (special-flag x))]))]
      ;; the rest will cause some output, so show the prefix and go
      [else (when pfx
              (let ([cur (getcol)])
                (if (number? pfx)
                  ;; number: add spaces to get to that column
                  (let ([n (- pfx cur)])
                    (when (> n 0) (write-bytes (make-spaces n) p)))
                  ;; prefix: omit characters from the prefix that we went past
                  (cond [(zero? cur) (write-bytes pfx p)]
                        [(< cur (bytes-length pfx)) (write-bytes pfx p cur)]))))
            (cond
              ;; strings output indentation in internal newlines too
              [(string? x) (do-string write-string string-length #rx"\n")]
              [(bytes?  x) (do-string write-bytes  bytes-length  #rx#"\n")]
              ;; additional values that are displayed as usual
              [(symbol? x) (display x p)]
              [(char? x)   (write-char x p)]
              [(number? x) (write x p)]
              ;; useful to represent attributes with keywords (same as symbols)
              [(keyword? x) (write-string (keyword->string x) p)]
              ;; generic fallback: throw an error
              [else (error 'output "don't know how to render value: ~v" x)])]))
  (void))

(define-struct special (flag contents))

(define (verbatim   . contents) (make-special 'verbatim   contents))
(define (unverbatim . contents) (make-special 'unverbatim contents))
(define (prefix pfx . contents) (make-special 'prefix (cons pfx contents)))

(define make-spaces
  (let ([t (make-hasheq)])
    (lambda (n)
      (or (hash-ref t n #f)
          (let ([spaces (make-bytes n 32)]) (hash-set! t n spaces) spaces)))))
