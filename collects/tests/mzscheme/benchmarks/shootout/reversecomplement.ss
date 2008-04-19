
#lang scheme/base
(require scheme/cmdline)

(define translation (make-vector 128))

(for-each (lambda (from-to)
            (let ([char (lambda (sym)
                          (string-ref (symbol->string sym) 0))])
              (let ([from (char (car from-to))]
                    [to (char->integer (char-upcase (char (cadr from-to))))])
                (vector-set! translation (char->integer from) to)
                (vector-set! translation (char->integer (char-upcase from)) to))))
          '([a t]
            [c g]
            [g c]
            [t a]
            [u a]
            [m k]
            [r y]
            [w w]
            [s s]
            [y R]
            [k M]
            [v b]
            [h d]
            [d h]
            [b v]
            [n n]))

(define (output lines)
  (let* ([str (apply bytes-append lines)]
         [o (current-output-port)]
         [len (bytes-length str)])
    (for ([offset (in-range 0 len 60)])
      (write-bytes str o offset (min len (+ offset 60)))
      (newline o))))

(let ([in (current-input-port)])
  (let loop ([accum null])
    (let ([l (read-bytes-line in)])
      (if (eof-object? l)
          (output accum)
          (cond
           [(regexp-match? #rx#"^>" l)
            (output accum)
            (printf "~a\n" l)
            (loop null)]
           [else
            (let* ([len (bytes-length l)]
                   [dest (make-bytes len)])
              (for ([i (in-range len)])
                (bytes-set! dest
                            (- (- len i) 1)
                            (vector-ref translation (bytes-ref l i))))
              (loop (cons dest accum)))])))))
