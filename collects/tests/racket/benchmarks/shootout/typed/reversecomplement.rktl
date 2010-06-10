;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

(require scheme/cmdline)

(define translation (make-vector 128))

(for: : Void
      ([from-to : (List Symbol Symbol)
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
                  [n n])])
      (let ([char (lambda: ((sym : Symbol))
                           (string-ref (symbol->string sym) 0))])
        (let ([from (char (car from-to))]
              [to (char->integer (char-upcase (char (cadr from-to))))])
          (vector-set! translation (char->integer from) to)
          (vector-set! translation (char->integer (char-upcase from)) to))))

(: output ((Listof Bytes) -> Void))
(define (output lines)
  (let*: ([str : Bytes (apply bytes-append lines)]
          [o : Output-Port (current-output-port)]
          [len : Natural (bytes-length str)])
    (for: : Void
          ([offset : Natural (in-range 0 len 60)])
          (write-bytes str o offset (min len (+ offset 60)))
          (newline o))))

(let ([in (current-input-port)])
  (let: loop : Void ([accum : (Listof Bytes) null])
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
