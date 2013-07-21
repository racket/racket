;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

(: translation (Vectorof Integer))
(define translation (make-vector 128))

(for: ([from : Char (in-string "ACGTUMRWSYKVHDBN")]
       [to   : Char (in-string "TGCAAKYWSRMBDHVN")])
  (let ([to (char->integer to)])
    (vector-set! translation (char->integer from) to)
    (vector-set! translation (char->integer (char-downcase from)) to)))

(define I (current-input-port))
(define O (current-output-port))

(define marker (char->integer #\>))

(require racket/require (for-syntax racket/base)
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define line-length 60)
(define buf-size (* 64 1024))
(define out-buf ; so there's always enough room for newlines
  (make-bytes (+ buf-size 1 (quotient buf-size line-length))))
(define LF (char->integer #\newline))

#|
The basic idea is to read the input in chunks, and keep pointers to
them, then on output process each chunk to translate and reverse it
before dumping it out.
|#

(define-type Chunk (Vector Integer Integer Bytes))

(: output ((Listof Chunk) -> Void))
(define (output chunks)
  (let: loop : Void
        ([chunks : (Listof Chunk) chunks] [col : Integer line-length])
    (when (pair? chunks)
      (let ([chunk (car chunks)])
        (let: ([start  : Integer (vector-ref chunk 0)]
               [end    : Integer (vector-ref chunk 1)]
               [in-buf : Bytes   (vector-ref chunk 2)])
          (let: chunk-loop : Void
                ([i : Integer end] [j : Integer 0] [col : Integer col])
            (if (fx> i start)
              (let* ([i (fx- i 1)] [b (bytes-ref in-buf i)])
                (if (fx= b LF)
                  (chunk-loop i j col)
                  (let ([b (vector-ref translation b)])
                    (if (fx= 0 col)
                      (begin (bytes-set! out-buf j LF)
                             (bytes-set! out-buf (fx+ j 1) b)
                             (chunk-loop i (fx+ j 2) (fx- line-length 1)))
                      (begin (bytes-set! out-buf j b)
                             (chunk-loop i (fx+ j 1) (fx- col 1)))))))
              (begin (write-bytes out-buf O 0 j)
                     (loop (cdr chunks) col))))))))
  (newline O))

(define-syntax case-regexp-posns
  (syntax-rules (=> else)
    [(_ rx buf start [id B1 ...] [else B2 ...])
     (let ([m (regexp-match-positions rx buf start)])
       (if m (let ([id (assert (car m))]) B1 ...) (begin B2 ...)))]))

(let ([m (regexp-match #rx"^([^\n]+)\n" I)]) (display (car (assert m))))

(let: loop : Void
      ([buf : (U Bytes EOF) (read-bytes buf-size I)]
       [start : Integer 0]
       [chunks : (Listof Chunk) '()])
  (if (eof-object? buf)
    (begin (output chunks) (void))
    (case-regexp-posns #rx">" buf start
      [p1 (output (cons (ann (vector start (car p1) buf)
                             (Vector Integer Integer Bytes))
                        chunks))
          (case-regexp-posns #rx"\n" buf (cdr p1)
            [p2 (write-bytes buf O (car p1) (cdr p2))
                (loop buf (cdr p2) '())]
            [else (write-bytes buf O (car p1))
                  (let header-loop ()
                    (let ([buf (assert (read-bytes buf-size I) bytes?)])
                      (case-regexp-posns #rx"\n" buf 0
                        [p2 (write-bytes buf O 0 (cdr p2))
                            (loop buf (cdr p2) '())]
                        [else (write-bytes buf O) (header-loop)])))])]
      [else (loop (read-bytes buf-size I) 0
                  (cons (ann (vector start (bytes-length buf) buf)
                             (Vector Integer Integer Bytes))
                        chunks))])))
