#lang racket/base

(provide copy-port-bytes
         open-input-bytes/no-copy
         open-input-string/lazy)

;; Copy up to `n` bytes from `in` to `out`, where
;; #f for `n` means copy to end-of-file
(define (copy-port-bytes in out n)
  (define bstr (make-bytes (min 4096 (or n 4096))))
  (define (copy got expect)
    (cond
     [(eof-object? got) #f]
     [else
      (when out
        (write-bytes bstr out 0 got))
      (or (and (not n)
               (positive? got))
          (and n
               (= got expect)))]))
  (let loop ([n n])
    (if (and n (n . < . 4096))
        (copy (read-bytes! bstr in 0 n) n)
        (and (copy (read-bytes! bstr in) 4096)
             (loop (and n (- n 4096)))))))

;; Similar to `open-input-bytes`, but never copies the
;; argument, and more efficienyl handles a start and
;; end range
(define (open-input-bytes/no-copy bstr pos end)
  (define (fill! dest-bstr skip)
    (define pos+skip (+ pos skip))
    (cond
     [(pos+skip . >= . end) eof]
     [else
      (define len (min (bytes-length dest-bstr)
                       (- end pos+skip)))
      (bytes-copy! dest-bstr 0 bstr pos+skip (+ pos+skip len))
       len]))
  (make-input-port
   'bytes
   (lambda (dest-bstr)
     (define len (fill! dest-bstr 0))
     (unless (eof-object? len)
       (set! pos (+ len pos)))
     len)
   (lambda (dest-bstr skip evt)
     (fill! dest-bstr skip))
   void))

;; Similar to `open-input-string`, but lazily decodes
;; a range of the string
(define (open-input-string/lazy str pos end)
  (define bstr (make-bytes 64))
  (define bstr-pos 0)
  (define bstr-end 0)
  (define (fill! dest-bstr skip)
    (define bstr-pos+skip (+ bstr-pos skip))
    (when (bstr-pos+skip . >= . bstr-end)
      ;; Try to decode more
      (decode-more! (add1 bstr-pos+skip)))
    (cond
     [(bstr-pos+skip . >= . bstr-end) eof]
     [else
      (define len (min (bytes-length dest-bstr)
                       (- bstr-end bstr-pos+skip)))
      (bytes-copy! dest-bstr 0 bstr bstr-pos+skip (+ bstr-pos+skip len))
      len]))
  (define (decode-more! target-pos)
    (cond
     [(= pos end) (void)]
     [else
      (define len (min 64 (- end pos)))
      ;; We could use the decoder interface here to
      ;; avoid byte-string allocations, but we expect
      ;; that savings to be in the noise:
      (define new-bstr
        (string->bytes/utf-8 str 0 pos (+ pos len)))
      (set! pos (+ len pos))
      (define new-len (bytes-length new-bstr))
      (when ((- (bytes-length bstr) bstr-end) . < . new-len)
        (define bstr2 (make-bytes (max (* (bytes-length bstr) 2)
                                       (+ bstr-end new-len))))
        (bytes-copy! bstr2 0 bstr 0 bstr-end)
        (set! bstr bstr2))
      (bytes-copy! bstr bstr-end new-bstr)
      (set! bstr-end (+ bstr-end new-len))
      (when (bstr-end . < . target-pos)
        (decode-more! target-pos))]))
  (make-input-port
   'string
   (lambda (dest-bstr)
     (define len (fill! dest-bstr 0))
     (unless (eof-object? len)
       (set! bstr-pos (+ bstr-pos len)))
     len)
   (lambda (dest-bstr skip evt)
     (fill! dest-bstr skip))
   void))
