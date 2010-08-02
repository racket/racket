#lang racket/base

(require unstable/srcloc racket/pretty)

(provide blame?
         make-blame
         blame-source
         blame-positive
         blame-negative
         blame-contract
         blame-value
         blame-original?
         blame-swapped?
         blame-swap
         blame-replace-negative ;; used for indy blame

         raise-blame-error
         current-blame-format
         (struct-out exn:fail:contract:blame))

(define (blame=? a b equal?/recur)
  (and (equal?/recur (blame-positive a) (blame-positive b))
       (equal?/recur (blame-negative a) (blame-negative b))
       (equal?/recur (blame-contract a) (blame-contract b))
       (equal?/recur (blame-value a) (blame-value b))
       (equal?/recur (blame-source a) (blame-source b))
       (equal?/recur (blame-original? a) (blame-original? b))))

(define (blame-hash b hash/recur)
  (bitwise-xor (hash/recur (blame-positive b))
               (hash/recur (blame-negative b))
               (hash/recur (blame-contract b))
               (hash/recur (blame-value b))
               (hash/recur (blame-source b))
               (hash/recur (blame-original? b))))

(define-struct blame
  [source value contract positive negative user original?]
  #:property prop:equal+hash
  (list blame=? blame-hash blame-hash))

(define (blame-swap b)
  (struct-copy
   blame b
   [original? (not (blame-original? b))]
   [positive (blame-negative b)]
   [negative (blame-positive b)]))

(define (blame-replace-negative b new-neg)
  (struct-copy blame b [negative new-neg]))

(define (blame-swapped? b)
  (not (blame-original? b)))

(define-struct (exn:fail:contract:blame exn:fail:contract) [object]
  #:transparent)

(define (raise-blame-error b x fmt . args)
  (raise
   (make-exn:fail:contract:blame
    ((current-blame-format) b x (apply format fmt args))
    (current-continuation-marks)
    b)))

(define (default-blame-format b x custom-message)
  (let* ([source-message (source-location->prefix (blame-source b))]
         [positive-message (show/display (blame-positive b))]
         [contract-message (show/write (blame-contract b))]
         [value-message (if (blame-value b)
                          (format " on ~a" (show/display (blame-value b)))
                          "")]
         [user-message (if (or (blame-original? b)
                               (equal? (blame-positive b) (blame-user b)))
                           ""
                           (format " given to ~a" (show/display (blame-user b))))])
    (format "~a~a broke the contract ~a~a~a; ~a"
            source-message
            positive-message
            contract-message
            value-message
            user-message
            custom-message)))

(define ((show f) v)
  (let* ([line
          (parameterize ([pretty-print-columns 'infinity])
            (f v))])
    (if (< (string-length line) 30)
      line
      (parameterize ([pretty-print-print-line show-line-break]
                     [pretty-print-columns 50])
        (f v)))))

(define (pretty-format/display v [columns (pretty-print-columns)])
  (let ([port (open-output-string)])
    (pretty-display v port)
    (get-output-string port)))

(define (pretty-format/write v [columns (pretty-print-columns)])
  (let ([port (open-output-string)])
    (pretty-write v port)
    (get-output-string port)))

(define show/display (show pretty-format/display))
(define show/write (show pretty-format/write))

(define (show-line-break line port len cols)
  (newline port)
  (if line
    (begin (display "  " port) 2)
    0))

(define current-blame-format
  (make-parameter default-blame-format))
