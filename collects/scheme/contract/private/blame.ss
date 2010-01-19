#lang scheme/base

(require unstable/srcloc scheme/pretty setup/main-collects)

(provide blame?
         make-blame
         blame-source
         blame-guilty
         blame-innocent
         blame-contract
         blame-value
         blame-positive
         blame-negative
         blame-swapped?
         blame-swap

         raise-blame-error
         current-blame-format
         (struct-out exn:fail:contract:blame))

(define-struct blame
  [source value contract positive negative swapped?]
  #:transparent)

(define (blame-guilty b)
  (if (blame-swapped? b)
    (blame-negative b)
    (blame-positive b)))

(define (blame-innocent b)
  (if (blame-swapped? b)
    (blame-positive b)
    (blame-negative b)))

(define (blame-swap b)
  (struct-copy blame b [swapped? (not (blame-swapped? b))]))

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
         [guilty-message (show (blame-guilty b))]
         [contract-message (show (blame-contract b))]
         [value-message (if (blame-value b)
                          (format " on ~a" (show (blame-value b)))
                          "")])
    (format "~a~a broke the contract ~a~a; ~a"
            source-message
            guilty-message
            contract-message
            value-message
            custom-message)))

(define (show v)
  (let* ([line
          (parameterize ([pretty-print-columns 'infinity])
            (pretty-format v))])
    (if (< (string-length line) 30)
      line
      (parameterize ([pretty-print-print-line show-line-break]
                     [pretty-print-columns 50])
        (pretty-format v)))))

(define (show-line-break line port len cols)
  (newline port)
  (if line
    (begin (display "  " port) 2)
    0))

(define current-blame-format
  (make-parameter default-blame-format))
