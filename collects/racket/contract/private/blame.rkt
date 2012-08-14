#lang racket/base
(require syntax/srcloc racket/pretty setup/path-to-relative)

(provide blame?
         (rename-out [-make-blame make-blame])
         blame-source
         blame-positive
         blame-negative
         blame-contract
         blame-value
         blame-original?
         blame-swapped?
         blame-swap
         blame-replace-negative ;; used for indy blame
         blame-add-context
         blame-add-unknown-context
         blame-context 
         
         raise-blame-error
         current-blame-format
         (struct-out exn:fail:contract:blame)
         blame-fmt->-string)

(define (blame=? a b equal?/recur)
  (and (equal?/recur (blame-source a) (blame-source b))
       (equal?/recur (blame-value a) (blame-value b))
       (equal?/recur (blame-contract a) (blame-contract b))
       (equal?/recur (blame-positive a) (blame-positive b))
       (equal?/recur (blame-negative a) (blame-negative b))
       (equal?/recur (blame-original? a) (blame-original? b))))

(define (blame-hash b hash/recur)
  (bitwise-xor (hash/recur (blame-source b))
               (hash/recur (blame-value b))
               (hash/recur (blame-contract b))
               (hash/recur (blame-positive b))
               (hash/recur (blame-negative b))
               (hash/recur (blame-original? b))))

(define-struct blame
  [source value build-name positive negative original? context top-known? important]
  #:property prop:equal+hash
  (list blame=? blame-hash blame-hash))

(define -make-blame
  (let ([make-blame
         (λ (source value build-name positive negative original?)
           (make-blame source value build-name positive negative original? '() #t #f))])
    make-blame))

;; s : (or/c string? #f)
(define (blame-add-context b s #:important [important #f] #:swap? [swap? #f])
  (struct-copy
   blame b
   [original? (if swap? (not (blame-original? b)) (blame-original? b))]
   [positive (if swap? (blame-negative b) (blame-positive b))]
   [negative (if swap? (blame-positive b) (blame-negative b))]
   [important (or important (blame-important b))]
   [context (if s (cons s (blame-context b)) (blame-context b))]
   [top-known? #t]))

(define (blame-add-unknown-context b)
  (define old (blame-context b))
  (struct-copy
   blame b
   [top-known? #f]
   [context (if (blame-top-known? b)
                (blame-context b)
                (cons "..." (blame-context b)))]))

(define (blame-contract b) ((blame-build-name b)))

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

(define (raise-blame-error blame x fmt . args)
  (raise
   (make-exn:fail:contract:blame
    ((current-blame-format) 
     blame x 
     (apply format (blame-fmt->-string blame fmt) args))
    (current-continuation-marks)
    blame)))

(define (blame-fmt->-string blame fmt)
  (cond
    [(string? fmt) fmt]
    [else
     (let loop ([strs fmt]
                [so-far '()]
                [last-ended-in-whitespace? #t])
       (cond
         [(null? strs)
          (apply string-append (reverse so-far))]
         [else
          (define fst (car strs))
          (define (add-indent s)
            (if (null? so-far)
                s
                (string-append "\n " s)))
          (define nxt
            (cond
              [(eq? 'given: fst) (add-indent
                                  (if (blame-original? blame)
                                      "produced:"
                                      "given:"))]
              [(eq? 'given fst) (if (blame-original? blame)
                                    "produced"
                                    "given")]
              [(eq? 'expected: fst) (add-indent
                                     (if (blame-original? blame)
                                         "promised:"
                                         "expected:"))]
              [(eq? 'expected fst) (if (blame-original? blame)
                                       "promised"
                                       "expected")]
              [else fst]))
          (define new-so-far
            (if (or last-ended-in-whitespace?
                    (regexp-match #rx"^[\n ]" nxt))
                (cons nxt so-far)
                (list* nxt " " so-far)))
          (loop (cdr strs)
                new-so-far
                (regexp-match #rx" $" nxt))]))]))

(define (default-blame-format blme x custom-message)
  (define source-message (source-location->string (blame-source blme)))
  (define positive-message (show/display (convert-blame-party (blame-positive blme))))
  
  (define context (blame-context blme))
  (define context-lines (if (null? context)
                            #f
                            (apply string-append 
                                   (for/list ([context (in-list context)]
                                              [n (in-naturals)])
                                     (format (if (zero? n)
                                                 " in: ~a\n"
                                                 "     ~a\n")
                                             context)))))
  (define contract-line (show/write (blame-contract blme) #:alone? #t))
  (define at-line (if (string=? source-message "")
                      #f
                      (format " at: ~a" source-message)))
  
  (define self-or-not (if (blame-original? blme)
                          "broke its contract"
                          "contract violation"))
  
  (define start-of-message
    (cond
      [(blame-important blme)
       (format "~a: ~a" (blame-important blme) self-or-not)]
      [(blame-value blme)
       (format "~a: ~a" (blame-value blme) self-or-not)]
      [else
       (format "~a:" self-or-not)]))
  
  (define blaming-line (format " blaming: ~a" positive-message))
  
  (define from-line 
    (if (blame-original? blme)
        (format " contract from: ~a" positive-message)
        (let ([negative-message (show/display (convert-blame-party (blame-negative blme)))])
          (format " contract from: ~a" negative-message))))
  
  (combine-lines
   start-of-message
   (format " ~a"  custom-message)
   context-lines
   (if context-lines
       contract-line
       (string-append
        " in:" 
        (substring contract-line 5 (string-length contract-line))))
   from-line
   blaming-line
   at-line))

;; combine-lines : (->* #:rest (listof (or/c string? #f))) string?)
;; combines each of 'lines' into a single message, dropping #fs,
;; and otherwise guaranteeing that each string is on its own line,
;; with no ending newline.
(define (combine-lines . lines)
  (regexp-replace
   #rx"\n$"
   (apply 
    string-append
    (for/list ([line (in-list lines)]
               #:when (string? line))
      (if (regexp-match #rx"\n$" line)
          line
          (string-append line "\n"))))
   ""))

(define ((show f) v #:alone? [alone? #f])
  (let* ([line
          (parameterize ([pretty-print-columns 'infinity])
            (f v))])
    (if (< (string-length line) 30)
        (cond
          [alone? (string-append spacer line)]
          [else line])
        (parameterize ([pretty-print-print-line (show-line-break alone?)]
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

(define (convert-blame-party x)
  (cond
    [(path? x) (path->relative-string/library x)]
    [else x]))

(define show/display (show pretty-format/display))
(define show/write (show pretty-format/write))


(define ((show-line-break alone?) line port len cols)
  (if alone?
      (unless (equal? line 0) (newline port))
      (newline port))
  (if line
      (begin (display spacer port) 6)
      0))

(define spacer "      ")

(define current-blame-format
  (make-parameter default-blame-format))

