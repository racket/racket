#lang racket/base
(require syntax/srcloc racket/pretty setup/path-to-relative racket/list)

(provide blame?
         (rename-out [-make-blame make-blame])
         blame-source
         (rename-out [show-blame-positive blame-positive])
         (rename-out [show-blame-negative blame-negative])
         blame-contract
         blame-value
         blame-original?
         blame-swapped?
         blame-swap
         blame-replace-negative ;; used for indy blame
         blame-update ;; used for option contract transfers
         blame-add-context
         blame-add-unknown-context
         blame-context
         
         blame-add-missing-party
         
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
         (lambda (source value build-name positive negative original?)
           (unless (srcloc? source)
             (raise-argument-error 'make-blame "srcloc?" 0
                                   source value build-name positive negative original?))
           (unless (and (procedure? build-name)
                        (procedure-arity-includes? build-name 0))
             (raise-argument-error 'make-blame "(-> any)" 2
                                   source value build-name positive negative original?))
           (unless positive
             (raise-type-error 'make-blame "(not/c #f)" 3
                               source value build-name positive negative original?))
           (make-blame
            source
            value
            build-name
            (list positive)
            (and negative (list negative))
            original?
            '()
            #t 
            #f))])
    make-blame))

;; s : (or/c string? #f)
(define (blame-add-context b s #:important [name #f] #:swap? [swap? #f])
  (define new-original? (if swap? (not (blame-original? b)) (blame-original? b)))
  (define new-context (if s (cons s (blame-context b)) (blame-context b)))
  (struct-copy
   blame b
   [original? new-original?]
   [positive (if swap? (blame-negative b) (blame-positive b))]
   [negative (if swap? (blame-positive b) (blame-negative b))]
   [important (if name (important name new-original?) (blame-important b))]
   [context new-context]
   [top-known? #t]))

(struct important (name sense-swapped?))

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
  (struct-copy blame b [negative (list new-neg)]))

(define (blame-replace-positive b new-pos)
  (struct-copy blame b [positive (list new-pos)]))


(define (blame-update blame-info extra-positive extra-negative)
  (ensure-blame-known 'blame-update blame-info)
  (struct-copy 
   blame 
   blame-info
   [positive (cons extra-positive (blame-positive blame-info))]
   [negative (cons extra-negative (blame-negative blame-info))]))

(define (ensure-blame-known who blame)
  (unless (and (blame-positive blame)
               (blame-negative blame))
    (error who "blame info is not known; positive ~s negative ~s"
           (blame-positive blame)
           (blame-negative blame))))

(define (show-blame accessor blm)
  (define info (accessor blm))
  (cond [(not info) #f]
        [(empty? (rest info)) (first info)]
        [else info]))
(define (show-blame-positive b) (show-blame blame-positive b))
(define (show-blame-negative b) (show-blame blame-negative b))

(define (blame-swapped? b)
  (not (blame-original? b)))

(define-struct (exn:fail:contract:blame exn:fail:contract) [object]
  #:transparent)

(define (raise-blame-error raw-blame x #:missing-party [missing-party #f] fmt . args)
  (unless (blame? raw-blame)
    (apply
     raise-argument-error
     'raise-blame-error
     "blame?" 0
     raw-blame x fmt args))
  (unless (or (string? fmt)
              (and (list? fmt)
                   (ormap (λ (x) (or (symbol? x) (string? x)))
                          fmt)))
    (apply
     raise-argument-error
     'raise-blame-error
     (format "~s" '(or/c string? (list/c (or/c string? symbol?))))
     2
     raw-blame x fmt args))
  
  (define blame 
    ;; if we're not going to blame the missing party,
    ;; don't insist on it being there.
    (cond
      [(and (blame-original? raw-blame)
            (not missing-party))
       raw-blame]
      [else
       (blame-add-missing-party raw-blame missing-party)]))
  
  (raise
   (make-exn:fail:contract:blame
    ((current-blame-format) 
     blame x 
     (apply format (blame-fmt->-string blame fmt) args))
    (current-continuation-marks)
    blame)))

(define (blame-add-missing-party b missing-party)
  (cond
    [(not missing-party) b]
    [(blame-swapped? b)
     (when (blame-positive b)
       (error 'add-missing-party "already have the party: ~s; trying to add ~s" 
              (blame-positive b)
              missing-party))
     (struct-copy blame b
                  [positive (list missing-party)])]
    [else
     (when (blame-negative b)
       (error 'add-missing-party "already have the party: ~s; trying to add ~s" 
              (blame-negative b)
              missing-party))
     (struct-copy blame b
                  [negative (list missing-party)])]))

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
                (string-append "\n  " s)))
          (define nxt
            (cond
              [(eq? 'given: fst) (add-indent
                                  (if (blame/important-original? blame)
                                      "produced:"
                                      "given:"))]
              [(eq? 'given fst) (if (blame/important-original? blame)
                                    "produced"
                                    "given")]
              [(eq? 'expected: fst) (add-indent
                                     (if (blame/important-original? blame)
                                         "promised:"
                                         "expected:"))]
              [(eq? 'expected fst) (if (blame/important-original? blame)
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

(define (blame/important-original? blme)
  (define i (blame-important blme))
  (cond
    [i (equal? (important-sense-swapped? i) (blame-original? blme))]
    [else (blame-original? blme)]))

(define (default-blame-format blme x custom-message)
  
  (unless (blame-positive blme)
    (raise-argument-error 'default-blame-format
                          "a blame object with a non-#f positive field"
                          blme))
  
  (define source-message (source-location->string (blame-source blme)))
  
  (define context (blame-context blme))
  (define context-lines (if (null? context)
                            #f
                            (apply string-append 
                                   (for/list ([context (in-list context)]
                                              [n (in-naturals)])
                                     (format (if (zero? n)
                                                 "  in: ~a\n"
                                                 "      ~a\n")
                                             context)))))
  (define contract-line (show/write (blame-contract blme) #:alone? #t))
  (define at-line (if (string=? source-message "")
                      #f
                      (format "  at: ~a" source-message)))
  
  (define self-or-not
    (if (blame/important-original? blme)
        "broke its contract"
        "contract violation"))
  
  (define start-of-message
    (cond
      [(blame-important blme)
       (format "~a: ~a" (important-name (blame-important blme)) self-or-not)]
      [(blame-value blme)
       (format "~a: ~a" (blame-value blme) self-or-not)]
      [else
       (format "~a:" self-or-not)]))
  
  (define blame-parties (blame-positive blme))
  (define blaming-line
    (cond
      [(null? (cdr blame-parties))
       (format "  blaming: ~a" (convert-blame-singleton (car blame-parties)))]
      [else
       (apply
        string-append 
        "  blaming multiple parties:"
        (for/list ([party (in-list blame-parties)])
          (format "\n  ~a" (convert-blame-singleton party))))]))
  
  (define on-line
    (and (blame-important blme)
         (blame-value blme)
         (format "  contract on: ~a" (blame-value blme))))
  
  (define from-line 
    (if (blame-original? blme)
        (let ([from-positive-message 
               (show/display
                (from-info (blame-positive blme)))])
          (format "  contract from: ~a" from-positive-message))
        (let ([from-negative-message 
               (show/display
                (from-info (blame-negative blme)))])
          (format "  contract from: ~a" from-negative-message))))
  
  (combine-lines
   start-of-message
   (format "  ~a"  custom-message)
   context-lines
   (if context-lines
       contract-line
       (string-append
        "  in:"
        (substring contract-line 5 (string-length contract-line))))
   from-line
   on-line
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

(define (convert-blame-singleton x)
  (cond
    [(path? x) (path->relative-string/library x)]
    [else x]))


(define (from-info x)
  (convert-blame-singleton (last x))) 


(define (convert-blame-party x)
  (let ((preface 
         (cond [(< 1 (length x))
                " -- multiple blame parties due to option contract transfers --\n"]
               [else ""])))
    (string-append 
     preface
     (foldr 
      (λ (fst rst) (string-append (format "~a\n" (convert-blame-singleton fst)) rst))
      ""
      x)))) 

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

