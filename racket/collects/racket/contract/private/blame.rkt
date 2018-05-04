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
         blame-missing-party?
         blame-add-extra-field
         
         raise-blame-error
         current-blame-format
         (struct-out exn:fail:contract:blame)
         blame-fmt->-string

         invariant-assertion-party)

(define invariant-assertion-party (string->uninterned-symbol "invariant-assertion"))

(define (blame=? a b equal?/recur)
  (and (equal?/recur (blame-positive a) (blame-positive b))
       (equal?/recur (blame-negative a) (blame-negative b))
       (equal?/recur (blame-original? a) (blame-original? b))
       (equal?/recur (blame-more-stuff a) (blame-more-stuff b))))

(define (combine-them . args)
  (let loop ([args args])
    (cond
      [(null? (cdr args)) (car args)]
      [else (bitwise-xor (car args) (* 3 (loop (cdr args))))])))

(define (blame-hash b hash/recur)
  (combine-them (hash/recur (blame-positive b))
                (hash/recur (blame-negative b))
                (hash/recur (blame-original? b))
                (hash/recur (blame-more-stuff b))))

;; missing-party? field is #t when the missing party
;; is still missing and it is #f when the missing party
;; has been filled in (or if it was filled in from the start)
(define-struct more-stuff
  [source value build-name context top-known? important missing-party? extra-fields]
  #:transparent)

(define-struct blame
  (positive negative original? [swapped #:mutable] more-stuff)
  #:property prop:equal+hash
  (list blame=? blame-hash blame-hash))

(define (blame-source b) (more-stuff-source (blame-more-stuff b)))
(define (blame-value b) (more-stuff-value (blame-more-stuff b)))
(define (blame-context b) (more-stuff-context (blame-more-stuff b)))
(define (blame-top-known? b) (more-stuff-top-known? (blame-more-stuff b)))
(define (blame-important b) (more-stuff-important (blame-more-stuff b)))
(define (blame-missing-party? b) (more-stuff-missing-party? (blame-more-stuff b)))
(define (blame-contract b) ((more-stuff-build-name (blame-more-stuff b))))
(define (blame-extra-fields b) (more-stuff-extra-fields (blame-more-stuff b)))

(define -make-blame
  (let ([make-blame
         (λ (source value build-name positive negative original?
                    #:track-context? [track-context? #t])
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
           (define build/memo-name
             (let* ([uniq (box #f)]
                    [ans uniq])
               (λ ()
                 (when (eq? uniq ans)
                   (set! ans (build-name)))
                 ans)))
           (define more-stuff
             (make-more-stuff
              source
              value
              build/memo-name
              (if track-context? '() #f)
              #t
              #f
              (not negative)
              '()))
           (new-blame (list positive)
                      (and negative (list negative))
                      original?
                      more-stuff))])
    make-blame))

(define (new-blame/more-stuff b more-stuff)
  (new-blame (blame-positive b)
             (blame-negative b)
             (blame-original? b)
             more-stuff))

(define (new-blame poss negs original? more-stuff)
  (define original-blame
    (make-blame
     poss
     negs
     original?
     #f
     more-stuff))
  (define swapped-blame
    (make-blame
     negs
     poss
     (not original?)
     original-blame
     more-stuff))
  (set-blame-swapped! original-blame swapped-blame)
  original-blame)

(define (blame-add-context b s #:important [name #f] #:swap? [swap? #f])
  (unless (blame? b)
    (raise-argument-error 'blame-add-context
                          "blame?"
                          0
                          b s))
  (unless (or (string? s)
              (not s))
    (raise-argument-error 'blame-add-context
                          (format "~s" '(or/c string? #f))
                          1
                          b s))
  (do-blame-add-context b s name swap?))

(define (blame-add-unknown-context b)
  (do-blame-add-context b #f #f #f))

(define (do-blame-add-context b s name swap?)
  (cond
    [(and (not (blame-context b))
          ;; if we are not tracking context,
          ;; we are not updating the name
          ;; at the top of the messages either
          ; (not name)
          (blame-top-known? b))
     (if swap? (blame-swap b) b)]
    [else
     (define new-original? (if swap? (not (blame-original? b)) (blame-original? b)))
     (define new-context (if (and s (blame-context b))
                             (cons s (blame-context b))
                             (blame-context b)))
     (define new-context/maybe-unknown
       (if (or s (blame-top-known? b))
           new-context
           (cons "..." new-context)))
     (define maybe-swapped (if swap? (blame-swap b) b))
     (new-blame/more-stuff
      maybe-swapped
      (struct-copy
       more-stuff (blame-more-stuff b)
       [important (if name (important name new-original?) (blame-important b))]
       [context new-context/maybe-unknown]
       [top-known? (string? s)]))]))

(struct important (name sense-swapped?) #:transparent)

(define (blame-swap b) (blame-swapped b))

(define (blame-replace-negative b new-neg)
  (new-blame (blame-positive b)
             (list new-neg)
             (blame-original? b)
             (blame-more-stuff b)))

(define (blame-replace-positive b new-pos)
  (new-blame (list new-pos)
             (blame-negative b)
             (blame-original? b)
             (blame-more-stuff b)))

(define (blame-update b extra-positive extra-negative)
  (ensure-blame-known 'blame-update b)
  (new-blame (cons extra-positive (blame-positive b))
             (cons extra-negative (blame-negative b))
             (blame-original? b)
             (blame-more-stuff b)))

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

  (define (check-and-fail)
    (unless (blame-missing-party? b)
      (error 'blame-add-missing-party "already have the party: ~s; trying to add ~s" 
             (if (blame-swapped? b) (blame-positive b) (blame-negative b))
             missing-party)))
  (cond
    [(not missing-party) b]
    [(blame-swapped? b)
     (check-and-fail)
     (new-blame (or (blame-positive b)
                    (list missing-party))
                (blame-negative b)
                (blame-original? b)
                (struct-copy
                 more-stuff (blame-more-stuff b)
                 [missing-party? #f]))]
    [else
     (check-and-fail)
     (new-blame (blame-positive b)
                (or (blame-negative b)
                    (list missing-party))
                (blame-original? b)
                (struct-copy
                 more-stuff (blame-more-stuff b)
                 [missing-party? #f]))]))

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
              [(eq? 'received: fst) (add-indent
                                     (if (blame/important-original? blame)
                                         "supplied:"
                                         "received:"))]
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
                          0
                          blme x custom-message))
  
  (define source-message (source-location->string (blame-source blme)))
  
  (define context (blame-context blme))
  (define context-lines (if (or (null? context) (not context))
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

  (define blame-parties (blame-positive blme))
  (define invariant-assertion-failure? (equal? blame-parties (list invariant-assertion-party)))
  
  (define self-or-not
    (cond
      [invariant-assertion-failure?
       "assertion violation"]
      [(blame/important-original? blme)
       "broke its own contract"]
      [else "contract violation"]))
  
  (define start-of-message
    (cond
      [(blame-important blme)
       (format "~a: ~a" (important-name (blame-important blme)) self-or-not)]
      [(blame-value blme)
       (format "~a: ~a" (blame-value blme) self-or-not)]
      [else
       (format "~a" self-or-not)]))
  
  (define blaming-line
    (cond
      [invariant-assertion-failure?
       ;; cause the blaming-line to be skipped
       '()]
      [(null? (cdr blame-parties))
       (format "  blaming: ~a" (convert-blame-singleton (car blame-parties)))]
      [else
       (apply
        string-append 
        "  blaming multiple parties:"
        (for/list ([party (in-list blame-parties)])
          (format "\n  ~a" (convert-blame-singleton party))))]))

  (define assumption-line
    (cond
      [invariant-assertion-failure?
       '()]
      [else "   (assuming the contract is correct)"]))
  
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
  
  (define custom-message-appears-to-start-with-fields?
    (regexp-match? #rx"^[^\n]*:" custom-message))

  (define extra-fields (blame-extra-fields blme))
  
  (combine-lines
   (if custom-message-appears-to-start-with-fields?
       start-of-message
       (string-append start-of-message ";"))
   (format (if custom-message-appears-to-start-with-fields?
               "  ~a"
               " ~a")
           custom-message)
   extra-fields
   context-lines
   (if context-lines
       contract-line
       (string-append
        "  in:"
        (substring contract-line 5 (string-length contract-line))))
   from-line
   on-line
   blaming-line
   assumption-line
   at-line))

(define (blame-add-extra-field b name field)
  (unless (blame? b)
    (raise-argument-error 'blame-add-extra-field
                          "blame?"
                          0 b name field))
  (unless (string? name)
    (raise-argument-error 'blame-add-extra-field
                          "string?"
                          1 b name field))
  (unless (string? field)
    (raise-argument-error 'blame-add-extra-field
                          "string?"
                          2 b name field))
  (new-blame/more-stuff
   b
   (struct-copy
    more-stuff (blame-more-stuff b)
    [extra-fields (cons (format "  ~a: ~a" name field)
                        (blame-extra-fields b))])))
  
;; combine-lines : (-> (listof (or/c string? #f))) string?)
;; combines each of 'lines' into a single message, dropping #fs,
;; and otherwise guaranteeing that each string is on its own line,
;; with no ending newline. (Note that the argument contract is
;; more restrictive than the function actually requires)
(define (combine-lines . lines)
  (regexp-replace
   #rx"\n$"
   (apply 
    string-append
    (for/list ([line (in-list (flatten lines))]
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

