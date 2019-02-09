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
  (and (or (and (blame-no-swap? a) (blame-no-swap? b))
           (and (blame-yes-swap? a) (blame-yes-swap? b)))
       (equal?/recur (blame-context-frame a) (blame-context-frame b))
       (equal?/recur (blame-and-more a) (blame-and-more b))))

(define (combine-them x y)
  (bitwise-xor x (* 3 y)))

(define (blame-hash/combine b hash/recur combine-them)
  (combine-them (hash/recur (blame-no-swap? b))
                (combine-them (hash/recur (blame-context-frame b))
                              (hash/recur (blame-and-more b)))))

(define (blame-hash b hash/recur)
  (blame-hash/combine b hash/recur (λ (x y) (bitwise-xor x (* 3 y)))))
(define (blame-secondary-hash b hash/recur)
  (blame-hash/combine b hash/recur (λ (x y) (bitwise-xor (* 5 x) y))))

;; missing-party? field is #t when the missing party
;; is still missing and it is #f when the missing party
;; has been filled in (or if it was filled in from the start)
(define-struct all-the-info
  [positive
   negative
   source value build-name important missing-party? context-limit extra-fields]
  #:transparent)

;; and-more : (or/c blame-no-swap? blame-swap? all-the-info?)
;; context : string?
(define-struct blame (context-frame and-more)
  #:property prop:equal+hash
  (list blame=? blame-hash blame-secondary-hash))

(define-struct (blame-no-swap blame) ()
  #:property prop:equal+hash
  (list blame=? blame-hash blame-secondary-hash))
(define-struct (blame-yes-swap blame) ()
  #:property prop:equal+hash
  (list blame=? blame-hash blame-secondary-hash))

(define (blame->all-the-info b)
  (let loop ([b b])
    (cond
      [(blame? b) (loop (blame-and-more b))]
      [else b])))
(define (blame-source b) (all-the-info-source (blame->all-the-info b)))
(define (blame-value b) (all-the-info-value (blame->all-the-info b)))
(define (blame-important b) (all-the-info-important (blame->all-the-info b)))
(define (blame-missing-party? b) (all-the-info-missing-party? (blame->all-the-info b)))
(define (blame-contract b) ((all-the-info-build-name (blame->all-the-info b))))
(define (blame-extra-fields b) (all-the-info-extra-fields (blame->all-the-info b)))
(define (blame-context-limit b) (all-the-info-context-limit (blame->all-the-info b)))

(define (blame-get-info b f)
  (let loop ([b b]
             [swapped? #f])
    (cond
      [(blame-yes-swap? b) (loop (blame-and-more b) (not swapped?))]
      [(blame-no-swap? b) (loop (blame-and-more b) swapped?)]
      [else (f b swapped?)])))
(define (blame-original? b) (blame-get-info b (λ (all-the-info swapped?) (not swapped?))))
(define (blame-swapped? b) (blame-get-info b (λ (all-the-info swapped?) swapped?)))
(define (blame-positive b)
  (blame-get-info b (λ (all-the-info swapped?)
                      (if swapped?
                          (all-the-info-negative all-the-info)
                          (all-the-info-positive all-the-info)))))
(define (blame-negative b)
  (blame-get-info b (λ (all-the-info swapped?)
                      (if swapped?
                          (all-the-info-positive all-the-info)
                          (all-the-info-negative all-the-info)))))

(define (blame-context b)
  (let loop ([top (blame-context-frame b)]
             [b (blame-and-more b)])
    (cond
      [(all-the-info? b)
       ;; there is a dummy #f at the end
       ;; but it might be dropped if the
       ;; context is limited, so we don't
       ;; include it in the list
       (if top (list top) '())]
      [else (cons top (loop (blame-context-frame b)
                            (blame-and-more b)))])))

(define -make-blame
  (let ([make-blame
         (λ (source value build-name positive negative original?
                    #:context-limit [context-limit #f])
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
           (unless (or (not context-limit)
                       (exact-nonnegative-integer? context-limit))
             (raise-argument-error 'make-blame
                                   (format "~s" '(or/c #f natural?))
                                   context-limit))
           (define build/memo-name
             (let* ([uniq (box #f)]
                    [ans uniq])
               (λ ()
                 (when (eq? uniq ans)
                   (set! ans (build-name)))
                 ans)))
           (define all-the-info
             (make-all-the-info
              (list positive)
              (and negative (list negative))
              source
              value
              build/memo-name
              #f
              (not negative)
              context-limit
              '()))
           ;; we always start with a yes-swap or no-swap struct
           ;; so be careful in other parts of the code to ignore
           ;; it, as appropriate.
           (if original?
               (blame-no-swap #f all-the-info)
               (blame-yes-swap #f all-the-info)))])
    make-blame))

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
  (cond
    [(string? s)
     (do-blame-add-context b s name swap?)]
    [else b]))

;; this has become a no op. it seems to never have been
;; documented. probably exported because of the Great
;; Extra Export ScrewUp that happened years back
(define (blame-add-unknown-context b) b)

(define (do-blame-add-context b s name swap?)
  (define context-limit (blame-context-limit b))
  (cond
    [(and context-limit
          ;; if we are not tracking context,
          ;; we are not updating the name
          ;; at the top of the messages either
          ;(not name)
          )
     (cond
       [(not (zero? context-limit))
        ;; if the limit is zero, we skip this case,
        ;; which has the effect of always keeping only
        ;; the dummy context frame
        (define-values (limited-b dropped-swap?) (drop-to-limit b context-limit))
        (if (equal? dropped-swap? swap?)
            (blame-no-swap s limited-b)
            (blame-yes-swap s limited-b))]
       [swap?
        (if (blame-yes-swap? b)
            (blame-no-swap (blame-context-frame b) (blame-and-more b))
            (blame-yes-swap (blame-context-frame b) (blame-and-more b)))]
       [else b])]
    [else
     (define blame-yes/no-swap (if swap? blame-yes-swap blame-no-swap))
     (define inside-part
       (cond
         [name
          (let loop ([inner-b b])
            (cond
              [(blame-yes-swap? inner-b)
               (blame-yes-swap (blame-context-frame inner-b) (loop (blame-and-more inner-b)))]
              [(blame-no-swap? inner-b)
               (blame-no-swap (blame-context-frame inner-b) (loop (blame-and-more inner-b)))]
              [else
               (define new-original? (if swap? (not (blame-original? b)) (blame-original? b)))
               ;; in this case, we need to make a new blame record
               (struct-copy
                all-the-info inner-b
                [important (if name
                               (important name new-original?)
                               (all-the-info-important inner-b))])]))]
         [else b]))
     (if swap?
         (blame-yes-swap s inside-part)
         (blame-no-swap s inside-part))]))

(define (drop-to-limit b context-limit)
  (define short-enough?
    (let loop ([b b]
               [n (- context-limit 1)])
      (cond
        [(all-the-info? b) #t]
        [(blame? b)
         (if (zero? n)
             #f
             (loop (blame-and-more b) (- n 1)))])))
  (cond
    [short-enough? (values b #f)]
    [else
     (define swapped? #f)
     (define limited-b
       (let loop ([b b]
                  [n (- context-limit 1)])
         (cond
           [(= n 0)
            (let loop ([b b]
                       [swap? #f])
              (cond
                [(blame-yes-swap? b) (loop (blame-and-more b) (not swap?))]
                [(blame-no-swap? b) (loop (blame-and-more b) swap?)]
                [else
                 (set! swapped? swap?)
                 b]))]
           [(blame-no-swap? b)
            (blame-no-swap (blame-context-frame b)
                                  (loop (blame-and-more b) (- n 1)))]
           [(blame-yes-swap? b)
            (blame-yes-swap (blame-context-frame b)
                                   (loop (blame-and-more b) (- n 1)))])))
     (values limited-b swapped?)]))
  
(struct important (name sense-swapped?) #:transparent)

(define (blame-swap b)
  (cond
    [(blame-yes-swap? b)
     (blame-no-swap (blame-context-frame b) (blame-and-more b))]
    [(blame-no-swap? b)
     (blame-yes-swap (blame-context-frame b) (blame-and-more b))]))

(define (blame-replace-negative b new-neg)
  (update-the-info
   b
   (λ (an-all-the-info swap?)
     (if swap?
         (all-the-info-replace-positive an-all-the-info new-neg)
         (all-the-info-replace-negative an-all-the-info new-neg)))))

(define (blame-replace-positive b new-pos)
  (update-the-info
   b
   (λ (an-all-the-info swap?)
     (if swap?
         (all-the-info-replace-negative an-all-the-info new-pos)
         (all-the-info-replace-positive an-all-the-info new-pos)))))

(define (all-the-info-replace-positive an-all-the-info new-pos)
  (struct-copy
   all-the-info an-all-the-info
   [positive (list new-pos)]))

(define (all-the-info-replace-negative an-all-the-info new-neg)
  (struct-copy
   all-the-info an-all-the-info
   [negative (list new-neg)]))

(define (blame-update b extra-positive extra-negative)
  (ensure-blame-known 'blame-update b)
  (update-the-info
   b
   (λ (an-all-the-info swap?)
     (if swap?
         (struct-copy
          all-the-info an-all-the-info
          [positive (cons extra-negative (all-the-info-positive an-all-the-info))]
          [negative (cons extra-positive (all-the-info-negative an-all-the-info))])
         (struct-copy
          all-the-info an-all-the-info
          [positive (cons extra-positive (all-the-info-positive an-all-the-info))]
          [negative (cons extra-negative (all-the-info-negative an-all-the-info))])))))

(define (update-the-info b f)
  (let loop ([b b]
             [swap? #f])
    (cond
      [(blame-yes-swap? b)
       (blame-yes-swap (blame-context-frame b) (loop (blame-and-more b) (not swap?)))]
      [(blame-no-swap? b)
       (blame-no-swap (blame-context-frame b) (loop (blame-and-more b) swap?))]
      [else (f b swap?)])))

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
    [else
     (unless (blame-missing-party? b)
       (error 'blame-add-missing-party "already have the party: ~s; trying to add ~s"
              (if (blame-swapped? b) (blame-positive b) (blame-negative b))
              missing-party))
     (update-the-info
      b
      (λ (an-all-the-info swap?)
        (struct-copy
         all-the-info an-all-the-info
         [negative (or (all-the-info-negative an-all-the-info)
                       (list missing-party))]
         [missing-party? #f])))]))

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
    [(equal? (blame-positive blme) (blame-negative blme))
     ;; if the positive and negative parties are the same,
     ;; we never want to say "broke its own contract"
     #f]
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
  (update-the-info
   b
   (λ (an-all-the-info swap?)
     (struct-copy
      all-the-info an-all-the-info
      [extra-fields (cons (format "  ~a: ~a" name field)
                          (blame-extra-fields b))]))))

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

