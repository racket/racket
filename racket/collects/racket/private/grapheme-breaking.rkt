(module grapheme-breaking "pre-base.rkt"
  (require (rename-in '#%unsafe
                      [unsafe-fx< fx<]
                      [unsafe-fx> fx>]
                      [unsafe-fx<= fx<=]
                      [unsafe-fx>= fx>=]
                      [unsafe-fx= fx=]
                      [unsafe-fx- fx-]
                      [unsafe-fx+ fx+]
                      [unsafe-fxand fxand]
                      [unsafe-string-length string-length]))

  (provide string-next-grapheme-length
           string-previous-grapheme-length
           string-next-grapheme-boundary
           string-previous-grapheme-boundary
           string-grapheme-count

           unsafe-string-next-grapheme-length
           unsafe-string-previous-grapheme-length
           unsafe-string-next-grapheme-boundary
           unsafe-string-previous-grapheme-boundary
           unsafe-string-grapheme-count)

  ;; safe versions

  (define (string-next-grapheme-length str offset)
    (check-args/next! 'string-next-grapheme-length str offset)
    (unsafe-string-next-grapheme-length str offset))

  (define (string-previous-grapheme-length str offset)
    (check-args/prev! 'string-previous-grapheme-length str offset)
    (unsafe-string-previous-grapheme-length str offset))

  (define (string-next-grapheme-boundary str offset)
    (check-args/next! 'string-next-grapheme-boundary str offset)
    (unsafe-string-next-grapheme-boundary str offset))

  (define (string-previous-grapheme-boundary str offset)
    (check-args/prev! 'string-previous-grapheme-boundary str offset)
    (unsafe-string-previous-grapheme-boundary str offset))

  (define (string-grapheme-count str)
    (unless (string? str)
      (raise-argument-error 'string-grapheme-count "string?" str))
    (unsafe-string-grapheme-count str 0 (string-length str)))

  (define (check-arg-types! who str offset)
    (unless (string? str)
      (raise-argument-error who "string?" str))

    (unless (fixnum? offset)
      (raise-argument-error who "fixnum?" offset)))

  (define (check-args/next! who str offset)
    (check-arg-types! who str offset)

    (unless (and (fx>= offset 0) (fx< offset (string-length str)))
      (raise-range-error who "string" "offset " offset str
                         0 (fx- (string-length str) 1))))

  (define (check-args/prev! who str offset)
    (check-arg-types! who str offset)

    (unless (and (fx> offset 0) (fx<= offset (string-length str)))
      (raise-range-error who "string" "offset " offset str
                         1 (string-length str))))

  ;; `break-between?/state` returns a fixnum that encodes three pieces
  ;; of information:
  ;;
  ;; 1. The low two bits (bits 0-1) of the return value mean:
  ;;    #b00 : There is no grapheme boundary here.
  ;;    #b01 : There is a grapheme boundary here.
  ;;    #b10 : There is a boundary here only if we're in an
  ;;           extended emoji sequence (rule GB11).
  ;;    #b11 : There is a boundary here only if there are an
  ;;           odd number of regional indicators before
  ;;           this position (rules GB12 & GB13).
  (define BOUNDARY-NO       #b00)
  (define BOUNDARY-YES      #b01)
  (define BOUNDARY-IN-EMOJI #b10)
  (define BOUNDARY-IN-RIS   #b11)
  (define-syntax-rule (boundary-value return-val) (fxand #b11 return-val))
  ;;
  ;; The rules are described in:
  ;;   https://www.unicode.org/reports/tr29/tr29-39.html#Grapheme_Cluster_Boundary_Rules)
  ;;
  ;; However, when iterating forwards, the low two bits will always
  ;; be either #b00 or #b01. That is to say, this function will never
  ;; give a conditional answer to a forwards iterator.
  ;;
  ;; 2. Bit 2 is set if we're in an extended emoji sequence,
  ;;    under the assumption that we're iterating forwards. For
  ;;    backwards iteration, this bit is meaningless.
  (define-syntax-rule (in-emoji? state)
    (not (fx= 0 (fxand #b0100 state))))
  ;;
  ;; 3. Bit 3 is set if there are an odd number of regional
  ;;    indicators before this position, under the assumption that
  ;;    we're iterating forwards. For backwards iteration, this bit
  ;;    is meaningless.
  (define-syntax-rule (odd-ri-count? state)
    (not (fx= 0 (fxand #b1000 state))))
  ;;
  ;; The `state` parameter is a fixnum in which bits 2-3 have exactly
  ;; the same meaning that they have in the return value. (Which also
  ;; means that this parameter is only meaningful for forwards
  ;; iteration.)
  (define (break-between?/state gb1 gb2 state backwards?)
    (cond [(and (eq? 'other gb1) (eq? 'other gb2))
           #b0001]
          [(and (eq? 'cr gb1) (eq? 'lf gb2))
           #b0000]
          [(or (eq? 'control gb1) (eq? 'cr gb1) (eq? 'lf gb1)
               (eq? 'control gb2) (eq? 'cr gb2) (eq? 'lf gb2))
           #b0001]
          [(and (eq? 'l gb1)
                (or (eq? 'l gb2) (eq? 'v gb2) (eq? 'lv gb2) (eq? 'lvt gb2)))
           #b0000]
          [(and (or (eq? 'lv gb1) (eq? 'v gb1))
                (or (eq? 'v gb2) (eq? 't gb2)))
           #b0000]
          [(and (or (eq? 'lvt gb1) (eq? 't gb1))
                (eq? 't gb2))
           #b0000]
          [(or (eq? 'extend gb2) (eq? 'zwj gb2))
           (cond [(or (eq? 'extended-pictographic gb1) (in-emoji? state))
                  #b0100]
                 [else
                  #b0000])]
          [(eq? 'spacing-mark gb2)
           #b0000]
          [(eq? 'prepend gb1)
           #b0000]
          [(and (eq? 'zwj gb1) (eq? 'extended-pictographic gb2))
           (cond [backwards?              #b0010]
                 [(not (in-emoji? state)) #b0001]
                 [else                    #b0000])]
          [(and (eq? 'regional-indicator gb1) (eq? 'regional-indicator gb2))
           (cond [backwards?              #b0011]
                 [(odd-ri-count? state)   #b0001]
                 [else                    #b1000])]
          [else
           #b0001]))

  (define (unsafe-string-next-grapheme-boundary str offset)
    (define len (string-length str))
    (define gb1 (char-grapheme-break-property (string-ref str offset)))

    (let loop ([i offset] [gb1 gb1] [state #b0000])
      (define j (fx+ i 1))

      (cond
        [(fx= j len)
         j]
        [else
         (define gb2 (char-grapheme-break-property (string-ref str j)))
         (define state* (break-between?/state gb1 gb2 state #f))
         (define boundary (boundary-value state*))

         ;; For a forwards iterator, `boundary` will always be
         ;; either BOUNDARY-YES or BOUNDARY-NO.
         (cond
           [(fx= BOUNDARY-YES boundary) j]
           [else                       (loop j gb2 state*)])])))

  (define (unsafe-string-previous-grapheme-boundary str offset)
    (define gb2 (char-grapheme-break-property (string-ref str (fx- offset 1))))

    (let loop ([i offset] [gb2 gb2] [state #b0000])
      (define j (fx- i 1))

      (cond
        [(fx= j 0)
         j]
        [else
         (define gb1 (char-grapheme-break-property (string-ref str (fx- j 1))))
         (define state* (break-between?/state gb1 gb2 state #t))
         (define boundary (boundary-value state*))

         (cond
           [(fx= BOUNDARY-YES boundary)
            j]
           [(fx= BOUNDARY-NO boundary)
            (loop j gb1 state*)]
           [(fx= BOUNDARY-IN-EMOJI boundary)
            (if (in-backwards-extended-emoji? str (fx- j 1))
                (loop j gb1 state*)
                j)]
           [else ; BOUNDARY-IN-RIS
            (if (in-backwards-ri-group? str (fx- j 1))
                (loop j gb1 state*)
                j)])])))

  (define (in-backwards-extended-emoji? str i)
    ;; `i` is the index of a ZWJ that comes before an
    ;; Extended_Pictographic (rule GB11)
    (let loop ([i i])
      (cond [(fx= i 0) #f]
            [else
             (define gb
               (char-grapheme-break-property (string-ref str (fx- i 1))))

             (cond
               [(eq? 'extend gb) (loop (fx- i 1))]
               [else             (eq? 'extended-pictographic gb)])])))

  (define (in-backwards-ri-group? str i)
    ;; `i` is the index of an RI that precedes another RI
    ;; (rules GB12 & GB13)
    (let loop ([i i] [ri-count 0])
      (cond [(fx= i 0)
             (fx= 0 (fxand ri-count 1))]
            [else
             (define gb
               (char-grapheme-break-property (string-ref str (fx- i 1))))

             (cond
               [(eq? 'regional-indicator gb)
                (loop (fx- i 1) (fx+ ri-count 1))]
               [else
                (fx= 0 (fxand ri-count 1))])])))

  (define (unsafe-string-next-grapheme-length str offset)
    (fx- (unsafe-string-next-grapheme-boundary str offset) offset))

  (define (unsafe-string-previous-grapheme-length str offset)
    (fx- offset (unsafe-string-previous-grapheme-boundary str offset)))

  (define (unsafe-string-grapheme-count str start end)
    (let loop ([n 0] [pos start])
      (cond
        [(fx< pos end)
         (loop (fx+ n 1) (unsafe-string-next-grapheme-boundary str pos))]
        [else
         n]))))
