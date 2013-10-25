#lang racket/base
(require "read-util.rkt"
         "../dir-chars.rkt"
         racket/set
         racket/port
         racket/contract
         syntax-color/lexer-contract)

#|

todo:
 - break up the table into pieces
   to better cope with edits

|#

(provide (contract-out [2d-lexer (-> lexer/c lexer/c)])
         cropped-regions)

(define (2d-lexer chained-lexer)
  (define uniform-chained-lexer
    (cond
      [(procedure-arity-includes? chained-lexer 3)
       chained-lexer]
      [else
       (λ (port offset mode)
         (define-values (val tok paren start end) (chained-lexer port))
         (values val tok paren start end 0 #f))]))
  (define (2dcond-lexer port offset _mode)
    (define a-2d-lexer-state (or _mode (2d-lexer-state '() #f #f)))
    (cond
      [(pair? (2d-lexer-state-pending-tokens a-2d-lexer-state))
       (define-values (line col pos) (port-next-location port))
       (define-values (val tok paren start end)
         (apply values (car (2d-lexer-state-pending-tokens a-2d-lexer-state))))
       
       ;; this helper function checks to make sure that what's
       ;; in the port is actually what was predicted by the
       ;; 'val' -- it isn't necessary for correct operation, but
       ;; helps find bugs earlier
       (define (check-char i c2)
         ;; here we want to check to make sure we're in sync, but:
         
         ;; 1) some lexers don't return strings (or return strings
         ;;    of the wrong sizes); these will be non-strings here
         (when (string? val)
           ;; 2) whitespace is not always right
           ;;    (the ones outside the table, 
           ;;    specifically, are always just spaces)
           (unless (eq? tok 'white-space)
             
             ;; 3) sometimes we get specials in the port
             (when (char? c2)

               (define c1 (string-ref val i))
               (unless (equal? c1 c2)
                 (error '2d/lexer.rkt "expected a ~s, got ~s while feeding token ~s" 
                        c1 c2 
                        (car (2d-lexer-state-pending-tokens a-2d-lexer-state))))))))
       
       ;; actually read the characters in
       (define last-i (- end start))
       (let loop ([i 0]
                  
                  ;; str-offset helps deal with the way line-counting ports handle 
                  ;; \r\n combinations. That is, (- end start) will be a number that
                  ;; doesn't match the length of the string in the case that there 
                  ;; are \r\n pairs in the port. We'll increment str-offset for each 
                  ;; of those and then use str-offset when indexing into the string
                  [str-offset 0])
         (unless (= i last-i)
           (define c2 (read-char-or-special port))
           (check-char (+ str-offset i) c2)
           (cond
             [(and (equal? c2 #\return)
                   (equal? (peek-char-or-special port) #\newline))
              (read-char-or-special port)
              (check-char (+ str-offset i 1) #\newline)
              (loop (+ i 1)
                    (+ str-offset 1))]
             [else 
              (loop (+ i 1)
                    str-offset)])))
       
       (define next-tokens 
         (cdr (2d-lexer-state-pending-tokens 
               a-2d-lexer-state)))
       (define new-state
         (struct-copy 2d-lexer-state
                      a-2d-lexer-state
                      [pending-tokens next-tokens]))
       (values val tok paren 
               pos
               (+ (- end start) pos)
               start
               (if (null? next-tokens)
                   new-state
                   (dont-stop new-state)))]
      [(equal? #\# (peek-char-or-special port))
       (define pp (peeking-input-port port))
       (define chars (list (read-char-or-special pp) 
                           (read-char-or-special pp) 
                           (read-char-or-special pp)))
       (cond
         [(equal? chars '(#\# #\2 #\d))
          (start-new-2d-cond-lexing port a-2d-lexer-state uniform-chained-lexer offset)]
         [else
          (call-chained-lexer uniform-chained-lexer port offset a-2d-lexer-state)])]
      [else
       (call-chained-lexer uniform-chained-lexer port offset a-2d-lexer-state)]))
  2dcond-lexer)


(define double-barred-chars-regexp
  (regexp
   (format "[~a]" (apply string double-barred-chars))))

(define (call-chained-lexer uniform-chained-lexer port offset a-2d-lexer-state)
  (define-values (a b c d e f new-mode) 
    (uniform-chained-lexer port offset (2d-lexer-state-chained-state a-2d-lexer-state)))
  (values a b c d e f (2d-lexer-state '() #f new-mode)))

(struct 2d-lexer-state (pending-tokens read-state chained-state))

(define (start-new-2d-cond-lexing port a-2d-lexer-state uniform-chained-lexer offset)
  (define-values (line col pos) (port-next-location port))
  ;; consume #\# #\2 and #\d that must be there (peeked them earlier)
  (read-char-or-special port)
  (read-char-or-special port)
  (read-char-or-special port)
  ;; read in the keyword and get those tokens
  
  (define-values (backwards-chars eol-string)
    (let loop ([kwd-chars '(#\d #\2 #\#)])
      (define c (peek-char port))
      (cond [(eof-object? c) (values kwd-chars "")]
            [(and (equal? c #\return)
                  (equal? (peek-char port 1) #\newline))
             (values kwd-chars (string c #\newline))]
            [(or (equal? c #\return)
                 (equal? c #\newline))
             (values kwd-chars (string c))]
            [else 
             (read-char-or-special port) ;; actually get the char
             (loop (cons c kwd-chars))])))
  (define first-tok-string
    (apply string (reverse backwards-chars)))
  (cond
    [(eof-object? (peek-char port))
     (values first-tok-string 
             'error 
             #f
             pos 
             (+ pos (string-length first-tok-string))
             0
             a-2d-lexer-state)]
    [else
     (define base-position
       ;; one might think that this should depend on the length of eol-string
       ;; but ports that have port-count-lines! enabled count the \r\n combination
       ;; as a single position in the port, not two.
       (let-values ([(_1 _2 c-pos) (port-next-location port)])
         c-pos))
     (define peek-port (peeking-input-port port))
     ;; pull the newline out of the peek-port
     (for ([x (in-range (string-length eol-string))]
           [c1 (in-string eol-string)])
       (define c2 (read-char-or-special peek-port))
       (unless (equal? c1 c2)
         (error 'unstable/2d/lexer.rkt "got an unexpected char.1 ~s vs ~s" c1 c2)))
     
     (define the-state (make-state line pos (string-length first-tok-string)))
     (setup-state the-state)

     ;; would like to be able to stop this loop
     ;; and process only part of the table,
     ;; but that works only when there are no broken
     ;; edges of the table that span the place I want to stop.
     (define failed
       (with-handlers ((exn:fail:read? values))
         (let loop ([map #f])
           (define new-map
             (parse-2dcond-one-step peek-port (object-name peek-port) #f #f pos the-state map))
           (when new-map
             (loop new-map)))))
     
     (define newline-token
       (list eol-string 'white-space #f 
             (+ pos (string-length first-tok-string))
             ;; no matter how long eol-string is, it counts for 1 position only.
             (+ pos (string-length first-tok-string) 1)))
     
     (cond
       [(exn:fail:read:eof? failed)
        ;; in this case, the source location for the error
        ;; should be the beginning of the #2d token, 
        ;; so we just turn the whole thing red in a single token
        (define tok-string 
          (string-append
           first-tok-string
           (apply string
                  (let loop ()
                    (define c (read-char port))
                    (cond
                      [(eof-object? c) '()]
                      [else (cons c (loop))])))))
        (values tok-string 'error #f
                pos (+ pos (string-length tok-string))
                0
                #f)]
       [else
        (define final-tokens
          (cond
            [(exn:fail:read? failed)
             (define error-pos (- (srcloc-position (car (exn:fail:read-srclocs failed)))
                                  base-position)) ;; account for the newline
             (when (< error-pos 0)
               (error 'unstable/2d/lexer.rkt "got error-pos < 0: ~s ~s" 
                      (srcloc-position (car (exn:fail:read-srclocs failed)))
                      base-position))
             (define peek-port2 (peeking-input-port port))
             (port-count-lines! peek-port2)
             (define (pull-chars n)
               (apply
                string
                (let loop ([n n])
                  (cond
                    [(zero? n) '()]
                    [else 
                     (define c (read-char-or-special peek-port2))
                     (cond
                       [(char? c)
                        (cons c (loop (- n 1)))]
                       [else
                        ;; drop replace specials with spaces
                        (cons #\space (loop (- n 1)))])]))))
             (cond
               
               [else
                
                ;; pull the newline out of peek-port2
                (for ([x (in-range (string-length eol-string))]) (read-char-or-special peek-port2))
                
                (define before-token (list (pull-chars error-pos)
                                           'no-color
                                           #f
                                           (+ base-position 1)
                                           (+ base-position 1 error-pos)))
                (define end-of-table-approx
                  (let ([peek-port3 (peeking-input-port peek-port2)])
                    (port-count-lines! peek-port3)
                    (define (read-line/check-double-barred)
                      (let loop ([found-double-barred? #f])
                        (define c (read-char-or-special peek-port3))
                        (cond
                          [(or (equal? c #\n) (eof-object? c))
                           found-double-barred?]
                          [else (loop (or found-double-barred?
                                          (member c double-barred-chars)))])))
                    (let loop ()
                      (define found-double-barred? (read-line/check-double-barred))
                      (cond
                        [found-double-barred?
                         (loop)]
                        [else
                         (define-values (line col pos) (port-next-location peek-port3))
                         pos]))))
                (define after-token
                  (list (pull-chars (- end-of-table-approx 1))
                        'error
                        #f
                        (+ base-position 1 error-pos)
                        (+ base-position 1 error-pos end-of-table-approx -1)))
                (if (zero? error-pos)
                    (list newline-token after-token)
                    (list newline-token before-token after-token))])]
            [else
             
             (define lhses (close-cell-graph cell-connections 
                                             (length table-column-breaks)
                                             (length rows)))
             (define scratch-string (make-string (for/sum ([ss (in-list rows)])
                                                   (for/sum ([s (in-list ss)])
                                                     (string-length s)))
                                                 #\space))
             (define collected-tokens '())
             (define rows-as-vector (apply vector (reverse rows)))
             (for ([set-of-indicies (in-list (sort (set->list lhses) compare/xy 
                                                   #:key smallest-representative))])
               (define regions
                 (fill-scratch-string set-of-indicies 
                                      rows-as-vector
                                      scratch-string 
                                      table-column-breaks 
                                      initial-space-count
                                      #t))
               (define port (open-input-string scratch-string))
               (port-count-lines! port)
               (let loop ([mode (2d-lexer-state-chained-state a-2d-lexer-state)])
                 (define-values (_1 _2 current-pos) (port-next-location port))
                 (define-values (tok-str tok paren start end backup new-mode)
                   (uniform-chained-lexer port (+ pos offset) mode))
                 (unless (equal? 'eof tok)
                   (for ([sub-region (in-list (cropped-regions start end regions))])
                     (define start (- (car sub-region) current-pos))
                     (define end (- (cdr sub-region) current-pos))
                     (set! collected-tokens 
                           (cons (list (if (and (string? tok-str)
                                                (< start (string-length tok-str))
                                                (<= end (string-length tok-str)))
                                           (substring tok-str start end)
                                           (list 'strange-token tok-str))
                                       tok
                                       paren 
                                       (+ base-position (car sub-region)) 
                                       (+ base-position (cdr sub-region)))
                                 collected-tokens)))
                   (loop new-mode))))
             
             (define (collect-double-barred-token pending-start i offset str)
               (when pending-start
                 (set! collected-tokens (cons (list (substring str pending-start i)
                                                    'parenthesis
                                                    #f
                                                    (+ base-position offset pending-start)
                                                    (+ base-position offset i))
                                              collected-tokens))))
             
             (for/fold ([offset 1]) ([strs (in-list (reverse (cons (list current-line) rows)))])
               (for/fold ([offset offset]) ([str (in-list strs)])
                 (let loop ([i 0]
                            [pending-start #f])
                   (cond
                     [(< i (string-length str))
                      (define c (string-ref str i))
                      (cond
                        [(member c double-barred-chars)
                         (loop (+ i 1)
                               (if pending-start pending-start i))]
                        [else
                         (collect-double-barred-token pending-start i offset str)
                         (loop (+ i 1) #f)])]
                     [else
                      (collect-double-barred-token pending-start i offset str)]))
                 (+ (string-length str) offset)))
             
             (define sorted-tokens (sort collected-tokens <
                                         #:key (λ (x) (list-ref x 3))))
             
             ;; there will be gaps that correspond to the places outside of the
             ;; outermost rectangle (at a minimum, newlines); this fills those 
             ;; in with whitespace tokens
             ;; NOTE: this code does not deal properly with \r\n newline combinations
             (define cracks-filled-in-tokens
               (let loop ([fst newline-token]
                          [tokens sorted-tokens])
                 (cond
                   [(null? tokens) (list fst)]
                   [else
                    (define snd (car tokens))
                    (cond
                      [(= (list-ref fst 4)
                          (list-ref snd 3))
                       (cons fst (loop snd (cdr tokens)))]
                      [else
                       (define new-start (list-ref fst 4))
                       (define new-end (list-ref snd 3))
                       (list* fst
                              (list 
                               ; these are not the real characters ...
                               (make-string (- new-end new-start) #\space)
                               'white-space
                               #f 
                               new-start
                               new-end)
                              (loop snd (cdr tokens)))])])))
             cracks-filled-in-tokens]))
     
        (values first-tok-string 'hash-colon-keyword #f
                pos (+ pos (string-length first-tok-string)) 
                0
                (dont-stop
                 (2d-lexer-state final-tokens
                                 #t
                                 (2d-lexer-state-chained-state a-2d-lexer-state))))])]))

(define (cropped-regions start end regions)
  (define result-regions '())
  (define (add start end)
    (unless (= start end)
      (set! result-regions (cons (cons start end) result-regions))))
  (let loop ([regions regions]
             [start start]
             [end end])
    (unless (null? regions)
      (define region (car regions))
      (cond
        [(<= start (car region))
         (cond
           [(<= end (car region))
            (void)]
           [(<= end (cdr region))
            (add (car region) end)]
           [else 
            (add (car region) (cdr region))
            (loop (cdr regions)
                  (cdr region)
                  end)])]
        [(<= start (cdr region))
         (cond
           [(<= end (cdr region))
            (add start end)]
           [else 
            (add start (cdr region))
            (loop (cdr regions)
                  (cdr region) 
                  end)])]
        [else 
         (loop (cdr regions) start end)])))
  result-regions)
  

#|
(define scratch-string (make-string (for/sum ([ss (in-vector lines)])
                                      (for/sum ([s (in-list ss)])
                                        (string-length s)))
                                    #\space))

(define heights
  (for/list ([line (in-vector lines)])
    (length line)))

`(,(string->symbol (string-append "2d" (apply string kwd-chars)))
  
  ,table-column-breaks
  ,heights
  
  ,@(for/list ([set-of-indicies (in-list (sort (set->list lhses) compare/xy 
                                               #:key smallest-representative))])
      (fill-scratch-string set-of-indicies 
                           lines 
                           scratch-string 
                           table-column-breaks 
                           initial-space-count)
      (define scratch-port (open-input-string scratch-string))
      (when post-2d-line (port-count-lines! scratch-port))
      (set-port-next-location! scratch-port post-2d-line post-2d-col post-2d-span)
      `[,(sort (set->list set-of-indicies) compare/xy)
        ,@(read-subparts source scratch-port 
                         initial-space-count table-column-breaks heights set-of-indicies
                         previous-readtable /recursive)]))
|#

#;
(module+ main 
  (define p (open-input-string (string-append
                                "╔══╦══╗\n"
                                "║1 ║2 ║\n"
                                "╠══╬══╣\n"
                                "║4 ║3 ║\n"
                                "╚══╩══╝\n")))
  (port-count-lines! p)
  ;; account for the "#2d" that was read from the first line
  (call-with-values (λ () (tokenize-2dcond p "source" 1 0 1 2))
                    list))
