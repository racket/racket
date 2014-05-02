#lang racket/base
(require racket/class
         racket/gui/base
         framework)

(define surrogate%
  (class (racket:text-mode-mixin 
          (color:text-mode-mixin
           mode:surrogate-text%))
    (define/override (on-enable-surrogate txt)
      (send (send txt get-keymap) chain-to-keymap at-exp-keymap #f)
      (super on-enable-surrogate txt))
    (define/override (on-disable-surrogate txt)
      (keymap:remove-chained-keymap txt at-exp-keymap)
      (super on-disable-surrogate txt))
    (super-new)))

(define at-exp-keymap (new keymap:aug-keymap%))
(define (reindent-paragraph t evt)
  (unless (send t is-stopped?)
    (define sp (send t get-start-position))
    (when (= sp (send t get-end-position))
      (paragraph-indentation t sp 60))))

(send at-exp-keymap add-function "reindent-paragraph" reindent-paragraph)
(send at-exp-keymap map-function "esc;q" "reindent-paragraph")
(send at-exp-keymap map-function "?:a:q" "reindent-paragraph")

;;(paragraph-indentation a-racket:text posi width) → void?
;; posi : exact-integer? = current given position
;; width : exact-integer? = user defined line width limitation
;; Indent a whole paragraph(multiple lines that contains the given position
(define (paragraph-indentation txt posi width)
  (let* ([current-line (send txt position-paragraph posi)]
         [guess-start-posi (send txt find-up-sexp posi)])
    (if guess-start-posi ;;inside a parenthesis
        (let* ([guess-start-line (send txt position-paragraph guess-start-posi)])
          (paragraph-indent-start txt guess-start-line current-line width))
        (paragraph-indent-start txt current-line current-line width))));;handle text, no boundry guess

;;(paragraph-indent-start a-racket:text guess-start current-line) → void?
;; guess-start : exact-integer? = (send text find-up-sexp posi)
;; current-line : exact-integer? = current line number
;; Indent a whole paragraph starts with empty line or guess-start, end with empty line
(define (paragraph-indent-start text guess-start current-line width)
  (define para-start-line (for/first ([line (in-range guess-start 0 -1)]
                                      #:when (empty-line? text line))
                            line))
  (when para-start-line
    (send text begin-edit-sequence)
    (let loop ([i (+ para-start-line 1)])
      (unless (and (empty-line? text i) (> i current-line))
        (define posi (send text paragraph-start-position i))
        (define amount (determine-spaces text posi))
        (when amount
          (adjust-spaces text i amount posi))
        (adjust-para-width text posi width)
        (loop (+ i 1))))
    (send text end-edit-sequence)))                              

;;(empty-line? a-racket:text line) → boolean?
;; line : exact-integer? = current line number
(define (empty-line? txt line)
  (let* ([line-start (send txt paragraph-start-position line)]
         [line-end (send txt paragraph-end-position line)]
         [line-classify (txt-position-classify txt line-start line-end)])
    (not (para-not-empty? line-classify))))

;;(rest-empty? a-racket:text line start) → boolean?
;; line : exact-integer? = (send text position-paragraph start)
;; start : exact-intefer? = the start position
(define (rest-empty? txt line start)
  (let* ([line-start (add1 start)]
         [line-end (send txt paragraph-end-position line)]
         [line-classify (txt-position-classify txt line-start line-end)])
    (not (para-not-empty? line-classify))))

;;(determine-spaces : a-racket:text position) → exact-integer?/boolean?
;; position : exact-integer? = current position
;; Return exact integer represents number of #\space to put in front of current paragraph(line) or #f
(define (determine-spaces txt posi)
  (define current-para (send txt position-paragraph posi))
  (if (not (empty-line? txt current-para));not an empty paragraph/comment string
      (let* ([para-start (send txt paragraph-start-position current-para)]
             [para-start-skip-space (start-skip-spaces txt current-para 'forward)]
             [char-classify (send txt classify-position para-start-skip-space)]
             [prev-posi (send txt find-up-sexp para-start-skip-space)])
        (cond (prev-posi
               (let ([this-para (send txt position-paragraph prev-posi)])
                 (cond ((equal? #\[ (send txt get-character prev-posi))
                        (let ((this-para-start (send txt paragraph-start-position this-para)))
                          (if (= current-para this-para)
                              0
                              (if (rest-empty? txt this-para prev-posi)
                                  1
                                  (add1 (- prev-posi this-para-start))))))
                       ;;if it is inside a racket function and not the first line of the racket function
                       ((equal? #\( (send txt get-character prev-posi))
                        (send txt tabify para-start) #f);call corresponding function to indent racket stuff
                       (else (count-parens txt prev-posi)))))
              ((equal? 'text char-classify) 0) ;;0 space if line is just a "outside" text
              (else (send txt tabify para-start) #f)));;call tabify
      #f));;empty line, do nothing 

;;(adjust-para-width a-racket:text position width) → boolean?
;; position : exact-integer? = current position 
;; width : exact-integer? = predefined value
;;Modify the given paragraph(line) if it exceed width limit by inserting #\newline to proper position
(define (adjust-para-width txt posi width)
  (let* ([para-num (send txt position-paragraph posi)]
         [para-start (send txt paragraph-start-position para-num)]
         [para-end (send txt paragraph-end-position para-num)]
         [para-len (add1 (- para-end para-start))]
         [para-classify (txt-position-classify txt para-start para-end)])
    (if (para-not-empty? para-classify) ;continue when current paragraph is not empty
        (cond ((> para-len width) ;paragraph too long
               (define new-line-created (select-cut-option txt para-start para-len width para-classify))
               (when (equal? new-line-created #t)
                 (let* ([next-para-num (+ para-num 2)]
                        [next-para-start (send txt paragraph-start-position next-para-num)]
                        [next-para-end (send txt paragraph-end-position next-para-num)]
                        [next-para-classify (txt-position-classify txt next-para-start next-para-end)])
                   (if (para-not-empty? next-para-classify) ;; next paragraph not empty
                       (begin (delete-end-spaces txt (+ para-num 1))
                              (delete-start-spaces txt (+ para-num 2))
                              (let* ([nxt-para-num (+ para-num 2)]
                                     [nxt-para-start (send txt paragraph-start-position nxt-para-num)]
                                     [nxt-para-end (send txt paragraph-end-position nxt-para-num)]
                                     [nxt-para-classify (txt-position-classify txt nxt-para-start nxt-para-end)])
                                (when (equal? 'text (car nxt-para-classify))
                                  ;now text
                                  (send txt delete nxt-para-start 'back)
                                  (send txt insert #\space (sub1 nxt-para-start)))))     
                       #t))))
              ;;push up the next paragraph if not empty, and it is text
              ((< para-len width) 
               (push-back-lines txt para-num para-start width)
               (let* ([new-end (send txt paragraph-end-position para-num)]
                      [new-len (add1 (- new-end para-start))])
                 (when (> new-len width)
                   (adjust-para-width txt para-start width))
                 ))
              (else #t))
        #t)))

;;(txt-position-classify a-racket:text start end) → void?
;; start : exact-integer? = position to start classify
;; end : exact-integer? = position to end classify
;; Basic position classify method that classify text within certain range
(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
    (send txt classify-position x)))

;;(is-at-sign? a-racket:text posi) → boolean?
;; posi : exact-integer? = a position in the text
;; Check if the given position is an @
(define (is-at-sign? txt posi)
  (and (equal? (send txt classify-position posi) 'parenthesis)
       (let-values ([(start end) (send txt get-token-range posi)])
         (and (equal? start posi)
              (equal? end (+ posi 1))))
       (equal? #\@ (send txt get-character posi))))

;;(para-not-empty? a-racket:text classify-lst) → boolean?
;; classify-lst : list? = (txt-position-classify text start end)
;; Check if current paragraph(line) is empty, we consider comment line as empty line
(define (para-not-empty? classify-lst) ;;we consider 'other  and 'comment as empty
  (if (or (member 'parenthesis classify-lst)
          (member 'string classify-lst)
          (member 'symbol classify-lst)
          (member 'text classify-lst))
      #t
      #f))

;;(start-skip-spaces a-racket:text para direction) → exact-integer?
;;para : exact-integer? = paragraph(line) number
;;direction : symbol? = 'forward/'backward
;;Return the first non-empty(#\space #\tab) character's position in given paragraph(line)
(define (start-skip-spaces txt para direction)
  (let* ([para-start (send txt paragraph-start-position para)]
         [para-end (send txt paragraph-end-position para)])
    (if (equal? direction 'forward)
        (for/first ([start-skip-space (in-range para-start para-end 1)]
                    #:when (not (member (send txt get-character start-skip-space) (list #\space #\tab))))
          start-skip-space)
        (for/first ([start-skip-space (in-range (sub1 para-end) para-start -1)];;ignore the newline
                    #:when (not (member (send txt get-character start-skip-space)  (list #\space #\tab))))
          start-skip-space))))

;;(delete-end-spaces a-racket:text para) → void?
;;para : exact-integer? = paragraph(line) number
;;Delete all #\space and #\tab at the end of given paragraph
(define (delete-end-spaces txt para)
  (let* ([para-end (send txt paragraph-end-position para)]
         [last-non-white (start-skip-spaces txt para 'backward)])
    (if last-non-white
        (send txt delete (+ last-non-white 1) para-end)
        #f)))

;;(delete-start-spaces a-racket:text para) → void?
;;para : exact-integer? = paragraph(line) number
;;Delete all #\space and #\tab at the beginning of given paragraph
(define (delete-start-spaces txt para)
  (let* ([para-start (send txt paragraph-start-position para)]
         [first-non-white (start-skip-spaces txt para 'forward)])
    (when (> first-non-white para-start)
      (send txt delete para-start first-non-white))))

;;(count-parens a-racket:text posi) → exact-integer?
;;posi : exact-integer? = a position in given text 
;;Return number of parenthesis till the outmost @ annotation,
;;if the there is "[", we check if it has "@" after at the same
;;line, if so, we add the number of characters between "[" and
;;the beginning of the line it appears
(define (count-parens txt posi)
  (define count 0)
  (do ([p posi (send txt find-up-sexp p)]);backward-containing-sexp p 0)])
    ((not p) count)
    (cond [(equal? #\{ (send txt get-character p)) (set! count (add1 count))]
          [(equal? #\[ (send txt get-character p))
           (let* ([this-para (send txt position-paragraph p)]
                  [this-para-start (send txt paragraph-start-position this-para)])
             (if (rest-empty? txt this-para p)
                 (set! count (add1 count))
                 (set! count (+ (add1 (- p this-para-start)) count))))]
          [else #t])))

;;(push-back-check? a-racket:text posi) → Boolean?
;;posi : exact-integer? = a position in given text which is the @'s position 
;;Return #f if we see:
;; 1) "[" with muptiple lines after
;; 2) keyworld "codeblock" or "verbatim"
;; otherwise return #t
(define (push-back-check? t posi)
  (define key-words (list "codeblock" "verbatim"))
  (if (is-at-sign? t posi)
      (let* ([first-char-posi (add1 posi)]
             [open-paren-posi (send t get-forward-sexp first-char-posi)]);start from the first char after @
        (cond 
          [(equal? #\[ (send t get-character open-paren-posi))
           (let* ([close-paren-posi-plus-one (send t get-forward-sexp open-paren-posi)]
                  [start-line (send t position-paragraph open-paren-posi)])
             (if close-paren-posi-plus-one
                 (let* ([close-paren-posi (sub1 close-paren-posi-plus-one)]
                        [end-line (send t position-paragraph close-paren-posi)])
                   (equal? start-line end-line))
                 #f))]
          [(equal? #\{ (send t get-character open-paren-posi))
           (define key-word (send t get-text first-char-posi open-paren-posi))
           (if (member key-word key-words)
               #f
               #t)]
          [else #f]))
      #t)) ;; e.g @verbatim|{}|

;;(push-back-line a-racket:text para width) → void?
;;para : exact-integer? = paragraph(line) number
;;para-start : exact-integer? = start position of given paragraph(line)
;;width : exact-intefer? = predefined paragrah width
(define (push-back-lines txt para para-start width)
  (let* ([para-end (send txt paragraph-end-position para)]
         [new-width (add1 (- para-end para-start))];;init with original line length
         [nxt-para (add1 para)]
         [nxt-para-end (send txt paragraph-end-position nxt-para)])
    (if (or (equal? para-end nxt-para-end) (equal? para-end (sub1 nxt-para-end))) ;;reach/exceed the last line
        #t;;all done
        (let* ([nxt-para-start (start-skip-spaces txt nxt-para 'forward)] 
               [nxt-para-classify (txt-position-classify txt nxt-para-start nxt-para-end)])
          (unless (not nxt-para-start) ;next line empty, start-skip-spaces returns #f
            (if (and (para-not-empty? nxt-para-classify) (push-back-check? txt nxt-para-start)
                     (equal? (send txt classify-position (sub1 para-end)) 'text);;previous line end with text 
                     (< new-width width))
                ;;we only push back those lines satisfy push-back-check rules
                (begin (delete-end-spaces txt para)
                       (delete-start-spaces txt nxt-para)
                       (let ([new-nxt-start (send txt paragraph-start-position nxt-para)])
                         (send txt delete new-nxt-start 'back)
                         (send txt insert #\space (sub1 new-nxt-start)))
                       (push-back-lines txt para para-start width)) ;;keep pushing back lines
                #t)))))) ;;done

;;Deprecated
;;(indent-racket-fuc a-racket:text posi) → exact-integer?/boolean?
;;posi : exact-integer? = a position in given text
;;Return 1 if the position is within first #\( of racket function, or #f
(define (indent-racket-func txt posi)
  (let* ([prev-posi (sub1 posi)]
         [back-one-level (send txt backward-containing-sexp prev-posi 0)])
    (if back-one-level
        (let ([paren (sub1 back-one-level)])
          (cond ((equal? #\{ (send txt get-character paren)) 1)
                ((equal? #\[ (send txt get-character paren));might be inside cond etc
                 (let ([back-two-level (send txt backward-containing-sexp (sub1 paren) 0)])
                   (if back-two-level
                       #f
                       1)))
                (else #f)))
        #f)));;#f needs to be replaced by racket indentation function

;;(select-cut-option a-racket:text start len width classify-lst) → boolean?
;; start : exact-integer? = (send text paragraph-start-position given-paragraph-number)
;; len : exact-integer? = length of current paragraph(line)
;; width : exact-integer? = predefined value
;; classify-lst: list? = (txt-position-classify text start end) here end is the end position
;;               of current paragraph(line)
;; Put #\newline to shorten given paragraph if necessary, return #t if #\newline inserted
;; Situations:
;;1) breake whole @... to next line, 
;;2) keep @.... in current line 
;;3) if it is a simple text, just cut it
(define (select-cut-option txt start len width classify-lst)
  (let ([adjust-result (list-ref classify-lst (sub1 width))]);;get the "end" position adjust result
    (cond [(equal? adjust-result 'text) 
           (let ([new-break (insert-break-text txt start (+ start width) (+ start len))])
             (if new-break
                 ;replace the #\space with #\newline 
                 (begin (send txt delete (add1 new-break) 'back)
                        (send txt insert #\newline new-break)
                        #t)
                 #f))]
          ;;'symbol 'parenthesis 'string or 'space
          ;;1)went backward to find @
          ;;2)went forward to find first 'text
          [else 
           (let ([posi (insert-break-func txt start len width classify-lst)])
             (if posi
                 (begin (send txt insert #\newline posi);;directly insert before @ or 'text
                        #t)
                 #f))])))

;;(insert-break-text a-racket:text start width-end end) → exact-integer?/boolean?
;; start : exact-integer? = (send text paragraph-start-position given-paragraph-number)
;; width-end : exact-integer? = (+ start width) here width is the user defined line width limit
;; end : exact-integer? = (send text paragraph-end-position given-paragraph-number)
;;Return the proper position to insert #\newline into given text line, #f if not found
(define (insert-break-text text start width-end end)
  (for/first ([break-ls (in-range width-end start -1)]
              #:when (equal? #\space (send text get-character break-ls)))
    break-ls))

;;(insert-break-func a-racket:text start len width classify-lst) → exact-integer?/boolean?
;; start : exact-integer? = (send text paragraph-start-position given-paragraph-number)
;; len : exact-integer? = length of current paragraph(line)
;; width : exact-integer? = predefined value
;; classify-lst: list? = (txt-position-classify text start end) here end is the end position
;;               of current paragraph(line)
;;Return the proper position to insert #\newline into given line, #f if not found
(define (insert-break-func text start len width classify-lst)
  (let ([at-sign-posi
         (for/first ([sign-posi (in-range (+ start width) start -1)]
                     #:when (is-at-sign? text sign-posi))
           sign-posi)])
    (if (and at-sign-posi
             (not (equal? 'white-space (list-ref classify-lst (sub1 (- at-sign-posi start))))))
        at-sign-posi
        (for/first ([posi (in-range width (- len 1))] 
                    #:when (equal? 'text (list-ref classify-lst posi)))
          posi))))

;;adjust-spaces for text
;;(adjust-spaces : a-racket:text para amount posi) → boolean?
;; para : exact-integer? = given paragraph(line) number
;; amount : exact-integer? = (determine-spaces text posi)
;; posi : exact-integer? = a position in the text
;; Delete #\spaces and #\tab in front of given paragraph(line) if not 
;;  equal to the amount given by determine-spaces. Then insert new amount of #\space
;;  in front of the paragraph(line)
(define (adjust-spaces text para amount posi)
  (define posi-skip-space (start-skip-spaces text para 'forward))
  (define origin-amount (- posi-skip-space posi))
  (when (and amount (not (= origin-amount amount)))
    (send text delete posi posi-skip-space)
    (when (> amount 0)
      (send text insert (make-string amount #\space) posi))) 
  #t)

;;test cases
(module+ test
  (require rackunit framework)
  
  ;test start-skip-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "test1 test2\n @test3\n")
                  (start-skip-spaces t 1 'forward)) 13)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n@efgh\n}")
                  (start-skip-spaces t 1 'forward)) 6)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n  efgh\n}")
                  (start-skip-spaces t 1 'forward)) 8)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abc\n       \n}")
                  (start-skip-spaces t 1 'forward)) #f)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abc\nefgh   \n}")
                  (start-skip-spaces t 1 'backward)) 8)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n\t\tefgh\n}");tab
                  (start-skip-spaces t 1 'forward)) 8)
  (define txt_1 (new racket:text%))
  (send txt_1 insert "#lang scribble/base\n@f{x}\n@;ghj\ntyty\n\n")
  
  ;test para-not-empty?
  (check-equal? (let ([result (txt-position-classify txt_1 0 5)])
                  (para-not-empty? result))
                #f);;consider 'other as empty line
  
  (check-equal? (let ([result (txt-position-classify txt_1 20 24)])
                  (para-not-empty? result))
                #t)
  
  (check-equal? (let ([result (txt-position-classify txt_1 27 31)])
                  (para-not-empty? result))
                #f);comment
  
  (check-equal? (let ([result (txt-position-classify txt_1 37 38)])
                  (para-not-empty? result))
                #f);empty line
  ;test is-at-sign
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(x)")
                  (is-at-sign? t 0))
                #f)
  (check-equal? (is-at-sign? txt_1 20) #t)
  (check-equal? (is-at-sign? txt_1 22) #f) 
  
  ;test determine-spaces
  (check-equal? (determine-spaces txt_1 15) #f)
  (check-equal? (determine-spaces txt_1 21) #f)
  
  (define txt_2 (new racket:text%))
  (send txt_2 insert "#lang scribble/base\n@f{\n @a\n@b\n}")
  (check-equal? (determine-spaces txt_2 25) 1)
  (check-equal? (determine-spaces txt_2 28) 1)
  
  (define txt_3 (new racket:text%))
  (send txt_3 insert "#lang scribble/base\n@f[@x\n@y\n]")
  (check-equal? (determine-spaces txt_3 24) #f) 
  (check-equal? (determine-spaces txt_3 27) 3)
  
  (define txt_4 (new racket:text%))
  (send txt_4 insert "#lang scribble/base\n@itemlist[@item{item1}\n@item{item2}\n]")
  (check-equal? (determine-spaces txt_4 22) #f)
  (check-equal? (determine-spaces txt_4 44) 10)
  
  (define txt_5 (new racket:text%))
  (send txt_5 insert "#lang scribble/base\n@boldlist{@me{item1}\n@me{item2}\n}")
  (check-equal? (determine-spaces txt_5 31) #f)
  (check-equal? (determine-spaces txt_5 46) 1)
  
  (define txt_6 (new racket:text%))
  (send txt_6 insert "@list{@me{item1}\n\n@me{item2}\n}")
  (check-equal? (determine-spaces txt_6 16) #f)
  (check-equal? (determine-spaces txt_6 17) #f);empty line!
  (check-equal? (determine-spaces txt_6 18) 1)
  
  (check-equal? (let ([txt_7 (new racket:text%)])
                  (send txt_7 insert "@(define (foo . a)\n(bar b))")
                  (determine-spaces txt_7 19)) #f)
  
  (define txt_8 (new racket:text%))
  (send txt_8 insert "@a{me}\n@b[\n@c{@d{e} f\ng\nh}\n")
  (check-equal? (count-parens txt_8 22) 2)
  (check-equal? (count-parens txt_8 13) 2);;include current parenthesis
  (check-equal? (determine-spaces txt_8 22) 2)
  (check-equal? (determine-spaces txt_8 12) 1) 
  
  (define txt_9 (new racket:text%))
  (send txt_9 insert "@a[\n(b c)\n(d\n[(e) f]\n[g h])\n]\n")
  (check-equal? (indent-racket-func txt_9 4) 1)
  (check-equal? (indent-racket-func txt_9 6) #f)
  (check-equal? (determine-spaces txt_9 13) #f) 
  (check-equal? (determine-spaces txt_9 4) 1)
  
  ;;two test cases for count-parens
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[@b{c d\ne f g}\n@h{i j}]")
                  (count-parens t 5)) 4)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[@b{\n@c{d e f}\ng}]")
                  (count-parens t 9)) 5)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(d f\n(l [()\n(f ([a (b c)])\n(d e)))])")
                  (indent-racket-func t 12)) #f)      
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[\n     ]\n")
                  (determine-spaces t 4))
                1)      
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntest1\n     test2\n")
                  (determine-spaces t 28))
                0)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntestcase @a{b\n\n\n\n\n      c}\n\n")
                  (determine-spaces t 39))
                1)
  ;;test cases for:delete-end-spaces delete-start-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde\t\t\t\t\nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \n\n3\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\n\n3\n}")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "  {abcde\nfgh\n}")
                  (delete-start-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[\n      ]\n")
                  (delete-start-spaces t 1)
                  (send t get-text)) "@a[\n]\n")
  
  ;;adjust-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[\n     ]\n")
                  (adjust-spaces t 1 1 4)
                  (adjust-spaces t 1 1 4)
                  (send t get-text)) "@a[\n ]\n")
  ;;push-back-check?
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n@test[@a{}]\n")
                  (push-back-check? t 20))
                #t)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n@test[@a{}\n@b{}]\n")
                  (push-back-check? t 20))
                #f)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n @test{}\n@test{}\n")
                  (push-back-check? t 21))
                #t)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n@codeblockfake{}\n")
                  (push-back-check? t 20))
                #t)
  
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\n@codeblock{}\n")
                  (push-back-check? t 21))
                #f)
  
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n@verbatim{}\n")
                  (push-back-check? t 20))
                #f)
  
  ;;push-back-lines
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\ntest1\n     test2\n @test3\n")
                  (push-back-lines t 1 20 22)
                  (send t get-text))
                "#lang scribble/base\ntest1 test2\n @test3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\ntest1\n     test2\n\t\ttest3\n")
                  (push-back-lines t 1 20 12)
                  (send t get-text))
                "#lang scribble/base\ntest1 test2\n\t\ttest3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\ntest1\n     test2\n\t\ttest3\ntest4\n")
                  (push-back-lines t 1 20 18)
                  (send t get-text))
                "#lang scribble/base\ntest1 test2 test3\ntest4\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\ntest1\n     test2\n\t\ttest3\n")
                  (push-back-lines t 1 20 22)
                  (send t get-text))
                "#lang scribble/base\ntest1 test2 test3\n")
  
  ;;paragraph indentation
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\naaa bbb ccc\n  @ddd[eee] fff\n ggg hhh iii jjj\n")
                  (paragraph-indentation t 23 23)
                  (send t get-text))
                "#lang scribble/base\n\naaa bbb ccc @ddd[eee]\nfff ggg hhh iii jjj\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\n@itemlist[@item{aaa bbb ccc\n                eee fff}\n          @item{ggg hhh iii\n  jjj kkk lll mmm nnn ooo\n  ppp qqq\nrrr\nsss ttt uuu vvv}]")
                  (paragraph-indentation t 38 29)
                  (send t get-text))
                "#lang scribble/base\n\n@itemlist[@item{aaa bbb ccc\n           eee fff}\n          @item{ggg hhh iii\n           jjj kkk lll mmm\n           nnn ooo ppp qqq\n           rrr sss ttt uuu\n           vvv}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\n@itemlist[@item{aaa bbb ccc\n                eee fff\n          @item{ggg hhh iii\n  jjj kkk lll mmm nnn ooo\n  ppp qqq\nrrr\nsss ttt uuu vvv}}]")
                  (paragraph-indentation t 38 29)
                  (send t get-text))
                "#lang scribble/base\n\n@itemlist[@item{aaa bbb ccc\n           eee fff @item{ggg\n            hhh iii jjj kkk\n            lll mmm nnn ooo\n            ppp qqq rrr sss\n            ttt uuu vvv}}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb\n @ccc ddd")
                  (adjust-para-width t 22 12) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb\n @ccc ddd")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntest1\n     test2\n\t\ttest3\n")
                  (paragraph-indentation t 22 6)
                  (send t get-text))
                "#lang scribble/base\n\ntest1\ntest2\ntest3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntest1\n     test2\n\t\ttest3\n")
                  (paragraph-indentation t 22 20)
                  (send t get-text))
                "#lang scribble/base\n\ntest1 test2 test3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntest1\n     test2\n\t\ttest3 test4\n")
                  (paragraph-indentation t 22 20)
                  (send t get-text))
                "#lang scribble/base\n\ntest1 test2 test3\ntest4\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntestcase @a{b\n\n\n\n\n      c}\n\n")
                  (paragraph-indentation t 39 14)
                  (send t get-text))
                "#lang scribble/base\n\ntestcase @a{b\n\n\n\n\n c}\n\n")
  
  ;;test case for adjust paragraph width
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb\n @ccc ddd")
                  (adjust-para-width t 22 12) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb\n @ccc ddd")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb\nccc ddd @e[f @g{h}]")
                  (adjust-para-width t 22 12) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb ccc\nddd @e[f @g{h}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb ccc ddd @e[f @g{h}]")
                  (adjust-para-width t 22 12) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb ccc\nddd @e[f @g{h}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e[f @g{h}]")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d \n@e[f @g{h}]");;keep the space, does not matter
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e{}\n f g\n")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d \n@e{} f g\n")
  
  ;;test insert-break
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaa bbb ccc ddd")
                  (let ([new-break (insert-break-text t 0 6 14)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaa\nbbb ccc ddd");;prefer shorter than the "width limit"
  
  ;;for the situation there isn't any #\space on right side 
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaaa bbbb")
                  (let ([new-break (insert-break-text t 0 5 8)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaaa\nbbbb") 
  )

(provide determine-spaces adjust-para-width paragraph-indentation
         surrogate%)