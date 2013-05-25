#lang racket/base
(require racket/class
         (only-in racket/gui/base
                  color% 
                  font%
                  the-clipboard
                  clipboard-client%
                  key-event%
                  mouse-event%)
         racket/snip
         mred/private/wxme/mline
         mred/private/wxme/editor
         mred/private/wxme/text
         mred/private/wxme/pasteboard
         "test-editor-admin.rkt"
         mred/private/wxme/stream
         mred/private/wxme/keymap
         mred/private/wxme/editor-snip
         (for-syntax racket/base))

(define wrong-cnt 0)
(define test-cnt 0)

(define-syntax (expect stx)
  (syntax-case stx ()
    [(_ a b)
     #`(expect/proc #,(syntax-line stx) a b)]))

(define (expect/proc line v v2)
  (set! test-cnt (add1 test-cnt))
  (unless (equal? v v2)
    (set! wrong-cnt (add1 wrong-cnt))
    (eprintf "FAILED: line ~a\nexpected: ~s\n     got: ~s\n" 
             line
             v2
             v)))

(define (done)
  (printf "\n~a tests\n" test-cnt)
  (if (zero? wrong-cnt)
      (printf "all passed\n")
      (eprintf "~s FAILED\n" wrong-cnt)))

;; ----------------------------------------
;; String snips and lines

(define s (make-object string-snip% "helko"))
(send s insert "cat " 4 2)
(void (send s get-text 0 (send s get-count)))
(send s set-flags (cons 'invisible (send s get-flags)))
(void (send s get-flags))
(void (send (send (get-the-snip-class-list) find "wxtext") get-classname))

(define root-box (box mline-NIL))
(define m20 (mline-insert #f root-box #t))
(expect (mline-get-line m20) 0)
(define m00 (mline-insert m20 root-box #t))
(expect (mline-get-line m00) 0)
(expect (mline-get-line m20) 1)
(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 0)
(mline-set-length m00 5)
(mline-set-length m20 20)
(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 5)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line inserts and deletes

(define m5 (mline-insert m20 root-box #t))
(mline-check-consistent (unbox root-box))

(mline-set-length m5 10)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m5) 5)
(expect (mline-get-position m20) 15)

(mline-delete m5 root-box)
(expect (mline-get-position m20) 5)

(set! m5 (mline-insert m20 root-box #t))
(mline-set-length m5 8)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m5) 5)
(expect (mline-get-position m20) 13)

(mline-delete m5 root-box)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line counts and positions

(define m30 (mline-insert m20 root-box #f))

(expect (mline-get-line m00) 0)
(expect (mline-get-line m20) 1)
(expect (mline-get-line m30) 2)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 5)
(expect (mline-get-position m30) 25)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; More line lines and positions

(define m05 (mline-insert m00 root-box #f))

(mline-set-length m05 2)

(expect (mline-get-line m00) 0)
(expect (mline-get-line m05) 1)
(expect (mline-get-line m20) 2)
(expect (mline-get-line m30) 3)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m05) 5)
(expect (mline-get-position m20) 7)
(expect (mline-get-position m30) 27)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line inserts and deletes, radomized

(let ([added
       (let loop ([l (list m00 m05 m20 m30)]
                  [n 100])
         (let ([m (mline-insert (list-ref l (random (length l))) 
                                root-box
                                (zero? (random 2)))])
           (mline-check-consistent (unbox root-box))
           (if (zero? n)
               (cons m l)
               (loop (cons m l) (sub1 n)))))])
  (for-each (lambda (i)
              (mline-delete i root-box)
              (mline-check-consistent (unbox root-box)))
            (cdr added))
  (expect (mline-next (car added)) #f)
  (expect (mline-prev (car added)) #f)
  (expect (unbox root-box)
          (car added)))

;; ----------------------------------------
;; Styles, deltas, lists

(define d1 (new style-delta%))
(define d2 (new style-delta%))
(expect (send d1 get-underlined-on) #f)
(expect (send d1 equal? d2) #t)
(send d1 set-underlined-on #t)
(expect (send d1 equal? d2) #f)
(void (send d2 collapse d1))
(expect (send d2 get-underlined-on) #t)
(send d2 set-underlined-on #f)
(send d1 copy d2)
(expect (send d1 get-underlined-on) #f)

(define sl (new style-list%))
(expect #t (eq? (send sl basic-style) (send sl basic-style)))
(define s-plain (send sl find-or-create-style (send sl basic-style)
                      (new style-delta%)))
(expect (send sl find-or-create-style (send sl basic-style)
              (new style-delta%))
        s-plain)

(send d1 set-underlined-on #t)
(define s-underlined (send sl find-or-create-style s-plain d1))
(expect (send s-plain get-underlined) #f)
(expect (send s-underlined get-underlined) #t)

(send d2 set-underlined-off #t)
(send d2 set-smoothing-on 'partly-smoothed)
(define s-nonunderlined1 (send sl find-or-create-style s-underlined d2))
(expect (send s-nonunderlined1 get-underlined) #f)
(expect (send s-nonunderlined1 get-base-style) (send sl basic-style)) ; due to collpasing

(define s-named-underlined (send sl new-named-style "underlined" s-underlined))
(define s-nonunderlined (send sl find-or-create-style s-named-underlined d2))
(expect (send s-nonunderlined get-underlined) #f)
(expect (send s-nonunderlined get-base-style) s-named-underlined)

(send d1 set-family 'modern)
(define s-modern (send sl find-or-create-style s-plain d1))
(expect (send s-modern get-underlined) #t)
(expect (send s-modern get-family) 'modern)
(expect (send s-plain get-family) 'default)

(expect (send s-plain is-join?) #f)

(define s-modern+nonunderlined (send sl find-or-create-join-style
                                     s-modern
                                     s-nonunderlined))
(expect (send s-modern+nonunderlined get-underlined) #f)
(expect (send s-modern+nonunderlined get-smoothing) 'partly-smoothed)
(expect (send s-modern+nonunderlined get-family) 'modern)
(expect (send s-modern+nonunderlined is-join?) #t)

(send d2 set-smoothing-on 'base)
(send s-nonunderlined set-delta d2)
(expect (send s-nonunderlined get-smoothing) 'default)
(expect (send s-modern+nonunderlined get-smoothing) 'default)

(send d1 set-style-on 'italic)
(send s-modern set-delta d1)
(expect (send s-modern get-style) 'italic)
(expect (send s-modern+nonunderlined get-style) 'italic)

(expect (send s-plain get-alignment) 'bottom)
(expect (send (send s-plain get-background) red) 255)
(expect (send s-plain get-base-style) (send sl basic-style))
(expect (send s-modern+nonunderlined get-base-style) s-modern)
(expect (send s-plain get-face) #f)
(expect (send s-plain get-name) #f)
(expect (send s-plain get-shift-style) (send sl basic-style))
(expect (send s-modern+nonunderlined get-shift-style) s-nonunderlined)
(expect (send s-plain get-size-in-pixels) #f)
(expect (send s-plain get-transparent-text-backing) #t)
(expect (send s-plain get-weight) 'normal)

(expect (send s-nonunderlined get-base-style) s-named-underlined)
(send s-nonunderlined set-base-style s-modern+nonunderlined) ; would create cycle
(expect (send s-nonunderlined get-base-style) s-named-underlined)

(send s-modern+nonunderlined set-base-style s-plain)
(expect (send s-modern+nonunderlined get-family) 'default)
(expect (send s-modern+nonunderlined get-style) 'normal)

(send s-modern+nonunderlined set-shift-style s-modern+nonunderlined) ; would create cycle

(define sl2 (new style-list%))
(define s2-modern (send sl2 convert s-modern))
(expect (send s2-modern get-family) 'modern)

;; ----------------------------------------
;; Lines, positions, paragraphs

(define t (new text%))
(expect (send t get-text) "")
(expect (send t last-position) 0)
(expect (send t get-start-position) 0)
(expect (send t get-end-position) 0)
(expect (send t position-line 0) 0)
(expect (send t position-paragraph 0) 0)

(send t insert "hello")
(expect (send t get-text) "hello")
(expect (send t get-text 3) "lo")
(expect (send t get-text 2 4) "ll")
(expect (send t last-position) 5)
(expect (send t last-line) 0)
(expect (send t get-start-position) 5)
(expect (send t get-end-position) 5)
(expect (send t get-character 1) #\e)
(expect (send t position-line 1) 0)
(expect (send t position-paragraph 1) 0)

(send t insert "!\nbye")
(expect (send t get-text) "hello!\nbye")
(expect (send t last-position) 10)
(expect (send t line-length 0) 7)
(expect (send t line-length 1) 3)
(expect (send t last-line) 1)
(expect (send t line-start-position 0) 0)
(expect (send t line-start-position 1) 7)
(expect (send t line-end-position 0) 6)
(expect (send t position-line 0) 0)
(expect (send t position-line 1) 0)
(expect (send t position-line 6) 0)
(expect (send t position-line 7 #t) 0)
(expect (send t position-line 7) 1)
(expect (send t position-line 10) 1)
(expect (send t position-paragraph 1) 0)
(expect (send t position-paragraph 6) 0)
(expect (send t position-paragraph 7 #t) 1) ; no eol ambiguity for paragraphs
(expect (send t position-paragraph 7) 1)
(expect (send t position-paragraph 8) 1)
(expect (send t get-start-position) 10)
(expect (send t get-end-position) 10)

(send t set-position 7 8)
(expect (send t get-start-position) 7)
(expect (send t get-end-position) 8)
(expect
 (let ([b (box 0)][e (box 0)])
   (list
    (begin (send t get-position b) (unbox b))
    (begin (send t get-position #f e) (list (unbox b) (unbox e)))))
 '(7 (7 8)))

(send t insert ".\t," 2 4)
(expect (send t get-text) "he.\t,o!\nbye")
(expect (send t get-start-position) 8)
(expect (send t get-end-position) 9)

(send t insert "\n3\n" 10)
(expect (send t get-text) "he.\t,o!\nby\n3\ne")
(expect (send t last-line) 3)
(expect (send t get-start-position) 8)
(expect (send t get-end-position) 9)
(send t set-position 100)
(expect (send t get-start-position) 14)
(expect (send t get-end-position) 14)
(send t set-position 14)
(expect (send t get-start-position) 14)
(expect (send t get-end-position) 14)

(send t delete (send t last-position))
(expect (send t get-text) "he.\t,o!\nby\n3\n")
(expect (send t last-line) 3)
(expect (send t get-start-position) 13)
(expect (send t get-end-position) 13)

(send t insert "4" (send t last-position))
(expect (send t get-text) "he.\t,o!\nby\n3\n4")
(expect (send t last-line) 3)
(send t delete 9 11)
(expect (send t last-line) 2)
(expect (send t get-text) "he.\t,o!\nb3\n4")

(send t set-position 2 4)
(send t delete)
(expect (send t get-text) "he,o!\nb3\n4")
(expect (send t last-line) 2)
(expect (send t get-start-position) 2)
(expect (send t get-end-position) 2)
(expect (send t position-line 6) 1)
(expect (send t position-line 7) 1)
(expect (send t position-line 12) 2)

(send t insert (make-object string-snip% "?") 2)
(expect (send t get-text) "he?,o!\nb3\n4")

(expect (send t find-string "o") 4)
(expect (send t find-string "q") #f)
(expect (send t find-string "\n") 6)
(expect (send t find-string "\n" 'forward) 6)
(expect (send t find-string "\n" 'forward 7) 9)
(expect (send t find-string "\n" 'backward 7) 7)
(expect (send t find-string "\n" 'backward 9) 7)
(expect (send t find-string-all "\n") '(6 9))
(expect (send t find-string-all "\n" 'forward 3 7) '(6))
(expect (send t find-string-all "\n" 'backward 8 4) '(7))
(expect (send t find-string-all "\n" 'backward 8 4 #f) '(6))
(expect (send t find-string "\n4") 9)
(expect (send t find-string "O") #f)
(expect (send t find-string "O" 'forward 0 20 #t #f) 4)

(expect (send t find-next-non-string-snip #f) #f)

;; ----------------------------------------

;; Insert very long strings to test max-string-length handling
(send t delete 0 (send t last-position))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 1024 #\a))
(expect (send t last-position) 2048)

;; ----------------------------------------
;; Moving and word boundaries

(send t delete 0 (send t last-position))
(send t insert "do you like\ngreen eggs and ham?")
(expect (send t position-paragraph 0) 0)
(expect (send t position-paragraph 12) 1)
(expect (send t paragraph-start-position 1) 12)
(expect (send t paragraph-start-position 2) 31)
(expect (send t find-newline 'forward 0) 12)
(expect (send t find-newline 'forward 12) 31)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(send t set-position 0)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 2)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 6)
(send t move-position 'left #f 'word)
(expect (send t get-start-position) 3)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 6)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 11)
(send t move-position 'right #f 'simple)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 17)
(send t set-position 11)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 17)

(define (check-positions graphics?)
  (define snips+counts
    (let loop ([snip (send t find-first-snip)])
      (if snip
          (cons (cons snip (send snip get-count))
                (loop (send snip next)))
          null)))

  (let ([x (box 0.0)]
        [y (box 0.0)])
    (let loop ([s+c snips+counts]
               [pos 0])
      (unless (null? s+c)
        (let ([p (send t get-snip-position (caar s+c))])
          (expect p pos)
          (let ([p2 (box 0)])
            (when graphics?
              (if (send t get-snip-position-and-location (caar s+c) p2 x y)
                  (expect (unbox p2) pos)
                  (expect #f #t)))
            (loop (cdr s+c) (+ pos (cdar s+c))))))))

  (for-each 
   (lambda (before)
     (let loop ([pos 0][s+c snips+counts][snip-pos 0])
       (if (null? s+c)
           (expect pos (add1 (send t last-position)))
           (let* ([s-pos (box 0)]
                  [s (send t find-snip pos before s-pos)])
             (let ([es (if (and (= pos 0) (eq? before 'before-or-none))
                           #f
                           (caar s+c))])
               (expect s es)
               (expect (unbox s-pos) snip-pos)
               (let ([next? (= pos (+ snip-pos (cdar s+c)))])
                 (loop (add1 pos)
                       (if next?
                           (cdr s+c)
                           s+c)
                       (if next?
                           (+ snip-pos (cdar s+c))
                           snip-pos))))))))
   '(before before-or-none))

  (for-each 
   (lambda (after)
     (let loop ([pos 0][s+c snips+counts][snip-pos 0][prev #f][prev-snip-pos 0])
       (let* ([s-pos (box 0)]
              [s (send t find-snip pos after s-pos)]
              [end? (null? s+c)]
              [es (if end?
                      (if (eq? after 'after-or-none)
                          #f
                          (car prev))
                      (caar s+c))]
              [ep (if end? (if es prev-snip-pos 0) snip-pos)])
         (expect s es)
         (expect (unbox s-pos) ep)
         (if end?
             (expect pos (send t last-position))
             (let ([next? (= (add1 pos) (+ snip-pos (cdar s+c)))])
               (loop (add1 pos)
                     (if next?
                         (cdr s+c)
                         s+c)
                     (if next?
                         (+ snip-pos (cdar s+c))
                         snip-pos)
                     (car s+c)
                     snip-pos))))))
   '(after after-or-none)))

(check-positions #f)

;; ----------------------------------------
;; Line flow

;; Every character is 10.0 high, 10.0 wide, 1.0 descent, 1.0 top space
(send t set-admin (new test-editor-admin%))

(define (check-simple-locations pl pt pr pb)
  (expect (let ([x (box 0.0)] [y (box 0.0)])
            (list (begin
                    (send t position-location 1 x y)
                    (list (unbox x) (unbox y)))
                  (begin
                    (send t position-location 1 x y #f)
                    (list (unbox x) (unbox y)))))
          (list (list (+ pl 10.0) (+ pt 0.0))
                (list (+ pl 10.0) (+ pt 10.0))))
  (expect (let ([x (box 0.0)] [y (box 0.0)])
            (list (begin
                    (send t position-location 14 x y)
                    (list (unbox x) (unbox y)))
                  (begin
                    (send t position-location 14 x y #f)
                    (list (unbox x) (unbox y)))))
          (list (list (+ pl 20.0) (+ pt 11.0))
                (list (+ pl 20.0) (+ pt 21.0))))
  (expect (let ([w (box 0.0)] [h (box 0.0)])
            (send t get-extent w h)
            (list (unbox w) (unbox h)))
          (list (+ 192.0 pl pr)
                (+ 22.0 pt pb))))
(check-simple-locations 0 0 0 0)

(send t set-padding 5.0 8.0 11.0 13.0)
(check-simple-locations 5 8 11 13)
(send t set-padding 0 0 0 0)

(expect (send t find-position 0.0 0.0) 0)
(expect (send t find-position 0.0 3.0) 0)
(expect (send t find-position 10.0 0.0) 1)
(expect (send t find-position 13.0 0.0) 1)
(expect (send t find-position 0.0 12.0) 12)
(expect (send t find-position 13.0 12.0) 13)
(expect (send t find-position 13.0 23.0) 31)
(expect (send t find-position 0.0 230.0) 31)
(expect (send t find-position 300.0 2.0) 11)
(expect (send t find-position -1.0 12.0) 12)
(expect (send t find-position 109.0 2.0) 10)
(expect (send t find-position 110.0 2.0) 11)
(expect (let ([b (box #f)])
          (send t find-position 1.0 12.0 #f b)
          (unbox b))
        #t)
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (send t find-position -1.0 12.0 #f b e)
          (list (unbox b) (unbox e)))
        '(#f 100.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 109.0 2.0 #f b e)
                (unbox b) 
                (unbox e)))
        '(10 #t 1.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 102.0 2.0 #f b e)
                (unbox b) 
                (unbox e)))
        '(10 #t -2.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 110.0 2.0 #f b e)
                (unbox b)
                (unbox e)))
        '(11 #f 100.0))
(expect (send t find-position-in-line 0 14.0) 1)
(expect (send t find-position-in-line 1 14.0) 13)

(send t set-position 1 1)
(send t move-position 'down #f 'line)
(expect (send t get-start-position) 13)
(send t move-position 'right #f 'simple)
(send t move-position 'up #f 'line)
(expect (send t get-start-position) 2)

(check-positions #t)

(send t set-max-width 71.0)

(define (check-ge&h-flow)
  (expect (send t last-line) 6)
  (expect (send t line-start-position 0) 0)
  (expect (send t line-start-position 1) 3)
  (expect (send t line-start-position 2) 7)
  (expect (send t line-start-position 3) 12)
  (expect (send t line-start-position 4) 18)
  (expect (send t line-start-position 5) 23)
  (expect (send t line-start-position 6) 27)
  (expect (send t last-paragraph) 1)
  (expect (send t paragraph-start-position 0) 0)
  (expect (send t paragraph-end-position 0) 11)
  (expect (send t paragraph-start-position 1) 12)
  (expect (send t paragraph-end-position 1) 31)
  (expect (send t paragraph-start-position 2) 31)
  (void))
(check-ge&h-flow)

(check-positions #t)

(send t set-max-width 200.0)
(expect (send t last-line) 1)

(send t set-max-width 71.0)
(check-ge&h-flow)

(send t insert "Sir: " 0)
(expect (send t last-line) 7)
(expect (send t line-start-position 7) 32)
(send t delete 0 5)
(check-ge&h-flow)

(define (check-line-starts)
  (let ([lens (let loop ([snip (send t find-first-snip)][len 0])
                (if snip
                    (let ([len (+ len (send snip get-count))])
                      (let ([s (send snip get-text 0 (send snip get-count))])
                        (when (regexp-match? #rx"\n" s)
                          (unless (and (memq 'hard-newline (send snip get-flags))
                                       (string=? s "\n"))
                            (error "embedded newline!")))
                        (if (or (memq 'newline (send snip get-flags))
                                (memq 'hard-newline (send snip get-flags)))
                            (cons len (loop (send snip next) 0))
                            (loop (send snip next) len))))
                    (list len)))])
    (for/fold ([pos 0]) ([i (in-range (add1 (send t last-line)))]
                         [len (in-list lens)])
      (expect (send t line-start-position i #f) pos)
      (expect (send t line-end-position i #f) (+ pos len))
      (+ pos len))))

(for-each
 (lambda (str)
   ;; (printf ">> ~s <<\n" str)
   (for ([i (in-range (add1 (send t last-position)))])
     (check-line-starts)
     (send t insert str i)
     (check-line-starts)
     ;; (printf "=> ~a ~s\n" i (send t get-text 0 'eof #t #t))
     (send t last-line)
     (send t delete i (+ i (string-length str)))
     (check-line-starts)
     ;; (printf "~a ~s <=\n" i (send t get-text 0 'eof #t #t))
     (check-ge&h-flow)))
 '(" a" "a " "qvzxw " " qvxzw" "qqq qqqq" "a\nb"))

;; ----------------------------------------
;; Undo

(send t set-modified #f)
(send t set-max-undo-history 100)
(send t delete 0 3)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(expect (send t modified?) #t)
(send t undo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(expect (send t modified?) #f)
(send t redo)
(expect (send t modified?) #t)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(send t set-position 0)
(send t insert #\d)
(send t insert #\o)
(send t insert #\space)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(send t undo)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(send t redo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")

(send t begin-edit-sequence)
(send t delete 0 3)
(send t delete (- (send t last-position) 4) (send t last-position))
(send t end-edit-sequence)

(expect (send t get-text) "you like\ngreen eggs and ")
(send t delete 0 4)
(expect (send t get-text) "like\ngreen eggs and ")
(send t undo)
(send t undo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")

;; ----------------------------------------
;; Stream out base

(define fbo (make-object editor-stream-out-bytes-base%))
(expect (send fbo tell) 0)
(expect (send fbo write-bytes #"abc") (void))
(expect (send fbo tell) 3)
(expect (send fbo get-bytes) #"abc")
(send fbo seek 2)
(expect (send fbo write-bytes #"012345" 1 4) (void))
(expect (send fbo tell) 5)
(expect (send fbo get-bytes) #"ab123")
(expect (send fbo bad?) #f)
(expect (send fbo write '(#\o #\l #\d)) (void))
(expect (send fbo get-bytes) #"ab123old")

;; ----------------------------------------
;; Stream in base

(define fbi (make-object editor-stream-in-bytes-base% #"ab123old"))
(define ibuf (make-bytes 3))
(expect (send fbi tell) 0)
(expect (send fbi read-bytes ibuf) 3)
(expect ibuf #"ab1")
(expect (send fbi tell) 3)
(send fbi seek 2)
(expect (send fbi read-bytes ibuf 1 2) 1)
(expect ibuf #"a11")
(send fbi skip 2)
(expect (send fbi read-bytes ibuf 0 2) 2)
(expect ibuf #"ol1")
(expect (send fbi bad?) #f)

;; ----------------------------------------
;; Stream writing

(define fbo2 (make-object editor-stream-out-bytes-base%))
(define fo (make-object editor-stream-out% fbo2))

(expect (send fo tell) 0)
(void (send fo put 2))
(expect (send fbo2 get-bytes) #"\n2")
(void (send fo put 2.0))
(expect (send fbo2 get-bytes) #"\n2 2.0")
(expect (send fo tell) 2)
(send fo jump-to 0)
(void (send fo put 3))
(send fo jump-to 2)
(expect (send fbo2 get-bytes) #"\n3 2.0")
(void (send fo put #"hi"))
(expect (send fbo2 get-bytes) #"\n3 2.0 3 #\"hi\\0\"")
(void (send fo put 3 #"bye?"))
(expect (send fbo2 get-bytes) #"\n3 2.0 3 #\"hi\\0\"\n3 #\"bye\"")
(void (send fo put 80 #"0123456789abcdefghij0123456789ABCDEFGHIJ0123456789abcdefghij0123456\"89ABCDEFGHIJ"))
(expect (send fbo2 get-bytes) 
        (bytes-append
         #"\n3 2.0 3 #\"hi\\0\"\n3 #\"bye\"\n80\n"
         #"(\n"
         #" #\"0123456789abcdefghij0123456789ABCD\"\n"
         #" #\"EFGHIJ0123456789abcdefghij0123456\\\"89ABCDEFGHIJ\"\n"
         #")"))

(define fbo3 (make-object editor-stream-out-bytes-base%))
(define fo3 (make-object editor-stream-out% fbo3))
(void (send fo3 put 2))
(expect (send fo3 tell) 1)
(void (send fo3 put-fixed 5))
(expect (send fo3 tell) 2)
(void (send fo3 put-fixed -8))
(void (send fo3 put 2 #"hi"))
(expect (send fbo3 get-bytes) #"\n2           5          -8 2 #\"hi\"")
(send fo3 jump-to 1)
(void (send fo3 put-fixed -4))
(send fo3 jump-to 2)
(void (send fo3 put-fixed 7))
(expect (send fbo3 get-bytes) #"\n2          -4           7 2 #\"hi\"")

;; ----------------------------------------
;; Stream reading

(define fbi2 (make-object editor-stream-in-bytes-base% (bytes-append #"1 ; comment \n 2 "
                                                                     #"#| | x # #|  |# q |# 4.0"
                                                                     #" 2 #\"hi\""
                                                                     #" 3 #\"hi\\\"\""
                                                                     #" 23 ( #\"0123456789ABCDEFappl\" #\"e!\\0\" ) 88")))
(define fi2 (make-object editor-stream-in% fbi2))

(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 0)
(expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 1)
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 1)
(expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 2)
(expect (send fi2 ok?) #t)
(expect (let ([b (box 0.0)]) (send fi2 get b) (unbox b)) 4.0)
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 3)
(expect (send fi2 get-unterminated-bytes) #"hi")
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 5)
(expect (send fi2 get-unterminated-bytes) #"hi\"")
(expect (send fi2 ok?) #t)
(expect (send fi2 get-bytes) #"0123456789ABCDEFapple!")
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 9)

(send fi2 jump-to 3)
(expect (send fi2 tell) 3)
(expect (send fi2 get-unterminated-bytes) #"hi")
(send fi2 skip 4)
(expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 88)
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 10)

(send fi2 jump-to 3)
(send fi2 set-boundary 2)
(expect (send fi2 get-unterminated-bytes) #"hi")
(send fi2 jump-to 3)
(expect (send fi2 ok?) #t)
(expect (send fi2 tell) 3)
(send fi2 set-boundary 1)
(expect (with-handlers ([values (lambda (exn) #"")])
          (send fi2 get-unterminated-bytes))
        #"")
(expect (send fi2 ok?) #f)

;; ----------------------------------------
;; Save & load

(send t delete 0 (send t last-position))
(send t clear-undos)
(send t insert "one\ntwo\n")
(send t set-position 0 3)
(send t copy #f 0)
(send t set-position 8)
(send t paste 0) ;; probably uses the snip% `copy' method
(expect (send t get-text) "one\ntwo\none")
(define (move-to-serialized-clipboard)
  (let ([data (send the-clipboard get-clipboard-data "WXME" 0)])
    (send the-clipboard set-clipboard-client
          (new (class clipboard-client%
                 (inherit add-type)
                 (super-new)
                 (add-type "WXME")
                 (define/override (get-data format) data)))
          0)))
(move-to-serialized-clipboard)
(send t paste 0) ;; uses above clipboard
(expect (send t get-text) "one\ntwo\noneone")
(send the-clipboard set-clipboard-string "\u3BB" 0)
(send t paste 0)
(expect (send t get-text) "one\ntwo\noneone\u3BB")

(send t set-position 3 4)
(send t copy #f 0)
(send t set-position 4 7)
(send t copy #t 0)
(send t set-position (send t last-position))
(send t paste 0)
(expect (send t get-text) "one\ntwo\noneone\u3BB\ntwo")
(send t paste-next)
(expect (send t get-text) "one\ntwo\noneone\u3BBone")

(send t cut #f 0 0 4)
(expect (send t get-text) "two\noneone\u3BBone")

(define-values (in7 out7) (make-pipe))
(expect (send t save-port out7 'text) #t)
(close-output-port out7)
(expect (read-string 100 in7)  "two\noneone\u3BBone")

(define out8 (open-output-bytes))
(expect (send t save-port out8 'standard) #t)
(define in8 (open-input-bytes (get-output-bytes out8)))
(expect (peek-bytes 31 0 in8) #"#reader(lib\"read.ss\"\"wxme\")WXME")
(send t erase)
(expect (send t get-text) "")
(expect (send t insert-port in8) 'standard)
(expect (send t get-text) "two\noneone\u3BBone")

;; ----------------------------------------
;; Styles on text

(define (check-color pos r g b w)
  (let* ([s (send (send t find-snip pos 'after) get-style)]
         [c (send s get-foreground)]
         [f (send s get-font)])
    (expect (send c red) r)
    (expect (send c green) g)
    (expect (send c blue) b)
    (expect (send f get-weight) w)))

(send t erase)
(send t insert "red\nblue")
(check-color 0 0 0 0 'normal)
(let ([d (send (new style-delta%) set-delta-foreground (make-object color% 255 0 0))])
  (send d set-weight-on 'bold)
  (send t change-style d 0 3))
(send t change-style
      (send (new style-delta%) set-delta-foreground (make-object color% 0 0 255))
      4 8)
(check-color 0 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(define out9 (open-output-bytes))
(expect (send t save-port out9 'standard) #t)
(define in9 (open-input-bytes (get-output-bytes out9)))
(send t erase)
(expect (send t insert-port in9) 'standard)
(expect (send t get-text) "red\nblue")
(check-color 0 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(define (check-random-delta d)
  (expect (send d get-alignment-on) 'top)
  (expect (send d get-alignment-off) 'base)
  (expect (send (send d get-background-add) get-r) 25)
  (expect (send (send d get-background-add) get-g) 25)
  (expect (send (send d get-background-add) get-b) 25)
  (expect (send (send d get-background-mult) get-r) 0.5)
  (expect (send (send d get-background-mult) get-g) 0.5)
  (expect (send (send d get-background-mult) get-b) 0.5)
  (expect (send (send d get-foreground-add) get-r) 50)
  (expect (send (send d get-foreground-add) get-g) 50)
  (expect (send (send d get-foreground-add) get-b) 50)
  (expect (send (send d get-foreground-mult) get-r) 0.6)
  (expect (send (send d get-foreground-mult) get-g) 0.6)
  (expect (send (send d get-foreground-mult) get-b) 0.6)
  (expect (send d get-face) "Purty") 
  (expect (send d get-family) 'decorative)
  (expect (send d get-size-in-pixels-on) #t)
  (expect (send d get-size-in-pixels-off) #f)
  (expect (send d get-smoothing-off) 'smoothed)
  (expect (send d get-smoothing-on) 'base)
  (expect (send d get-style-on) 'italic)
  (expect (send d get-style-off) 'base)
  (expect (send d get-transparent-text-backing-on) #t)
  (expect (send d get-transparent-text-backing-off) #f)
  (expect (send d get-underlined-off) #t)
  (expect (send d get-underlined-on) #f)
  (expect (send d get-weight-on) 'light)
  (expect (send d get-weight-off) 'base))

(let ([d (new style-delta%)])
  (send d set-alignment-on 'top)
  (send (send d get-background-add) set 25 25 25)
  (send (send d get-background-mult) set 0.5 0.5 0.5)
  (send (send d get-foreground-add) set 50 50 50)
  (send (send d get-foreground-mult) set 0.6 0.6 0.6)
  (send d set-delta-face "Purty" 'decorative)
  (send d set-size-in-pixels-on #t)
  (send d set-smoothing-off 'smoothed)
  (send d set-style-on 'italic)
  (send d set-transparent-text-backing-on #t)
  (send d set-underlined-off #t)
  (send d set-weight-on 'light)

  (check-random-delta d)

  (let* ([sl (send t get-style-list)]
         [s (send sl find-or-create-style (send sl basic-style) d)])
    (send t change-style s 0 1)))

(define out10 (open-output-bytes))
(expect (send t save-port out10 'standard) #t)
(define in10 (open-input-bytes (get-output-bytes out10)))
(send t erase)
(expect (send t insert-port in10 'guess #t) 'standard)
(expect (send t get-text) "red\nblue")
(check-color 0 50 50 50 'light)
(check-color 1 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(let ([d (new style-delta%)])
  (send (send (send t find-first-snip) get-style) get-delta d)
  (check-random-delta d))

;; ----------------------------------------
;; Keymaps

(define km (new keymap%))
(define hit #f)
(define kevt (new key-event%))

(send km add-function "letter-a" (lambda (obj evt) (set! hit #\a)))
(send km add-function "letter-m" (lambda (obj evt) (set! hit #\m)))
(send km add-function "letter-n" (lambda (obj evt) (set! hit #\n)))
(send km add-function "letter-up" (lambda (obj evt) (set! hit 'up)))
(send km add-function "letter-UP" (lambda (obj evt) (set! hit 'UP)))
(send km add-function "letter-down" (lambda (obj evt) (set! hit 'down)))
(send km add-function "letter-DOWN" (lambda (obj evt) (set! hit 'DOWN)))

(send km map-function "a" "letter-a")
(send kevt set-key-code #\x)
(expect (send km handle-key-event 'obj kevt) #f)
(send kevt set-key-code #\a)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\a)

(send km map-function "up" "letter-up")
(send kevt set-key-code 'up)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'up)
(set! hit #f)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'up)

(send km map-function "s:up" "letter-UP")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'UP)

(send km map-function ":down" "letter-down")
(send kevt set-key-code 'down)
(send kevt set-shift-down #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)
(set! hit #f)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)

(send km map-function "s:down" "letter-DOWN")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'DOWN)

(expect (with-handlers ([values
                         (lambda (exn)
                           (and (regexp-match? #rx"mapped as a non-prefix key" (exn-message exn))
                                'bad-remap))])
          (send km map-function "s:down;z" "oops"))
        'bad-remap)

;; Check sequence
(set! hit #f)
(send km map-function "d;O" "letter-down")
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-key-code #\o)
(expect (send km handle-key-event 'obj kevt) #f)
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)

;; Interrupt sequence
(set! hit #f)
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send km break-sequence)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)
(expect hit #f)

;; Check success with alternate, then override with more specific non-alternate
(send kevt set-key-code #\m)
(send kevt set-other-shift-key-code #\n)
(send kevt set-shift-down #f)
(send km map-function "?:n" "letter-n")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\n)
(send km map-function "?:m" "letter-m")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\m)

(define km2 (new keymap%))
(send km chain-to-keymap km2 #t)

;; Chained keymap more specific overrides less specific
(send km2 add-function "letter-n2" (lambda (obj evt) (set! hit 'n2)))
(send km2 map-function "n" "letter-n2")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\m)
(send kevt set-key-code #\n)
(send kevt set-other-shift-key-code #\p)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'n2)

;; Check sequence in chained keymap
(send km2 add-function "letter-t" (lambda (obj evt) (set! hit #\t)))
(send km2 map-function "c:x;t" "letter-t")
(send kevt set-key-code #\x)
(send kevt set-control-down #t)
(send kevt set-other-shift-key-code #f)
(set! hit #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-control-down #f)
(send kevt set-key-code #\t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\t)

;; Chained keymap non-prefixed overrides prefixed
(send km2 add-function "letter-d" (lambda (obj evt) (set! hit #\d)))
(send km2 map-function "d" "letter-d")
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\d)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)
(expect hit #\d)

;; Remove chained keymap
(send km remove-chained-keymap km2)
(send kevt set-key-code #\d)
(send kevt set-shift-down #f)
(set! hit #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)

;; Key grab
(send kevt set-key-code #\m)
(send kevt set-shift-down #f)
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect km-in km)
                                 (expect evt kevt)
                                 (set! hit (list str ed))
                                 #t))
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit '("letter-m" obj))
(send kevt set-key-code #\p)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit '(#f obj))
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect str "letter-m")
                                 (expect ed 'obj2)
                                 (set! hit 'nope)
                                 #f))
(send kevt set-key-code #\m)
(expect (send km handle-key-event 'obj2 kevt) #t)
(expect hit #\m)
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect str #f)
                                 (expect ed 'obj3)
                                 (set! hit 'nope)
                                 #f))
(send kevt set-key-code #\p)
(expect (send km handle-key-event 'obj3 kevt) #f)
(expect hit 'nope)

;; Mouse events
(define mevt/l (new mouse-event% [event-type 'left-down]))
(send mevt/l set-left-down #t)
(send km add-function "mouse-right" (lambda (obj evt) (set! hit 'right)))
(send km add-function "mouse-left" (lambda (obj evt) (set! hit 'left)))
(send km add-function "mouse-left2" (lambda (obj evt) (set! hit 'left2)))

(expect (send km handle-mouse-event 'obj mevt/l) #f)
(send mevt/l set-time-stamp 501) ;; FIXME: depends on double-click time
(send km map-function "leftbutton" "mouse-left")
(send km map-function "leftbuttondouble" "mouse-left2")
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left2)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)
(send mevt/l set-time-stamp 10100)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)

(set! hit #f)
(send km map-function "rightbuttonseq" "mouse-right")
(define mevt/r (new mouse-event% [event-type 'right-down]))
(send mevt/r set-right-down #t)
(define mevt/r/up (new mouse-event% [event-type 'right-up]))
(expect (send km handle-mouse-event 'obj mevt/r) #t)
(expect hit 'right)
(set! hit #f)
(expect (send km handle-mouse-event 'obj mevt/r/up) #t)
(expect hit 'right)

(send km set-grab-mouse-function (lambda (str km-in ed evt)
                                   (set! hit 'm)
                                   #t))
(define mevt/m (new mouse-event% [event-type 'middle-down]))
(send mevt/m set-middle-down #t)
(expect (send km handle-mouse-event 'obj mevt/m) #t)
(expect hit 'm)
(send km remove-grab-mouse-function)
(expect (send km handle-mouse-event 'obj mevt/m) #f)

;; ----------------------------------------
;; editor snips, content

(define oe (new text%))
(define ie (new text%))
(define es (new editor-snip% [editor ie]))
(send ie insert "Hello")
(send oe insert es)

(expect (send oe get-text 0 'eof #f) ".")
(expect (send oe get-flattened-text) "Hello")

(send es show-border #t)
(expect (send es border-visible?) #t)
(send es set-margin 1 2 3 4)
(define (check-border es)
  (let ([l (box 0)][t (box 0)][r (box 0)][b (box 0)])
    (send es get-margin l t r b)
    (expect (list (unbox l) (unbox t) (unbox r) (unbox b))
            (list 1 2 3 4))))
(check-border es)

(send oe set-position 0 1)
(send oe copy #f 0)
(send oe set-position 1)
(send oe paste 0) ;; probably uses the snip% `copy' method
(expect (send oe last-position) 2)
(define es2 (send oe find-snip 1 'after-or-none))
(check-border es2)
(move-to-serialized-clipboard)
(send oe paste 0) ;; uses above clipboard
(define es3 (send oe find-snip 2 'after-or-none))
(check-border es3)
(expect (send es3 border-visible?) #t)
(expect (send es3 get-align-top-line) #f)

(send (send es2 get-editor) insert "zzz" 2 2)
(expect (send oe get-text 0 'eof #f) "...")
(expect (send oe get-flattened-text) "HelloHezzzlloHello")

(send oe insert "a\n" 0)
(send oe insert "\nb" (send oe last-position))
(expect (send oe get-flattened-text) "a\nHelloHezzzlloHello\nb")

;; ----------------------------------------
;; editor snips, locations

(send oe set-admin (new test-editor-admin%))
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send oe get-extent w h)
          (list (unbox w) (unbox h)))
        '(197.0 40.0))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (list (begin
                  (send oe position-location 0 x y)
                  (list (unbox x) (unbox y)))
                (begin
                  (send oe position-location 1 x y #f)
                  (list (unbox x) (unbox y)))))
        '((0.0 0.0) (10.0 10.0)))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (list (begin
                  (send oe position-location 2 x y)
                  (list (unbox x) (unbox y)))
                (begin
                  (send oe position-location 3 x y #f)
                  (list (unbox x) (unbox y)))))
        '((0.0 11.0) (55.0 28.0)))

(send (send es2 get-editor) insert "\nmore" 100)
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send oe get-extent w h)
          (list (unbox w) (unbox h)))
        '(197.0 51.0))

;; ----------------------------------------
;; Pasteboard

(define pb (new pasteboard%))
(expect (send pb find-first-snip) #f)
(expect (send pb find-snip 10.0 10.0) #f)
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send pb get-extent w h)
          (list (unbox w) (unbox h)))
        '(0.0 0.0))

(define ss1 (new string-snip%))
(send ss1 insert "one" 3)
(send pb insert ss1 12.0 17.5)
(expect (send pb find-first-snip) ss1)
(expect (send pb get-flattened-text) "one")

(define ss2 (new string-snip%))
(send ss2 insert "two!" 4)
(send pb insert ss2 ss1 32.0 7.5)
(expect (send pb find-first-snip) ss2)
(expect (send pb get-flattened-text) "two!one")
(send pb lower ss2)
(expect (send pb get-flattened-text) "onetwo!")
(send pb raise ss2)
(expect (send pb get-flattened-text) "two!one")

(send pb set-admin (new test-editor-admin%))
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send pb get-extent w h)
          (list (unbox w) (unbox h)))
        '(74.0 29.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #t)
          (list (unbox x) (unbox y)))
        '(72.0 17.5))
(send ss2 insert "more" 4 3)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #t)
          (list (unbox x) (unbox y)))
        '(112.0 17.5))
(expect (send pb get-flattened-text) "twomore!one")

(send pb no-selected)
(expect (send pb find-next-selected-snip #f) #f)
(send pb add-selected ss1)
(expect (send pb find-next-selected-snip #f) ss1)
(expect (send pb find-next-selected-snip ss1) #f)
(send pb no-selected)
(send pb add-selected 0.0 0.0 10.0 10.0)
(expect (send pb find-next-selected-snip #f) #f)
(send pb add-selected 10.0 10.0 20.0 20.0)
(expect (send pb find-next-selected-snip #f) ss1)
(expect (send pb find-next-selected-snip ss1) #f)
(send pb add-selected 10.0 10.0 40.0 40.0)
(expect (send pb find-next-selected-snip #f) ss2)
(expect (send pb find-next-selected-snip ss2) ss1)

(send pb set-max-undo-history 10)

(send pb move 3 4)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss1 x y #f)
          (list (unbox x) (unbox y)))
        '(15.0 21.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(35.0 11.5))
(send pb undo)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss1 x y #f)
          (list (unbox x) (unbox y)))
        '(12.0 17.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

(send pb remove-selected ss1)
(expect (send pb find-snip 15.0 20.0) ss1)
(expect (send pb find-snip 35.0 10.0) ss2)
(expect (send pb find-first-snip) ss2)
(send pb delete) ; "delete"
(expect (send pb find-first-snip) ss1)
(expect (send pb find-snip 15.0 20.0) ss1)
(expect (send pb find-snip 35.0 10.0) #f)
(send pb undo) ; "undo"
(expect (send pb find-first-snip) ss2)
(expect (send pb find-snip 35.0 10.0) ss2)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

(define out20 (open-output-bytes))
(expect (send pb save-port out20 'standard) #t)
(define in20 (open-input-bytes (get-output-bytes out20)))
(expect (peek-bytes 31 0 in20) #"#reader(lib\"read.ss\"\"wxme\")WXME")

(define t10 (make-object text%))
(expect (send t10 insert-port in20) 'standard)
(expect (send t10 get-flattened-text) "twomore!one")

(define in21 (open-input-bytes (get-output-bytes out20)))
(define pb2 (make-object pasteboard%))
(expect (send pb2 insert-port in21) 'standard)
(expect (send pb2 get-flattened-text) "twomore!one")
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb2 get-snip-location (send pb2 find-first-snip) x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

;; ----------------------------------------

(let ()
  (define (mk) (make-object image-snip% (collection-file-path "b-run.png" "icons") 'unknown #f #f))
  
  (define is (mk))
  (define copy-is
    (let ()
      (define sp (open-output-string))
      (define t (new text%))
      (send t insert (mk))
      (send t save-port sp)
      (define t2 (new text%))
      (send t2 insert-port (open-input-string (get-output-string sp)))
      (send t2 find-first-snip)))
  
  (expect (send (mk) get-filename)
          (send copy-is get-filename)))


;; ----------------------------------------
;; get-extend-start-position and
;; get-extend-end-position

(let ([t (new text%)])
  (send t insert (make-string 40 #\a))
  (send t set-position 10 20)
  
  (expect (send t get-start-position) 10)
  (expect (send t get-end-position) 20)
  (expect (send t get-extend-start-position) 10)
  (expect (send t get-extend-end-position) 20)
  
  (send t set-anchor #t)
  (send t set-position 5 25)
  
  (expect (send t get-start-position) 5)
  (expect (send t get-end-position) 25)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25)
  
  (send t extend-position 30)
  (expect (send t get-start-position) 5)
  (expect (send t get-end-position) 30)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25)

  (send t extend-position 0)
  (expect (send t get-start-position) 0)
  (expect (send t get-end-position) 25)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25))

;; ----------------------------------------

(let ()
  (define t (new text%))
  (send t insert "1\n12\n123\n")
  (expect (send t paragraph-start-position 3) 9)
  (expect (send t paragraph-end-position 3) 9)
  (expect (send t line-end-position 3) 9))

(let ()
  (define t (new text%))
  (send t insert "1\n12\n123\n\n")
  (expect (send t paragraph-start-position 3) 9)
  (expect (send t paragraph-end-position 3) 9)
  (expect (send t line-end-position 3) 9))

;; ----------------------------------------
;; tabs

(let ([t1 (new text%)])
  (send t1 set-admin (new test-editor-admin%))  
  (send t1 set-tabs '(100 200 300 400 500 600 700 800 900 1000 100) 1 #t)
  (send t1 insert "Hello\tWorld")
  (send t1 get-extent (box 0) (box 0)))


;; ----------------------------------------

(done)
