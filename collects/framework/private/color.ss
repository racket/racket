#lang scheme/unit
  (require (lib "class.ss")
           (lib "thread.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "token-tree.ss" "syntax-color")
           (lib "paren-tree.ss" "syntax-color")
           (lib "default-lexer.ss" "syntax-color")
           string-constants/string-constant
           "../preferences.ss"
           "sig.ss")
  
  (import [prefix icon: framework:icon^]
          [prefix mode: framework:mode^]
          [prefix text: framework:text^]
          [prefix color-prefs: framework:color-prefs^]
          [prefix scheme: framework:scheme^])
  
  (export (rename framework:color^
                  (-text<%> text<%>)
                  (-text% text%)
                  (-text-mode<%> text-mode<%>)))
  
  (init-depend framework:text^ framework:mode^)
  
  (define (should-color-type? type)
    (not (memq type '(white-space no-color))))
  
  (define -text<%>
    (interface (text:basic<%>)
      start-colorer
      stop-colorer
      force-stop-colorer
      
      is-stopped?
      is-frozen?
      freeze-colorer
      thaw-colorer
      
      reset-region
      get-region
      update-region-end
      
      skip-whitespace
      backward-match
      backward-containing-sexp
      forward-match
      insert-close-paren
      classify-position))
  
  (define text-mixin
    (mixin (text:basic<%>) (-text<%>)
      
      ;; For profiling
      (define timer #f)
      
      ;; ---------------------- Coloring modes ----------------------------
      
      ;; The tokenizer is stopped.  This is used by the surrogate to enter
      ;; a mode with no coloring or paren matching.
      (define stopped? #t)
      
      ;; The tokenizer is stopped and prevented from starting.  This is 
      ;; an internal call for debugging.
      (define force-stop? #f)
      
      ;; color-callback has been suspended because the text% became locked
      ;; and should be requeued when the text% is unlocked.
      (define restart-callback #f)
      
      ;; Some other tool wants to take over coloring the buffer, so the 
      ;; colorer shouldn't color anything.
      (define frozen? #f)
      ;; true iff the colorer must recolor from scratch when the freeze
      ;; is over.
      (define force-recolor-after-freeze #f)
      
      ;; ---------------------- Lexing state ------------------------------
      
      ;; The tree of valid tokens, starting at start-pos
      (define tokens (new token-tree%))
      
      ;; If the tree is completed
      (define up-to-date? #t)
      (define/public (get-up-to-date?) up-to-date?)
      
      ;; The tree of tokens that have been invalidated by an edit
      ;; but might still be valid.
      (define invalid-tokens (new token-tree%))
      
      ;; The position right before the invalid-tokens tree
      (define invalid-tokens-start +inf.0)
      
      ;; The position right before the next token to be read
      (define current-pos start-pos)
      
      ;; The lexer
      (define get-token #f)
      
      ;; ---------------------- Parenethesis matching ----------------------
      
      ;; The pairs of matching parens
      (define pairs '())
      (define parens (new paren-tree% (matches pairs)))
      
      
      ;; ---------------------- Interactions state ------------------------
      ;; The positions right before and right after the area to be tokenized
      (define start-pos 0)
      (define end-pos 'end)
      
      (inherit last-position)
      
      ;; See docs
      (define/public (reset-region start end)
        (unless (and (= start start-pos) (eqv? end end-pos))
          (unless (<= 0 start (last-position))
            (raise-mismatch-error 'reset-region
                                  "start position not inside editor: "
                                  start))
          (unless (or (eq? 'end end) (<= 0 end (last-position)))
            (raise-mismatch-error 'reset-region
                                  "end position not inside editor: "
                                  end))
          (unless (or (eq? 'end end) (<= start end))
            (raise-mismatch-error 'reset-region
                                  "end position before start position: "
                                  (list end start)))
          (set! start-pos start)
          (set! end-pos end)
          (reset-tokens)
          (do-insert/delete start 0)))
      
      (define/public (get-region) (values start-pos end-pos))
      
      ;; Modify the end of the region.
      (define/public (update-region-end end)
        (set! end-pos end))
      
      ;; ---------------------- Preferences -------------------------------
      (define should-color? #t)
      (define token-sym->style #f)
      
      ;; ---------------------- Multi-threading ---------------------------
      ;; A list of thunks that color the buffer
      (define colors null)
      ;; The coroutine object for tokenizing the buffer
      (define tok-cor #f)
      ;; The editor revision when tok-cor was created
      (define rev #f)
      
      
      (inherit change-style begin-edit-sequence end-edit-sequence highlight-range
               get-style-list in-edit-sequence? get-start-position get-end-position
               local-edit-sequence? get-styles-fixed has-focus?
               get-fixed-style)
      
      (define/private (reset-tokens)
        (send tokens reset-tree)
        (send invalid-tokens reset-tree)
        (set! invalid-tokens-start +inf.0)
        (set! up-to-date? #t)
        (set! restart-callback #f)
        (set! force-recolor-after-freeze #f)
        (set! parens (new paren-tree% (matches pairs)))
        (set! current-pos start-pos)
        (set! colors null)
        (when tok-cor
          (coroutine-kill tok-cor))
        (set! tok-cor #f)
        (set! rev #f))
      
      ;; Actually color the buffer.
      (define/private (color)
        (unless (null? colors)
          ((car colors))
          (set! colors (cdr colors))
          (color)))
      
      ;; Discard extra tokens at the first of invalid-tokens
      (define/private (sync-invalid)
        (when (and (not (send invalid-tokens is-empty?))
                   (< invalid-tokens-start current-pos))
          (send invalid-tokens search-min!)
          (let ((length (send invalid-tokens get-root-length)))
            (send invalid-tokens remove-root!)
            (set! invalid-tokens-start (+ invalid-tokens-start length)))
          (sync-invalid)))
      
      (define/private (re-tokenize in in-start-pos enable-suspend)
        (let-values ([(lexeme type data new-token-start new-token-end) 
                      (get-token in)])
          (unless (eq? 'eof type)
            (enable-suspend #f)
            #;(printf "~a at ~a to ~a~n" lexeme (+ in-start-pos (sub1 new-token-start))
                      (+ in-start-pos (sub1 new-token-end)))
            (let ((len (- new-token-end new-token-start)))
              (set! current-pos (+ len current-pos))
              (sync-invalid)
              (when (and should-color? (should-color-type? type) (not frozen?))
                (set! colors
                      (cons
                       (let* ([style-name (token-sym->style type)]
                              (color (send (get-style-list) find-named-style style-name))
                              (sp (+ in-start-pos (sub1 new-token-start)))
                              (ep (+ in-start-pos (sub1 new-token-end))))
                         (λ ()
                           (change-style color sp ep #f)))
                       colors)))
              ; Using the non-spec version takes 3 times as long as the spec
              ; version.  In other words, the new greatly outweighs the tree
              ; operations.
              ;(insert-last! tokens (new token-tree% (length len) (data type)))
              (insert-last-spec! tokens len type)
              (send parens add-token data len)
              (cond
                ((and (not (send invalid-tokens is-empty?))
                      (= invalid-tokens-start current-pos))
                 (send invalid-tokens search-max!)
                 (send parens merge-tree
                       (send invalid-tokens get-root-end-position))
                 (insert-last! tokens invalid-tokens)
                 (set! invalid-tokens-start +inf.0)
                 (enable-suspend #t))
                (else
                 (enable-suspend #t)
                 (re-tokenize in in-start-pos enable-suspend)))))))
      
      (define/private (do-insert/delete edit-start-pos change-length)
        (unless (or stopped? force-stop?)
          (unless up-to-date?
            (sync-invalid))
          (cond
            (up-to-date?
             (let-values 
                 (((orig-token-start orig-token-end valid-tree invalid-tree)
                   (send tokens split (- edit-start-pos start-pos))))
               (send parens split-tree orig-token-start)
               (set! invalid-tokens invalid-tree)
               (set! tokens valid-tree)
               (set! invalid-tokens-start
                     (if (send invalid-tokens is-empty?)
                         +inf.0
                         (+ start-pos orig-token-end change-length)))
               (set! current-pos (+ start-pos orig-token-start))
               (set! up-to-date? #f)
               (queue-callback (λ () (colorer-callback)) #f)))
            ((>= edit-start-pos invalid-tokens-start)
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (send invalid-tokens split (- edit-start-pos start-pos))))
               (set! invalid-tokens invalid-tree)
               (set! invalid-tokens-start
                     (+ invalid-tokens-start tok-end change-length))))
            ((> edit-start-pos current-pos)
             (set! invalid-tokens-start (+ change-length invalid-tokens-start)))
            (else
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (send tokens split (- edit-start-pos start-pos))))
               (send parens truncate tok-start)
               (set! tokens valid-tree)
               (set! invalid-tokens-start (+ change-length invalid-tokens-start))
               (set! current-pos (+ start-pos tok-start)))))))
      
      (inherit is-locked? get-revision-number)
      
      (define/private (colorer-driver)
        (unless up-to-date?
          #;(printf "revision ~a~n" (get-revision-number))
          (unless (and tok-cor (= rev (get-revision-number)))
            (when tok-cor
              (coroutine-kill tok-cor))
            #;(printf "new coroutine~n")
            (set! tok-cor
                  (coroutine
                   (λ (enable-suspend)
                     (parameterize ((port-count-lines-enabled #t))
                       (re-tokenize (open-input-text-editor this current-pos end-pos 
                                                            (λ (x) #f))
                                    current-pos
                                    enable-suspend)))))
            (set! rev (get-revision-number)))
          (with-handlers ((exn:fail?
                           (λ (exn)
                             (parameterize ((print-struct #t))
                               ((error-display-handler) 
                                (format "exception in colorer thread: ~s" exn)
                                exn))
                             (set! tok-cor #f))))
            #;(printf "begin lexing~n")
            (when (coroutine-run 10 tok-cor)
              (set! up-to-date? #t)))
          #;(printf "end lexing~n")
          #;(printf "begin coloring~n")
          ;; This edit sequence needs to happen even when colors is null
          ;; for the paren highlighter.
          (begin-edit-sequence #f #f)
          (color)
          (end-edit-sequence)
          #;(printf "end coloring~n")))
      
      (define/private (colorer-callback)
        (cond
          ((is-locked?)
           (set! restart-callback #t))
          (else
           (unless (in-edit-sequence?)
             (colorer-driver))
           (unless up-to-date?
             (queue-callback (λ () (colorer-callback)) #f)))))
      
      ;; Must not be called when the editor is locked
      (define/private (finish-now)
        (unless stopped?
          (let loop ()
            (unless up-to-date?
              (colorer-driver)
              (loop)))))
      
      ;; See docs
      (define/public (start-colorer token-sym->style- get-token- pairs-)
        (unless force-stop?
          (set! stopped? #f)
          (reset-tokens)
          (set! should-color? (preferences:get 'framework:coloring-active))
          (set! token-sym->style token-sym->style-)
          (set! get-token get-token-)
          (set! pairs pairs-)
          (set! parens (new paren-tree% (matches pairs)))
          ;; (set! timer (current-milliseconds))
          (do-insert/delete start-pos 0)))
      
      ;; See docs
      (define/public stop-colorer
        (opt-lambda ((clear-colors #t))
          (set! stopped? #t)
          (when (and clear-colors (not frozen?))
            (begin-edit-sequence #f #f)
            (change-style (get-fixed-style) start-pos end-pos #f)
            (end-edit-sequence))
          (match-parens #t)
          (reset-tokens)
          (set! pairs null)
          (set! token-sym->style #f)
          (set! get-token #f)))
      
      (define/public (is-frozen?) frozen?)
      (define/public (is-stopped?) stopped?)
      
      ;; See docs
      (define/public (freeze-colorer)
        (when (is-locked?)
          (error 'freeze-colorer "called on a locked color:text<%>."))
        (unless frozen?
          (finish-now)
          (set! frozen? #t)))
      
      ;; See docs
      (define/public thaw-colorer
        (opt-lambda ((recolor? #t)
                     (retokenize? #f))
          (when frozen?
            (set! frozen? #f)
            (cond
              (stopped?
               (stop-colorer))
              ((or force-recolor-after-freeze recolor?)
               (cond
                 (retokenize?
                  (let ((tn token-sym->style)
                        (gt get-token)
                        (p pairs))
                    (stop-colorer (not should-color?))
                    (start-colorer tn gt p)))
                 (else
                  (begin-edit-sequence #f #f)
                  (finish-now)
                  (send tokens for-each
                        (λ (start len type)
                          (when (and should-color? (should-color-type? type))
                            (let ((color (send (get-style-list) find-named-style
                                               (token-sym->style type)))
                                  (sp (+ start-pos start))
                                  (ep (+ start-pos (+ start len))))
                              (change-style color sp ep #f)))))
                  (end-edit-sequence))))))))
      
      
      (define/private (toggle-color on?)
        (cond
          ((and frozen? (not (equal? on? should-color?)))
           (set! should-color? on?)
           (set! force-recolor-after-freeze #t))
          ((and (not should-color?) on?)
           (set! should-color? on?)
           (reset-tokens)
           (do-insert/delete start-pos 0))
          ((and should-color? (not on?))
           (set! should-color? on?)
           (begin-edit-sequence #f #f)
           (change-style (get-fixed-style) start-pos end-pos #f)
           (end-edit-sequence))))
      
      ;; see docs
      (define/public (force-stop-colorer stop?)
        (set! force-stop? stop?)
        (when stop?
          (stop-colorer)))
      
      
      ;; ----------------------- Match parentheses ----------------------------
      
      (define clear-old-locations void)
      
      (define mismatch-color (make-object color% "PINK"))
      (define/private (get-match-color) (preferences:get 'framework:paren-match-color))
      
      ;; higlight : number number number (or/c color any)
      ;;   if color is a color, then it uses that color to higlight
      ;;   Otherwise, it treats it like a boolean, where a true value  
      ;;   means the normal paren color and #f means an error color. 
      ;; numbers are expected to have zero be start-pos.
      (define/private (highlight start end caret-pos color)
        (let ([off (highlight-range (+ start-pos start) (+ start-pos end)
                                    (if (is-a? color color%)
                                        color
                                        (if color mismatch-color (get-match-color)))
                                    (and (send (icon:get-paren-highlight-bitmap)
                                               ok?)
                                         (icon:get-paren-highlight-bitmap))
                                    (= caret-pos (+ start-pos start)))])
          (set! clear-old-locations
                (let ([old clear-old-locations])
                  (λ ()
                    (old)
                    (off))))))
      
      (define in-match-parens? #f)
      
      ;; the forward matcher signaled an error because not enough of the
      ;; tree has been built.
      (define/private (f-match-false-error start end error)
        (and error (<= (+ start-pos error) current-pos) (not up-to-date?)))
      
      
      ;; If there is no match because the buffer isn't lexed far enough yet,
      ;; this will do nothing, but the edit sequence for changing the colors 
      ;; will trigger a callback that will call this to try and match again.
      ;; This edit sequence is used even if the coloring is disabled in
      ;; the preferences, although nothing is actually colored during it.
      ;; This leads to the nice behavior that we don't have to block to
      ;; highlight parens, and the parens will be highlighted as soon as
      ;; possible.
      (define/private match-parens
        (opt-lambda ([just-clear? #f])
          ;;(printf "(match-parens ~a)~n" just-clear?)
          (when (and (not in-match-parens?)
                     ;; Trying to match open parens while the
                     ;; background thread is going slows it down.
                     ;; The random number slows down how often it
                     ;; tries.
                     (or just-clear? up-to-date? (= 0 (random 5))))
            (set! in-match-parens? #t)
            (begin-edit-sequence #f #f)
            (clear-old-locations)
            (set! clear-old-locations void)
            (when (and (preferences:get 'framework:highlight-parens)
                       (not just-clear?))
              (let* ((here (get-start-position)))
                (when (= here (get-end-position))
                  (let-values (((start-f end-f error-f)
                                (send parens match-forward (- here start-pos))))
                    (when (and (not (f-match-false-error start-f end-f error-f))
                               start-f end-f)
                      (if error-f
                          (highlight start-f end-f here error-f)
                          (highlight-nested-region start-f end-f here))))
                  (let-values  (((start-b end-b error-b)
                                 (send parens match-backward (- here start-pos))))
                    (when (and start-b end-b)
                      (if error-b
                          (highlight start-b end-b here error-b)
                          (highlight-nested-region start-b end-b here)))))))
            (end-edit-sequence)
            (set! in-match-parens? #f))))
      
      (define/private (highlight-nested-region orig-start orig-end here)
        (let paren-loop ([start orig-start]
                         [end orig-end]
                         [depth 0])
          (when (< depth (vector-length (get-parenthesis-colors)))
            
            ;; when there is at least one more color in the vector we'll look
            ;; for regions to color at that next level
            (when (< (+ depth 1) (vector-length (get-parenthesis-colors)))
              (let seq-loop ([inner-sequence-start (+ start 1)])
                (when (< inner-sequence-start end)
                  (let ([post-whitespace (- (skip-whitespace (+ inner-sequence-start start-pos) 'forward #t) start-pos)])
                    (let-values ([(start-inner end-inner error-inner)
                                  (send parens match-forward post-whitespace)])
                      (cond
                        [(and start-inner end-inner (not error-inner))
                         (paren-loop start-inner end-inner (+ depth 1))
                         (seq-loop end-inner)]
                        [(skip-past-token post-whitespace)
                         =>
                         (λ (after-non-paren-thing)
                           (seq-loop after-non-paren-thing))]))))))
            
            (highlight start end here (vector-ref (get-parenthesis-colors) depth)))))
      
      ;; See docs
      (define/public (forward-match position cutoff)
        (do-forward-match position cutoff #t))
      
      (define/private (do-forward-match position cutoff skip-whitespace?)
        (let ((position 
               (if skip-whitespace? 
                   (skip-whitespace position 'forward #t)
                   position)))
          (let-values (((start end error)
                        (send parens match-forward (- position start-pos))))
            (cond
              ((f-match-false-error start end error)
               (colorer-driver)
               (do-forward-match position cutoff #f))
              ((and start end (not error))
               (let ((match-pos (+ start-pos end)))
                 (cond
                   ((<= match-pos cutoff) match-pos)
                   (else #f))))
              ((and start end error) #f)
              (else
               (skip-past-token position)
               #;
               (let-values (((tok-start tok-end)
                             (begin
                               (tokenize-to-pos position)
                               (send tokens search! (- position start-pos))
                               (values (send tokens get-root-start-position)
                                       (send tokens get-root-end-position)))))
                 (cond
                   ((or (send parens is-close-pos? tok-start)
                        (= (+ start-pos tok-end) position))
                    #f)
                   (else
                    (+ start-pos tok-end)))))))))
      
      (define/private (skip-past-token position)
        (let-values (((tok-start tok-end)
                      (begin
                        (tokenize-to-pos position)
                        (send tokens search! (- position start-pos))
                        (values (send tokens get-root-start-position)
                                (send tokens get-root-end-position)))))
          (cond
            ((or (send parens is-close-pos? tok-start)
                 (= (+ start-pos tok-end) position))
             #f)
            (else
             (+ start-pos tok-end)))))
      
      
      ;; See docs
      (define/public (backward-match position cutoff)
        (let ((x (internal-backward-match position cutoff)))
          (cond
            ((eq? x 'open) #f)
            (else x))))
      
      (define/private (internal-backward-match position cutoff)
        (when stopped?
          (error 'backward-match "called on a color:text<%> whose colorer is stopped."))
        (let ((position (skip-whitespace position 'backward #t)))
          (let-values (((start end error)
                        (send parens match-backward (- position start-pos))))
            (cond
              ((and start end (not error))
               (let ((match-pos (+ start-pos start)))
                 (cond
                   ((>= match-pos cutoff) match-pos)
                   (else #f))))
              ((and start end error) #f)
              (else
               (let-values (((tok-start tok-end)
                             (begin
                               (send tokens search!
                                     (if (> position start-pos)
                                         (- position start-pos 1)
                                         0))
                               (values (send tokens get-root-start-position)
                                       (send tokens get-root-end-position)))))
                 (cond
                   ((or (send parens is-open-pos? tok-start)
                        (= (+ start-pos tok-start) position))
                    'open)
                   (else
                    (+ start-pos tok-start)))))))))
      
      ;; See docs
      (define/public (backward-containing-sexp position cutoff)
        (when stopped?
          (error 'backward-containing-sexp "called on a color:text<%> whose colorer is stopped."))
        (let loop ((cur-pos position))
          (let ((p (internal-backward-match cur-pos cutoff)))
            (cond
              ((eq? 'open p) cur-pos)
              ((not p) #f)
              (else (loop p))))))
      
      ;; Determines whether a position is a 'comment, 'string, etc.
      (define/public (classify-position position)
        (when stopped?
          (error 'classify-position "called on a color:text<%> whose colorer is stopped."))
        (tokenize-to-pos position)
        (send tokens search! (- position start-pos))
        (send tokens get-root-data))
      
      (define/private (tokenize-to-pos position)
        (when (and (not up-to-date?) (<= current-pos position))
          (colorer-driver)
          (tokenize-to-pos position)))
      
      ;; See docs
      (define/public (skip-whitespace position direction comments?)
        (when stopped?
          (error 'skip-whitespace "called on a color:text<%> whose colorer is stopped."))
        (cond
          ((and (eq? direction 'forward)
                (>= position (if (eq? 'end end-pos) (last-position) end-pos)))
           position)
          ((and (eq? direction 'backward) (<= position start-pos))
           position)
          (else
           (tokenize-to-pos position)
           (send tokens search! (- (if (eq? direction 'backward) (sub1 position) position)
                                   start-pos))
           (cond
             ((or (eq? 'white-space (send tokens get-root-data))
                  (and comments? (eq? 'comment (send tokens get-root-data))))
              (skip-whitespace (+ start-pos
                                  (if (eq? direction 'forward)
                                      (send tokens get-root-end-position)
                                      (send tokens get-root-start-position)))
                               direction
                               comments?))
             (else position)))))
      
      (define/private (get-close-paren pos closers)
        (cond
          ((null? closers) #f)
          (else
           (let* ((c (car closers))
                  (l (string-length c)))
             (insert c pos)
             (let ((m (backward-match (+ l pos) start-pos)))
               (cond
                 ((and m
                       (send parens is-open-pos? (- m start-pos))
                       (send parens is-close-pos? (- pos start-pos)))
                  (delete pos (+ l pos))
                  c)
                 (else
                  (delete pos (+ l pos))
                  (get-close-paren pos (cdr closers)))))))))
      
      (inherit insert delete flash-on on-default-char)
      ;; See docs
      (define/public (insert-close-paren pos char flash? fixup?)
        (let ((closer
               (begin
                 (begin-edit-sequence #f #f)
                 (get-close-paren pos (if fixup? (map symbol->string (map cadr pairs)) null)))))
          (end-edit-sequence)
          (let ((insert-str (if closer closer (string char))))
            (for-each (lambda (c)
                        (on-default-char (new key-event% (key-code c))))
                      (string->list insert-str))
            (when flash?
              (unless stopped?
                (let ((to-pos (backward-match (+ (string-length insert-str) pos) 0)))
                  (when (and to-pos
                             (send parens is-open-pos? (- to-pos start-pos))
                             (send parens is-close-pos? (- pos start-pos)))
                    (flash-on to-pos (+ 1 to-pos)))))))))
      
      (define/public (debug-printout)
        (let* ((x null)
               (f (λ (a b c) (set! x (cons (list a b c) x)))))
          (send tokens for-each f)
          (printf "tokens: ~e~n" (reverse x))
          (set! x null)
          (send invalid-tokens for-each f)
          (printf "invalid-tokens: ~e~n" (reverse x))
          (printf "start-pos: ~a current-pos: ~a invalid-tokens-start ~a~n"
                  start-pos current-pos invalid-tokens-start)
          (printf "parens: ~e~n" (car (send parens test)))))
      
      ;; ------------------------- Callbacks to Override ----------------------
      
      (define/override (lock x)
        ;;(printf "(lock ~a)~n" x)
        (super lock x)
        (when (and restart-callback (not x))
          (set! restart-callback #f)
          (queue-callback (λ () (colorer-callback)))))
      
      
      (define/override (on-focus on?)
        ;;(printf "(on-focus ~a)~n" on?)
        (super on-focus on?)
        (match-parens (not on?)))
      
      (define/augment (after-edit-sequence)
        ;;(printf "(after-edit-sequence)~n")
        (when (has-focus?)
          (match-parens))
        (inner (void) after-edit-sequence))
      
      (define/augment (after-set-position)
        ;;(printf "(after-set-position)~n")
        (unless (local-edit-sequence?)
          (when (has-focus?)
            (match-parens)))
        (inner (void) after-set-position))
      
      (define/augment (after-change-style a b)
        ;;(printf "(after-change-style)~n")
        (unless (get-styles-fixed)
          (unless (local-edit-sequence?)
            (when (has-focus?)
              (match-parens))))
        (inner (void) after-change-style a b))
      
      (define/augment (on-set-size-constraint)
        ;;(printf "(on-set-size-constraint)~n")
        (unless (local-edit-sequence?)
          (when (has-focus?)
            (match-parens)))
        (inner (void) on-set-size-constraint))
      
      (define/augment (after-insert edit-start-pos change-length)
        ;;(printf "(after-insert ~a ~a)~n" edit-start-pos change-length)
        (do-insert/delete edit-start-pos change-length)
        (inner (void) after-insert edit-start-pos change-length))
      
      (define/augment (after-delete edit-start-pos change-length)
        ;;(printf "(after-delete ~a ~a)~n" edit-start-pos change-length)
        (do-insert/delete edit-start-pos (- change-length))
        (inner (void) after-delete edit-start-pos change-length))
      
      (super-new)
      
      ;; need pref-callback to be in a private field
      ;; so that the editor hangs on to the callback
      ;; when the editor goes away, so does the callback
      (define (pref-callback k v) (toggle-color v))
      (preferences:add-callback 'framework:coloring-active pref-callback #t)))
  
  (define parenthesis-color-table #f)
  (define (get-parenthesis-colors-table)
    (unless parenthesis-color-table
      (set! parenthesis-color-table
            (list
             (list 'shades-of-gray  
                   (string-constant paren-color-shades-of-gray)
                   (between 180 180 180
                            220 220 220))
             (list 'shades-of-blue
                   (string-constant paren-color-shades-of-blue)
                   (between 204 204 255
                            153 153 255))
             (list 'spring
                   (string-constant paren-color-spring)
                   (between 255 255 153
                            204 255 153))
             (list 'fall
                   (string-constant paren-color-fall)
                   (between 255 204 153
                            204 153 102))
             (list 'winter
                   (string-constant paren-color-winter)
                   (between 204 205 255
                            255 255 255)))))
    (cons (list 'basic-grey
                (string-constant paren-color-basic-grey)
                (vector (preferences:get 'framework:paren-match-color)))
          parenthesis-color-table))
  
  (define (get-parenthesis-colors) 
    (let ([choice (or (assoc (preferences:get 'framework:paren-color-scheme)
                             (get-parenthesis-colors-table))
                      (car (get-parenthesis-colors-table)))])
      (caddr choice)))
      
  (define (between start-r start-g start-b end-r end-g end-b)
    (let ([size 4])
      (build-vector
       4 
       (lambda (x) 
         (let ([between (λ (start end) (floor (+ start (* (- end start) (/ x (- size 1))))))])
           (make-object color% 
             (between start-r end-r)
             (between start-g end-g)
             (between start-b end-b)))))))
  
  (define -text% (text-mixin text:keymap%))
  
  (define -text-mode<%> (interface ()))
  
  (define text-mode-mixin
    (mixin (mode:surrogate-text<%>) (-text-mode<%>)
      ;; The arguments here are only used to be passed to start-colorer.  Refer to its
      ;; documentation.
      (init-field (get-token default-lexer) 
                  (token-sym->style (λ (x) "Standard"))
                  (matches null))
      
      (define/override (on-disable-surrogate text)
        (super on-disable-surrogate text)
        (send text stop-colorer))
      
      (define/override (on-enable-surrogate text)
        (super on-enable-surrogate text)
        (send text start-colorer token-sym->style get-token matches))
      
      (super-new)))
  
  (define text-mode% (text-mode-mixin mode:surrogate-text%))
