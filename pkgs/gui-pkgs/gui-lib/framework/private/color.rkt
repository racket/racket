#lang racket/unit
#|
update-region-end is now gone
get-region is gone
added reset-regions
added get-regions
|#

(require racket/class
         racket/gui/base
         syntax-color/token-tree
         syntax-color/paren-tree
         syntax-color/default-lexer
         syntax-color/lexer-contract
         string-constants
         "../preferences.rkt"
         "sig.rkt"
         "aspell.rkt"
         "color-local-member-name.rkt")

(import [prefix icon: framework:icon^]
        [prefix mode: framework:mode^]
        [prefix text: framework:text^]
        [prefix color-prefs: framework:color-prefs^]
        [prefix racket: framework:racket^])

(export (rename framework:color^
                (-text<%> text<%>)
                (-text% text%)
                (-text-mode<%> text-mode<%>)))

(init-depend framework:text^ framework:mode^)

(define (should-color-type? type)
  (not (memq type '(no-color))))

(define (make-data type mode backup-delta) 
  (if (zero? backup-delta)
      (cons type mode)
      (vector type mode backup-delta)))
(define (data-type data) (if (pair? data) (car data) (vector-ref data 0)))
(define (data-lexer-mode data) (if (pair? data) (cdr data) (vector-ref data 1)))
(define (data-backup-delta data) (if (vector? data) (vector-ref data 2) 0))

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
    reset-regions
    get-regions
    
    skip-whitespace
    backward-match
    backward-containing-sexp
    forward-match
    insert-close-paren
    classify-position
    get-token-range
    
    set-spell-check-strings
    get-spell-check-strings
    set-spell-check-text
    get-spell-check-text
    get-spell-current-dict))

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
    
    ;; if we were coloring and discovered that an edit-sequence was 
    ;; going on, then we postpone coloring until the edit-sequence
    ;; ends
    (define continue-after-edit-sequence? #f)
    
    ;; ---------------------- Parenethesis matching ----------------------
    
    ;; The pairs of matching parens
    (define pairs '())
    
    ;; ---------------------- Lexing state ------------------------------
    
    (define-struct lexer-state
      (start-pos 
       end-pos
       ;; The tree of valid tokens, starting at start-pos
       tokens ; = (new token-tree%)
       ;; If the tree is completed
       up-to-date? ; #t
       ;; The tree of tokens that have been invalidated by an edit
       ;; but might still be valid.
       invalid-tokens ; = (new token-tree%)
       ;; The position right before the invalid-tokens tree
       invalid-tokens-start ; = +inf.0
       invalid-tokens-mode
       ;; The position right before the next token to be read
       current-pos
       ;; Thread a mode through lexing, and remember the mode
       ;;  at each token boundary, so that lexing can depend on
       ;;  previous tokens. This is the mode for lexing at
       ;;  current-pos:
       current-lexer-mode
       ;; Paren-matching
       parens 
       )
      #:mutable #:transparent)
    
    ;; The lexer
    (define get-token #f)
    
    (define/private (make-new-lexer-state start end)
      (make-lexer-state start
                        end
                        (new token-tree%)
                        #t
                        (new token-tree%)
                        +inf.0
                        #f
                        start
                        #f
                        (new paren-tree% (matches pairs))))
    
    (define lexer-states (list (make-new-lexer-state 0 'end)))
    (define/public (get-up-to-date?) 
      (andmap lexer-state-up-to-date? lexer-states))
    
    (define/private (find-ls pos)
      (ormap (lambda (ls)
               (and (<= (lexer-state-start-pos ls)
                        pos
                        (let ([end (lexer-state-end-pos ls)])
                          (if (eq? end 'end)
                              +inf.0
                              end)))
                    ls))
             lexer-states))
    
    ;; ---------------------- Interactions state ------------------------
    ;; The positions right before and right after the area to be tokenized
    
    (inherit last-position)
    
    (define/public (reset-region start end)
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
      (reset-regions (list (list start end))))
    
    (define/public (reset-regions _regions)
      (let loop ([regions _regions]
                 [pos 0])
        (cond
          [(null? regions) (void)]
          [(pair? regions)
           (let ([region (car regions)])
             (unless (and (list? region)
                          (= 2 (length region))
                          (number? (list-ref region 0))
                          (or (number? (list-ref region 1))
                              (and (null? (cdr regions))
                                   (eq? 'end (list-ref region 1)))))
               (error 'reset-regions 
                      "got a region that is not a list of two numbers (or 'end if it is the last region): ~e, all regions ~e" 
                      region
                      regions))
             (unless (and (<= pos (list-ref region 0))
                          (or (eq? 'end (list-ref region 1))
                              (<= (list-ref region 0) (list-ref region 1))))
               (error 'reset-regions "found regions with numbers out of order ~e" regions))
             (loop (cdr regions) (list-ref region 1)))]
          [else
           (error 'reset-regions "expected a list of regions, got ~e" regions)]))
      
      (set! lexer-states
            (let loop ([old lexer-states]
                       [new _regions])
              (cond
                [(null? new) null]
                [(and (pair? old)
                      (equal? (caar new) (lexer-state-start-pos (car old)))
                      (equal? (cadar new) (lexer-state-end-pos (car old))))
                 (cons (car old)
                       (loop (cdr old) (cdr new)))]
                [else
                 (cons (make-new-lexer-state (caar new) (cadar new))
                       (loop null (cdr new)))])))
      (update-lexer-state-observers))
    
    
    (define/public (get-regions) 
      (map (lambda (ls)
             (list (lexer-state-start-pos ls)
                   (lexer-state-end-pos ls)))
           lexer-states))
    
    ;; ---------------------- Preferences -------------------------------
    (define should-color? #t)
    (define token-sym->style #f)
    (define spell-check-strings? (preferences:get 'framework:spell-check-strings?))
    (define spell-check-text? (preferences:get 'framework:spell-check-text?))
    
    (define/public (get-spell-check-strings) spell-check-strings?)
    (define/public (get-spell-check-text) spell-check-text?)
    (define/public (set-spell-check-strings s) 
      (define new-val (and s #t))
      (unless (equal? new-val spell-check-strings?)
        (set! spell-check-strings? s)
        (spell-checking-values-changed)))
    (define/public (set-spell-check-text s) 
      (define new-val (and s #t))
      (unless (equal? new-val spell-check-text?)
        (set! spell-check-text? s)
        (spell-checking-values-changed)))
    (define/private (spell-checking-values-changed)
      (reset-tokens)
      (start-colorer token-sym->style get-token pairs))
    (define current-dict (preferences:get 'framework:aspell-dict))
    (define/public (set-spell-current-dict d)
      (unless (equal? d current-dict)
        (set! current-dict d)
        (reset-tokens)
        (start-colorer token-sym->style get-token pairs)))
    (define/public (get-spell-current-dict) current-dict)
    
    ;; ---------------------- Multi-threading ---------------------------
    ;; The editor revision when the last coloring was started
    (define revision-when-started-parsing #f)

    ;; The editor revision when after the last edit to the buffer
    (define revision-after-last-edit #f)
    
    (inherit change-style begin-edit-sequence end-edit-sequence highlight-range
             get-style-list in-edit-sequence? get-start-position get-end-position
             local-edit-sequence? get-styles-fixed has-focus?
             get-fixed-style get-text)
    
    (define lexers-all-valid? #t)
    (define/private (update-lexer-state-observers)
      (define new (for/and ([ls (in-list lexer-states)])
                    (lexer-state-up-to-date? ls)))
      (unless (eq? new lexers-all-valid?)
        (set! lexers-all-valid? new)
        (on-lexer-valid lexers-all-valid?)))
    (define/pubment (on-lexer-valid valid?)
      (inner (void) on-lexer-valid valid?))
    (define/public-final (is-lexer-valid?) lexers-all-valid?)
    
    (define/private (reset-tokens)
      (for-each
       (lambda (ls)
         (send (lexer-state-tokens ls) reset-tree)
         (send (lexer-state-invalid-tokens ls) reset-tree)
         (set-lexer-state-invalid-tokens-start! ls +inf.0)
         (set-lexer-state-up-to-date?! ls #t)
         (set-lexer-state-current-pos! ls (lexer-state-start-pos ls))
         (set-lexer-state-current-lexer-mode! ls #f)
         (set-lexer-state-parens! ls (new paren-tree% (matches pairs))))
       lexer-states)
      (update-lexer-state-observers)
      (set! restart-callback #f)
      (set! continue-after-edit-sequence? #f)
      (set! force-recolor-after-freeze #f)
      (set! revision-when-started-parsing #f))
    
    ;; Discard extra tokens at the first of invalid-tokens
    (define/private (sync-invalid ls)
      (let ([invalid-tokens (lexer-state-invalid-tokens ls)]
            [invalid-tokens-start (lexer-state-invalid-tokens-start ls)])
        (when (and (not (send invalid-tokens is-empty?))
                   (< invalid-tokens-start
                      (lexer-state-current-pos ls)))
          (send invalid-tokens search-min!)
          (let ((length (send invalid-tokens get-root-length))
                (mode (data-lexer-mode (send invalid-tokens get-root-data))))
            (send invalid-tokens remove-root!)
            (set-lexer-state-invalid-tokens-start! ls (+ invalid-tokens-start length))
            (set-lexer-state-invalid-tokens-mode! ls mode))
          (sync-invalid ls))))
    
    (define/private (re-tokenize-move-to-next-ls start-time ok-to-stop?)
      (cond
        [(null? re-tokenize-lses) 
         ;; done: return #t
         #t]
        [else
         (define ls (car re-tokenize-lses))
         (set! re-tokenize-lses (cdr re-tokenize-lses))
         (define in
           (open-input-text-editor this 
                                   (lexer-state-current-pos ls)
                                   (lexer-state-end-pos ls)
                                   (λ (x) #f)))
         (port-count-lines! in)
         (continue-re-tokenize start-time ok-to-stop? ls in
                               (lexer-state-current-pos ls)
                               (lexer-state-current-lexer-mode ls))]))
    
    (define re-tokenize-lses #f)
    
    (define/public (tokenizing-give-up-early) 'defer)
    
    (define/private (continue-re-tokenize start-time ok-to-stop? ls in in-start-pos lexer-mode)
      (cond
        [(and ok-to-stop?
              (case (tokenizing-give-up-early)
                [(#t) #t]
                [(#f) #f]
                [(defer) ((+ start-time 20.0) . <= . (current-inexact-milliseconds))]))
         #f]
        [else
         (define-values (_line1 _col1 pos-before) (port-next-location in))
         (define-values (lexeme type data new-token-start new-token-end backup-delta new-lexer-mode/cont) 
           (get-token in in-start-pos lexer-mode))
         (define-values (_line2 _col2 pos-after) (port-next-location in))
         (define new-lexer-mode (if (dont-stop? new-lexer-mode/cont)
                                    (dont-stop-val new-lexer-mode/cont)
                                    new-lexer-mode/cont))
         (define next-ok-to-stop? (not (dont-stop? new-lexer-mode/cont)))
         (cond
           [(eq? 'eof type) 
            (set-lexer-state-up-to-date?! ls #t)
            (re-tokenize-move-to-next-ls start-time next-ok-to-stop?)]
           [else
            (unless (<= pos-before new-token-start pos-after)
              (error 'color:text<%>
                     "expected the token start to be between ~s and ~s, got ~s" pos-before pos-after new-token-start))
            (unless (<= pos-before new-token-end pos-after)
              (error 'color:text<%>
                     "expected the token end to be between ~s and ~s, got ~s" pos-before pos-after new-token-end))
            (let ((len (- new-token-end new-token-start)))
              (set-lexer-state-current-pos! ls (+ len (lexer-state-current-pos ls)))
              (set-lexer-state-current-lexer-mode! ls new-lexer-mode)
              (sync-invalid ls)
              (when (and should-color? (should-color-type? type) (not frozen?))
                (add-colorings type in-start-pos new-token-start new-token-end))
              ;; Using the non-spec version takes 3 times as long as the spec
              ;; version.  In other words, the new greatly outweighs the tree
              ;; operations.
              ;;(insert-last! tokens (new token-tree% (length len) (data type)))
              (insert-last-spec! (lexer-state-tokens ls) len (make-data type new-lexer-mode backup-delta))
              #; (show-tree (lexer-state-tokens ls))
              (send (lexer-state-parens ls) add-token data len)
              (cond
                [(and (not (send (lexer-state-invalid-tokens ls) is-empty?))
                      (= (lexer-state-invalid-tokens-start ls)
                         (lexer-state-current-pos ls))
                      (equal? new-lexer-mode 
                              (lexer-state-invalid-tokens-mode ls)))
                 (send (lexer-state-invalid-tokens ls) search-max!)
                 (send (lexer-state-parens ls) merge-tree
                       (send (lexer-state-invalid-tokens ls) get-root-end-position))
                 (insert-last! (lexer-state-tokens ls)
                               (lexer-state-invalid-tokens ls))
                 (set-lexer-state-invalid-tokens-start! ls +inf.0)
                 (set-lexer-state-up-to-date?! ls #t)
                 (re-tokenize-move-to-next-ls start-time next-ok-to-stop?)]
                [else
                 (continue-re-tokenize start-time next-ok-to-stop? ls in in-start-pos new-lexer-mode)]))])]))

    (define/private (add-colorings type in-start-pos new-token-start new-token-end)
      (define sp (+ in-start-pos (sub1 new-token-start)))
      (define ep (+ in-start-pos (sub1 new-token-end)))
      (define style-name (token-sym->style type))
      (define color (send (get-style-list) find-named-style style-name))
      (define do-spell-check?
        (cond
          [(equal? type 'string) spell-check-strings?]
          [(equal? type 'text) spell-check-text?]
          [else #f]))
      (cond
        [do-spell-check?
         (define misspelled-color (send (get-style-list) find-named-style misspelled-text-color-style-name))
         (cond
           [misspelled-color
            (define strs (regexp-split #rx"\n" (get-text sp ep)))
            (let loop ([strs strs]
                       [pos sp])
              (unless (null? strs)
                (define str (car strs))
                (let loop ([spellos (query-aspell str current-dict)]
                           [lp 0])
                  (cond
                    [(null? spellos) 
                     (add-coloring color (+ pos lp) (+ pos (string-length str)))]
                    [else
                     (define err (car spellos))
                     (define err-start (list-ref err 0))
                     (define err-len (list-ref err 1))
                     (add-coloring misspelled-color (+ pos err-start) (+ pos err-start err-len))
                     (add-coloring color (+ pos lp) (+ pos err-start))
                     (loop (cdr spellos) (+ err-start err-len))]))
                (loop (cdr strs)
                      (+ pos (string-length str) 1))))]
           [else
            (add-coloring color sp ep)])]
        [else
         (add-coloring color sp ep)]))
    
    (define/private (add-coloring color sp ep) 
      (change-style color sp ep #f))
    
    (define/private (show-tree t)
      (printf "Tree:\n")
      (send t search-min!)
      (let loop ([old-s -inf.0])
        (let ([s (send t get-root-start-position)]
              [e (send t get-root-end-position)])
          (unless (= s old-s)
            (printf " ~s\n" (list s e))
            (send t search! e)
            (loop s)))))
    
    (define/private (split-backward ls valid-tree pos)
      (let loop ([pos pos][valid-tree valid-tree][old-invalid-tree #f])
        (let-values (((orig-token-start orig-token-end valid-tree invalid-tree orig-data)
                      (send valid-tree split/data (- pos (lexer-state-start-pos ls)))))
          (let ([backup-pos (- pos (data-backup-delta orig-data))]
                [invalid-tree (or old-invalid-tree invalid-tree)])
            (if (backup-pos . < . pos)
                ;; back up more:
                (loop pos valid-tree invalid-tree)
                ;; that was far enough:
                (values orig-token-start orig-token-end valid-tree invalid-tree orig-data))))))
    
    (define/private (do-insert/delete/ls ls edit-start-pos change-length)
      (unless (lexer-state-up-to-date? ls)
        (sync-invalid ls))
      (cond
        ((lexer-state-up-to-date? ls)
         (let-values (((orig-token-start orig-token-end valid-tree invalid-tree orig-data)
                       (split-backward ls (lexer-state-tokens ls) edit-start-pos)))
           (send (lexer-state-parens ls) split-tree orig-token-start)
           (set-lexer-state-invalid-tokens! ls invalid-tree)
           (set-lexer-state-tokens! ls valid-tree)
           (set-lexer-state-invalid-tokens-start!
            ls
            (if (send (lexer-state-invalid-tokens ls) is-empty?)
                +inf.0
                (+ (lexer-state-start-pos ls) orig-token-end change-length)))
           (set-lexer-state-invalid-tokens-mode! ls (and orig-data (data-lexer-mode orig-data)))
           (let ([start (+ (lexer-state-start-pos ls) orig-token-start)])
             (set-lexer-state-current-pos! ls start)
             (set-lexer-state-current-lexer-mode! ls
                                                  (if (= start (lexer-state-start-pos ls))
                                                      #f
                                                      (begin
                                                        (send valid-tree search-max!)
                                                        (data-lexer-mode (send valid-tree get-root-data))))))
           (set-lexer-state-up-to-date?! ls #f)
           (update-lexer-state-observers)
           (queue-callback (λ () (colorer-callback)) #f)))
        ((and (>= edit-start-pos (lexer-state-invalid-tokens-start ls))
              (> edit-start-pos (lexer-state-current-pos ls)))
         (let-values (((tok-start tok-end valid-tree invalid-tree orig-data)
                       (split-backward ls (lexer-state-invalid-tokens ls) edit-start-pos)))
           (set-lexer-state-invalid-tokens! ls invalid-tree)
           (set-lexer-state-invalid-tokens-start!
            ls
            (+ (lexer-state-invalid-tokens-start ls) tok-end change-length))
           (set-lexer-state-invalid-tokens-mode! ls (and orig-data (data-lexer-mode orig-data)))))
        ((> edit-start-pos (lexer-state-current-pos ls))
         (set-lexer-state-invalid-tokens-start! 
          ls 
          (+ change-length (lexer-state-invalid-tokens-start ls))))
        (else
         (let-values (((tok-start tok-end valid-tree invalid-tree data)
                       (split-backward ls (lexer-state-tokens ls) edit-start-pos)))
           (send (lexer-state-parens ls) truncate tok-start)
           (set-lexer-state-tokens! ls valid-tree)
           (set-lexer-state-invalid-tokens-start! ls (+ change-length (lexer-state-invalid-tokens-start ls)))
           (let ([start (+ (lexer-state-start-pos ls) tok-start)])
             (set-lexer-state-current-pos! ls start)
             (set-lexer-state-current-lexer-mode! 
              ls
              (if (= start (lexer-state-start-pos ls))
                  #f
                  (begin
                    (send valid-tree search-max!)
                    (data-lexer-mode (send valid-tree get-root-data))))))))))
    
    (define/private (do-insert/delete edit-start-pos change-length)
      (unless (or stopped? force-stop?)
        (let ([ls (find-ls edit-start-pos)])
          (when ls
            (do-insert/delete/ls ls edit-start-pos change-length)))))
    
    (define/private (do-insert/delete-all)
      (for-each (lambda (ls)
                  (do-insert/delete/ls ls (lexer-state-start-pos ls) 0))
                lexer-states))
    
    (inherit is-locked? get-revision-number)
    
    (define/private (colorer-driver)
      (unless (andmap lexer-state-up-to-date? lexer-states)
        (begin-edit-sequence #f #f)
        (c-log "starting to color")
        (set! re-tokenize-lses (let loop ([lexer-states lexer-states])
                                 (cond
                                   [(null? lexer-states) null]
                                   [else (if (lexer-state-up-to-date? (car lexer-states))
                                             (loop (cdr lexer-states))
                                             lexer-states)])))
        (define finished? (re-tokenize-move-to-next-ls 
                           (current-inexact-milliseconds) 
                           ;; #f initially here ensures we do at least
                           ;; one step of tokenization before giving up
                           #f))
        (c-log (format "coloring stopped ~a" (if finished? "because it finished" "with more to do")))
        (when finished?
          (update-lexer-state-observers)
          (c-log "updated observers"))
        (c-log "starting end-edit-sequence")
        (end-edit-sequence)
        (c-log "finished end-edit-sequence")))
    
    (define/private (colorer-callback)
      (cond
        ((is-locked?)
         (set! restart-callback #t))
        (else
         (cond
           [(in-edit-sequence?)
            (set! continue-after-edit-sequence? #t)]
           [else
            (colorer-driver)
            (unless (andmap lexer-state-up-to-date? lexer-states)
              (queue-callback (λ () (colorer-callback)) #f))]))))
    
    ;; Must not be called when the editor is locked
    (define/private (finish-now)
      (unless stopped?
        (let loop ()
          (unless (andmap lexer-state-up-to-date? lexer-states)
            (colorer-driver)
            (loop)))))
    
    ;; See docs
    (define/public (start-colorer token-sym->style- get-token- pairs-)
      (unless force-stop?
        (set! stopped? #f)
        (reset-tokens)
        (set! should-color? (preferences:get 'framework:coloring-active))
        (set! token-sym->style token-sym->style-)
        (set! get-token (if (procedure-arity-includes? get-token- 3)
                            ;; New interface: thread through a mode:
                            get-token-
                            ;; Old interface: no offset, backup delta, or mode
                            (lambda (in offset mode)
                              (let-values ([(lexeme type data new-token-start new-token-end) 
                                            (get-token- in)])
                                (values lexeme type data new-token-start new-token-end 0 #f)))))
        (set! pairs pairs-)
        (for-each
         (lambda (ls)
           (set-lexer-state-parens! ls (new paren-tree% (matches pairs))))
         lexer-states)
        ;; (set! timer (current-milliseconds))
        (do-insert/delete-all)))
    
    ;; See docs
    (define/public stop-colorer
      (lambda ((clear-the-colors #t))
        (set! stopped? #t)
        (when (and clear-the-colors (not frozen?))
          (clear-colors))
        (match-parens #t)
        (reset-tokens)
        (set! pairs null)
        (set! token-sym->style #f)
        (set! get-token #f)))
    
    (define/private (clear-colors)
      (begin-edit-sequence #f #f)
      (for-each
       (λ (ls)
         (change-style (get-fixed-style) 
                       (lexer-state-start-pos ls) 
                       (lexer-state-end-pos ls) 
                       #f))
       lexer-states)
      (end-edit-sequence))
    
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
      (lambda ((recolor? #t)
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
                (when should-color?
                  (for-each
                   (lambda (ls)
                     (let ([tokens (lexer-state-tokens ls)]
                           [start-pos (lexer-state-start-pos ls)])
                       (send tokens for-each
                             (λ (start len data)
                               (let ([type (data-type data)])
                                 (when (should-color-type? type)
                                   (let ((color (send (get-style-list) find-named-style
                                                      (token-sym->style type)))
                                         (sp (+ start-pos start))
                                         (ep (+ start-pos (+ start len))))
                                     (change-style color sp ep #f))))))))
                   lexer-states))
                (end-edit-sequence))))))))
    
    
    (define/private (toggle-color on?)
      (cond
        ((and frozen? (not (equal? on? should-color?)))
         (set! should-color? on?)
         (set! force-recolor-after-freeze #t))
        ((and (not should-color?) on?)
         (set! should-color? on?)
         (reset-tokens)
         (do-insert/delete-all))
        ((and should-color? (not on?))
         (set! should-color? on?)
         (clear-colors))))
    
    ;; see docs
    (define/public (force-stop-colorer stop?)
      (set! force-stop? stop?)
      (when stop?
        (stop-colorer)))
    
    
    ;; ----------------------- Match parentheses ----------------------------
    
    (define clear-old-locations void)
    
    (define mismatch-color (make-object color% "PINK"))
    (define/private (get-match-color) 
      (color-prefs:lookup-in-color-scheme 'framework:paren-match-color))
    
    
    ;; higlight : number number number (or/c color any)
    ;;   if color is a color, then it uses that color to higlight
    ;;   Otherwise, it treats it like a boolean, where a true value  
    ;;   means the normal paren color and #f means an error color. 
    ;; numbers are expected to have zero be start-pos.
    (define/private (highlight ls start end caret-pos color [priority 'low])
      (let* ([start-pos (lexer-state-start-pos ls)]
             [off (highlight-range (+ start-pos start) (+ start-pos end)
                                   (if (is-a? color color%)
                                       color
                                       (if color mismatch-color (get-match-color)))
                                   (= caret-pos (+ start-pos start))
                                   priority)])
        (set! clear-old-locations
              (let ([old clear-old-locations])
                (λ ()
                  (old)
                  (off))))))
    
    (define in-match-parens? #f)
    
    ;; the forward matcher signaled an error because not enough of the
    ;; tree has been built.
    (define/private (f-match-false-error ls start end error)
      (and error 
           (<= (+ (lexer-state-start-pos ls) error) 
               (lexer-state-current-pos ls))
           (not (lexer-state-up-to-date? ls))))
    
    ;; If there is no match because the buffer isn't lexed far enough yet,
    ;; this will do nothing, but the edit sequence for changing the colors 
    ;; will trigger a callback that will call this to try and match again.
    ;; This edit sequence is used even if the coloring is disabled in
    ;; the preferences, although nothing is actually colored during it.
    ;; This leads to the nice behavior that we don't have to block to
    ;; highlight parens, and the parens will be highlighted as soon as
    ;; possible.
    (define/private match-parens
      (lambda ([just-clear? #f])
        ;;(printf "(match-parens ~a)\n" just-clear?)
        (when (and (not in-match-parens?)
                   ;; Trying to match open parens while the
                   ;; background thread is going slows it down.
                   ;; The random number slows down how often it
                   ;; tries.
                   (or just-clear? 
                       (andmap lexer-state-up-to-date? lexer-states)
                       (= 0 (random 5))))
          (set! in-match-parens? #t)
          (begin-edit-sequence #f #f)
          (clear-old-locations)
          (set! clear-old-locations void)
          (when (and (preferences:get 'framework:highlight-parens)
                     (not just-clear?))
            (let* ((here (get-start-position)))
              (when (= here (get-end-position))
                (let ([ls (find-ls here)])
                  (when ls
                    (let-values (((start-f end-f error-f)
                                  (send (lexer-state-parens ls) match-forward 
                                        (- here (lexer-state-start-pos ls)))))
                      (when (and (not (f-match-false-error ls start-f end-f error-f))
                                 start-f end-f)
                        (if error-f
                            (highlight ls start-f end-f here error-f)
                            (highlight-nested-region ls start-f end-f here))))
                    (let-values  (((start-b end-b error-b)
                                   (send (lexer-state-parens ls) match-backward 
                                         (- here (lexer-state-start-pos ls)))))
                      (when (and start-b end-b)
                        (if error-b
                            (highlight ls start-b end-b here error-b)
                            (highlight-nested-region ls start-b end-b here)))))))))
          (end-edit-sequence)
          (set! in-match-parens? #f))))
    
    ;; highlight-nested-region : lexer-state number number number -> void
    ;; colors nested regions of parentheses.
    (define/private (highlight-nested-region ls orig-start orig-end here)
      (define priority (get-parenthesis-priority))
      (define paren-colors (get-parenthesis-colors))
      (let paren-loop ([start orig-start]
                       [end orig-end]
                       [depth 0])
        (when (< depth (vector-length paren-colors))
          
          ;; when there is at least one more color in the vector we'll look
          ;; for regions to color at that next level
          (when (< (+ depth 1) (vector-length paren-colors))
            (let seq-loop ([inner-sequence-start (+ start 1)])
              (when (< inner-sequence-start end)
                (let ([post-whitespace (skip-whitespace inner-sequence-start 'forward #t)])
                  (let-values ([(start-inner end-inner error-inner)
                                (send (lexer-state-parens ls) match-forward post-whitespace)])
                    (cond
                      [(and start-inner end-inner (not error-inner))
                       (paren-loop start-inner end-inner (+ depth 1))
                       (seq-loop end-inner)]
                      [(skip-past-token ls post-whitespace)
                       =>
                       (λ (after-non-paren-thing)
                         (seq-loop after-non-paren-thing))]))))))
          
          (highlight ls start end here (vector-ref paren-colors depth) priority))))
    
    ;; See docs
    (define/public (forward-match position cutoff)
      (do-forward-match position cutoff #t))
    
    (define/private (do-forward-match position cutoff skip-whitespace?)
      (let ((position 
             (if skip-whitespace? 
                 (skip-whitespace position 'forward #t)
                 position)))
        (let ([ls (find-ls position)])
          (and 
           ls
           (let-values (((start end error)
                         (send (lexer-state-parens ls) match-forward
                               (- position (lexer-state-start-pos ls)))))
             (cond
               ((f-match-false-error ls start end error)
                (colorer-driver)
                (do-forward-match position cutoff #f))
               ((and start end (not error))
                (let ((match-pos (+ (lexer-state-start-pos ls) end)))
                  (cond
                    ((<= match-pos cutoff) match-pos)
                    (else #f))))
               ((and start end error) #f)
               (else
                (skip-past-token ls position))))))))
    
    (define/private (skip-past-token ls position)
      (let-values (((tok-start tok-end)
                    (begin
                      (tokenize-to-pos ls position)
                      (send (lexer-state-tokens ls) search! 
                            (- position (lexer-state-start-pos ls)))
                      (values (send (lexer-state-tokens ls) get-root-start-position)
                              (send (lexer-state-tokens ls) get-root-end-position)))))
        (cond
          ((or (send (lexer-state-parens ls) is-close-pos? tok-start)
               (= (+ (lexer-state-start-pos ls) tok-end) position))
           #f)
          (else
           (+ (lexer-state-start-pos ls) tok-end)))))
    
    ;; See docs
    (define/public (backward-match position cutoff)
      (let ((x (internal-backward-match position cutoff)))
        (cond
          ((or (eq? x 'open) (eq? x 'beginning)) #f)
          (else x))))
    
    (define/private (internal-backward-match position cutoff)
      (when stopped?
        (error 'backward-match "called on a color:text<%> whose colorer is stopped."))
      (let* ([position (skip-whitespace position 'backward #t)]
             [ls (find-ls position)]
             [start-pos (and ls (lexer-state-start-pos ls))])
        (and
         ls
         (let-values (((start end error)
                       (send (lexer-state-parens ls) match-backward (- position start-pos))))
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
                              (send (lexer-state-tokens ls) search!
                                    (if (> position start-pos)
                                        (- position start-pos 1)
                                        0))
                              (values (send (lexer-state-tokens ls) get-root-start-position)
                                      (send (lexer-state-tokens ls) get-root-end-position)))))
                (cond
                  ((send (lexer-state-parens ls) is-open-pos? tok-start)
                   'open)
                  ((= (+ start-pos tok-start) position)
                   'beginning)
                  (else
                   (+ start-pos tok-start))))))))))
    
    ;; See docs
    (define/public (backward-containing-sexp position cutoff)
      (when stopped?
        (error 'backward-containing-sexp "called on a color:text<%> whose colorer is stopped."))
      (let loop ((cur-pos position))
        (let ((p (internal-backward-match cur-pos cutoff)))
          (cond
            ((eq? 'open p) 
             ;; Should this function skip  
             ;; backwards past whitespace? 
             ;; the docs seem to indicate it
             ;; does, but it doesn't really
             cur-pos)
            ((eq? 'beginning p) #f)
            ((not p) #f)
            (else (loop p))))))
    
    ;; Determines whether a position is a 'comment, 'string, etc.
    (define/public (classify-position position)
      (define-values (tokens ls) (get-tokens-at-position 'classify-position position))
      (and tokens
           (let ([root-data (send tokens get-root-data)])
             (and root-data
                  (data-type root-data)))))
    
    (define/public (get-token-range position)
      (define-values (tokens ls) (get-tokens-at-position 'get-token-range position))
      (values (and tokens ls
                   (+ (lexer-state-start-pos ls)
                      (send tokens get-root-start-position)))
              (and tokens ls
                   (+ (lexer-state-start-pos ls)
                      (send tokens get-root-end-position)))))
    
    (define/private (get-tokens-at-position who position)
      (when stopped?
        (error who "called on a color:text<%> whose colorer is stopped."))
      (let ([ls (find-ls position)])
        (if ls
            (let ([tokens (lexer-state-tokens ls)])
              (tokenize-to-pos ls position)
              (send tokens search! (- position (lexer-state-start-pos ls)))
              (values tokens ls))
            (values #f #f))))
    
    (define/private (tokenize-to-pos ls position)
      (when (and (not (lexer-state-up-to-date? ls)) 
                 (<= (lexer-state-current-pos ls) position))
        (colorer-driver)
        (tokenize-to-pos ls position)))
    
    ;; See docs
    ;;  Note: this doesn't seem to handle sexp-comments correctly  .nah.
    (define/public (skip-whitespace position direction comments?)
      (when stopped?
        (error 'skip-whitespace "called on a color:text<%> whose colorer is stopped."))
      (let ([ls (find-ls position)])
        (if (not ls) 
            position
            (let ([start-pos (lexer-state-start-pos ls)]
                  [end-pos (lexer-state-end-pos ls)]
                  [tokens (lexer-state-tokens ls)])
              (cond
                ((and (eq? direction 'forward)
                      (>= position (if (eq? 'end end-pos) (last-position) end-pos)))
                 position)
                ((and (eq? direction 'backward) (<= position start-pos))
                 position)
                (else
                 (tokenize-to-pos ls position)
                 (send tokens search! (- (if (eq? direction 'backward) (sub1 position) position)
                                         start-pos))
                 (cond
                   ((and (send tokens get-root-data)
                         (or (eq? 'white-space (data-type (send tokens get-root-data)))
                             (and comments? (eq? 'comment (data-type (send tokens get-root-data))))))
                    (skip-whitespace (+ start-pos
                                        (if (eq? direction 'forward)
                                            (send tokens get-root-end-position)
                                            (send tokens get-root-start-position)))
                                     direction
                                     comments?))
                   (else position))))))))
    
    ;; this returns the start/end positions
    ;; of the matching close paren of the first open paren to the left of pos,
    ;; if it is properly balanced. (this one assumes though that closers
    ;; really contains only 'parenthesis type characters)
    ;; find-next-outer-paren : number (list string)
    ;;            -> (values (or #f number) (or #f number) (or #f string))
    (define/private (find-next-outer-paren pos closers)
      (let loop ([pos pos])
        (define after-ws (skip-whitespace pos 'forward #f))
        (cond
          [after-ws
           (define tok (classify-position after-ws))
           (define-values (a b) (get-token-range after-ws))
           (cond
             [(eq? tok 'parenthesis)
              (define str (get-text a b))
              (cond
                [(member str closers) 
                 (values a b str)]
                [else
                 (define m (forward-match a (last-position)))
                 (cond
                   [m (loop m)]
                   [else (values #f #f #f)])])]
             [(< b (last-position))
              (loop b)]
             [else
              (values #f #f #f)])]
          [else (values #f #f)])))
    

    ;; returns the start and end positions of the next token at or after
    ;;   pos that matches any of the given list of closers, as well as
    ;;   the string of the matching token itself and whether it
    ;;   occurred immediately adjacent to pos, ignoring whitespace and comments
    ;; find-next-close-paren : number (list string) boolean
    ;;      -> (values (or #f number) (or #f number) (or #f string) boolean)
    (define/private (find-next-close-paren pos closers [adj? #t])
      (define next-pos (skip-whitespace pos 'forward #t))
      (define ls (find-ls next-pos))
      (cond
        [ls
         (define ls-start (lexer-state-start-pos ls))
         (define tree (lexer-state-tokens ls))
         (send tree search! (- next-pos ls-start))
         (define start-pos (+ ls-start (send tree get-root-start-position)))
         (define end-pos (+ ls-start (send tree get-root-end-position))) 
         
         #;(printf "~a |~a| |~a|~n" (list pos next-pos start-pos end-pos (send tree get-root-data)) closers (get-text start-pos end-pos))
         
         (cond
           [(or (not (send tree get-root-data)) (<= end-pos pos))
            (values #f #f #f #f)]    ;; didn't find /any/ token ending after pos
           [(and (<= pos start-pos)
                 (member (get-text start-pos end-pos) closers)) ; token at start-pos matches
            (values start-pos end-pos (get-text start-pos end-pos) adj?)]
           [else   ; skip ahead
            (find-next-close-paren end-pos closers #f)])]
        [else
         (values #f #f #f #f)]))


    ;; given end-pos, a position right after a closing parens,
    ;; flash the matching open parens
    (define/private (flash-from end-pos)
      (let ((to-pos (backward-match end-pos 0)))
        (when to-pos
          (let ([ls (find-ls to-pos)])
            (when ls
              (let ([start-pos (lexer-state-start-pos ls)]
                    [parens (lexer-state-parens ls)])
                (when (and (send parens is-open-pos? (- to-pos start-pos))
                           (send parens is-close-pos? (- end-pos 1 start-pos)))
                  (flash-on to-pos (+ 1 to-pos)))))))))


    (inherit insert delete flash-on on-default-char set-position get-character)
    ;; See docs
    ;; smart-skip : (or/c #f 'adjacent 'forward)
    (define/public (insert-close-paren pos char flash? fixup? [smart-skip #f])
      (begin-edit-sequence #t #f)
      (define closers (map symbol->string (map cadr pairs)))
      (define closer (get-close-paren pos
                                      (if fixup? ;; Ensure preference for given character:
                                          (cons (string char) (remove (string char) closers))
                                          null)
                                      #f))
      (define insert-str (if closer closer (string char)))
      (define (insert)
        (for ([c (in-string insert-str)])
          (on-default-char (new key-event% (key-code c))))
        (+ pos (string-length insert-str)))
      (define (skip p)
        (set-position p)
        p)
      
      (define end-pos
        (cond
          [smart-skip
           (define-values (next-close-start next-close-end next-close-str next-close-adj?)
             (find-next-close-paren pos closers))
           (cond
             [(eq? smart-skip 'adjacent)
              (if (and next-close-start next-close-adj?
                       (string=? insert-str next-close-str))
                  (skip next-close-end)
                  (insert))]
             [(eq? smart-skip 'forward)
              (define-values (outer-close-start outer-close-end outer-close-str)
                (find-next-outer-paren pos closers))
              (cond
                [(and outer-close-start
                      (or fixup? (string=? insert-str outer-close-str)))
                 (skip outer-close-end)]
                [(and next-close-start
                      (or fixup? (string=? insert-str next-close-str)))
                 (skip next-close-end)]
                [else  (insert)])]
             [else
              (error 'insert-close-paren
                     (format "invalid smart-skip option: ~a" smart-skip))])]
          [else
           (insert)]))
      (end-edit-sequence)
      (when (and flash? (not stopped?)) (flash-from end-pos)))
    
    ;; find-closer : exact-nonnegative-integer? -> (or/c #f string?)
    ;; returns what the close paren should be at position pos to
    ;; match whatever the opening paren is at the other end, unless
    ;; the position is inside some other token or there is no opening
    ;; paren, in which case it returns #f.
    (define/private (get-close-paren pos closers continue-after-non-paren?)
      (cond
        [(null? closers) #f]
        [else
         (define c (car closers))
         (define l (string-length c))
         (define ls (find-ls pos))
         (cond
           [ls
            (define start-pos (lexer-state-start-pos ls))
            (insert c pos)
            (define cls (classify-position pos))
            (cond
              [(eq? cls 'parenthesis)
               (define m (backward-match (+ l pos) start-pos))
               (cond
                 [(and m
                       (send (lexer-state-parens ls) is-open-pos? (- m start-pos))
                       (send (lexer-state-parens ls) is-close-pos? (- pos start-pos)))
                  (delete pos (+ l pos))
                  c]
                 [else
                  (delete pos (+ l pos))
                  (get-close-paren pos (cdr closers) #t)])]
              [else
               (delete pos (+ l pos))
               (if continue-after-non-paren?
                   (get-close-paren pos (cdr closers) #t)
                   #f)])]
           [else c])]))

    
    (define/public (debug-printout)
      (for-each 
       (lambda (ls)
         (let* ((x null)
                (f (λ (a b c) (set! x (cons (list a b c) x)))))
           (send (lexer-state-tokens ls) for-each f)
           (printf "tokens: ~.s\n" (reverse x))
           (set! x null)
           (send (lexer-state-invalid-tokens ls) for-each f)
           (printf "invalid-tokens: ~.s\n" (reverse x))
           (printf "start-pos: ~a current-pos: ~a invalid-tokens-start ~a\n"
                   (lexer-state-start-pos ls)
                   (lexer-state-current-pos ls)
                   (lexer-state-invalid-tokens-start ls))
           (printf "parens: ~.s\n" (car (send (lexer-state-parens ls) test)))))
       lexer-states))
    
    ;; ------------------------- Callbacks to Override ----------------------
    
    (define/override (lock x)
      ;;(printf "(lock ~a)\n" x)
      (super lock x)
      (when (and restart-callback (not x))
        (set! restart-callback #f)
        (queue-callback (λ () (colorer-callback)))))
    
    
    (define/override (on-focus on?)
      ;;(printf "(on-focus ~a)\n" on?)
      (super on-focus on?)
      (match-parens (not on?)))
    
    (define/augment (after-edit-sequence)
      ;;(printf "(after-edit-sequence)\n")
      (when continue-after-edit-sequence?
        (set! continue-after-edit-sequence? #f)
        (queue-callback (λ () (colorer-callback)) #f))
      (when (has-focus?)
        (match-parens))
      (inner (void) after-edit-sequence))
    
    (define/augment (after-set-position)
      ;;(printf "(after-set-position)\n")
      (unless (local-edit-sequence?)
        (when (has-focus?)
          (match-parens)))
      (inner (void) after-set-position))
    
    (define/augment (after-change-style a b)
      ;;(printf "(after-change-style)\n")
      (unless (get-styles-fixed)
        (unless (local-edit-sequence?)
          (when (has-focus?)
            (match-parens))))
      (inner (void) after-change-style a b))
    
    (define/augment (on-set-size-constraint)
      ;;(printf "(on-set-size-constraint)\n")
      (unless (local-edit-sequence?)
        (when (has-focus?)
          (match-parens)))
      (inner (void) on-set-size-constraint))
    
    (define/augment (after-insert edit-start-pos change-length)
      ;;(printf "(after-insert ~a ~a)\n" edit-start-pos change-length)
      (do-insert/delete edit-start-pos change-length)
      (inner (void) after-insert edit-start-pos change-length))
    
    (define/augment (after-delete edit-start-pos change-length)
      ;;(printf "(after-delete ~a ~a)\n" edit-start-pos change-length)
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
  (define (reverse-vec v) (list->vector (reverse (vector->list v))))
  (unless parenthesis-color-table
    (set! parenthesis-color-table
          (list
           (list 'shades-of-gray  
                 (string-constant paren-color-shades-of-gray)
                 (reverse-vec
                  (between .2 0 0 0
                           .2 0 0 0))
                 'high)
           (list 'shades-of-blue
                 (string-constant paren-color-shades-of-blue)
                 (between .4 153 153 255
                          .4 153 153 255)
                 'high)
           (list 'spring
                 (string-constant paren-color-spring)
                 (between 1 255 255 153
                          1 204 255 153)
                 'low)
           (list 'fall
                 (string-constant paren-color-fall)
                 (between 1 255 204 153
                          1 204 153 102)
                 'low)
           (list 'winter
                 (string-constant paren-color-winter)
                 (between 1 204 205 255
                          1 238 238 255)
                 'low))))
  (cons (list 'basic-grey
              (string-constant paren-color-basic-grey)
              (vector (color-prefs:lookup-in-color-scheme 'framework:paren-match-color))
              'high)
        parenthesis-color-table))

(define (get-parenthesis-colors) 
  (let ([choice (or (assoc (preferences:get 'framework:paren-color-scheme)
                           (get-parenthesis-colors-table))
                    (car (get-parenthesis-colors-table)))])
    (caddr choice)))

(define (get-parenthesis-priority) 
  (let ([choice (or (assoc (preferences:get 'framework:paren-color-scheme)
                           (get-parenthesis-colors-table))
                    (car (get-parenthesis-colors-table)))])
    (list-ref choice 3)))

(define (between start-a start-r start-g start-b end-a end-r end-g end-b)
  (let ([size 4])
    (build-vector
     4 
     (lambda (x) 
       (define (between start end) (+ start (* (- end start) (/ x (- size 1)))))
       (make-object color% 
         (floor (between start-r end-r))
         (floor (between start-g end-g))
         (floor (between start-b end-b))
         (between start-a end-a))))))

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

(define misspelled-text-color-style-name "Misspelled Text")

(define logger (make-logger 'framework/colorer (current-logger)))
(define-syntax-rule
  (c-log exp)
  (when (log-level? logger 'debug)
    (log-message logger 'debug exp (current-inexact-milliseconds))))
