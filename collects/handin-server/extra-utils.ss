(module extra-utils mzscheme

(require (lib "utils.ss" "handin-server")
         (lib "file.ss") (lib "list.ss") (lib "class.ss")
         (lib "mred.ss" "mred"))

(provide (all-from-except mzscheme #%module-begin)
         (all-from (lib "utils.ss" "handin-server")))

(provide (rename module-begin~ #%module-begin))
(define-syntax (module-begin~ stx)
  (let ([e (if (syntax? stx) (syntax-e stx) stx)])
    (if (pair? e)
      (with-syntax ([user-pre (datum->syntax-object stx 'user-pre stx)]
                    [user-post (datum->syntax-object stx 'user-post stx)])
        (datum->syntax-object
         (quote-syntax here)
         (list* (quote-syntax #%plain-module-begin)
                (datum->syntax-object stx (quote-syntax (provide checker)))
                #'(define user-pre #f)
                #'(define user-post #f)
                (cdr e))
         stx))
      (raise-syntax-error #f "bad syntax" stx))))

(define server-dir (current-directory))

(define (error* fmt . args)
  (error (apply format fmt args)))

(define fields
  (map car (get-preference 'extra-fields (lambda () #f) #f
                           (build-path server-dir "config.ss"))))

(provide user-data)
(define (user-data user)
  ;; the student always assumed to exist
  (cdr (get-preference (if (string? user) (string->symbol user) user)
                       (lambda () #f) #f (build-path server-dir "users.ss"))))

(provide user-substs)
(define (user-substs user str)
  (subst str `(("username" . ,user) ,@(map cons fields (user-data user)))))

(define (subst str substs)
  (if (list? str)
    (map (lambda (x) (subst x substs)) str)
    (let* ([m (regexp-match-positions #rx"{([^{}]+)}" str)]
           [s (and m (substring str (caadr m) (cdadr m)))])
      (if m
        (subst (string-append (substring str 0 (caar m))
                              (cond [(assoc s substs) => cdr]
                                    [else (error 'subst
                                                 "unknown substitution: ~s" s)])
                              (substring str (cdar m)))
               substs)
        str))))

(define (verify-line-length line n len)
  (define (string-width str)
    (let loop ([l (string->list str)] [w 0])
      (if (null? l)
        w
        (loop (cdr l)
              (if (char=? #\tab (car l)) (+ 8 (- w (modulo w 8))) (add1 w))))))
  (unless (<= (bytes-length line) len)
    (let ([line (bytes->string/utf-8 line)])
      (unless (or (< (string-length line) len)
                  (< (string-width line) len))
        (error* "~a \"~a\" is longer than ~a characters"
                (if n (format "Line #~a" n) "The line")
                (regexp-replace #rx"^[ \t]*(.*?)[ \t]*$" line "\\1")
                len)))))

;; ============================================================================
;; Text conversion

;; Code that turns binary stuff into text is split into three places:
;; * input-port->text-input-port implements a simple generic textualization
;;   filter
;; * snip->text is used earlier in the process, where comment-box text is still
;;   available
;; * test-boxes are registered through some hacked up code that will turn them
;;   into an editor% with text that input-port->text-input-port will then spit
;;   out.

(require (lib "framework.ss" "framework")) ; for drscheme snips, used below

;; input-port->text-input-port : input-port (any -> any) -> input-port
;;  the `filter' function is applied to special values; the filter result is
;;  `display'ed into the stream in place of the special
(define (input-port->text-input-port src . filter)
  ;; note that snip->text below already takes care of some snips
  (define (item->text x)
    (cond [(is-a? x snip%)
           (format "~a" (or (send x get-text 0 (send x get-count) #t) x))]
          [(special-comment? x)
           (format "#| ~a |#" (special-comment-value x))]
          [(syntax? x) (syntax-object->datum x)]
          [else x]))
  (let-values ([(filter) (if (pair? filter) (car filter) item->text)]
               [(in out) (make-pipe 4096)])
    (thread
     (lambda ()
       (let ([s (make-bytes 4096)])
         (let loop ()
           (let ([c (read-bytes-avail! s src)])
             (cond [(number? c) (write-bytes s out 0 c) (loop)]
                   [(procedure? c)
                    (let ([v (let-values ([(l col p) (port-next-location src)])
                               (c (object-name src) l col p))])
                      (display (filter v) out))
                    (loop)]
                   [else (close-output-port out)])))))) ; Must be EOF
    in))

(define (snip->text x)
  (let ([name (and (is-a? x snip%)
                   (send (send x get-snipclass) get-classname))])
    (cond [(equal? name "wximage") "{{IMAGE}}"]
          [(equal? name "(lib \"comment-snip.ss\" \"framework\")")
           ;; comments will have ";" prefix on every line, and "\n" suffix
           (format ";{{COMMENT:\n~a;}}\n"
                   (send x get-text 0 (send x get-count)))]
          [else x])))

(define (submission->string submission maxwidth textualize?)
  (let-values ([(defs inters) (unpack-submission submission)])
    (parameterize ([current-output-port (open-output-string)]
                   [current-input-port
                    (if textualize?
                      (input-port->text-input-port
                       (open-input-text-editor defs 0 'end snip->text))
                      (open-input-text-editor defs))])
      (let loop ([n 1])
        (let ([line (if textualize?
                      (read-bytes-line)
                      (with-handlers ([void
                                       (lambda (e)
                                         (error* "The submission must not ~a"
                                                 "have non-textual items"))])
                        (read-bytes-line)))])
          (unless (eof-object? line)
            (let ([line (regexp-replace #rx#"[ \t]+$" line #"")])
              (when maxwidth
                (verify-line-length line (and (not textualize?) n) maxwidth))
              (display line) (newline) (loop (add1 n))))))
      (get-output-string (current-output-port)))))

;; ---------------------------------------------------------
;; This code will hack textualization of test and text boxes

(define (insert-to-editor editor . xs)
  (for-each (lambda (x)
              (send editor insert
                    (if (string? x) x (make-object editor-snip% x))))
            xs))

;; support for "test-case-box%"
(define test-sc
  (new (class snip-class%
         (define/override (read f)
           (let ([test (new test%)]) (send test read-from-file f) test))
         (super-new))))
(define test%
  (class editor-snip%
    (inherit set-snipclass get-editor)
    (define to-test       (new text%))
    (define expected      (new text%))
    (define predicate     (new text%))
    (define should-raise  (new text%))
    (define error-message (new text%))
    (define/public (read-from-file f)
      (unless (eq? 2 (send test-sc reading-version f)) (error "BOOM"))
      (send to-test       read-from-file f)
      (send expected      read-from-file f)
      (send predicate     read-from-file f)
      (send should-raise  read-from-file f)
      (send error-message read-from-file f)
      (send f get (box 0))  ; enabled?
      (send f get (box 0))  ; collapsed?
      (send f get (box 0))) ; error-box
    (super-new)
    (set-snipclass test-sc)
    (insert-to-editor (get-editor)
      "{{TEST:\n  expression: " to-test "\n  should be:  " expected "\n}}")))
(send test-sc set-classname "test-case-box%")
(send test-sc set-version 2)
(send (get-the-snip-class-list) add test-sc)

;; support for "text-box%"
(define text-box-sc
  (new (class snip-class%
         (define/override (read f)
           (let ([text (new text-box%)]) (send text read-from-file f) text))
         (super-new))))
(define text-box%
  (class editor-snip%
    (inherit set-snipclass get-editor)
    (define text (new text%))
    (define/public (read-from-file f)
      (unless (eq? 1 (send text-box-sc reading-version f)) (error "BOOM"))
      (send text read-from-file f))
    (super-new)
    (set-snipclass text-box-sc)
    (insert-to-editor (get-editor) "{{TEXT: " text "}}")))
(send text-box-sc set-classname "text-box%")
(send text-box-sc set-version 2)
(send (get-the-snip-class-list) add text-box-sc)

;; ============================================================================
;; Checker function

(provide submission-eval)
(define submission-eval (make-parameter #f))

;; without this the primitive eval is not available
(provide (rename eval prim-eval))

;; for adding lines in the checker
(define added-lines (make-thread-cell #f))
(provide add-header-line!)
(define (add-header-line! line)
  (let ([new (list line)] [cur (thread-cell-ref added-lines)])
    (if cur (append! cur new) (thread-cell-set! added-lines new))))

(provide check:)
(define-syntax (check: stx)
  (define (id s) (datum->syntax-object stx s stx))
  (let loop ([stx (syntax-case stx () [(_ x ...) #'(x ...)])]
             [keyvals '()])
    (define (get key . default)
      (cond [(assq key keyvals) => (lambda (x) (set-car! x #f) (caddr x))]
            [(pair? default) (car default)]
            [else #f]))
    (syntax-case stx ()
      [(key val x ...)
       (and (identifier? #'key)
            (regexp-match #rx"^:" (symbol->string (syntax-e #'key))))
       (loop #'(x ...) (cons (list (syntax-e #'key) #'key #'val) keyvals))]
      [(body ...)
       (with-syntax
           ([users*         (get ':users #'#f)]
            [eval?*         (get ':eval? #'#t)]
            [language*      (get ':language #'#f)]
            [teachpacks*    (get ':teachpacks #''())]
            [create-text?*  (get ':create-text? #'#t)]
            [textualize?*   (get ':textualize? #'#f)]
            [maxwidth*      (get ':maxwidth #'79)]
            [student-line*
             (get ':student-line
                  #'"Student: {username} ({Full Name} <{Email}>)")]
            [extra-lines*
             (get ':extra-lines
                  #''("Maximum points for this assignment: <+100>"))]
            [value-printer* (get ':value-printer #'#f)]
            [coverage?*     (get ':coverage? #'#f)]
            [output*        (get ':output #'"hw.scm")]
            [user-error-message*
             (get ':user-error-message #'"Error in your code --\n~a")]
            [checker        (id 'checker)]
            [users          (id 'users)]
            [submission     (id 'submission)]
            [eval           (id 'eval)]
            [execute-counts (id 'execute-counts)]
            [with-submission-bindings (id 'with-submission-bindings)]
            [user-pre       (id 'user-pre)]
            [user-post      (id 'user-post)]
            [(body ...) (syntax-case #'(body ...) ()
                          [() #'(void)] [_ #'(body ...)])])
         (for-each (lambda (x)
                     (when (car x)
                       (raise-syntax-error #f "unknown keyword" stx (cadr x))))
                   keyvals)
         #'(define checker
             (let ([allowed (let ([us users*])
                              (if (list? us)
                                (map (lambda (x)
                                       (if (list? x)
                                         (quicksort x string<?)
                                         (list x)))
                                     us)
                                us))]
                   [eval?          eval?*]
                   [language       language*]
                   [teachpacks     teachpacks*]
                   [create-text?   create-text?*]
                   [textualize?    textualize?*]
                   [maxwidth       maxwidth*]
                   [student-line   student-line*]
                   [extra-lines    extra-lines*]
                   [value-printer  value-printer*]
                   [coverage?      coverage?*]
                   [output-file    output*]
                   [user-error-message user-error-message*]
                   [execute-counts #f])
               ;; ========================================
               ;; verify submitting users
               (define (pre users submission)
                 (current-run-status "checking submission username(s)")
                 (cond [(list? allowed)
                        (unless (member users allowed)
                          (error*
                           "You are not registered ~a for this submission"
                           (case (length users)
                             [(1) "for individual submission"]
                             [(2) "as a pair"]
                             [else "as a group"])))]
                       [(procedure? allowed) (allowed users)]
                       [(not allowed) ; default is single-user submission
                        (unless (= 1 (length users))
                          (error*
                           "This homework is for individual submissions"))]
                       [else (error* "bad user specifications")])
                 (when user-pre (user-pre users submission)))
               ;; ========================================
               ;; convert to text, evaluate, check
               (define (check users submission)
                 (define text-file "grading/text.scm")
                 (define (write-text)
                   (with-output-to-file text-file
                     (lambda ()
                       (define added (or (thread-cell-ref added-lines) '()))
                       (for-each
                        (lambda (user)
                          (printf ";;> ~a\n" (user-substs user student-line)))
                        users)
                       (for-each (lambda (l) (printf ";;> ~a\n" l)) extra-lines)
                       (for-each (lambda (l) (printf ";;> ~a\n" l)) added)
                       (display submission-text))
                     'truncate))
                 (define submission-text
                   (and create-text?
                        (submission->string submission maxwidth textualize?)))
                 (when create-text?
                   (make-directory "grading")
                   (when (regexp-match #rx";>" submission-text)
                     (error* "You cannot use \";>\" in your code!"))
                   (write-text))
                 (when value-printer (current-value-printer value-printer))
                 (when coverage? (coverage-enabled #t))
                 (current-run-status "checking submission")
                 (cond
                  [language
                   (let ([eval
                          (with-handlers
                              ([void
                                (lambda (e)
                                  (let ([m (if (exn? e)
                                             (exn-message e)
                                             (format "~a" e))])
                                    (cond
                                     [(procedure? user-error-message)
                                      (user-error-message m)]
                                     [(not (string? user-error-message))
                                      (error*
                                       "badly configured user-error-message")]
                                     [(regexp-match #rx"~[aesvAESV]"
                                                    user-error-message)
                                      (error* user-error-message m)]
                                     [else
                                      (error* "~a" user-error-message)])))])
                            (call-with-evaluator/submission
                             language teachpacks submission values))])
                     (when coverage?
                       (set! execute-counts (eval #f 'execute-counts)))
                     (current-run-status "running tests")
                     (parameterize ([submission-eval eval])
                       (let-syntax ([with-submission-bindings
                                     (syntax-rules ()
                                       [(_ bindings body*1 body* (... ...))
                                        (with-bindings eval bindings
                                          body*1 body* (... ...))])])
                         (let () body ...))
                       (when (thread-cell-ref added-lines) (write-text))))]
                  [(not eval?) (let () body ...)]
                  [else (error* "no language configured for submissions")])
                 output-file)
               ;; ========================================
               ;; indirection for user-post (may be set after `check:')
               (define (post users submission)
                 (when user-post (user-post users submission)))
               ;; ========================================
               ;; configuration sanity checks
               (let ([bad (cond [(and eval? (not language))
                                 "`eval?' without `language'"]
                                [(and (not create-text?) textualize?)
                                 "`textualize?' without `create-text?'"]
                                [(and (not eval?) coverage?)
                                 "`coverage?' without `eval?'"]
                                [(and textualize? coverage?)
                                 "`textualize?' and `coverage?'"]
                                [else #f])])
                 (when bad
                   (error* "bad checker specifications: cannot use ~a" bad)))
               ;; ========================================
               (list pre check post))))])))

(define-syntax (with-bindings stx)
  (syntax-case stx ()
    [(_ get (var ...) body ...)
     (with-syntax ([(>var ...)
                    (map (lambda (v)
                           (datum->syntax-object
                            v
                            (string->symbol
                             (string-append
                              "~" (symbol->string (syntax-e v))))
                            v))
                         (syntax->list #'(var ...)))])
       #'(let ([>var (get 'var)] ...) body ...))]))

;; Similar utilities for pre- and post-checkers
(provide pre: post:)
(define-syntaxes (pre: post:)
  (let ([make-pre/post:
         (lambda (what)
           (lambda (stx)
             (define (id s) (datum->syntax-object stx s stx))
             (syntax-case stx ()
               [(_ body ...)
                (with-syntax ([users      (id 'users)]
                              [submission (id 'submission)]
                              [what       (id what)])
                  #'(set! what (lambda (users submission) body ...)))])))])
    (values (make-pre/post: 'user-pre) (make-pre/post: 'user-post))))

;; ============================================================================
;; User-checker utilities

;; To verify single submissions, we use a "wants-single-submission" directory
;; in the user's main submission directory to mark if already verified.  If the
;; users regrets the submission, the *whole* directory is removed.  This
;; assumes that there is nothing there, so this check should not be added after
;; submission has already began since students may lose work.

(define (warn-single user)
  ;; we're in ATTEMPT, climb up to the main directory
  (parameterize ([current-directory ".."])
    (unless (directory-exists? "wants-single-submission")
      (if (eq? 'yes
               (message
                (string-append
                 "You have chosen to submit your work individually;"
                 " if you continue, it will be impossible for you to"
                 " later submit with a partner.  Are you sure you want"
                 " to continue?"
                 (path->string (current-directory)))
                '(yes-no)))
        (make-directory "wants-single-submission")
        (error* "Aborting single submission!")))))

(provide pairs-or-singles-with-warning)
(define (pairs-or-singles-with-warning users)
  (case (length users)
    [(2) #t]
    [(1) (warn-single (car users))]
    [else (error* "too many users in the team: ~a" users)]))

(provide teams-in-file)
(define (teams-in-file file)
  (define last-time 0)
  (define teams '())
  (define file* (build-path server-dir file))
  (define (read-teams!)
    (let ([cur-time (file-or-directory-modify-seconds file*)])
      (unless (equal? last-time cur-time)
        (set! last-time cur-time)
        (set! teams
              (with-input-from-file file*
                (lambda ()
                  (let loop ([r '()])
                    (let ([x (read)])
                      (cond [(eof-object? x) (reverse! r)]
                            [(null? x) (loop r)]
                            [(list? x) (loop (cons (quicksort x string<?) r))]
                            [else      (loop (cons (list x) r))])))))))))
  (lambda (users)
    (read-teams!)
    (unless (member users teams)
      (error* "You are not registered ~a for this submission"
              (case (length users)
                [(1) "for individual submission"]
                [(2) "as a pair"]
                [else "as a group"])))))

;; ============================================================================
;; Checker utilities

(define (->disp x)
  (cond [(pair? x)
         (if (and (eq? 'unquote (car x)) (= 2 (length x)))
           (->disp (cadr x))
           (cons (->disp (car x)) (->disp (cdr x))))]
        [(not (symbol? x)) x]
        [else (regexp-replace "^~" (symbol->string x) "") x]))

(provide procedure/arity?)
(define (procedure/arity? proc arity)
  (and (procedure? proc) (procedure-arity-includes? proc arity)))

(provide !defined)
(define-syntax !defined
  (syntax-rules ()
    ;; expected to be used only with identifiers
    [(_ id ...) (begin (with-handlers
                           ([exn:fail:contract:variable?
                             (lambda (_)
                               (error* "missing binding: ~a" (->disp 'id)))])
                         ((submission-eval) `id))
                       ...)]))

(provide !procedure* !procedure)
(define-syntax !procedure*
  (syntax-rules ()
    [(_ expr)
     (unless (procedure? ((submission-eval) `expr))
       (error* "~a is expected to be bound to a procedure" (->disp 'expr)))]
    [(_ expr arity)
     (let ([ar  arity]
           [val ((submission-eval) `expr)])
       (unless (procedure? val)
         (error* "~a is expected to be bound to a procedure" (->disp 'expr)))
       (unless (procedure-arity-includes? val ar)
         (error* "~a is expected to be bound to a procedure of ~s arguments"
                 (->disp 'expr) ar)))]))
(define-syntax !procedure
  (syntax-rules ()
    [(_ expr) (begin (!defined expr) (!procedure* expr))]
    [(_ expr arity) (begin (!defined expr) (!procedure* expr arity))]))

(provide !integer* !integer)
(define-syntax !integer*
  (syntax-rules ()
    [(_ expr)
     (unless (integer? ((submission-eval) `expr))
       (error* "~a is expected to be bound to an integer" (->disp 'expr)))]))
(define-syntax !integer
  (syntax-rules ()
    [(_ expr) (begin (!defined expr) (!integer* expr))]))

(provide !test)
(define-syntax !test
  (syntax-rules ()
    [(_ expr)
     (unless ((submission-eval) `expr)
       (error* "your code failed a test: ~a is false" (->disp 'expr)))]
    [(_ expr result) (!test expr result equal?)]
    [(_ expr result equal?)
     (let ([val ((submission-eval) `expr)])
       (unless (equal? result val)
         (error*
          "your code failed a test: ~a evaluated to ~a, expecting ~a"
          (->disp 'expr) (->disp val) (->disp result))))]))

(provide !all-covered)
(define (!all-covered)
  (define execute-counts ((submission-eval) #f 'execute-counts))
  (define (coverage-error stx)
    (error* "your code is not completely covered by test cases~a"
            (cond [(and (syntax-line stx) (syntax-column stx))
                   (format ": uncovered expression at ~a:~a"
                           (syntax-line stx) (syntax-column stx))]
                  [(syntax-position stx)
                   (format ": uncovered expression at position ~a"
                           (syntax-position stx))]
                  [else ""])))
  (if execute-counts
    #|
    ;; Go over all counts that are syntax-original, avoiding code that macros
    ;; insert
    (for-each (lambda (x)
                (when (and (zero? (cdr x)) (syntax-original? (car x)))
                  (coverage-error (car x))))
              execute-counts)
    |#
    ;; Better: try to find if there is some source position that is not
    ;; covered, if so, it means that there is some macro that originates in
    ;; some real source position that is not covered.  Also, return the first
    ;; one found of the biggest span (so an error will point at `(+ 1 2)', not
    ;; on the `+').
    (let ([table (make-hash-table)])
      (for-each
       (lambda (x)
         (let* ([loc (syntax-position (car x))]
                [h (hash-table-get table loc (lambda () #f))])
           (when (or (not h) (< (cdr h) (cdr x)))
             (hash-table-put! table loc x))))
       execute-counts)
      (let ([1st #f])
        (hash-table-for-each table
          (lambda (key val)
            (when (and (zero? (cdr val))
                       (or (not 1st)
                           (let ([car-pos (syntax-position (car val))]
                                 [1st-pos (syntax-position 1st)])
                             (or (< car-pos 1st-pos)
                                 (and (= car-pos 1st-pos)
                                      (> (syntax-span (car val))
                                         (syntax-span 1st)))))))
              (set! 1st (car val)))))
        (when 1st (coverage-error 1st))))
    (error* "mis-configuration: requires coverage, but no coverage info!")))

)
