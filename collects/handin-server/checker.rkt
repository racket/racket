#lang scheme/base

(require (for-syntax scheme/base) "utils.ss"
         scheme/file scheme/class mred)

(provide (except-out (all-from-out scheme/base) #%module-begin)
         (all-from-out "utils.ss"))

(provide (rename-out [module-begin~ #%module-begin]))
(define-syntax (module-begin~ stx)
  (let ([e (if (syntax? stx) (syntax-e stx) stx)])
    (if (pair? e)
      (with-syntax ([user-pre (datum->syntax stx 'user-pre stx)]
                    [user-post (datum->syntax stx 'user-post stx)])
        (datum->syntax
         (quote-syntax here)
         (list* (quote-syntax #%plain-module-begin)
                #'(define user-pre #f)
                #'(define user-post #f)
                (cdr e))
         stx))
      (raise-syntax-error #f "bad syntax" stx))))

(define (error* fmt . args)
  (error (apply format fmt args)))

(define fields (map car (get-conf 'extra-fields)))

(provide submission-dir)
(define submission-dir-re
  (regexp (string-append "[/\\]([^/\\]+)[/\\](?:[^/\\]+)[/\\]"
                         "(?:SUCCESS-[0-9]+|ATTEMPT)[/\\]?$")))
(define (submission-dir)
  (let ([m (regexp-match submission-dir-re
                         (path->string (current-directory)))])
    (if m
      (cadr m)
      (error* "internal error: unexpected directory name: \"~a\""
              (current-directory)))))

(provide user-data)
(define (user-data user)
  ;; the student is always assumed to exist
  (cdr (get-preference (if (string? user) (string->symbol user) user)
                       (lambda () #f) 'timestamp
                       (build-path server-dir "users.ss"))))

(provide user-substs)
(define (user-substs user str)
  (subst str `(("username" . ,user) ("submission" . ,submission-dir)
               ,@(map cons fields (user-data user)))))

(define (subst str substs)
  (if (list? str)
    (map (lambda (x) (subst x substs)) str)
    (let* ([m (regexp-match-positions #rx"{([^{}]+)}" str)]
           [s (and m (substring str (caadr m) (cdadr m)))])
      (if m
        (subst (string-append
                (substring str 0 (caar m))
                (cond [(assoc s substs)
                       => (lambda (x)
                            (let ([s (cdr x)])
                              (if (procedure? s) (s) s)))]
                      [else (error 'subst "unknown substitution: ~s" s)])
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
        (error* "~a \"~a\" in ~a is longer than ~a characters"
                (if n (format "Line #~a" n) "The line")
                (regexp-replace #rx"^[ \t]*(.*?)[ \t]*$" line "\\1")
                (currently-processed-file-name)
                len)))))

;; ============================================================================
;; Text conversion

;; Code that turns binary stuff into text is split into three places:
;; * input-port->text-input-port implements a simple generic textualization
;;   filter
;; * snip->text is used earlier in the process, where comment-box text is still
;;   available

(require framework) ; for drscheme snips, used below

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
          [(syntax? x) (syntax->datum x)]
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

(define (untabify str)
  (let loop ([idx 0] [pos 0] [strs '()])
    (let ([tab (regexp-match-positions #rx"\t" str idx)])
      (if tab
        (let* ([pos (+ pos (- (caar tab) idx))]
               [newpos (* (add1 (quotient pos 8)) 8)])
          (loop (cdar tab) newpos
                (list* (make-bytes (- newpos pos) 32)
                       (subbytes str idx (caar tab))
                       strs)))
        (apply bytes-append (reverse (cons (subbytes str idx) strs)))))))

(define current-processed-file ; set when processing multi-file submissions
  (make-parameter #f))
(define (currently-processed-file-name)
  (let ([c (current-processed-file)])
    (if c (format "\"~a\"" c) "your code")))

(define (input->process->output maxwidth textualize? untabify? bad-re)
  (let loop ([n 1])
    (let ([line (if textualize?
                  (read-bytes-line (current-input-port) 'any)
                  (with-handlers ([void
                                   (lambda (e)
                                     (error* "The submission must not ~a"
                                             "have non-textual items"))])
                    (read-bytes-line (current-input-port) 'any)))])
      (unless (eof-object? line)
        (let* ([line (regexp-replace #rx#"[ \t]+$" line #"")]
               [line (if (and untabify? (regexp-match? #rx"\t" line))
                       (untabify line) line)])
          (when (and bad-re (regexp-match? bad-re line))
            (error* "You cannot use \"~a\" in ~a!~a"
                    (if (regexp? bad-re) (object-name bad-re) bad-re)
                    (currently-processed-file-name)
                    (if textualize? "" (format " (line ~a)" n))))
          (when maxwidth
            (verify-line-length line (and (not textualize?) n) maxwidth))
          (display line) (newline) (loop (add1 n)))))))

(define (submission->bytes submission maxwidth textualize? untabify?
                           markup-prefix bad-re)
  (define magic #rx#"^(?:#reader[(]lib\"read.ss\"\"wxme\"[)])?WXME")
  (unless (regexp-match? magic submission)
    (error* "bad submission format, expecting a single DrRacket submission"))
  (let-values ([(defs inters) (unpack-submission submission)])
    (parameterize ([current-input-port
                    (if textualize?
                      (input-port->text-input-port
                       (open-input-text-editor defs 0 'end snip->text))
                      (open-input-text-editor defs))]
                   [current-output-port (open-output-bytes)])
      (input->process->output maxwidth textualize? untabify? bad-re)
      (get-output-bytes (current-output-port)))))

;; ------------------------------------------------
;; This code will hack textualization of text boxes

(define (insert-to-editor editor . xs)
  (for ([x (in-list xs)])
    (send editor insert (if (string? x) x (make-object editor-snip% x)))))

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
;; Dealing with multi-file submissions

(define (read-multifile . port)
  (define magic #"<<<MULTI-SUBMISSION-FILE>>>")
  (define (assert-format b)
    (unless b
      (error* "bad submission format, expecting a multi-file submission -- ~a"
              "use the multi-file submission tool")))
  (define (read-it)
    (assert-format (equal? magic (read-bytes (bytes-length magic))))
    (let loop ([files '()])
      (let ([f (with-handlers ([void void]) (read))])
        (if (eof-object? f)
          (sort files (lambda (x y) (string<? (car x) (car y))))
          (loop (cons f files))))))
  (let ([files (if (pair? port)
                 (parameterize ([current-input-port (car port)]) (read-it))
                 (read-it))])
    (assert-format (and (list? files)
                        (andmap (lambda (x)
                                  (and (list? x) (= 2 (length x))
                                       (string? (car x)) (bytes? (cadr x))))
                                files)))
    files))

(define ((unpack-multifile-submission names-checker raw-file-name)
         submission maxwidth textualize? untabify?
         markup-prefix prefix-re)
  (let* ([files (read-multifile (open-input-bytes submission))]
         [names (map car files)])
    (cond [(ormap (lambda (f)
                    (and (regexp-match? #rx"^[.]|[/\\ ]" (car f)) (car f)))
                  files)
           => (lambda (file) (error* "bad filename: ~e" file))])
    (cond [(procedure? names-checker) (names-checker names)]
          [(or (regexp? names-checker)
               (string? names-checker) (bytes? names-checker))
           (cond [(ormap (lambda (n)
                           (and (not (regexp-match? names-checker n)) n))
                         names)
                  => (lambda (file) (error* "bad filename: ~e" file))])]
          [(and (list? names-checker) (andmap string? names-checker))
           (let ([missing (remove* names names-checker)])
             (when (pair? missing) (error* "missing files: ~e" missing)))
           (let ([extra (remove* names-checker names)])
             (when (pair? extra) (error* "unexpected files: ~e" extra)))]
          [names-checker (error* "bad names-checker specification: ~e"
                                 names-checker)])
    ;; problem: students might think that submitting files one-by-one will keep
    ;; them all; solution: if there is already a submission, then warn against
    ;; files that disappear.
    (let* ([raw (build-path 'up raw-file-name)]
           [old (and (file-exists? raw)
                     (with-handlers ([void (lambda _ #f)])
                       (with-input-from-file raw read-multifile)))]
           [removed (and old (remove* names (map car old)))])
      (when (and (pair? removed)
                 (not (eq? 'ok (message
                                (apply string-append
                                       "The following file"
                                       (if (pair? (cdr removed)) "s" "")
                                       " will be lost:"
                                       (map (lambda (n) (string-append " " n))
                                            removed))
                                '(ok-cancel caution)))))
        (error* "Aborting...")))
    ;; This will create copies of the original files
    ;; (for ([file (in-list files)])
    ;;   (with-output-to-file (car file)
    ;;     (lambda () (display (cadr file)) (flush-output))))
    (let* ([pfx-len  (string-length markup-prefix)]
           [line-len (- maxwidth pfx-len)]
           [=s       (lambda (n) (if (<= 0 n) (make-string n #\=) ""))]
           [===      (format "~a~a\n" markup-prefix (=s line-len))])
      (define (sep name)
        (newline)
        (display ===)
        (let ([n (/ (- line-len 4 (string-length name)) 2)])
          (printf "~a~a< ~a >~a\n"
                  markup-prefix (=s (floor n)) name (=s (ceiling n))))
        (display ===)
        (newline))
      (parameterize ([current-output-port (open-output-bytes)])
        (for ([file (in-list files)])
          (sep (car file))
          (parameterize ([current-input-port (open-input-bytes (cadr file))]
                         [current-processed-file (car file)])
            (input->process->output
             maxwidth textualize? untabify? prefix-re)))
        (get-output-bytes (current-output-port))))))

;; ============================================================================
;; Checker function

(provide submission-eval)
(define submission-eval (make-parameter #f))

;; without this the primitive eval is not available
(provide (rename-out [eval prim-eval]))

;; for adding lines in the checker
(define added-lines (make-thread-cell #f))
(provide add-header-line!)
(define (add-header-line! line)
  (let ([new (list line)] [cur (thread-cell-ref added-lines)])
    (if cur
      (set-box! cur (append (unbox cur) new))
      (thread-cell-set! added-lines (box new)))))

(define ((wrap-evaluator eval) expr)
  (define unknown "unknown")
  (define (reraise exn)
    (raise
     (let-values ([(struct-type skipped?) (struct-info exn)])
       (if (and struct-type (not skipped?))
         (let ([vals (cdr (vector->list (struct->vector exn unknown)))])
           (if (memq unknown vals)
             exn
             (apply (struct-type-make-constructor struct-type)
                    (format "while evaluating ~s:\n  ~a" expr (car vals))
                    (cdr vals))))
         exn))))
  (with-handlers ([exn? reraise]) (eval expr)))

(provide check:)
(define-syntax (check: stx)
  (define (id s) (datum->syntax stx s stx))
  (let loop ([stx (syntax-case stx () [(_ x ...) #'(x ...)])]
             [keyvals '()]
             [got null])
    (define (get key . default)
      (cond [(assq key keyvals)
             => (lambda (x) (set! got (cons x got)) (caddr x))]
            [(pair? default) (car default)]
            [else #f]))
    (syntax-case stx ()
      [(key val x ...)
       (and (identifier? #'key)
            (regexp-match? #rx"^:" (symbol->string (syntax-e #'key))))
       (loop #'(x ...)
             (cons (list (syntax-e #'key) #'key #'val) keyvals)
             (cons (syntax-e #'key) got))]
      [(body ...)
       (with-syntax
           ([users*         (get ':users         #'#f)]
            [eval?*         (get ':eval?         #'#t)]
            [language*      (get ':language      #'#f)]
            [requires*      (get ':requires      #''())]
            [teachpacks*    (get ':teachpacks    #''())]
            [create-text?*  (get ':create-text?  #'#t)]
            [untabify?*     (get ':untabify?     #'#t)]
            [textualize?*   (get ':textualize?   #'#f)]
            [maxwidth*      (get ':maxwidth      #'79)]
            [markup-prefix* (get ':markup-prefix #'#f)]
            [prefix-re*     (get ':prefix-re     #'#f)]
            [student-line*
             (get ':student-line
                  #'"Student: {username} ({Full Name} <{Email}>)")]
            [extra-lines*
             (get ':extra-lines
                  #''("Maximum points for this assignment: <+100>"))]
            [value-printer* (get ':value-printer #'#f)]
            [coverage?*     (get ':coverage?     #'#f)]
            [output*        (get ':output        #'"hw.scm")]
            [multi-file*    (get ':multi-file    #'#f)]
            [names-checker* (get ':names-checker #'#f)]
            [user-error-message*
             (get ':user-error-message #'"Error in your code --\n~a")]
            [checker        (id 'checker)]
            [users          (id 'users)]
            [submission     (id 'submission)]
            [eval           (id 'eval)]
            [with-submission-bindings (id 'with-submission-bindings)]
            [user-pre       (id 'user-pre)]
            [user-post      (id 'user-post)]
            [(body ...) (syntax-case #'(body ...) ()
                          [() #'(void)] [_ #'(body ...)])])
         (for ([x (in-list keyvals)])
           (unless (memq (car x) got)
             (raise-syntax-error #f "unknown keyword" stx (cadr x))))
         #'(begin
             (provide checker)
             (define checker
               (let ([allowed (let ([us users*])
                                (if (list? us)
                                  (map (lambda (x)
                                         (if (list? x)
                                           (sort x string<?)
                                           (list x)))
                                       us)
                                  us))]
                     [eval?          eval?*]
                     [language       language*]
                     [requires       requires*]
                     [teachpacks     teachpacks*]
                     [create-text?   create-text?*]
                     [untabify?      untabify?*]
                     [textualize?    textualize?*]
                     [maxwidth       maxwidth*]
                     [markup-prefix  markup-prefix*]
                     [prefix-re      prefix-re*]
                     [student-line   student-line*]
                     [extra-lines    extra-lines*]
                     [value-printer  value-printer*]
                     [coverage?      coverage?*]
                     [output-file    output*]
                     [multi-file     multi-file*]
                     [names-checker  names-checker*]
                     [uem            user-error-message*])
                 ;; ========================================
                 ;; set defaults that depend on file name
                 (define suffix
                   (let ([sfx (string->symbol
                               (string-downcase
                                (if multi-file
                                  (format "~a" multi-file)
                                  (and output-file
                                       (regexp-replace
                                        #rx"^.*[.]" output-file "")))))])
                     (case sfx
                       [(java c cc c++)
                        (unless markup-prefix (set! markup-prefix "//> "))
                        (unless prefix-re     (set! prefix-re     #rx"//>"))]
                       [else
                        (unless markup-prefix (set! markup-prefix ";;> "))
                        (unless prefix-re     (set! prefix-re     #rx";>"))])
                     sfx))
                 ;; ========================================
                 ;; verify submitting users
                 (define (pre users submission)
                   (set-run-status "checking submission username(s)")
                   (cond [(list? allowed)
                          (unless (member users allowed)
                            (error*
                             "You are not registered for ~a submission"
                             (case (length users)
                               [(1) "individual"]
                               [(2) "pair"]
                               [else "group"])))]
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
                   (define text-file (format "grading/text.~a" suffix))
                   (define (prefix-line str)
                     (printf "~a~a\n" markup-prefix str))
                   (define generic-substs `(("submission" . ,submission-dir)))
                   (define (prefix-line/substs str)
                     (prefix-line (subst str generic-substs)))
                   (define (write-text)
                     (set-run-status "creating text file")
                     (with-output-to-file text-file #:exists 'truncate
                       (lambda ()
                         (for ([user (in-list users)])
                           (prefix-line (user-substs user student-line)))
                         (for-each prefix-line/substs extra-lines)
                         (for-each prefix-line/substs
                                   (cond [(thread-cell-ref added-lines)
                                          => unbox]
                                         [else '()]))
                         (display submission-text))))
                   (define submission-text
                     (and create-text?
                          (begin (set-run-status "reading submission")
                                 ((if multi-file
                                    (unpack-multifile-submission
                                     names-checker output-file)
                                    submission->bytes)
                                  submission maxwidth textualize? untabify?
                                  markup-prefix prefix-re))))
                   (define (uem-handler e)
                     (let ([m (if (exn? e) (exn-message e) (format "~a" e))])
                       (cond
                         [(procedure? uem) (uem m)]
                         [(not (string? uem))
                          (error* "badly configured user-error-message")]
                         [(regexp-match? #rx"~[aesvAESV]" uem) (error* uem m)]
                         [else (error* "~a" uem)])))
                   (when create-text? (make-directory "grading") (write-text))
                   (when value-printer (current-value-printer value-printer))
                   (when coverage? (sandbox-coverage-enabled #t))
                   (set-run-status "checking submission")
                   (cond
                     [(not eval?) (let () body ...)]
                     [language
                      (let ([eval (with-handlers ([void uem-handler])
                                    (call-with-evaluator/submission
                                     language (append requires teachpacks)
                                     submission values))])
                        (set-run-status "running tests")
                        (parameterize ([submission-eval (wrap-evaluator eval)])
                          (let-syntax ([with-submission-bindings
                                        (syntax-rules ()
                                          [(_ bindings body*1 body* (... ...))
                                           (with-bindings eval bindings
                                             body*1 body* (... ...))])])
                            (let () body ...))
                          ;; will do nothing when called a second time
                          (when coverage? (!all-covered))
                          (when (thread-cell-ref added-lines) (write-text))))]
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
                                  [(and maxwidth (not untabify?))
                                   "`untabify?' without `maxwidth'"]
                                  [(and (not eval?) coverage?)
                                   "`coverage?' without `eval?'"]
                                  [(and (pair? requires) (pair? teachpacks))
                                   "`requires' and `teachpacks'"]
                                  ;; [(and textualize? coverage?)
                                  ;;  "`textualize?' and `coverage?'"]
                                  [else #f])])
                   (when bad
                     (error* "bad checker specifications: ~e" bad)))
                 ;; ========================================
                 (list pre check post)))))])))

(define-syntax (with-bindings stx)
  (syntax-case stx ()
    [(_ get (var ...) body ...)
     (with-syntax ([(>var ...)
                    (map (lambda (v)
                           (datum->syntax
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
             (define (id s) (datum->syntax stx s stx))
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
                 " to continue?")
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
                      (cond [(eof-object? x) (reverse r)]
                            [(null? x) (loop r)]
                            [(list? x) (loop (cons (sort x string<?) r))]
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

(define (get-namespace evaluator)
  (call-in-sandbox-context evaluator current-namespace))

;; checks that ids are defined, either as variables or syntaxes
(provide !defined)
(define-syntax-rule (!defined id ...)
  ;; expected to be used only with identifiers
  (begin (with-handlers ([exn:fail:contract:variable?
                          (lambda (_)
                            (error* "missing binding: ~.s" (->disp 'id)))]
                         [exn:fail:syntax? void])
           (parameterize ([current-namespace (get-namespace (submission-eval))])
             (namespace-variable-value `id)))
         ...))

;; checks that ids are defined as variables, not syntaxes
(provide !bound)
(define-syntax-rule (!bound id ...)
  ;; expected to be used only with identifiers
  (begin (with-handlers ([exn:fail:contract:variable?
                          (lambda (_)
                            (error* "missing binding: ~.s" (->disp 'id)))]
                         [exn:fail:syntax?
                          (lambda (_)
                            (error* "bound to a syntax, expecting a value: ~e"
                                    (->disp 'id)))])
           (parameterize ([current-namespace (get-namespace (submission-eval))])
             (namespace-variable-value `id)))
         ...))

;; checks that ids are defined as syntaxes, not variables
(provide !syntax)
(define-syntax-rule (!syntax id ...)
  ;; expected to be used only with identifiers
  (begin (with-handlers ([exn:fail:syntax? void]
                         [exn:fail:contract:variable?
                          (lambda (_)
                            (error* "missing binding: ~.s" (->disp 'id)))])
           (parameterize ([current-namespace (get-namespace (submission-eval))])
             (namespace-variable-value `id))
           (error* "bound to a value, expecting a syntax: ~.s" (->disp 'id)))
         ...))

(provide !procedure* !procedure)
(define-syntax !procedure*
  (syntax-rules ()
    [(_ expr)
     (unless (procedure? ((submission-eval) `expr))
       (error* "~e is expected to be bound to a procedure" (->disp 'expr)))]
    [(_ expr arity)
     (let ([ar  arity]
           [val ((submission-eval) `expr)])
       (unless (procedure? val)
         (error* "~.s is expected to be bound to a procedure" (->disp 'expr)))
       (unless (procedure-arity-includes? val ar)
         (error* "~.s is expected to be bound to a procedure of ~s arguments"
                 (->disp 'expr) ar)))]))
(define-syntax !procedure
  (syntax-rules ()
    [(_ id) (begin (!defined id) (!procedure* id))]
    [(_ id arity) (begin (!defined id) (!procedure* id arity))]))

(provide !integer* !integer)
(define-syntax-rule (!integer* expr)
  (unless (integer? ((submission-eval) `expr))
    (error* "~.s is expected to be bound to an integer" (->disp 'expr))))
(define-syntax-rule (!integer id)
  (begin (!defined id) (!integer* id)))

(provide !eval)
(define-syntax-rule (!eval expr) ((submission-eval) `expr))

(provide !test)
(define-syntax !test
  (syntax-rules ()
    [(_ expr)
     (unless ((submission-eval) `expr)
       (error* "your code failed a test: ~.s is false" (->disp 'expr)))]
    [(_ expr result) (!test expr result equal?)]
    [(_ expr result equal?)
     (let ([val ((submission-eval) `expr)])
       (unless (equal? result val)
         (error* "your code failed a test: ~.s evaluated to ~e, expecting ~e"
                 (->disp 'expr) (->disp val) (->disp result))))]))

(provide !test/exn)
(define-syntax (!test/exn stx)
  (syntax-case stx ()
     [(_ test-exp)
      #`(unless (with-handlers ([exn:fail? (lambda (exn) #t)])
                  ((submission-eval) `test-exp)
                  #f)
	  (error* "expected exception on test expression: ~v"
		  (->disp 'test-exp)))]))

(provide !all-covered)
(define coverage-checked (make-thread-cell #f))
(define (!all-covered . proc)
  (define uncovered (get-uncovered-expressions (submission-eval)))
  (define (handler loc)
    (error* "your code is not completely covered by tests: ~a ~a ~s"
            "uncovered expression at" loc proc))
  (cond
    [(thread-cell-ref coverage-checked) #f]
    [(pair? uncovered)
     (let* ([stx (car uncovered)]
            [loc
             (cond [(not stx) #f]
                   [(and (syntax-line stx) (syntax-column stx))
                    (format "~a:~a" (syntax-line stx) (syntax-column stx))]
                   [(syntax-position stx) => (lambda (p) (format "#~a" p))]
                   [else "(unknown location)"])])
       (when loc
         (thread-cell-set! coverage-checked #t)
         ((if (pair? proc) (car proc) handler) loc)))]
    [(null? uncovered) #f]
    [else (error* "bad checker: no coverage information for !all-covered")]))
