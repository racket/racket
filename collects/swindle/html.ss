;;; ===========================================================================
;;; Swindle HTML Generator
;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

(module html (lib "turbo.ss" "swindle")

(provide (all-from (lib "turbo.ss" "swindle")))

;; ============================================================================
;; Global parameters

(define (make-dir-param name default)
  (make-parameter default
    (lambda (dir)
      (let ([dir (if (path? dir) (path->string dir) dir)])
        (cond [(or (not dir) (equal? "" dir)) ""]
              [(not (string? dir))
               (error name "expecting a directory string")]
              [(eq? #\/ (string-ref dir (sub1 (string-length dir)))) dir]
              [else (concat dir "/")])))))
(define (make-suffix-param name default)
  (make-parameter default
    (lambda (sfx)
      (cond [(or (not (string? sfx)) (equal? sfx ""))
             (error name "expecting a non-empty string")]
            [(eq? #\. (string-ref sfx 0)) sfx]
            [else (concat "." sfx)]))))

(define* *html-target-dir*  (make-dir-param '*html-target-dir* ""))
(define* *html-suffix*      (make-suffix-param '*html-suffix* ".html"))
(define* *image-dir*        (make-dir-param '*image-dir* "images/"))
(define* *doc-type*
  (make-parameter "HTML 4.0 Transitional"
    ;;XHTML '("XHTML 1.0 Transitional"
    ;;   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"")
    ))
(define* *charset-type*     (make-parameter "ISO-8859-1"))
(define* *prefix*           (make-parameter #f))
(define* *current-html-obj* (make-parameter #f))

;; ============================================================================
;; Basic stuff - keywords, case, special evaluation

(define (split-newlines-string str)
  (let ([l (let loop ([str str])
             (cond
              [(regexp-match-positions #rx" *\n *" str) =>
               (lambda (p)
                 (let ([a (caar p)] [b (cdar p)] [len (string-length str)])
                   (if (eq? b len)
                     (list (substring str 0 a))
                     (cons (substring str 0 a)
                           (loop (substring str b len))))))]
              [else (list str)]))])
    (if (and (pair? l) (equal? (car l) "")) (cdr l) l)))

(define* __infix-:__ list) ; ugly hack to make the ugly hack below work...

;; Turn `x : x' to `(list x x)' and _"..."_ to split strings
(define special-eval
  (let ([orig-eval (current-eval)])
    (lambda (expr)
      (define (:-args x y r)
        (let loop ([r r] [a (list y x)])
          (syntax-case r (:)
            [(: x . xs) (loop #'xs (cons #'x a))]
            [xs (values (reverse! a) #'xs)])))
      (orig-eval
       (let loop ([expr (datum->syntax-object #f expr)] [q 0])
         (syntax-case expr (: _)
           [(_ x _ . r) (string? (syntax-e #'x))
            (let ([strs (map (lambda (s) (datum->syntax-object #'x s))
                             (split-newlines-string (syntax-e #'x)))])
              (loop (quasisyntax/loc expr
                      (#,@(if (null? strs) (list #'"") strs) . r))
                    q))]
           [(qop x) (and (identifier? #'qop)
                         (memq (syntax-object->datum #'qop)
                               '(quote quasiquote unquote unquote-splicing)))
            (let ([x1 (loop #'x (case (syntax-object->datum #'qop)
                                  [(quote) +inf.0]
                                  [(quasiquote) (add1 q)]
                                  [(unquote unquote-splicing) (sub1 q)]))])
              (if (eq? x1 #'x) expr (quasisyntax/loc expr (qop #,x1))))]
           [(x : y . r)
            (let-values ([(xs rest) (:-args #'x #'y #'r)])
              (loop (if (> q 0)
                      (quasisyntax/loc expr (#,xs . #,rest))
                      (quasisyntax/loc expr ((__infix-:__ . #,xs) . #,rest)))
                    q))]
           [(x . xs)
            (let ([x1 (loop #'x q)] [xs1 (loop #'xs q)])
              (if (and (eq? x1 #'x) (eq? xs1 #'xs))
                expr
                (quasisyntax/loc expr (#,x1 . #,xs1))))]
           [x #'x]))))))

;; Activate it
(unless (eq? special-eval (current-eval)) (current-eval special-eval))
;; Make it case-sensitive by default
(read-case-sensitive #t)
;; (Note that both the above do not change parsing of this file.)

;; ============================================================================
;; Utilities

(define* (mapconcat f lst sep)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (f (car lst)))]
        [else (cons (f (car lst))
                    (apply append! (map (lambda (x) (list sep (f x)))
                                        (cdr lst))))]))

(define* (string-capital str)
  (let ([s (string-copy str)])
    (string-set! s 0 (char-upcase (string-ref s 0)))
    s))

(define (string-quote s)
  (let ([s (format "~s" s)])
    (substring s 1 (sub1 (string-length s)))))

(define* (keyword->string symbol)
  (and (keyword? symbol)
       (let ([str (symbol->string symbol)])
         (and (not (equal? str ":"))
              (substring str 1 (string-length str))))))

(define* (basename path)
  (let-values ([(_1 name _2) (split-path path)]) (path->string name)))

(define* (dirname path)
  (let-values ([(dir _1 _2) (split-path path)])
    (cond [(path? dir) (regexp-replace #rx"(.)/$" (path->string dir) "\\1")]
          [(string? dir) (regexp-replace #rx"(.)/$" dir "\\1")]
          [(eq? dir 'relative) "."]
          [(not dir) "/"])))

(define* (relativize-path path)
  (if (and (string? path) ; hack -- non-strings are just ignored
           (not (regexp-match #rx"^[a-z]+://" path)))
    (let ([cur-path
           (cond [(*current-html-obj*) => (lambda (x) (getarg x :name))]
                 [else #f])])
      (if (and cur-path (regexp-match #rx"/" cur-path))
        (let loop ([path path] [cur-path cur-path])
          (let ([x (regexp-match #rx"^([^/]*/)(.*)" path)])
            (if (and x (>= (string-length cur-path) (string-length (cadr x)))
                     (equal? (cadr x)
                             (substring cur-path 0 (string-length (cadr x)))))
              (loop (caddr x) (substring cur-path
                                         (string-length (cadr x))
                                         (string-length cur-path)))
              (regexp-replace #rx"(/?)([^/]*)$"
                              (regexp-replace #rx"[^/]*/" cur-path "../")
                              (string-append "\\1" path)))))
        path))
    path))

;; ============================================================================
;; I/O stuff

(define* (input->output)
  ;; new buffer on every call in case of threading
  (let* ([bufsize 4096] [buffer (make-string bufsize)])
    (let loop ()
      (let ([l (read-bytes-avail! buffer)])
        (unless (eof-object? l)
          (write-bytes buffer (current-output-port) 0 l)
          (loop))))))

(define* (with-output-filter filter proc)
  (let-values ([(i o) (make-pipe)])
    (define err #f)
    (define (err! e)
      (unless (or err (exn:break? e)) (set! err e))
      (close-input-port i)
      (if (eq? (current-thread) t1) (kill-thread t2) (break-thread t1)))
    (define t1 (current-thread))
    (define t2 (parameterize ([current-input-port i])
                 (thread (thunk (with-handlers ([void err!])
                                  (filter) (close-input-port i))))))
    (parameterize ([current-output-port o])
      (with-handlers ([void err!])
        (proc) (close-output-port o) (thread-wait t2)))
    (when err (raise err))))

(define* (with-input-filter filter proc)
  (let-values ([(i o) (make-pipe)])
    (define err #f)
    (define (err! e)
      (unless (or err (exn:break? e)) (set! err e))
      (close-output-port o)
      (if (eq? (current-thread) t1) (kill-thread t2) (break-thread t1)))
    (define t1 (current-thread))
    (define t2 (parameterize ([current-output-port o])
                 (thread (thunk (with-handlers ([void err!])
                                  (filter) (close-output-port o))))))
    (parameterize ([current-input-port i])
      (with-handlers ([void err!])
        (proc) (close-input-port i) (thread-wait t2)))
    (when err (raise err))))

(define (process-metas
         &keys [metas         '("<#" "#>")]
               [scheme?       #f]
               [string-quotes #f]
               [split-lines?  #f])
  (define scm? #t)
  (define meta-begin (car metas))
  (define meta-end   (cadr metas))
  (define-values (string-begin string-end)
    (cond [(list? string-quotes) (apply values string-quotes)]
          [string-quotes (values string-quotes string-quotes)]
          [(*in-quote-html?*)
           (values (concat "\"" (string-quote literal-begin))
                   (concat (string-quote literal-end) "\""))]
          [else (values "\"" "\"")]))
  (define split-indent    #f)
  (define meta-regexp     #f)
  (define (make-meta-regexp!)
    (let ([b (regexp-quote meta-begin)] [e (regexp-quote meta-end)])
      (set! meta-regexp (regexp (format "(~a|~a)(~a|~a)?" b e b e)))))
  (define (open)
    (when scm? (error 'process-metas "unexpected meta-begin token"))
    (unless split-lines? (display string-end)) (set! scm? #t))
  (define (close)
    (unless scm? (error 'process-metas "unexpected meta-end token"))
    (unless split-lines? (display string-begin))
    (set! scm? #f)
    (set! split-indent 'x))
  (define (disp x)
    (let ([x (if scm? x (string-quote x))])
      (if (or (not split-lines?) scm?)
        (unless (equal? x "") (display x))
        (let ([p (cond [(regexp-match-positions #rx"[^ ]" x) => caar]
                       [else #f])])
          (display string-begin)
          (when p
            (if (eq? 'x split-indent)
              (display (substring x p (string-length x)))
              (begin (when (or (not split-indent) (< p split-indent))
                       (set! split-indent p))
                     (display (substring x split-indent (string-length x))))))
          (display string-end)))))
  (make-meta-regexp!)
  (unless scheme? (close))
  (let loop ([str (read-line)])
    (cond
     [(eof-object? str)]
     [(regexp-match-positions meta-regexp str) =>
      (lambda (x)
        (let ([prefix (substring str 0 (caar x))]
              [suffix (substring str (cdar x) (string-length str))]
              [token  (substring str (caadr x) (cdadr x))] ; first ()'s
              [token2 (and (caddr x)                       ; second ()'s
                           (substring str (caaddr x) (cdaddr x)))])
          (when (or scm?
                    (not split-lines?)
                    (not (regexp-match #rx"^ *$" prefix)))
            (disp prefix))
          (cond
           [(and (not scm?) token2
                 (equal? meta-begin token) (equal? meta-begin token2))
            (let ([y (regexp-match-positions meta-regexp suffix)])
              (unless (and (caddr y)
                           (equal? meta-end
                                   (substring suffix (caadr y) (cdadr y)))
                           (equal? meta-end
                                   (substring suffix (caaddr y) (cdaddr y)))
                           (> (caar y) 0)
                           (zero? (modulo (caar y) 2))) ; even string
                (error 'process-metas
                       "Expected a double closing-sequence in ~s" suffix))
              ;; split the new metas
              (set! meta-begin (substring suffix 0 (/ (caar y) 2)))
              (set! meta-end (substring suffix (/ (caar y) 2) (caar y)))
              (make-meta-regexp!)
              ;; loop with the rest of suffix
              (loop str))]
           [(equal? token meta-begin) (open)
            (loop suffix)]
           [(equal? token meta-end) (close)
            (loop (if (and split-lines? (regexp-match #rx"^ *$" suffix))
                    (begin (set! split-indent #f) (read-line))
                    suffix))]
           ;; remove one "\" (never happens -- see comment above)
           [else (error 'process-metas "Internal error")])))]
     [else (disp str) (newline)
           (when (eq? 'x split-indent) (set! split-indent #f))
           (loop (read-line))]))
  (unless scheme? (open)))

(define* (display-file file)
  (cond [(not file) (input->output)]
        [(input-port? file)
         (parameterize ([current-input-port file]) (input->output))]
        [else (with-input-from-file file input->output)]))

(define* (display-mixed-file file &rest args)
  (define (doit)
    (with-input-filter
     (if (null? args) process-metas (lambda () (apply process-metas args)))
     (thunk (parameterize ([*newline?* (*newline?*)] [*space?* (*space?*)])
              (let loop ([x (read-syntax "mixed-file-input")])
                (unless (eof-object? x)
                  (output (eval (namespace-syntax-introduce x)))
                  (loop (read-syntax "mixed-file-input"))))))))
  (cond [(not file) (doit)]
        [(input-port? file)
         (parameterize ([current-input-port file]) (doit))]
        [else (with-input-from-file file doit)]))

;; ============================================================================
;; Text processing

(define* (regexp-replacements replacements)
  (unless (list? (car replacements)) (set! replacements (list replacements)))
  (let ([replacements
         (map (lambda (x)
                (define re (if (regexp? (car x)) (car x) (regexp (car x))))
                (cons re
                      (if (and (string? (cadr x))
                               (regexp-match #rx"\\\\[0-9]" (cadr x)))
                        (lambda (str . rest)
                          (if (string? str)
                            (regexp-replace re str (cadr x))
                            str))
                        (cadr x))))
              replacements)])
    (define (replacement str &optional no-r)
      (let loop ([rs replacements])
        (cond
         [(or (null? rs) (not (string? str)) (equal? str "")) str]
         [(eq? rs no-r) (loop (cdr rs))]
         [(regexp-match-positions (caar rs) str) =>
          (lambda (posns)
            (let* ([r (cdar rs)]
                   [prfx (replacement (substring str 0 (caar posns)))]
                   [sffx (replacement
                          (substring str (cdar posns) (string-length str)))]
                   [str (cond
                         [(and (procedure? r)
                               (procedure-arity-includes? r (length posns)))
                          (apply
                           r
                           (map
                            (lambda (p)
                              (cond
                               [(not p) p]
                               [(eq? (car p) (cdr p)) ""]
                               [else
                                (replacement
                                 (if (and (eq? (car p) 0)
                                          (eq? (cdr p) (string-length str)))
                                   str (substring str (car p) (cdr p)))
                                 rs)]))
                            posns))]
                         [else r])])
              ((if (and (string? prfx) (string? str) (string? sffx))
                 concat list)
               prfx str sffx)))]
         [else (loop (cdr rs))])))
    replacement))

(define* (do-replacements replacements x . more)
  (define replace (if (procedure? replacements)
                    replacements (regexp-replacements replacements)))
  (maptree replace (if (null? more) x (cons x more))))

(define* (with-replacements replacements . body)
  (define replace (if (procedure? replacements)
                    replacements (regexp-replacements replacements)))
  (define (filter &optional not-first?)
    (let ([l (read-line)])
      (unless (eof-object? l)
        (when not-first? (newline))
        (output (replace l))
        (filter #t))))
  (list (thunk (parameterize ([*newline?*    'x]
                              [*space?*      'x]
                              [*indentation* (*indentation*)]
                              [*verbatim?*   #f])
                 (with-output-filter filter (thunk (output body)))))
        (thunk (*space?* #f) (*newline?* #f))))

(define* text-replacements
  (map (lambda (x)
         (list (car x) (lambda (_ txt) (list (cadr x) txt))))
       `((#rx"\\*([^*]*)\\*" b:) (#rx"_([^_]*)_" u:) (#rx"/([^/]*)/" i:))))

;; ============================================================================
;; HTML contents generation

(define* *newline?*    (make-parameter 'x))
(define* *space?*      (make-parameter 'x))
(define* *indentation* (make-parameter 0))
(define* *verbatim?*   (make-parameter #f))

(define *tag-table* (make-hash-table))

;; If it gets deeper, then browsers will start crying anyway!
(define *indentations* (make-vector 200 #f))

(define* (display!: x . xs)
  (unless (*verbatim?*)
    (cond
     [(eq? #t (*newline?*))
      (newline)
      (display (or (vector-ref *indentations* (*indentation*))
                   (let* ([n (* 2 (*indentation*))]
                          [i (concat (make-string (quotient n 8) #\tab)
                                     (make-string (remainder n 8) #\space))])
                     (vector-set! *indentations* (*indentation*) i)
                     i)))]
     [(eq? #t (*space?*)) (display " ")])
    (*space?* #f) (*newline?* #f))
  (display x)
  (unless (null? xs) (for-each display xs)))

(define* (newline!:)
  (unless (or (*verbatim?*) (*newline?*)) (*newline?* #t)))

(define* (space!:)
  (unless (or (*verbatim?*) (*space?*)) (*space?* #t)))

;; Can't use defform because it will override the bindings
(hash-table-put! *tag-table* 'newline   newline)
(hash-table-put! *tag-table* 'newline!: newline!:)
(hash-table-put! *tag-table* 'space!:   space!:)
;; the following makes quoted quotes disapper
(hash-table-put! *tag-table* 'quote     (lambda x x))

(define* *arg-funcs*
  (let ([rel (lambda (t a v) (values a (relativize-path v)))])
    (make-parameter `(:href ,rel :src ,rel))))

;; This function is not too elegant since it tries to be very efficient
(define (output-form x form-info)
  (define tag  (car form-info))
  (define info (cdr form-info))
  (define xs (cdr x)) ; body values
  (define ks '())     ; keyword symbols
  (define as '())     ; attribute names
  (define vs '())     ; attribute values
  (define ms '())     ; meta keyword/values
  (define fms '())    ; formatting meta keyword/values
  ;; meta values marked as unspecified
  (define ? "?")
  (define func      ?) ; function to process body
  (define empty?    ?) ; if no body (& close tag) needed
  (define 1st-args  ?) ; first argument[s] should be for this keyword[s]
  (define arg-funcs ?) ; alist of keyword processing arguments
  (define literal?  ?) ; no quote-html in body (def: #f)
  (define verbatim? ?) ; no indentation & newline formattings (def: literal?)
  (define indent?   ?) ; indent body (def: #f)
  (define newlines? ?) ; newline separators (def: indent?)
  (define spaces?   ?) ; space separators (def: (not newlines?))
  (define (kloop xs)
    (if (and (pair? xs) (pair? (cdr xs)) (symbol? (car xs)))
      (let* ([k (car xs)] [v (cadr xs)]
             [a (keyword->string k)])
        (cond
         [(memq k ks) (kloop (cddr xs))] ; ignore later key values
         [(not a) xs]
         [(eq? #\: (string-ref a 0))
          (case k
            [(::args)      (when v (set-cdr! (cdr xs) (append v (cddr xs))))]
            [(::func)      (when (eq? ? func)      (set! func      v))]
            [(::empty?)    (when (eq? ? empty?)    (set! empty?    v))]
            [(::1st-args)  (when (eq? ? 1st-args)  (set! 1st-args  v))]
            [(::arg-funcs) (when (eq? ? arg-funcs) (set! arg-funcs v)
                                 (set! fms (list* v k fms)))]
            [(::literal?)  (when (eq? ? literal?)  (set! literal?  v))]
            [(::verbatim?) (when (eq? ? verbatim?) (set! verbatim? v))]
            [(::indent?)   (when (eq? ? indent?)   (set! indent?   v)
                                 (set! fms (list* v k fms)))]
            [(::newlines?) (when (eq? ? newlines?) (set! newlines? v)
                                 (set! fms (list* v k fms)))]
            [(::spaces?)   (when (eq? ? spaces?)   (set! spaces?   v)
                                 (set! fms (list* v k fms)))]
            [else          (set! ms (list* v k ms))])
          (kloop (cddr xs))]
         [else
          (set! ks (cons k ks)) (set! as (cons a as)) (set! vs (cons v vs))
          (kloop (cddr xs))]))
      xs))
  (set! xs (kloop xs))
  (set! xs (append (kloop info) xs)) ; append if entry has args
  (let 1st-args-loop ()
    (when (and 1st-args (not (eq? ? 1st-args)))
      (let ([as 1st-args])
        (set! 1st-args ?)
        (cond [(symbol? as) (set! xs (kloop (cons as xs)))]
              [(pair? as)
               (set! xs (let loop ([xs xs] [as as] [l '()])
                          (cond
                           [(null? as) (kloop (append! (reverse! l) xs))]
                           [(null? xs)
                            (if (pair? (car as))
                              (loop xs (cdr as)
                                    (list* (cadar as) (caar as) l))
                              (error 'output-form
                                     "`~a' expecting an argument for `~a'."
                                     tag (car as)))]
                           [else
                            (loop (cdr xs) (cdr as)
                                  (list* (car xs)
                                         ((if (pair? (car as)) caar car) as)
                                         l))])))])
        (1st-args-loop))))
  (set! ks (reverse! ks))
  (set! as (reverse! as))
  (set! vs (reverse! vs))
  (set! ms (reverse! ms))
  ;; set default meta values
  (when (eq? ? empty?)    (set! empty?    '?)) ; unspec => empty if no body
  (when (eq? ? arg-funcs) (set! arg-funcs (*arg-funcs*)))
  (when (eq? ? literal?)  (set! literal?  #f))
  (when (eq? ? verbatim?) (set! verbatim? #f))
  (when (eq? ? indent?)   (set! indent?   #f))
  (when (eq? ? newlines?) (set! newlines? indent?))
  (when (eq? ? spaces?)   (set! spaces?   (not newlines?)))
  (when (eq? ? func)
    (set! func (and (or (procedure? tag) (symbol? tag))
                    (begin0 tag (set! tag #f)))))
  (when (and (eq? empty? #t) (pair? xs))
    (error 'output-form "`~a' got a non-empty body: ~s." (or tag func) xs))
  (when tag
    (when newlines? (newline!:))
    (display!: literal-begin "<" tag)
    (for-each
     (lambda (a v k)
       (cond [(and arg-funcs (getarg arg-funcs k)) =>
              (lambda (f) (when f (set!-values (a v) (f tag a v))))])
       (when v
         (if (eq? v #t)
           (display!: " " a)
           (begin (display!: " " a "=\"") (output v) (display!: "\"")))))
     as vs ks)
    ;;XHTML (display!: (if empty? " />" ">") literal-end)
    (display!: ">" literal-end))
  (unless (and (null? xs) empty? (not func))
    (when tag
      (if newlines? (newline!:) (begin (*newline?* 'x) (*space?* 'x))))
    (when literal? (display literal-begin))
    (let ([body
           (thunk
             (if func
               (output
                (let loop ([ks ks] [vs vs] [l '()])
                  (if (null? ks)
                    (let ([body (append! (reverse! fms) ms (reverse! l) xs)])
                      (if (procedure? func)
                        (apply func body)
                        (cons func body))) ; allows using a symbol as alias
                    (loop (cdr ks) (cdr vs) (list* (car vs) (car ks) l)))))
               (for-each
                (cond
                 [newlines? (newline!:) (lambda (x) (output x) (newline!:))]
                 [spaces? (space!:) (lambda (x) (output x) (space!:))]
                 [else output])
                xs)))])
      (cond [func (body)]
            [(and indent? verbatim?)
             (parameterize
                 ([*indentation* (add1 (*indentation*))] [*verbatim?* #t])
               (body))]
            [indent?
             (parameterize ([*indentation* (add1 (*indentation*))]) (body))]
            [verbatim? (parameterize ([*verbatim?* #t]) (body))]
            [else (body)]))
    (when literal? (display literal-end))
    (when tag
      (if newlines? (newline!:) (begin (*newline?* 'x) (*space?* 'x)))
      (display!: literal-begin "</" tag ">" literal-end)
      (when newlines? (newline!:)))))

(define* (output x)
  ;; optimized by frequency
  (cond
   ;; This can be used instead of the special-eval hack above, but it'll be
   ;; much more limited.
   ;; [(eq? x '!) (*space?* #t) (*newline?* #t)]
   [(string? x) (display!: x)]
   [(and (pair? x) (symbol? (car x))
         (hash-table-get
          *tag-table* (car x)
          (thunk
            (let ([s (symbol->string (car x))])
              ;; maybe do this to all symbols?
              (and (eq? #\: (string-ref s (sub1 (string-length s))))
                   (list (substring s 0 (sub1 (string-length s))))))))) =>
    (lambda (info)
      (cond [(procedure? info) (output (apply info (cdr x)))]
            [(eq? 'form~: info)
             (output-form (cons (car x) (cddr x)) (list (cadr x)))]
            [else (output-form x info)]))]
   [(list? x)      (for-each output x)]
   [(procedure? x) (output (x))] ; it might return stuff to output too
   [(void? x)      #f]
   [(promise? x)   (output (force x))]
   [(pair? x)      (output (car x)) (output (cdr x))]
   ;; [(parameter? x) (output (x))] ; not needed -- procedure? returns #t
   [x              (display!: x)]
   [else           #f]))

;; A form `constructor' -- can be modified to protect form lists so, for
;; example, appending results won't screw things up...
(define* make-form list*)
(define* (make-safe-forms! &optional (safe? #t))
  (set! make-form (if safe? (lambda args (list (apply list* args))) list*)))

(defsyntax* (defform stx)
  (syntax-case stx ()
    [(_ (name . vars) . body+args)
     (let loop ([b+a #'body+args] [body '()])
       (cond [(syntax? b+a) (loop (syntax-e b+a) body)]
             [(or (null? b+a) (keyword? (syntax-e (car b+a))))
              (quasisyntax/loc stx
                (defform name (lambda vars #,@(reverse! body)) #,@b+a))]
             [else (loop (cdr b+a) (cons (car b+a) body))]))]
    [(_ name . args) (identifier? #'name)
     (let ([str (symbol->string (syntax-object->datum #'name))])
       (if (or (equal? str "")
               (not (memq (string-ref str (sub1 (string-length str)))
                          '(#\: #\~))))
         (raise-syntax-error #f "got a name that doesn't end with a colon"
                             stx #'name)
         (let* ([str (regexp-replace #rx"^(.*[^~:])[~:]*:$" str "\\1")]
                [val
                 (syntax-case #'args ()
                   [() #`(list #,str)]
                   [(#f . as) #`(list . args)]
                   [(str . as) (string? (syntax-e #'str)) #`(list . args)]
                   [(a . as)
                    (let ([as? (not (null? (syntax-e #'as)))])
                      #`(let ([t a])
                          (cond
                           [(procedure? t) #,(if as? #'(list t . as) #'t)]
                           [(and (symbol? t) (not (keyword? t))
                                 (hash-table-get *tag-table* t (thunk #f)))
                            => (lambda (t1) #,(if as? #'(list t . as) #'t1))]
                           [else (list #,str t . as)])))])])
           #`(begin (let ([v #,val])
                      (when (pair? v)
                        (let-values ([(t1 t2) (keys/args (cdr v))])
                          (unless (null? t2)
                            (error 'defform "bad info list: ~s." v))))
                      (hash-table-put! *tag-table* 'name v))
                    (define name (lambda body (make-form 'name body)))))))]))

(defsubst* (defwrapper name args ...) (defform name args ... ::empty? #f))

(defsubst* (deftag name args ...) (defform name args ... ::empty? #t))

(make-provide-syntax defform    defform*)
(make-provide-syntax defwrapper defwrapper*)
(make-provide-syntax deftag     deftag*)

;; stuff for general formatting
(defwrapper* literal:  #f ::literal?  #t)
(defwrapper* verbatim: #f ::verbatim? #t)
(defwrapper* indent:   #f ::indent?   #t)
(defwrapper* newlines: #f ::newlines? #t)
(defwrapper* spaces:   #f ::spaces?   #t)
(defwrapper* text:     #f ::newlines? #t)

;; file utility forms
(defform* include-file: display-file)
(defform* include-mixed-file: display-mixed-file)

;; generic wrapper (expecting a string as a first argument)
(hash-table-put! *tag-table* 'form~: 'form~:)
(define* (form~: . args) (cons 'form~: args))
(defform* (wrapper~: x . xs) (list* 'form~: x ::empty? #f xs))
(defform* (tag~:     x . xs) (list* 'form~: x ::empty? #t xs))
;; some convenient functions
(define* (((form:->:: w:) . args1) &all-keys args2 &body body)
  (apply w: (append args2 args1 body))) ; arg2 precede
(define* (((form~:->~:: w~:) x . args1) &all-keys args2 &body body)
  (apply w~: x (append args2 args1 body))) ; arg2 precede

(defform* (recform: &keys (tag ::tag #f) (n ::n 1)
                    &other-keys keys &body body)
  (cond
   [(zero? n) body]
   [(and (null? body) (symbol? tag)
         ;; try to see of the tag symbol is ::empty?
         (cond [(hash-table-get *tag-table* tag (thunk #f)) =>
                (lambda (x)
                  (and (pair? x) (eq? #t (getarg (cdr x) ::empty?))))]))
    (let ([tag (if (symbol? tag) (list* tag keys) (apply tag keys))])
      (let loop ([n n] [l '()])
        (if (zero? n) l (loop (sub1 n) (cons tag l)))))]
   [else (let ([tag (if (symbol? tag) (lambda x (cons tag x)) tag)])
           (let loop ([n (sub1 n)] [l (apply tag (append keys body))])
             (if (zero? n)
               l (loop (sub1 n) (apply tag (append keys (list l)))))))]))

;; ============================================================================
;; HTML tags

(deftag* br:) (deftag* break: 'br:)
(deftag* break~: 'recform: ::tag 'br: ::1st-args '((::n 1)))
(deftag* hr:) (deftag* hline: 'hr:)

(defwrapper* html:     ::newlines? #t
  ;;XHTML :xmlns "http://www.w3.org/1999/xhtml" :xml:lang "en" :lang "en"
  )
(defwrapper* head:     ::indent?   #t)
(defwrapper* body:     ::newlines? #t)
(defwrapper* title:)
(deftag*     link:     ::indent? #t)
(deftag*     link-rel~ 'link: ::1st-args '(:rel :href))
(deftag*     link-rev~ 'link: ::1st-args '(:rev :href))
(deftag*     base:)
(defwrapper* frameset: ::indent? #t)
(deftag*     frame:)
(defwrapper* noframes:)
(defwrapper* iframe:)
(deftag*     meta:     ::indent? #f)
(deftag*     meta-content~ 'meta: ::1st-args '(:name :content))
(deftag*     http-equiv~   'meta: ::1st-args '(:http-equiv :content))

(defwrapper* p:       ::newlines? #t) (defwrapper* par: 'p:)
(defwrapper* b:       )
(defwrapper* i:       )
(defwrapper* u:       )
(defwrapper* em:      )
(defwrapper* strong:  )
(defwrapper* blink:   )
(defwrapper* s:       )
(defwrapper* strike:  )
(defwrapper* tt:      )
(defwrapper* cite:    )
(defwrapper* dfn:     )
(defwrapper* code:    )
(defwrapper* samp:    )
(defwrapper* kbd:     )
(defwrapper* var:     )
(defwrapper* abbr:    )
(defwrapper* acronym: )
(defwrapper* h1:      )
(defwrapper* h2:      )
(defwrapper* h3:      )
(defwrapper* h4:      )
(defwrapper* h5:      )
(defwrapper* h6:      )
(defwrapper* sub:     )
(defwrapper* sup:     )
(defwrapper* ins:     )
(defwrapper* del:     )
(defwrapper* nobr:    )

(defwrapper* big:     )
(defwrapper* big~:    recform: ::tag 'big: ::1st-args ::n)
(defwrapper* small:   )
(defwrapper* small~:  recform: ::tag 'small: ::1st-args ::n)

(defwrapper* font:    )

(defwrapper* face~: 'font: ::1st-args :face)

(defwrapper* (size~: s . body)
  (list* 'font: :size (list (and (number? s) (> s 0) "+") s) body))
(defwrapper* size+0: 'font: :size "+0")
(defwrapper* size+1: 'font: :size "+1")
(defwrapper* size+2: 'font: :size "+2")
(defwrapper* size+3: 'font: :size "+3")
(defwrapper* size+4: 'font: :size "+4")
(defwrapper* size-1: 'font: :size "-1")
(defwrapper* size-2: 'font: :size "-2")

(defwrapper* color~:  'font: ::1st-args :color)
(defwrapper* black:   'font: :color "black")
(defwrapper* white:   'font: :color "white")
(defwrapper* red:     'font: :color "red")
(defwrapper* green:   'font: :color "green")
(defwrapper* blue:    'font: :color "blue")
(defwrapper* cyan:    'font: :color "cyan")
(defwrapper* magenta: 'font: :color "magenta")
(defwrapper* yellow:  'font: :color "yellow")
(defwrapper* purple:  'font: :color "purple")

(defwrapper* div: ::indent? #t)
(defwrapper* left:    'div: ::indent? #t :align 'left)
(defwrapper* right:   'div: ::indent? #t :align 'right)
(defwrapper* justify: 'div: ::indent? #t :align 'justify)
(defwrapper* center:  'div: ::indent? #t :align 'center)

(defwrapper* rtl:     'div: ::indent? #t :dir 'rtl)
(defwrapper* ltr:     'div: ::indent? #t :dir 'ltr)

(defwrapper* span: ::indent? #t)
(defwrapper* class~: 'span: ::1st-args :class ::newlines? #f ::indent? #t)

(defwrapper* address:    ::indent? #t)
(defwrapper* blockquote: ::indent? #t)
(defwrapper* quote:      'blockquote: ::indent? #t)
(defwrapper* q:)
(defwrapper* pre:        ::verbatim? #t)

(deftag* img: :alt "")
(deftag* image~
  (lambda (&keys [type ::type #f] [my? ::my? #f] src &rest-keys args)
    (if (string? src)
      (begin ; use concat for relativize-path
        (when type (set! src (concat src "." type)))
        (when my?  (set! src (concat (*image-dir*) src))))
      (begin
        (when type (set! src (list src "." type)))
        (when my?  (set! src (list (*image-dir*) src)))))
    (apply img: :src src args))
  ::1st-args '(:src (:alt #f #|XHTML ""|#)))
(defform* gif~    'image~ ::type "gif")
(defform* jpg~    'image~ ::type "jpg")
(defform* png~    'image~ ::type "png")
(defform* my-image~ 'image~ ::my? #t)
(defform* my-gif~ 'gif~ ::my? #t)
(defform* my-jpg~ 'jpg~ ::my? #t)
(defform* my-png~ 'png~ ::my? #t)
(defwrapper* map:)
(deftag*     area:)
(deftag*     spacer:)

;; Links

(defwrapper* a:)
(defwrapper* ref~:
  (lambda (&keys [base ::base #f] href &rest-keys args)
    (apply a: :href (if base (list base href) href) args))
  ::1st-args :href)
(defwrapper* name~:   'a: ::1st-args :name)
(defwrapper* http~:   'ref~: ::base "http://")
(defwrapper* ftp~:    'ref~: ::base "ftp://")
(defwrapper* telnet~: 'ref~: ::base "telnet://")
(defwrapper* mailto~: 'ref~: ::base "mailto:")
(defform* (ref~    x) (ref~:    x (tt: x)))
(defform* (http~   x) (http~:   x (tt: x)))
(defform* (ftp~    x) (ftp~:    x (tt: x)))
(defform* (telnet~ x) (telnet~: x (tt: x)))
(defform* (mailto~ x) (mailto~: x (tt: x)))

;; Lists and tables

(define* !>    '!>)
(define* item> 'item>)
(define* row>  'row>)
(define* col>  'col>)

(defwrapper* li: ::indent? #t ::newlines? #f)

(define (split-by key args)
  (define (splitter args)
    (let loop ([args args] [acc '()])
      (cond
       [(null? args) (cons (reverse! acc) '())]
       [(eq? (car args) key) (cons (reverse! acc) (splitter (cdr args)))]
       [else (loop (cdr args) (cons (car args) acc))])))
  (splitter args))

(defwrapper* (list~: &keys [tag ::tag #f] [br ::br 0]
                           [subtag ::subtag '(li:)]
                           [split ::split-by '(item>)]
                           [subtag2 ::subtag2 #f]
                           [subargs ::subargs #f]
                     &rest-keys args)
  (define (wrap tag keys body subargs br)
    (cond [;; kludge: if the body begins with a `foo:' wrap it in a list
           ;; -- there is no other way to distinguish ("a" "b") and (b: "x")
           (and (pair? body) (symbol? (car body))
                (hash-table-get *tag-table* (car body)
                  (thunk
                    (let ([s (symbol->string (car body))])
                      (eq? #\: (string-ref s (sub1 (string-length s))))))))
           (set! body (list body))]
          [(not (list? body)) (set! body (list body))])
    (cond [(or (not (pair? subargs)) (null? (car subargs)))]
          [(pair? (car subargs)) (set! body (append (car subargs) body))]
          [else (set! body (append subargs body))])
    (when (and (pair? br) (number? (car br)))
      (set! body (append body (list (break~: (car br))))))
    (cond [(string? tag) (list* 'wrapper~: tag (append keys body))]
          [(symbol? tag) (cons tag (append keys body))]
          [(apply tag (append keys body))]))
  (let loop ([args    args]
             [splits  (if (list? split)  split  (list split))]
             [subtags (if (list? subtag) subtag (list subtag))]
             [tag     tag]
             [subargs (cons '() subargs)]
             [br      (and (> br 0) (list #f br))]) ; br only on 2nd level
    (let-values ([(keys items)
                  (cond [(not (list? args)) (values '() args)]
                        [(and (pair? splits) (memq (car splits) args))
                         (let ([xs (split-by (car splits) args)])
                           (values (car xs) (cdr xs)))]
                        [else (keys/args args)])])
      (cond [(not items) #f] ; filter out false items
            [(pair? splits)
             (wrap tag keys
                   (map (lambda (i)
                          (loop i (cdr splits) (cdr subtags) (car subtags)
                                (and (list? subargs) (list? (car subargs))
                                     (cdr subargs))
                                (and (pair? br) (cdr br))))
                        items)
                   subargs br)]
            [subtag2
             (let ([x (split-by !> items)])
               (cond [(null? (cdr x)) (wrap tag keys items br)]
                     [(pair? (cddr x))
                      (error 'list~: "multiple `!>'s in ~s." items)]
                     [else
                      (list (wrap tag keys (car x) subargs #f)
                            (indent:
                             (let-values ([(k b) (keys/args (cadr x))])
                               (wrap subtag2 k b
                                     (and (list? subargs)
                                          (list? (car subargs))
                                          (cdr subargs))
                                     br))))]))]
            [else (wrap tag keys items subargs br)])))
  ::indent? #t ::1st-args '::tag)

(define* list~:: (form~:->~:: list~:))

;; use strings as tags to avoid recursion
(defwrapper* enumerate:  (list~:: "ol"))
(defwrapper* itemize:    (list~:: "ul"))
(defwrapper* menu:       (list~:: "menu"))
(defwrapper* dir:        (list~:: "dir"))
(defwrapper* itemize-bullet: (list~:: "ul" :type 'disc))
(defwrapper* itemize-circle: (list~:: "ul" :type 'circle))
(defwrapper* itemize-square: (list~:: "ul" :type 'square))
(defwrapper* description:    (list~:: "dl" ::subtag "dt" ::subtag2 "dd"))

(defwrapper* table: ::indent? #t)
(defwrapper* th:    ::indent? #t)
(defwrapper* tr:    ::indent? #t)
(defwrapper* td:    ::indent? #f ::newlines? #f)

;; A version that uses `list:' -- easier for manual tables, but sensitive to
;; lists, so `table:' might be more useful for some programs.
(defwrapper* table*:
  (list~:: "table" ::subtag '(tr: td:) ::split-by '(row> col>))
  ::indent? #t)

;; Form stuff

(defwrapper* form: ::indent? #t)
(deftag*     input:)
(deftag*     button:         'input: :type 'button)
(deftag*     submit-button:  'input: :type 'submit)
(deftag*     submit~:        'submit-button: ::1st-args :value)
(deftag*     text-input:     'input: :type 'text)
(deftag*     checkbox:       'input: :type 'checkbox)
(deftag*     radiobox:       'input: :type 'radio)
(deftag*     password-input: 'input: :type 'password)
(deftag*     hidden-input:   'input: :type 'hidden)
(defwrapper* select: ::indent? #t)
(defwrapper* option: ::indent? #f ::newlines? #f)
(defwrapper* option~: ::indent? #f ::newlines? #f ::1st-args :value)
(defwrapper* options: (list~:: "select" ::subtag 'option~:)
  ::indent? #t)
(defform* (select-options: &all-keys keys &body options)
  (apply select: (append keys (map (lambda (o)
                                     (if (list? o)
                                       `(option: :value ,@o)
                                       `(option: :value ,o ,o)))
                                   options))))
(defwrapper* button*: "button")
(defwrapper* label~: ::1st-args :for)
(defwrapper* textarea: ::verbatim? #t)
(defwrapper* legend:)
(defwrapper* (fieldset: . body)
  (if (memq !> body)
    (let ([xs (split-by !> body)])
      (when (pair? (cddr xs))
        (error 'fieldset: "multiple `!>'s in ~s." body))
      (apply wrapper~: "fieldset" ::indent? #t
             (apply legend: (car xs)) (cadr xs)))
    (apply wrapper~: "fieldset" body)))

;; Comments scripts and styles

(defform* comment:
  (lambda (&keys [code? ::code? #f] &body lines)
    (unless (null? lines)
      (list literal-begin "<!--"
            (cond [code?               (list (apply indent: lines) "// ")]
                  [(null? (cdr lines)) (list " " (car lines) " ")]
                  [else                (apply indent: lines)])
            "-->" literal-end)))
  ::newlines? #t)

(defwrapper* script: ::func comment: ::code? #t
                     :type "text/javascript" :language "JavaScript")
(defwrapper* script-src~ 'script: ::1st-args :src)
(defwrapper* noscript: ::indent? #t)
(defwrapper* style: ::func comment: ::code? #t :type "text/css")
(defwrapper* style-src~ 'link: ::1st-args :href
                               :rel "stylesheet" :type "text/css")

(defwrapper* applet: ::indent? #t)
(defwrapper* object: ::indent? #t)
(deftag*     param:)
(deftag*     param~: ::1st-args '(:name :value))
(defwrapper* applet-params:
  (list~:: "applet" ::subtag 'param~:) ::indent? #t)
(defwrapper* object-params:
  (list~:: "object" ::subtag 'param~:) ::indent? #t)

(deftag*     embed:)
(defwrapper* noembed: ::indent? #t)

;; ============================================================================
;; A little higher abstraction level...

(defform* (html~: title head body
                  &keys [charset-type (*charset-type*)]
                        [prefix *prefix*])
  (html: (apply head: prefix
                (meta-content~ 'generator "Scheme!")
                (and charset-type
                     (http-equiv~ "Content-Type"
                                  (list "text/html; charset=" charset-type)))
                (and title (title: title))
                (or head '()))
         body))

(defform* (document: &keys [comment  ::comment  #f]
                           [comment1 ::comment1 comment]
                           [comment2 ::comment2 comment]
                     &rest-keys body)
  (text: (cond [(*doc-type*) =>
                (lambda (t)
                  (literal: (list "<!DOCTYPE html PUBLIC \"-//W3C//DTD "
                                  (if (list? t) (car t) t) "//EN\""
                                  (and (pair? t) (cons " " (cdr t)))
                                  ">")))])
         (and comment1 (comment: comment1))
         body
         (and comment2 (comment: comment2))))

;; ============================================================================
;; HTML quotations

;; Quote some characters.
(define* html-quotes
  (make-parameter '((#\< "lt") (#\> "gt") (#\" "quot") (#\& "amp"))))

;; Expand some other characters.
(define* html-specials
  (make-parameter
   '((#\space "nbsp") (#\C "copy") (#\R "reg") (#\T "trade") (#\- "mdash")
     (#\< "laquo") (#\> "raquo") (#\1 "sup1") (#\2 "sup2") (#\3 "sup3")
     (#\* "bull"))))

(define *in-quote-html?* (make-parameter #f))

(define* literal-begin "\0{")
(define* literal-end   "\0}")

;; Quote HTML text using the above.
;;   Things in html-quotes get translated: "<" --> "&lt;"
;;   Things in html-specials are translated when escaped: "\\ " --> "&nbsp;"
;;   All other characters after "\" appear literal.
;;   Meta quotes for literal text are "NUL{" and "NUL)" - they prevent any
;;     special processing inside (and can be nested).  The idea is that user
;;     strings and files never contains these, if needed, the literal-begin
;;     and literal-end should be used from user code.
(define* (quote-html html-proc)
  (define cur-html-quotes   (html-quotes))
  (define cur-html-specials (html-specials))
  (define (display-char ch specials)
    (cond [(assq ch specials) =>
           (lambda (x) (display #\&) (display (cadr x)) (display #\;))]
          [else (display ch)]))
  (define (quote-html)
    (let ([literal 0])
      (let loop ()
        (let ([ch (read-char)])
          (unless (eof-object? ch)
            (cond [(eq? ch #\nul)
                   (set! ch (read-char))
                   (case ch
                     [(#\{) (set! literal (add1 literal))]
                     [(#\}) (if (> literal 0)
                              (set! literal (sub1 literal))
                              (error 'quote-html "Unexpected literal-end."))]
                     [else (display ch)])]
                  [(and (eq? ch #\\) (zero? literal))
                   (display-char (read-char) cur-html-specials)]
                  [(> literal 0) (display ch)]
                  [else (display-char ch cur-html-quotes)])
            (loop))))
      (when (> literal 0) (error 'quote-html "Unmatched open-literal."))))
  (parameterize ([*in-quote-html?* #t])
    (with-output-filter quote-html html-proc)))

;; ============================================================================
;; Website creation

(define* *defined-htmls* '())
(define* (add-defined-html html)
  (set! *defined-htmls* (cons html *defined-htmls*)))

(defsyntax* (html-obj! stx)
  (syntax-case stx ()
    [(_ . body)
     (let ([body #'body])
       (let loop ([as body] [ks '()])
         (syntax-case as (:contents)
           [(:contents c . r) #f]
           [(key val . r)
            (and (identifier? #'key) (syntax-keyword? #'key))
            (loop #'r (list* #'val #'key ks))]
           [(b ...)
            (set! body `(,@(reverse! ks)
                         ,#':contents ,#'(delay (begin b ...))))]))
       #`(let ([html (list #,@body)]) (add-defined-html html) html))]))

(defsyntax* (defhtml stx)
  (syntax-case stx ()
    [(_ var . body-) (identifier? #'var)
     (let ([body #'body-])
       (let loop ([bs body])
         (syntax-case bs (:name)
           [(:name n . r) #`(define var (html-obj! . body-))]
           [(key val . r)
            (and (identifier? #'key) (syntax-keyword? #'key))
            (loop #'r)]
           [_ (let ([name (symbol->string (syntax-e #'var))])
                (when (eq? (string-ref name 0) #\_)
                  (set! name (substring name 1 (string-length name))))
                (when (eq? (string-ref name (sub1 (string-length name))) #\/)
                  (set! name (string-append name "index")))
                #`(define var (html-obj! :name #,name . body-)))])))]))

(define (maybe-add-suffix str suffix)
  (let ([len1 (string-length str)]
        [len2 (string-length suffix)])
    (if (and (>= len1 len2)
             (equal? suffix (substring str (- len1 len2) len1)))
      str (concat str suffix))))

(define* (html-file-name file-or-html &keys relative?)
  (let* ([file (if (string? file-or-html)
                 file-or-html
                 (getarg file-or-html :name))]
         [name (maybe-add-suffix (concat (*html-target-dir*) file)
                                 (*html-suffix*))])
    (if relative? (relativize-path name) name)))

(define* (html-ref-name file-or-html &keys relative?)
  (let* ([file (if (string? file-or-html)
                 file-or-html
                 (getarg file-or-html :name))]
         [name (maybe-add-suffix file (*html-suffix*))])
    (if relative? (relativize-path name) name)))

(define* (output-html html)
  ;; this is only used as a top-level wrapper for quote-html with output
  (parameterize ([*newline?* 'x] [*space?* 'x] [*verbatim?* #f])
    (quote-html (thunk (output html)
                       (when (boolean? (*newline?*)) (newline))))))

(define* (output-to-html file html)
  (let ([fname (html-file-name (or file html))]
        [html  (thunk
                 ;; Due to strange bug with Sun and NFS
                 (file-stream-buffer-mode (current-output-port) 'block)
                 (output-html html))])
    (if fname
      (begin (printf "Making ~a\n" fname)
             (let ([d (dirname fname)])
               (unless (directory-exists? d)
                 (make-directory d)))
             (when (file-exists? fname) (delete-file fname))
             (with-output-to-file fname html))
      (begin ; (eprintf "Warning: no filename, using stdout.\n")
        (html)))))

(define* (make-html page . more-args)
  (parameterize ([*current-html-obj* page])
    (apply
     (lambda (&keys name contents &rest args)
       (when (promise? contents) (set! contents (force contents)))
       (output-to-html (if (symbol? name) (symbol->string name) name)
         (thunk
           (let ([contents
                  (cond [(and (procedure? contents)
                              (arity-at-least? (procedure-arity contents)))
                         (apply contents args)]
                        [else contents])])
             (output
              (apply document:
                     ::comment1 '("Generated by Swindle/html "
                                  "(http://www.barzilay.org/Swindle/)")
                     ::comment2 "Generated by Swindle/html"
                     contents))))))
     (append page more-args))))

(define* (make-htmls pages . more-args)
  (unless (equal? "" (*html-target-dir*))
    (unless (directory-exists? (*html-target-dir*))
      (make-directory (*html-target-dir*)))
    (unless (directory-exists? (*html-target-dir*))
      (error 'make-htmls
             "could not create output directory: ~s." (*html-target-dir*))))
  (for-each (lambda (page) (apply make-html page more-args)) pages))

(define* (make-defined-htmls . more-args)
  ;; repeat while making pages create more pages
  (when (pair? *defined-htmls*)
    (let ([pages (reverse *defined-htmls*)])
      (set! *defined-htmls* '())
      (apply make-htmls pages more-args)
      (make-defined-htmls))))

(define (find-html-by-string str)
  (let* ([sym (string->symbol str)]
         [val (namespace-variable-value sym #f (lambda () #f))])
    (if (and val (list? val) (getarg val :name))
      val
      (let loop ([hs *defined-htmls*])
        (and (pair? hs) (let ([name (getarg (car hs) :name)])
                          (if (or (equal? str name)
                                  (equal? str (html-file-name name))
                                  (equal? str (html-ref-name name)))
                            (car hs)
                            (loop (cdr hs)))))))))

(define* (html-main args . more-args)
  (let ([args (cond [(list? args) args]
                    [(vector? args) (vector->list args)]
                    [else (list args)])])
    (if (null? args)
      (apply make-defined-htmls more-args)
      (for-each (lambda (x)
                  (cond [(not (string? x)) (apply make-html x more-args)]
                        [(find-html-by-string x) =>
                         (lambda (x) (apply make-html x more-args))]
                        [else (eprintf "Ignoring ~s\n" x)]))
                args))))

;; ============================================================================

)
