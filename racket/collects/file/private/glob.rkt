#lang racket/base

;; Filepath globbing.

;; A glob is a path-string that describes a set of path-strings via "wildcards":
;; - * matches any sequence of characters in a file or directory name
;; - ** matches any sequence of characters or directories in a path
;; - ? matches any single character
;; - [] matches any character between the brackets

;; Terms / Conventions in this file:
;; - variable names ending with a star (*) represent lists,
;;   e.g., `path*` is "a list of paths".
;; - "amb" = "ambiguous"
;; - "unamb" = "unambiguous"

(provide
  glob/c
  glob
  in-glob
  glob-match?
  glob-quote
  glob-capture-dotfiles?)

(require
  racket/contract/base
  racket/generator
  (only-in racket/list
    append*
    splitf-at)
  (only-in racket/path
    shrink-path-wrt)
  (only-in racket/sequence
    sequence/c
    sequence-append
    sequence-map
    sequence-filter
    sequence->list)
  (only-in racket/string
    string-contains?
    string-join)
  (for-syntax racket/base))

(module+ test ;; faking RackUnit

  (define current-test-case (make-parameter #f))
  (define num-tests (box 0))

  (define-syntax-rule (test-case str e* ...)
    (parameterize ([current-test-case str])
      e* ...))

  (define-syntax (check-equal? stx)
    (syntax-case stx ()
     [(_ actual expect)
      #`(let ([a actual]
              [e expect])
          (set-box! num-tests (+ (unbox num-tests) 1))
          (unless (equal? a e)
            (raise-user-error 'check-equal?
                              "~a [~a:~a:~a] actual value ~a differs from expected ~a"
                              (current-test-case)
                              #,(syntax-line stx)
                              #,(syntax-column stx)
                              #,(syntax-span stx)
                              a
                              e)))]))

  (define-syntax-rule (check-true e)
    (check-equal? e #t))

  (define-syntax-rule (check-false e)
    (check-equal? e #f))

  (define-syntax-rule (check-pred f e)
    (check-equal? (and (f e) #t) #t))

  (define-syntax-rule (check-exn pred thunk)
    (check-equal? (with-handlers ([pred (λ (e) #t)]) (and (thunk) #f)) #t)))

;; =============================================================================

(define glob/c
  (or/c path-string? (sequence/c path-string?)))

(define glob-capture-dotfiles? (make-parameter #f))

(define (glob pattern #:capture-dotfiles? [cd? (glob-capture-dotfiles?)])
  (sequence->list (in-glob pattern #:capture-dotfiles? cd?)))

(define (in-glob pattern #:capture-dotfiles? [cd? (glob-capture-dotfiles?)])
  ;; 2016-10-01: assuming `pattern` doesn't specify many globs
  (apply sequence-append
    (for/list ([p (in-list (flatten-glob pattern))])
      (define r* (in-glob/single p #:capture-dotfiles? cd?))
      (if (ends-with-path-sep? p)
        ;; then: filter non-directory results
        (sequence-filter directory-exists? r*)
        r*))))

(define (glob-match? pattern ps-raw #:capture-dotfiles? [cd? (glob-capture-dotfiles?)])
  (define ps (normalize-path ps-raw))
  (define swd (starts-with-dot? (path-string->string ps-raw)))
  (for/or ([p (in-list (flatten-glob pattern))])
    (and (glob-match?/single p ps swd #:capture-dotfiles? cd?)
         (or (not (ends-with-path-sep? p))
             ;; then: pattern must be a syntactic or real directory
             (or (ends-with-path-sep? ps)
                 (directory-exists? ps))))))

(define (glob-quote ps)
  (if (path? ps)
    (string->path (glob-quote/string (path->string ps)))
    (glob-quote/string ps)))

;; -----------------------------------------------------------------------------
;; -- parsing

;; (define parsed-glob/c
;;   (or/c glob-unambiguous?
;;         glob-recursive?
;;         glob-split?))

(define glob-unambiguous?
  path-string?)

(define glob-recursive?
  (cons/c path-string? regexp?))

(define glob-split?
  ;; "split" globs have 1 unambiguous element
  ;;  and at least 1 ambiguous elements (with wildcards)
  (cons/c path-string? (cons/c path-string? (listof path-string?))))

;; : glob-recursive? boolean? #:capture-dotfiles? boolean? -> (path-string? -> boolean?)
(define (glob->regexp-matcher g pattern-starts-with-dot? #:capture-dotfiles? capture-dotfiles?)
  (λ (ps)
    (and
      (if (starts-with-dot? ps)
        (or capture-dotfiles?
            pattern-starts-with-dot?)
        #t)
      (regexp-match? g (normalize-path ps)))))

;; glob->unamb+amb* : glob-split? -> (values path? (listof path?))
(define (glob->unamb+amb* g)
  (values (car g) (cdr g)))

;; path-string->glob : path-string? boolean? -> parsed-glob/c
;;
;; Parse a path-string into either:
;; - a complete path, if the string has no wildcards
;; - a regular expression, if the string has the ** wildcard
;; - a pair of:
;;   - a wildcard-free path-string
;;   - a non-empty list of path-strings, some elements will use wildcards

(module+ test
  (define HOME (find-system-path 'home-dir))

  (test-case "path-string->glob"
    ;; -- no wildcards => unambiguous
    (check-pred glob-unambiguous? (path-string->glob "foo" #f))
    (check-pred glob-unambiguous?
      (path-string->glob (build-path (current-directory) "bar" "baz.c") #f))

    ;; -- anything with ** is recursive
    (check-pred glob-recursive? (path-string->glob "**" #f))
    (check-pred glob-recursive?
      (path-string->glob (build-path HOME "x" "**" "y") #f))

    ;; -- everything else is a "split" glob
    (check-pred glob-split? (path-string->glob "foo.rkt?" #f))
    (check-pred glob-split? (path-string->glob (build-path "." "yo" "[lo]") #f))
    (check-pred glob-split? (path-string->glob "*" #f))))

(define (path-string->glob pattern capture-dotfiles?)
  (define path (normalize-path (path-string->path pattern)))
  (define elem* (explode-path path))
  (define-values (unamb amb*)
    (let loop ([unamb* '()] [elem* elem*])
      (cond
       [(or (null? elem*) (has-**? (car elem*)))
        (values (normalize-path (path*->path (map glob-element->filename (reverse unamb*))))
                (if (not (null? elem*)) '** #f))]
       [(has-glob-pattern? (car elem*))
        (values (normalize-path (path*->path (map glob-element->filename (reverse unamb*))))
                (if (ormap has-**? (cdr elem*)) '** elem*))]
       [else
        (loop (cons (car elem*) unamb*) (cdr elem*))])))
  (case amb*
   [(#f)
    unamb]
   [(**)
    (cons (path->directory-path unamb)
          (regexp
            (format "^~a$" (glob-element->regexp path capture-dotfiles?))))]
   [else
    (cons (path->directory-path unamb) amb*)]))

;; has-glob-pattern? : path-element? -> boolean?
;; #t if argument contains a globbing wildcard (* ** ? [])
(define (has-glob-pattern? p)
  (define str (path-or-symbol->string p))
  (define in-brace? (box #f))
  (for/or ([c (in-string str)]
           [i (in-naturals)]
           #:when (not (escaped? str i)))
    (when (and (not (unbox in-brace?)) (eq? #\[ c))
      (set-box! in-brace? #t))
    (or (eq? #\* c)
        (eq? #\? c)
        (and (unbox in-brace?) (eq? #\] c)))))

;; -----------------------------------------------------------------------------
;; -- matching

;; in-glob/single : path-string #:capture-dotfiles? boolean? -> (sequence/c path-string)
(define (in-glob/single pattern #:capture-dotfiles? cd?)
  (define g (path-string->glob pattern cd?))
  (cond
   [(glob-unambiguous? g)
    ;; -- return at most 1 path
    (if (or (file-exists? g) (directory-exists? g))
      (list g)
      '())]
   [(glob-recursive? g)
    ;; -- recursively search filesystem for all (regexp) matches
    ;;    (start `in-directory` with the longest possible prefix)
    (define start (car g))
    (define matcher (glob->regexp-matcher (cdr g)
                                          (starts-with-dot? pattern)
                                          #:capture-dotfiles? cd?))
    (sequence-map normalize-path (sequence-filter matcher (in-directory start)))]
   [else
    ;; -- follow the glob through the filesystem, return all matches
    (define-values (unamb amb*) (glob->unamb+amb* g))
    (if (directory-exists? unamb)
      (in-producer (glob-generator amb* unamb #:capture-dotfiles? cd?) (void))
      '())]))

;; glob-match?/single : path-string? path-string? boolean? #:capture-dotfiles? boolean? -> boolean?
(define (glob-match?/single pattern ps ps-starts-with-dot? #:capture-dotfiles? cd?)
  ;; 2016-10-01 : need `ps-starts-with-dot?` because the `ps` here is normalized,
  ;;              instead of what the user submitted
  (define g (path-string->glob pattern cd?))
  (cond
   [(glob-unambiguous? g)
    (or (equal? g ps)
        (equal? (path->directory-path g) ps))]
   [(glob-recursive? g)
    (and (if ps-starts-with-dot?
           (or cd? (starts-with-dot? (path-string->string pattern)))
           #t)
         (regexp-match? (cdr g) ps))]
   [else
    (define-values (unamb amb*) (glob->unamb+amb* g))
    ;; -- pop the `unamb` prefix from `ps`, similar to `shrink-path-wrt`
    (define ps-rest
      (let loop ([unamb* (explode-path unamb)]
                 [ps* (explode-path ps)])
        (cond
         [(null? unamb*)
          ps*]
         [(null? ps*)
          #f]
         [else
          (and (equal? (car unamb*) (car ps*))
               (loop (cdr unamb*) (cdr ps*)))])))
    ;; -- match the `amb*`
    (and ps-rest
         (let loop ([amb* amb*]
                    [ps* ps-rest])
           (cond
            [(and (null? amb*) (null? ps*))
             #t]
            [(or (null? amb*) (null? ps*))
             #f]
            [else
             (and (not (null? (glob-filter (car amb*)
                                           (list (car ps*))
                                           #:capture-dotfiles? cd?)))
                  (loop (cdr amb*) (cdr ps*)))])))]))

;; glob-generator : (listof path-element?) path? #:capture-dotfiles? boolean? -> (sequence/c path?)
;; `((glob-generator p* p #:capture-dotfiles? cd?))` filters the contents
;;  of `p` to those matching the pattern `(car p*)`. In particular:
;; - `p*` is sequence of glob-patterns to match against the filesystem
;; - `p` is a file/directory to start the search from
;;  Starts a recursive search for each match in the current directory.
(define (glob-generator to-explore* current-path #:capture-dotfiles? cd?) (generator ()
  (define in-current-path* (directory-list current-path))
  (cond
   [(null? to-explore*)
    ;; -- finished searching, yield all matches
    (for ([f (in-list in-current-path*)])
      (yield (build-path current-path f)))
    (yield (void))]
   [(null? in-current-path*)
    (yield (void))]
   [else
    (define fst (car to-explore*))
    (define rst (cdr to-explore*))
    (define nothing-left? (null? rst))
    (define e* (glob-filter fst in-current-path* #:capture-dotfiles? cd?))
    (for ([elem (in-list e*)])
      (define new-current (build-path current-path elem))
      (cond
       [nothing-left?
        (yield new-current)]
       [(directory-exists? new-current)
        (for ([r (in-producer (glob-generator rst new-current #:capture-dotfiles? cd?) (void))])
          (yield r))]))
    (void)])))

;; glob-filter : path-element? (listof path?) #:capture-dotfiles? boolean? -> (listof path?)
;; `(glob-filter g p*) filters the list `p*`, removing elements that do not
;;  match pattern `g` (interpreted as a glob).
;; Assumes the `p*` do not contain any '/' characters
(define (glob-filter pattern path* #:capture-dotfiles? capture-dotfiles?)
  (define rx
    (regexp
      (string-append "^" (glob-element->regexp pattern capture-dotfiles?) "$")))
  (define pattern-starts-with-dot? (starts-with-dot? pattern))
  (if (not rx)
    '()
    (for/list ([path (in-list path*)]
               #:when (let ([str (path->string path)])
                        (and
                          ;; -- If `path` is a dotfile and * should not
                          ;;    capture dotfiles, then ignore `path`.
                          (if (starts-with-dot? str)
                            (or capture-dotfiles?
                                pattern-starts-with-dot?)
                            #t)
                          ;; -- Ignore `path` if it doesn't match `pattern`
                          (regexp-match? rx str))))
      path)))

;; -----------------------------------------------------------------------------
;; -- compiling

;; glob-element->regexp : path? boolean? -> string?
;; Convert a glob to a regular expression string
;; - interpret wildcards as regular expressions
;; - escape other regexp syntax
(define glob-element->regexp
  (let ([REGEXP-CHARS '(#\. #\( #\) #\| #\+ #\$ #\^ #\[ #\] #\{ #\})])
         ;; Need to quote these characters before using string as a regexp
    (λ (path capture-dotfiles?)
      (define str (path->string path))
      (define prev-brace-idx ;; (U #f Index), index of most-recent '[' character
        (box #f))
      (define in-** ;; if #t, prev char. was '*' and current char. is '*' too
        (box #f))
      (define len (string-length str))
      (define str*
        (for/list ([c (in-string str)]
                   [i (in-naturals)])
          (cond
           [(unbox in-**)
            (set-box! in-** #f)
            ""]
           [(escaped? str i)
            (string c)]
           [(unbox prev-brace-idx)
            ;; inside a [...]
            ;; - don't escape anything
            ;; - exit at first ']'
            ;; - unless we have '[]]', then exit at 2nd ']'
            (if (eq? c #\])
              (if (= (unbox prev-brace-idx) (- i 1))
                "]"
                (begin (set-box! prev-brace-idx #f) "]"))
              (string c))]
           [(eq? c #\*)
            ;; check for '**'
            ;; - if so, match anything even '/'
            ;; - else, match anything except '/'
            (if (and (< (+ i 1) len) (eq? (string-ref str (+ i 1)) #\*))
              (begin (set-box! in-** #t)
                     (if capture-dotfiles? ".*" "((?!/\\.).)*"))
              "[^/]*")]
           [(eq? c #\?)
            "[^/]"]
           [(and (eq? c #\[) (has-matching-bracket? str (+ i 1)))
            (set-box! prev-brace-idx i)
            "["]
           [(memq c REGEXP-CHARS)
            ;; escape characters that the regexp might interpret
            (string #\\ c)]
           [else
            ;; keep everything else
            (string c)])))
      (string-join str* ""))))

;; glob-element->filename : (and/c path-string?
;;                                 (not/c has-**?)
;;                                 (not/c has-glob-pattern?)) -> path-string?
;; Convert a pattern with no (unescaped) glob wildcards into a value suitable
;; for use in `file-exists?` queries --- by removing the escape characters
;; from the wildcards.
;;
;; (If a pattern has wildcards, then it is converted to a regexp and compared
;;  to filenames via `directory-list` and `regexp-match?`.
;;  If not, the pattern goes through this conversion function and is compared
;;  to filenames using `file-exists?` and `directory-exists?`.)
(define (glob-element->filename ps)
  (if (path? ps)
    (string->path (glob-element->filename/string (path->string ps)))
    (glob-element->filename/string ps)))

(define GLOB-WILDCARD-CHAR* '(#\* #\? #\[ #\] #\{ #\} #\,))

(define (glob-element->filename/string str)
  (define str*
    ;; examine `str` in reverse, remove #\\ from escaped wildcards
    (let loop ([c* (reverse (string->list str))]
               [i (- (string-length str) 1)])
      (cond
        [(null? c*)
         '()]
        [(and (memq (car c*) GLOB-WILDCARD-CHAR*)
              (escaped? str i))
         (cons (string (car c*)) (loop (cddr c*) (- i 2)))]
        [else
         (cons (string (car c*)) (loop (cdr c*) (- i 1)))])))
  (apply string-append (reverse str*)))

(module+ test
  (test-case "glob-element->filename/string"
    (check-equal? (glob-element->filename/string "a") "a")
    (check-equal? (glob-element->filename/string "foo\\*rkt") "foo*rkt")
    (check-equal? (glob-element->filename/string "?\\?\\]\\[\\*") "??][*")
    (check-equal? (glob-element->filename/string "\\}a\\,") "}a,")
    (check-equal? (glob-element->filename/string "\\normal") "\\normal")))

(define (glob-quote/string str)
  (define str*
    ;; add #\\ before all wildcards
    (for/list ([c (in-string str)])
      (if (memq c GLOB-WILDCARD-CHAR*)
        (string #\\ c)
        (string c))))
  (apply string-append str*))

(module+ test
  (test-case "glob-quote/string"
    (check-equal? (glob-quote/string "a") "a")
    (check-equal? (glob-quote/string "a*") "a\\*")
    (check-equal? (glob-quote/string "*][?") "\\*\\]\\[\\?")
    (check-equal? (glob-quote/string "racket/**/base") "racket/\\*\\*/base")
    (check-equal? (glob-quote/string "},{foo,bar}") "\\}\\,\\{foo\\,bar\\}")
    (check-equal? (glob-quote/string "\\") "\\")))

;; flatten-glob : glob/c -> (listof path-string?)
(define (flatten-glob pattern)
  (if (path-string? pattern)
    (expand-braces pattern)
    (append* (map expand-braces pattern))))

;; expand-braces : path-string boolean? -> (listof path-string?)
(define (expand-braces p [in-brace? #f])
  (define str (path-string->string p))
  (define len (string-length str))
  (define (alt**->string* alt**)
    (for/list ([alt* (in-list alt**)])
      (list->string (reverse alt*))))
  ;; loop : (listof (listof char)) natural-number/c -> (listof path-string?)
  ;; Walk through `str`, collect characters into a list of paths,
  ;; returns 1 string for each combination of brace-separated alternatives.
  (let loop ([alt** '()]
             [i 0])
    (if (= i len)
      (for/list ([alt* (in-list alt**)])
        (list->string (reverse alt*)))
      (let ([c (string-ref str i)]
            [esc (escaped? str i)])
        (cond
         [(and (eq? #\{ c)
               (not esc))
          ;; On {, collect all alternatives between matching }
          ;;  and accumulate a _new_ string for each
          (define j (or (find-matching-brace str i)
                        (malformed-glob-error "unmatched brace at character ~a in '~a'" i str)))
          (define str* (expand-braces (substring str (+ i 1) j) #t))
          (define alt**+
            (cond
             [(null? alt**)
              (for/list ([str (in-list str*)])
                (reverse (string->list str)))]
             [(null? str*)
              alt**]
             [else
              (append*
                (for/list ([str (in-list str*)])
                  (define char* (reverse (string->list str)))
                  (for/list ([alt* (in-list alt**)])
                    (append char* alt*))))]))
          (loop alt**+ (+ j 1))]
         [(and (not esc) in-brace? (eq? #\, c))
          ;; -- start a new alternative
          (append (alt**->string* alt**)
                  (loop '() (+ i 1)))]
         [(and (not esc) (eq? #\} c))
          (malformed-glob-error "unmatched } in glob '~a'" str)]
         [else
          (loop (if (null? alt**)
                  (list (list c))
                  (for/list ([alt* (in-list alt**)])
                    (cons c alt*)))
                (+ i 1))])))))

;; -----------------------------------------------------------------------------
;; -- other

(define-syntax-rule (malformed-glob-error msg arg* ...)
  (raise-user-error 'glob (string-append "malformed glob: " msg) arg* ...))

;; has-**? : path-element? -> boolean?
;; #t if `p` contains '**'
(define (has-**? p)
  (string-contains? (path-or-symbol->string p) "**"))

;; normalize-path : path? -> path?
(define (normalize-path p)
  (path->complete-path (simplify-path (expand-user-path p))))

;; path*->path : (listof path?) -> path?
(define (path*->path p*)
  (if (null? p*)
    (current-directory)
    (apply build-path p*)))

;; path-string->path : path-string? -> path?
(define (path-string->path p)
  (if (path? p) p (string->path p)))

;; `(has-matching-bracket? str i)` returns #t if `str` contains an unescaped #\]
;; character past position `i`

(module+ test
  (check-false (has-matching-bracket? "" 0))
  (check-false (has-matching-bracket? "foo" 0))
  (check-false (has-matching-bracket? "yo]lo" 2))
  (check-false (has-matching-bracket? "foo]" 5))
  (check-true (and (has-matching-bracket? "foo]" 0) #t))
  (check-equal? (has-matching-bracket? "yo]lo" 1) 2))

(define (has-matching-bracket? str left-idx)
  (for/or ([i (in-range (+ left-idx 1) (string-length str))])
    (and (eq? #\] (string-ref str i))
         (not (escaped? str i))
         i)))

;; find-matching-brace : string? integer? -> (or/c #f integer?)
;; Returns the index of the } that matches the one at index `i`
(define (find-matching-brace str i)
  (define len (string-length str))
  (and (<= 0 i (- len 1))
       (eq? #\{ (string-ref str i))
       ;; loop : natural-number/c natural-number/c -> natural-number/c
       (let loop ([brace-depth 0] ; "how many { are we currently inside"
                  [i (+ i 1)])    ; index into `str`
         (and (not (= i len))
              (let ([c (string-ref str i)]
                    [esc (escaped? str i)])
                (cond
                 [(and (not esc) (eq? c #\{))
                  ;; -- add 1 level of nesting
                  (loop (+ 1 brace-depth) (+ i 1))]
                 [(and (not esc) (eq? c #\}))
                  ;; -- maybe a match, maybe just lose 1 level of nesting
                  (if (zero? brace-depth)
                    i
                    (loop (- brace-depth 1) (+ i 1)))]
                 [else
                  (loop brace-depth (+ i 1))]))))))

;; `(escaped? str i)` returns #t if position `i` in the string `str`
;;  is escaped by a #\\ character

(module+ test
  (check-equal? (escaped? "be\\n" 0) #f)
  (check-equal? (escaped? "be\\\\n" 4) #f)
  (check-equal? (escaped? "be\\n" 3) #t)
  (check-equal? (escaped? "\\neb" 1) #t))

(define (escaped? str i)
  (case i
   [(0)
    #f]
   [(1)
    (eq? #\\ (string-ref str 0))]
   [else
    (and (eq? #\\ (string-ref str (- i 1)))
         (not (eq? #\\ (string-ref str (- i 2)))))]))

;; `(starts-with-dot? str)` returns #t if the first character in `str` is #\.

(module+ test
  (check-equal? (starts-with-dot? "foo") #f)
  (check-equal? (starts-with-dot? ".") #f)
  (check-equal? (starts-with-dot? "./yo") #f)
  (check-equal? (starts-with-dot? "") #f)
  (check-equal? (starts-with-dot? ".foo") #t)
  (check-equal? (starts-with-dot? ".barry") #t))

(define (starts-with-dot? ps)
  (define str (path-string->string ps))
  (and (< 1 (string-length str))
       (eq? #\. (string-ref str 0))
       (not (eq? #\/ (string-ref str 1)))))

(define (path-string->string ps)
  (if (string? ps) ps (path->string ps)))

(define (path-or-symbol->string p)
  (case p
   [(up) ".."]
   [(same) "."]
   [else
    (if (absolute-path? p)
      (path->string p)
      (path-element->string p))]))

(define (ends-with-path-sep? ps)
  (define str (path-string->string ps))
  (define len (string-length str))
  (and (< 0 len)
       (case (string-ref str (- (string-length str) 1))
        [(#\/) #t]
        [(#\\) (eq? 'windows (system-type 'os))]
        [else #f])))

;; =============================================================================

(module+ test

  (parameterize ([current-directory (symbol->string (gensym "glob-test"))])
    (test-case "path-string->glob:unambiguous"
      (define (check-path-string->glob/unamb input expect)
        (define r (path-string->glob input #f))
        (check-pred glob-unambiguous? r)
        (check-equal? r expect))

      (check-path-string->glob/unamb "/a/b/c" (build-path "/" "a" "b" "c"))
      (check-path-string->glob/unamb "/////a" (build-path "/" "a"))
      (check-path-string->glob/unamb "~/foo.txt" (build-path HOME "foo.txt"))
      (check-path-string->glob/unamb "~/foo/bar/baz.md"
                                     (build-path HOME "foo" "bar" "baz.md"))
      (check-path-string->glob/unamb "/a/b/c?/../e"
                                     (build-path "/" "a" "b" "e")))

    (test-case "path-string->glob:recursive"
      (define (check-path-string->glob/recur input expect)
        (define r (path-string->glob input #f))
        (check-pred glob-recursive? r)
        (check-equal? r expect))

      (check-path-string->glob/recur
        "/**/"
        (cons (string->path "/") #rx"^/((?!/\\.).)*/$"))
      (check-path-string->glob/recur
        "a.a/[b?]/**/c?"
        (cons (path->directory-path (build-path (current-directory) "a.a"))
              (regexp
                (format "^~a$"
                  (path->string
                    (build-path (current-directory) "a\\.a/[b?]/((?!/\\.).)*/c[^/]")))))))

    (test-case "path-string->glob:split"
      (define (check-path-string->glob/split input expect)
        (define r (path-string->glob input #f))
        (check-pred glob-split? r)
        (check-equal? r expect))

      (check-path-string->glob/split
        "*"
        (list (current-directory) (build-path "*")))
      (check-path-string->glob/split
        "/a/b/c?"
        (list (build-path "/" "a" "b/") (build-path "c?")))
      (check-path-string->glob/split
        "/a/b/c?/d/e"
        (cons (build-path "/" "a" "b/") (map string->path '("c?" "d" "e"))))
      (check-path-string->glob/split
        "~/foo/bar?/baz.md"
        (cons (build-path HOME "foo/") (map string->path '("bar?" "baz.md"))))
      (check-path-string->glob/split
        "~/foo/*/baz/.."
        (list (build-path HOME "foo/") (string->path "*")))
      (check-path-string->glob/split
        "~/foo/bar*/baz/.."
        (list (build-path HOME "foo/") (string->path "bar*")))
      (check-path-string->glob/split
        "/a[bc]/d/e/f/../g"
        (cons (find-system-path 'sys-dir)
              (map string->path '("a[bc]" "d" "e" "g")))))

    (test-case "has-glob-pattern?"
      (define (has-glob-pattern?* x)
        (has-glob-pattern? (string->path x)))

      (check-equal? (has-glob-pattern?* "foo") #f)
      (check-equal? (has-glob-pattern?* "foo") #f)
      (check-equal? (has-glob-pattern?* "]") #f)
      (check-equal? (has-glob-pattern?* "[") #f)
      (check-equal? (has-glob-pattern?* "lit\\*star") #f)
      (check-equal? (has-glob-pattern?* "\\?\\?") #f)
      (check-equal? (has-glob-pattern?* "\\[\\]x") #f)
      (check-equal? (has-glob-pattern?* "][") #f)
      (check-equal? (has-glob-pattern?* "*") #t)
      (check-equal? (has-glob-pattern?* "?") #t)
      (check-equal? (has-glob-pattern?* "*") #t)
      (check-equal? (has-glob-pattern?* "***") #t)
      (check-equal? (has-glob-pattern?* "[]") #t)
      (check-equal? (has-glob-pattern?* "*[ab*d]*") #t)
      (check-equal? (has-glob-pattern?* "[[[][") #t)
      (check-equal? (has-glob-pattern?* "foo?bar*") #t)
      (check-equal? (has-glob-pattern?* "???*") #t)
      (check-equal? (has-glob-pattern?* "ar[gh]r*") #t)
      (check-equal? (has-glob-pattern?* (string #\\ #\\ #\*)) #t))

    (define (glob-element->regexp* s)
      (glob-element->regexp (string->path s) #f))

    (test-case "glob-element->regexp"
      (check-equal? (glob-element->regexp* "foobar") "foobar")
      (check-equal? (glob-element->regexp* ".") "\\.")
      (check-equal? (glob-element->regexp* "*") "[^/]*")
      (check-equal? (glob-element->regexp* "foo*.txt") "foo[^/]*\\.txt")
      (check-equal? (glob-element->regexp* "(hello world)") "\\(hello world\\)")
      (check-equal? (glob-element->regexp* "^foo|bar$") "\\^foo\\|bar\\$")
      (check-equal? (glob-element->regexp* "things?") "things[^/]")
      (check-equal? (glob-element->regexp* "\tescaped\\things\n?")
                    "\tescaped\\things\n[^/]")
      (check-equal? (glob-element->regexp* "outside[in]") "outside[in]")
      (check-equal? (glob-element->regexp* ".?.?.?") "\\.[^/]\\.[^/]\\.[^/]")
      (check-equal? (glob-element->regexp* "[") "\\[")
      (check-equal? (glob-element->regexp* "][") "\\]\\[")
      (check-equal? (glob-element->regexp* "[]]") "[]]")
      (check-equal? (glob-element->regexp* "[a*?]") "[a*?]")
      (check-equal? (glob-element->regexp* "h[el]lo[") "h[el]lo\\["))

   (test-case "glob-element->regexp:tree.ss" ;; from tree.ss in the PLT SVN
      (check-equal? (glob-element->regexp* "glob") "glob")
      (check-equal? (glob-element->regexp* "gl?ob") "gl[^/]ob")
      (check-equal? (glob-element->regexp* "gl*ob") "gl[^/]*ob")
      (check-equal? (glob-element->regexp* "gl*?ob") "gl[^/]*[^/]ob")
      (check-equal? (glob-element->regexp* "gl?*ob") "gl[^/][^/]*ob")
      (check-equal? (glob-element->regexp* "gl.ob") "gl\\.ob")
      (check-equal? (glob-element->regexp* "gl?.ob") "gl[^/]\\.ob")
      (check-equal? (glob-element->regexp* "gl^ob") "gl\\^ob")
      (check-equal? (glob-element->regexp* "gl^?ob") "gl\\^[^/]ob")
      (check-equal? (glob-element->regexp* "gl\\.ob") "gl\\.ob")
      (check-equal? (glob-element->regexp* "gl\\ob") "gl\\ob")
      (check-equal? (glob-element->regexp* "gl\\*ob") "gl\\*ob")
      (check-equal? (glob-element->regexp* "gl\\?ob") "gl\\?ob")
      (check-equal? (glob-element->regexp* "gl\\|ob") "gl\\|ob")
      (check-equal? (glob-element->regexp* "gl\\{ob") "gl\\{ob")
      (check-equal? (glob-element->regexp* "gl[?]ob") "gl[?]ob")
      (check-equal? (glob-element->regexp* "gl[*?]ob") "gl[*?]ob")
      (check-equal? (glob-element->regexp* "gl[?*]ob") "gl[?*]ob")
      (check-equal? (glob-element->regexp* "gl[]*]ob") "gl[]*]ob")
      (check-equal? (glob-element->regexp* "gl[^]*]ob") "gl[^][^/]*\\]ob")
      (check-equal? (glob-element->regexp* "gl[^]*]*ob") "gl[^][^/]*\\][^/]*ob"))

  (test-case "expand-braces:simple"
    (check-equal? (expand-braces "") '())
    (check-equal? (expand-braces "a") '("a"))
    (check-equal? (expand-braces "anything\\,") '("anything\\,"))
    (check-equal? (expand-braces "a,b,c") '("a,b,c"))
    (check-equal? (expand-braces "{a,b,c}") '("a" "b" "c"))
    (check-equal? (expand-braces "foo,bar") '("foo,bar"))
    (check-equal? (expand-braces "{foo,bar}") '("foo" "bar"))
    (check-equal?
      (sort (expand-braces "{foo,bar}-{baz,qux}.rkt") string<?)
      '("bar-baz.rkt" "bar-qux.rkt" "foo-baz.rkt" "foo-qux.rkt"))
    (check-equal? (expand-braces "{a,,b}") '("a" "b")))

  (test-case "expand-braces:nested"
    (check-equal? (expand-braces "foo{bar},baz")
                  '("foobar,baz"))
    (check-equal? (expand-braces "{foo{bar},baz}")
                  '("foobar" "baz"))
    (check-equal? (expand-braces "{{{}{}}}")
                  '())
    (check-equal? (expand-braces "yo{{}}")
                  '("yo"))
    (check-equal? (expand-braces "foo{bar,baz},qux")
                  '("foobar,qux" "foobaz,qux"))
    (check-equal? (expand-braces "f{oo{bar,baz},qux}")
                  '("foobar" "foobaz" "fqux"))
    (check-equal?
      (sort (expand-braces "a{b,{c,d}},e,f{g,h},{i,j,k}l") string<?)
      (sort '("ab,e,fg,il" "ab,e,fg,jl" "ab,e,fg,kl"
              "ab,e,fh,il" "ab,e,fh,jl" "ab,e,fh,kl"
              "ac,e,fg,il" "ac,e,fg,jl" "ac,e,fg,kl"
              "ac,e,fh,il" "ac,e,fh,jl" "ac,e,fh,kl"
              "ad,e,fg,il" "ad,e,fg,jl" "ad,e,fg,kl"
              "ad,e,fh,il" "ad,e,fh,jl" "ad,e,fh,kl") string<?))
    (check-equal? (expand-braces "{a{b,{c,d}},e,f{g,h},{i,j,k}l}")
                  '("ab" "ac" "ad" "e" "fg" "fh" "il" "jl" "kl")))

  (test-case "expand-braces:malformed"
    (check-exn exn:fail:user?
      (lambda () (expand-braces "}")))
    (check-exn exn:fail:user?
      (lambda () (expand-braces "{")))
    (check-exn exn:fail:user?
      (lambda () (expand-braces "abc{d")))
    (check-exn exn:fail:user?
      (lambda () (expand-braces "a{b}c}"))))

    (test-case "glob-element->regexp:invalid"
      ;; these are all invalid regexps
      (check-equal? (glob-element->regexp* "[]") "\\[\\]")
      (check-equal? (glob-element->regexp* "[---]") "[---]"))

    (test-case "glob-filter"
      (define (glob-filter* p p* #:capture-dotfiles? [cd? (glob-capture-dotfiles?)])
        (map path->string
             (glob-filter (string->path p) (map string->path p*) #:capture-dotfiles? cd?)))

      (check-equal?
        (glob-filter* "foo" '() #:capture-dotfiles? #f)
        '())
      (check-equal?
        (glob-filter* "foo" '("foo") #:capture-dotfiles? #f)
        '("foo"))
      (parameterize ([glob-capture-dotfiles? #f])
        (check-equal? (glob-filter* "foo" '()) '())
        (check-equal? (glob-filter* "foo" '("foo")) '("foo"))
        (check-equal? (glob-filter* "*" '("foo" ".foo")) '("foo"))
        (check-equal? (glob-filter* "foo" '("qux" "foo" "bar")) '("foo"))
        (check-equal?
          (glob-filter* "*" '("cat" "dog" "goa"))
          '("cat" "dog" "goa"))
        (check-equal? (glob-filter* "ca?" '("ca" "car" "cat")) '("car" "cat"))
        (check-equal?
          (glob-filter* "ca?at" '("ca" "car" "catat" "caat"))
          '("catat"))
        (check-equal? (glob-filter* ".?.?.?" '("a" "ab" "abc" "abcd")) '())
        (check-equal? (glob-filter* ".?.?" '("." ".." "..." "....")) '("...."))
        (check-equal?
          (glob-filter* "*.txt" '("file.txt" "sheet.txt" "work.jar" "play.tab"))
          '("file.txt" "sheet.txt")))

      (check-equal?
        (glob-filter* "*" '("foo" ".foo") #:capture-dotfiles? #t)
        '("foo" ".foo"))
      (check-equal?
        (glob-filter* ".?.?" '("." ".." "..." "....") #:capture-dotfiles? #t)
        '("...."))
      (parameterize ([glob-capture-dotfiles? #t])
        (check-equal? (glob-filter* "*" '("foo" ".foo")) '("foo" ".foo"))
        (check-equal? (glob-filter* ".?.?" '("." ".." "..." "....")) '("...."))
        (void)))

  (test-case "find-matching-brace:simple"
    (check-equal? (find-matching-brace "yo{}lo" 2) 3)
    (check-equal? (find-matching-brace "we{p}a" 2) 4)
    (check-equal? (find-matching-brace "{abc}" 0) 4)
    (check-equal? (find-matching-brace "hel{lo}world" 3) 6)
  )

  (test-case "find-matching-brace:nested"
    (check-equal? (find-matching-brace "a{b{}c}d" 1) 6)
    (check-equal? (find-matching-brace "a{b{}c}d" 3) 4)
    (check-equal? (find-matching-brace "a{b{c{d}e}f}g" 3) 9))

  (test-case "find-matching-brace:unmatched"
    (check-equal? (find-matching-brace "{" 0) #f)
    (check-equal? (find-matching-brace "abc{d" 3) #f)
    (check-equal? (find-matching-brace "foo{bar{baz}" 3) #f)
    (check-equal? (find-matching-brace "a{b{c{d}e" 3) #f))

  (test-case "find-matching-brace:arg-error"
    (check-equal? (find-matching-brace "{" -1) #f)
    (check-equal? (find-matching-brace "{}" 2) #f)
    (check-equal? (find-matching-brace "" 0) #f)
    (check-equal? (find-matching-brace "asdf" 0) #f)
    (check-equal? (find-matching-brace "a{b}c" 0) #f))
    (test-case "path-or-symbol->string"
      (check-equal?
        (path-or-symbol->string 'up)
        "..")
      (check-equal?
        (path-or-symbol->string 'same)
        ".")
      (check-equal?
        (path-or-symbol->string (string->path "~"))
        "~")
      (check-equal?
        (path-or-symbol->string (string->path "hello/"))
        "hello"))

  (test-case "ends-with-path-sep?"
    (check-true (ends-with-path-sep? "foo/"))
    (check-true (ends-with-path-sep? "foo/bar/"))
    (check-true (ends-with-path-sep? "foo/bar/baz/"))
    (check-true (ends-with-path-sep? "foo//"))
    (check-true (ends-with-path-sep? (string->path "/")))
    (check-true (ends-with-path-sep? "*/"))
    (check-true (ends-with-path-sep? "/*/*/*/"))
    (check-true (ends-with-path-sep? (build-path "?" "?/")))

    (check-false (ends-with-path-sep? ""))
    (check-false (ends-with-path-sep? "a"))
    (check-false (ends-with-path-sep? "/a"))
    (check-false (ends-with-path-sep? (build-path "foo" "bar")))
    (check-false (ends-with-path-sep? "?/?"))
    (check-false (ends-with-path-sep? "[abc/]"))
  )

  (printf "~a tests passed\n" (unbox num-tests))
))
