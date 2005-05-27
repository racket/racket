(module slatex mzscheme
  (provide slatex::process-main-tex-file)

(define-syntax slatex::setf
  (lambda (so)
    (datum->syntax-object
      so
      (let ((so-d (syntax-object->datum so)))
        (let ((l (cadr so-d)) (r (caddr so-d)))
          (if (symbol? l)
            `(set! ,l ,r)
            (let ((a (car l)))
              (if (eq? a 'list-ref)
                `(set-car! (list-tail ,@(cdr l)) ,r)
                `(,(cond
                    ((eq? a 'string-ref) 'string-set!)
                    ((eq? a 'vector-ref) 'vector-set!)
                    ((eq? a 'slatex::of) 'slatex::the-setter-for-of)
                    (else (error "setf ~s ~s is ill-formed~%" l r)))
                  ,@(cdr l)
                  ,r)))))))))


;Configured for Scheme dialect plt by scmxlate, v 0m,
;(c) Dorai Sitaram, 
;http://www.ccs.neu.edu/~dorai/scmxlate/scmxlate.html

(define slatex::*slatex-version* "2.4z2")

(define slatex::*operating-system* (if (getenv "COMSPEC") 'windows 'unix))

(define-syntax slatex::defenum
  (lambda (so)
    (datum->syntax-object
      so
      (let ((so-d (syntax-object->datum so)))
        (let loop ((z (cdr so-d)) (i 0) (r '()))
          (if (null? z)
            `(begin ,@r)
            (loop
             (cdr z)
             (+ i 1)
             (cons `(define ,(car z) (integer->char ,i)) r))))))))

(define-syntax slatex::defrecord
  (lambda (so)
    (datum->syntax-object
      so
      (let ((so-d (syntax-object->datum so)))
        (let ((name (cadr so-d)) (fields (cddr so-d)))
          (let loop ((fields fields) (i 0) (r '()))
            (if (null? fields)
              `(begin (define ,name (lambda () (make-vector ,i))) ,@r)
              (loop
               (cdr fields)
               (+ i 1)
               (cons `(define ,(car fields) ,i) r)))))))))

(define-syntax slatex::the-setter-for-of
  (lambda (so)
    (datum->syntax-object
      so
      (let ((so-d (syntax-object->datum so)))
        (let ((r (cadr so-d))
              (i (caddr so-d))
              (j (cadddr so-d))
              (z (cddddr so-d)))
          (cond
           ((null? z) `(vector-set! ,r ,i ,j))
           ((and (eq? i '/) (= (length z) 1)) `(string-set! ,r ,j ,(car z)))
           (else `(slatex::the-setter-for-of (vector-ref ,r ,i) ,j ,@z))))))))

(define-syntax slatex::of
  (lambda (so)
    (datum->syntax-object
      so
      (let ((so-d (syntax-object->datum so)))
        (let ((r (cadr so-d)) (i (caddr so-d)) (z (cdddr so-d)))
          (cond
           ((null? z) `(vector-ref ,r ,i))
           ((and (eq? i '/) (= (length z) 1)) `(string-ref ,r ,(car z)))
           (else `(slatex::of (vector-ref ,r ,i) ,@z))))))))

(define slatex::ormapcdr
  (lambda (f l)
    (let loop ((l l)) (if (null? l) #f (or (f l) (loop (cdr l)))))))

(define slatex::list-prefix?
  (lambda (pfx l)
    (cond
     ((null? pfx) #t)
     ((null? l) #f)
     ((eqv? (car pfx) (car l)) (slatex::list-prefix? (cdr pfx) (cdr l)))
     (else #f))))

(define slatex::string-suffix?
  (lambda (sfx s)
    (let ((sfx-len (string-length sfx)) (s-len (string-length s)))
      (if (> sfx-len s-len)
        #f
        (let loop ((i (- sfx-len 1)) (j (- s-len 1)))
          (if (< i 0)
            #t
            (and (char=? (string-ref sfx i) (string-ref s j))
                 (loop (- i 1) (- j 1)))))))))

(define slatex::mapcan
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) '() (append! (f (car l)) (loop (cdr l)))))))

(define slatex::lassoc
  (lambda (x al eq)
    (let loop ((al al))
      (if (null? al)
        #f
        (let ((c (car al))) (if (eq (car c) x) c (loop (cdr al))))))))

(define slatex::lmember
  (lambda (x l eq)
    (let loop ((l l)) (if (null? l) #f (if (eq (car l) x) l (loop (cdr l)))))))

(define slatex::delete
  (lambda (x l eq)
    (let loop ((l l))
      (cond
       ((null? l) l)
       ((eq (car l) x) (loop (cdr l)))
       (else (set-cdr! l (loop (cdr l))) l)))))

(define slatex::adjoin
  (lambda (x l eq) (if (slatex::lmember x l eq) l (cons x l))))

(define slatex::delete-if
  (lambda (p s)
    (let loop ((s s))
      (cond
       ((null? s) s)
       ((p (car s)) (loop (cdr s)))
       (else (set-cdr! s (loop (cdr s))) s)))))

(define slatex::string-prefix?
  (lambda (s1 s2 i)
    (let loop ((j 0))
      (if (= j i)
        #t
        (and (char=? (string-ref s1 j) (string-ref s2 j)) (loop (+ j 1)))))))

(define slatex::sublist
  (lambda (l i f)
    (let loop ((l (list-tail l i)) (k i) (r '()))
      (cond
       ((>= k f) (reverse! r))
       ((null? l) (slatex::slatex-error "sublist: List too small"))
       (else (loop (cdr l) (+ k 1) (cons (car l) r)))))))

(define slatex::position-char
  (lambda (c l)
    (let loop ((l l) (i 0))
      (cond
       ((null? l) #f)
       ((char=? (car l) c) i)
       (else (loop (cdr l) (+ i 1)))))))

(define slatex::string-position-right
  (lambda (c s)
    (let ((n (string-length s)))
      (let loop ((i (- n 1)))
        (cond
         ((< i 0) #f)
         ((char=? (string-ref s i) c) i)
         (else (loop (- i 1))))))))

(define slatex::*return* (integer->char 13))

(define slatex::*tab* (integer->char 9))

(define slatex::slatex-error
  (lambda (where . what)
    (display "Error: ")
    (display where)
    (newline)
    (for-each (lambda (v) (write v) (newline)) what)
    (error "slatex-error")))

(define slatex::exit-slatex (lambda () (exit)))

(define slatex::*slatex-case-sensitive?* #t)

(define slatex::keyword-tokens
  (list
   "=>"
   "%"
   "abort"
   "and"
   "begin"
   "begin0"
   "case"
   "case-lambda"
   "cond"
   "define"
   "define!"
   "define-macro!"
   "define-syntax"
   "defmacro"
   "defrec!"
   "delay"
   "do"
   "else"
   "extend-syntax"
   "fluid-let"
   "if"
   "lambda"
   "let"
   "let*"
   "letrec"
   "let-syntax"
   "letrec-syntax"
   "or"
   "quasiquote"
   "quote"
   "rec"
   "record-case"
   "record-evcase"
   "recur"
   "set!"
   "sigma"
   "struct"
   "syntax"
   "syntax-rules"
   "trace"
   "trace-lambda"
   "trace-let"
   "trace-recur"
   "unless"
   "unquote"
   "unquote-splicing"
   "untrace"
   "when"
   "with"))

(define slatex::variable-tokens '())

(define slatex::constant-tokens '())

(define slatex::data-tokens '())

(define slatex::special-symbols
  (reverse
    (reverse
      '(("." . ".")
        ("..." . "{\\dots}")
        ("-" . "$-$")
        ("1-" . "\\va{1$-$}")
        ("-1+" . "\\va{$-$1$+$}")))))

(define slatex::macro-definers
  '("define-syntax" "syntax-rules" "defmacro" "extend-syntax" "define-macro!"))

(define slatex::case-and-ilk '("case" "record-case"))

(define slatex::tex-analog
  (lambda (c)
    (case c
      ((#\$ #\& #\% #\# #\_) (string #\\ c))
      ((#\{ #\}) (string #\$ #\\ c #\$))
      ((#\\) "$\\backslash$")
      ((#\+) "$+$")
      ((#\*) "$\\ast$")
      ((#\=) "$=$")
      ((#\<) "$\\lt$")
      ((#\>) "$\\gt$")
      ((#\^) "\\^{}")
      ((#\|) "$\\vert$")
      ((#\~) "\\~{}")
      ((#\@) "{\\atsign}")
      ((#\") "{\\tt\\dq}")
      (else (string c)))))

(define slatex::token=?
  (lambda (t1 t2)
    ((if slatex::*slatex-case-sensitive?* string=? string-ci=?) t1 t2)))

(define slatex::*slatex-enabled?* #t)

(define slatex::*slatex-reenabler* "UNDEFINED")

(define slatex::*intext-triggerers* (list "scheme"))

(define slatex::*resultintext-triggerers* (list "schemeresult"))

(define slatex::*display-triggerers* (list "schemedisplay"))

(define slatex::*response-triggerers* (list "schemeresponse"))

(define slatex::*respbox-triggerers* (list "schemeresponsebox"))

(define slatex::*box-triggerers* (list "schemebox"))

(define slatex::*topbox-triggerers* (list "schemetopbox"))

(define slatex::*input-triggerers* (list "schemeinput"))

(define slatex::*region-triggerers* (list "schemeregion"))

(define slatex::*math-triggerers* '())

(define slatex::*slatex-in-protected-region?* #f)

(define slatex::*protected-files* '())

(define slatex::*include-onlys* 'all)

(define slatex::*latex?* #t)

(define slatex::*slatex-separate-includes?* #f)

(define slatex::*tex-calling-directory* "")

(define slatex::*max-line-length* 200)

(slatex::defenum
  &void-space
  &plain-space
  &init-space
  &init-plain-space
  &paren-space
  &bracket-space
  &quote-space
  &inner-space)

(slatex::defenum &void-tab &set-tab &move-tab &tabbed-crg-ret &plain-crg-ret)

(slatex::defenum
  &void-notab
  &begin-comment
  &mid-comment
  &begin-string
  &mid-string
  &end-string
  &begin-math
  &mid-math
  &end-math)

(slatex::defrecord
  slatex::make-raw-line
  slatex::=rtedge
  slatex::=char
  slatex::=space
  slatex::=tab
  slatex::=notab)

(define slatex::make-line
  (lambda ()
    (let ((l (slatex::make-raw-line)))
      (slatex::setf (slatex::of l slatex::=rtedge) 0)
      (slatex::setf
        (slatex::of l slatex::=char)
        (make-string slatex::*max-line-length* #\space))
      (slatex::setf
        (slatex::of l slatex::=space)
        (make-string slatex::*max-line-length* &void-space))
      (slatex::setf
        (slatex::of l slatex::=tab)
        (make-string slatex::*max-line-length* &void-tab))
      (slatex::setf
        (slatex::of l slatex::=notab)
        (make-string slatex::*max-line-length* &void-notab))
      l)))

(define slatex::*line1* (slatex::make-line))

(define slatex::*line2* (slatex::make-line))

(slatex::defrecord
  slatex::make-case-frame
  slatex::=in-ctag-tkn
  slatex::=in-bktd-ctag-exp
  slatex::=in-case-exp)

(slatex::defrecord
  slatex::make-bq-frame
  slatex::=in-comma
  slatex::=in-bq-tkn
  slatex::=in-bktd-bq-exp)

(define slatex::*latex-paragraph-mode?* 'fwd1)

(define slatex::*intext?* 'fwd2)

(define slatex::*code-env-spec* "UNDEFINED")

(define slatex::*in* 'fwd3)

(define slatex::*out* 'fwd4)

(define slatex::*in-qtd-tkn* 'fwd5)

(define slatex::*in-bktd-qtd-exp* 'fwd6)

(define slatex::*in-mac-tkn* 'fwd7)

(define slatex::*in-bktd-mac-exp* 'fwd8)

(define slatex::*case-stack* 'fwd9)

(define slatex::*bq-stack* 'fwd10)

(define slatex::display-space
  (lambda (s p)
    (cond
     ((eq? s &plain-space) (display #\space p))
     ((eq? s &init-plain-space) (display #\space p))
     ((eq? s &init-space) (display "\\HL " p))
     ((eq? s &paren-space) (display "\\PRN " p))
     ((eq? s &bracket-space) (display "\\BKT " p))
     ((eq? s &quote-space) (display "\\QUO " p))
     ((eq? s &inner-space) (display "\\ " p)))))

(define slatex::display-tab
  (lambda (tab p)
    (cond
     ((eq? tab &set-tab) (display "\\=" p))
     ((eq? tab &move-tab) (display "\\>" p)))))

(define slatex::display-notab
  (lambda (notab p)
    (cond
     ((eq? notab &begin-string) (display "\\dt{" p))
     ((eq? notab &end-string) (display "}" p)))))

(define slatex::prim-data-token?
  (lambda (token)
    (or (char=? (string-ref token 0) #\#) (string->number token))))

(define slatex::set-keyword
  (lambda (x)
    (if (not (slatex::lmember x slatex::keyword-tokens slatex::token=?))
      (begin
        (set! slatex::constant-tokens
          (slatex::delete x slatex::constant-tokens slatex::token=?))
        (set! slatex::variable-tokens
          (slatex::delete x slatex::variable-tokens slatex::token=?))
        (set! slatex::data-tokens
          (slatex::delete x slatex::data-tokens slatex::token=?))
        (set! slatex::keyword-tokens (cons x slatex::keyword-tokens))))))

(define slatex::set-constant
  (lambda (x)
    (if (not (slatex::lmember x slatex::constant-tokens slatex::token=?))
      (begin
        (set! slatex::keyword-tokens
          (slatex::delete x slatex::keyword-tokens slatex::token=?))
        (set! slatex::variable-tokens
          (slatex::delete x slatex::variable-tokens slatex::token=?))
        (set! slatex::data-tokens
          (slatex::delete x slatex::data-tokens slatex::token=?))
        (set! slatex::constant-tokens (cons x slatex::constant-tokens))))))

(define slatex::set-variable
  (lambda (x)
    (if (not (slatex::lmember x slatex::variable-tokens slatex::token=?))
      (begin
        (set! slatex::keyword-tokens
          (slatex::delete x slatex::keyword-tokens slatex::token=?))
        (set! slatex::constant-tokens
          (slatex::delete x slatex::constant-tokens slatex::token=?))
        (set! slatex::data-tokens
          (slatex::delete x slatex::data-tokens slatex::token=?))
        (set! slatex::variable-tokens (cons x slatex::variable-tokens))))))

(define slatex::set-data
  (lambda (x)
    (if (not (slatex::lmember x slatex::data-tokens slatex::token=?))
      (begin
        (set! slatex::keyword-tokens
          (slatex::delete x slatex::keyword-tokens slatex::token=?))
        (set! slatex::constant-tokens
          (slatex::delete x slatex::constant-tokens slatex::token=?))
        (set! slatex::variable-tokens
          (slatex::delete x slatex::variable-tokens slatex::token=?))
        (set! slatex::data-tokens (cons x slatex::data-tokens))))))

(define slatex::set-special-symbol
  (lambda (x transl)
    (let ((c (slatex::lassoc x slatex::special-symbols slatex::token=?)))
      (if c
        (set-cdr! c transl)
        (set! slatex::special-symbols
          (cons (cons x transl) slatex::special-symbols))))))

(define slatex::unset-special-symbol
  (lambda (x)
    (set! slatex::special-symbols
      (slatex::delete-if
        (lambda (c) (slatex::token=? (car c) x))
        slatex::special-symbols))))

(define slatex::texify (lambda (s) (list->string (slatex::texify-aux s))))

(define slatex::texify-data
  (lambda (s)
    (let loop ((l (slatex::texify-aux s)) (r '()))
      (if (null? l)
        (list->string (reverse! r))
        (let ((c (car l)))
          (loop
           (cdr l)
           (if (char=? c #\-) (append! (list #\$ c #\$) r) (cons c r))))))))

(define slatex::texify-aux
  (let ((arrow (string->list "-$>$"))
        (em-dash (string->list "---"))
        (en-dash (string->list "--"))
        (arrow2 (string->list "$\\to$"))
        (em-dash-2 (string->list "${-}{-}{-}$"))
        (en-dash-2 (string->list "${-}{-}$")))
    (lambda (s)
      (let ((texified-sl
              (slatex::mapcan
                (lambda (c) (string->list (slatex::tex-analog c)))
                (string->list s))))
        (let loop ((d texified-sl))
          (cond
           ((null? d) #f)
           ((slatex::list-prefix? arrow d)
            (let ((d2 (list-tail d 4)))
              (set-car! d (car arrow2))
              (set-cdr! d (append (cdr arrow2) d2))
              (loop d2)))
           ((slatex::list-prefix? em-dash d)
            (let ((d2 (list-tail d 3)))
              (set-car! d (car em-dash-2))
              (set-cdr! d (append (cdr em-dash-2) d2))
              (loop d2)))
           ((slatex::list-prefix? en-dash d)
            (let ((d2 (list-tail d 2)))
              (set-car! d (car en-dash-2))
              (set-cdr! d (append (cdr en-dash-2) d2))
              (loop d2)))
           (else (loop (cdr d)))))
        texified-sl))))

(define slatex::display-begin-sequence
  (lambda (out)
    (if (or slatex::*intext?* (not slatex::*latex?*))
      (begin
        (display "\\" out)
        (display slatex::*code-env-spec* out)
        (newline out))
      (begin
        (display "\\begin{" out)
        (display slatex::*code-env-spec* out)
        (display "}%" out)
        (newline out)))))

(define slatex::display-end-sequence
  (lambda (out)
    (cond
     (slatex::*intext?*
      (display "\\end" out)
      (display slatex::*code-env-spec* out)
      (newline out))
     (slatex::*latex?*
      (display "\\end{" out)
      (display slatex::*code-env-spec* out)
      (display "}" out)
      (newline out))
     (else
      (display "\\end" out)
      (display slatex::*code-env-spec* out)
      (newline out)))))

(define slatex::display-tex-char
  (lambda (c p) (display (if (char? c) (slatex::tex-analog c) c) p)))

(define slatex::display-token
  (lambda (s typ p)
    (cond
     ((eq? typ 'syntax)
      (display "\\sy{" p)
      (display (slatex::texify s) p)
      (display "}" p))
     ((eq? typ 'variable)
      (display "\\va{" p)
      (display (slatex::texify s) p)
      (display "}" p))
     ((eq? typ 'constant)
      (display "\\cn{" p)
      (display (slatex::texify s) p)
      (display "}" p))
     ((eq? typ 'data)
      (display "\\dt{" p)
      (display (slatex::texify-data s) p)
      (display "}" p))
     (else
      (slatex::slatex-error
        'slatex::display-token
        "Unknown token type"
        typ)))))

(define slatex::get-line
  (let ((curr-notab &void-notab))
    (lambda (line)
      (let ((graphic-char-seen? #f))
        (let loop ((i 0))
          (let ((c (read-char slatex::*in*)))
            (cond
             (graphic-char-seen? (void))
             ((or (eof-object? c)
                  (char=? c slatex::*return*)
                  (char=? c #\newline)
                  (char=? c #\space)
                  (char=? c slatex::*tab*))
              (void))
             (else (set! graphic-char-seen? #t)))
            (cond
             ((eof-object? c)
              (cond
               ((eq? curr-notab &mid-string)
                (if (> i 0)
                  (slatex::setf
                    (slatex::of line slatex::=notab / (- i 1))
                    &end-string)))
               ((eq? curr-notab &mid-comment) (set! curr-notab &void-notab))
               ((eq? curr-notab &mid-math)
                (slatex::slatex-error
                  'slatex::get-line
                  "Found eof inside math")))
              (slatex::setf (slatex::of line slatex::=char / i) #\newline)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &void-notab)
              (slatex::setf (slatex::of line slatex::=rtedge) i)
              (if (eq? (slatex::of line slatex::=notab / 0) &mid-string)
                (slatex::setf
                  (slatex::of line slatex::=notab / 0)
                  &begin-string))
              (if (= i 0) #f #t))
             ((or (char=? c slatex::*return*) (char=? c #\newline))
              (if (and (memv
                        slatex::*operating-system*
                        '(dos windows os2 os2fat))
                       (char=? c slatex::*return*))
                (if (char=? (peek-char slatex::*in*) #\newline)
                  (read-char slatex::*in*)))
              (slatex::setf (slatex::of line slatex::=notab / i) &void-notab)
              (cond
               ((eq? curr-notab &mid-string)
                (slatex::setf
                  (slatex::of line slatex::=notab / i)
                  &end-string))
               ((eq? curr-notab &mid-comment) (set! curr-notab &void-notab))
               ((eq? curr-notab &mid-math)
                (slatex::slatex-error
                  'slatex::get-line
                  "Sorry, you can't split "
                  "math formulas across lines in Scheme code")))
              (slatex::setf (slatex::of line slatex::=char / i) #\newline)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf
                (slatex::of line slatex::=tab / i)
                (cond
                 ((eof-object? (peek-char slatex::*in*)) &plain-crg-ret)
                 (slatex::*intext?* &plain-crg-ret)
                 (else &tabbed-crg-ret)))
              (slatex::setf (slatex::of line slatex::=rtedge) i)
              (if (eq? (slatex::of line slatex::=notab / 0) &mid-string)
                (slatex::setf
                  (slatex::of line slatex::=notab / 0)
                  &begin-string))
              #t)
             ((eq? curr-notab &mid-comment)
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf
                (slatex::of line slatex::=space / i)
                (cond
                 ((char=? c #\space) &plain-space)
                 ((char=? c slatex::*tab*) &plain-space)
                 (else &void-space)))
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &mid-comment)
              (loop (+ i 1)))
             ((char=? c #\\)
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) curr-notab)
              (let ((i+1 (+ i 1)) (c+1 (read-char slatex::*in*)))
                (if (char=? c+1 slatex::*tab*) (set! c+1 #\space))
                (slatex::setf (slatex::of line slatex::=char / i+1) c+1)
                (slatex::setf
                  (slatex::of line slatex::=space / i+1)
                  (if (char=? c+1 #\space) &plain-space &void-space))
                (slatex::setf (slatex::of line slatex::=tab / i+1) &void-tab)
                (slatex::setf
                  (slatex::of line slatex::=notab / i+1)
                  curr-notab)
                (loop (+ i+1 1))))
             ((eq? curr-notab &mid-math)
              (if (char=? c slatex::*tab*) (set! c #\space))
              (slatex::setf
                (slatex::of line slatex::=space / i)
                (if (char=? c #\space) &plain-space &void-space))
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (cond
               ((memv c slatex::*math-triggerers*)
                (slatex::setf (slatex::of line slatex::=char / i) #\$)
                (slatex::setf (slatex::of line slatex::=notab / i) &end-math)
                (slatex::setf curr-notab &void-notab))
               (else
                (slatex::setf (slatex::of line slatex::=char / i) c)
                (slatex::setf (slatex::of line slatex::=notab / i) &mid-math)))
              (loop (+ i 1)))
             ((eq? curr-notab &mid-string)
              (if (char=? c slatex::*tab*) (set! c #\space))
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf
                (slatex::of line slatex::=space / i)
                (if (char=? c #\space) &inner-space &void-space))
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf
                (slatex::of line slatex::=notab / i)
                (cond
                 ((char=? c #\") (set! curr-notab &void-notab) &end-string)
                 (else &mid-string)))
              (loop (+ i 1)))
             ((char=? c #\space)
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf
                (slatex::of line slatex::=space / i)
                (cond
                 (slatex::*intext?* &plain-space)
                 (graphic-char-seen? &inner-space)
                 (else &init-space)))
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &void-notab)
              (loop (+ i 1)))
             ((char=? c slatex::*tab*)
              (let loop1 ((i i) (j 0))
                (if (< j 8)
                  (begin
                    (slatex::setf (slatex::of line slatex::=char / i) #\space)
                    (slatex::setf
                      (slatex::of line slatex::=space / i)
                      (cond
                       (slatex::*intext?* &plain-space)
                       (graphic-char-seen? &inner-space)
                       (else &init-space)))
                    (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
                    (slatex::setf
                      (slatex::of line slatex::=notab / i)
                      &void-notab)
                    (loop1 (+ i 1) (+ j 1)))))
              (loop (+ i 8)))
             ((char=? c #\")
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &begin-string)
              (set! curr-notab &mid-string)
              (loop (+ i 1)))
             ((char=? c #\;)
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf
                (slatex::of line slatex::=notab / i)
                &begin-comment)
              (set! curr-notab &mid-comment)
              (loop (+ i 1)))
             ((memv c slatex::*math-triggerers*)
              (slatex::setf (slatex::of line slatex::=char / i) #\$)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &begin-math)
              (set! curr-notab &mid-math)
              (loop (+ i 1)))
             (else
              (slatex::setf (slatex::of line slatex::=char / i) c)
              (slatex::setf (slatex::of line slatex::=space / i) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
              (slatex::setf (slatex::of line slatex::=notab / i) &void-notab)
              (loop (+ i 1))))))))))

(define slatex::peephole-adjust
  (lambda (curr prev)
    (if (or (slatex::blank-line? curr) (slatex::flush-comment-line? curr))
      (if (not slatex::*latex-paragraph-mode?*)
        (begin
          (set! slatex::*latex-paragraph-mode?* #t)
          (if (not slatex::*intext?*)
            (begin
              (slatex::remove-some-tabs prev 0)
              (let ((prev-rtedge (slatex::of prev slatex::=rtedge)))
                (if (eq?
                     (slatex::of prev slatex::=tab / prev-rtedge)
                     &tabbed-crg-ret)
                  (slatex::setf
                    (slatex::of
                      prev
                      slatex::=tab
                      /
                      (slatex::of prev slatex::=rtedge))
                    &plain-crg-ret)))))))
      (begin
        (if slatex::*latex-paragraph-mode?*
          (set! slatex::*latex-paragraph-mode?* #f)
          (if (not slatex::*intext?*)
            (let ((remove-tabs-from #f))
              (let loop ((i 0))
                (cond
                 ((char=? (slatex::of curr slatex::=char / i) #\newline)
                  (set! remove-tabs-from i))
                 ((char=? (slatex::of prev slatex::=char / i) #\newline)
                  (set! remove-tabs-from #f))
                 ((eq? (slatex::of curr slatex::=space / i) &init-space)
                  (if (eq? (slatex::of prev slatex::=notab / i) &void-notab)
                    (begin
                      (cond
                       ((or (char=? (slatex::of prev slatex::=char / i) #\()
                            (eq?
                             (slatex::of prev slatex::=space / i)
                             &paren-space))
                        (slatex::setf
                          (slatex::of curr slatex::=space / i)
                          &paren-space))
                       ((or (char=? (slatex::of prev slatex::=char / i) #\[)
                            (eq?
                             (slatex::of prev slatex::=space / i)
                             &bracket-space))
                        (slatex::setf
                          (slatex::of curr slatex::=space / i)
                          &bracket-space))
                       ((or (memv
                             (slatex::of prev slatex::=char / i)
                             '(#\' #\` #\,))
                            (eq?
                             (slatex::of prev slatex::=space / i)
                             &quote-space))
                        (slatex::setf
                          (slatex::of curr slatex::=space / i)
                          &quote-space)))
                      (if (memq
                           (slatex::of prev slatex::=tab / i)
                           (list &set-tab &move-tab))
                        (slatex::setf
                          (slatex::of curr slatex::=tab / i)
                          &move-tab))))
                  (loop (+ i 1)))
                 ((= i 0) (set! remove-tabs-from 0))
                 ((not (eq? (slatex::of prev slatex::=tab / i) &void-tab))
                  (set! remove-tabs-from (+ i 1))
                  (if (memq
                       (slatex::of prev slatex::=tab / i)
                       (list &set-tab &move-tab))
                    (slatex::setf
                      (slatex::of curr slatex::=tab / i)
                      &move-tab)))
                 ((memq
                   (slatex::of prev slatex::=space / i)
                   (list
                    &init-space
                    &init-plain-space
                    &paren-space
                    &bracket-space
                    &quote-space))
                  (set! remove-tabs-from (+ i 1)))
                 ((and (char=?
                         (slatex::of prev slatex::=char / (- i 1))
                         #\space)
                       (eq?
                        (slatex::of prev slatex::=notab / (- i 1))
                        &void-notab))
                  (set! remove-tabs-from (+ i 1))
                  (slatex::setf (slatex::of prev slatex::=tab / i) &set-tab)
                  (slatex::setf (slatex::of curr slatex::=tab / i) &move-tab))
                 (else
                  (set! remove-tabs-from (+ i 1))
                  (let loop1 ((j (- i 1)))
                    (cond
                     ((<= j 0) 'exit-loop1)
                     ((not (eq? (slatex::of curr slatex::=tab / j) &void-tab))
                      'exit-loop1)
                     ((memq
                       (slatex::of curr slatex::=space / j)
                       (list &paren-space &bracket-space &quote-space))
                      (loop1 (- j 1)))
                     ((or (not
                           (eq?
                            (slatex::of prev slatex::=notab / j)
                            &void-notab))
                          (char=? (slatex::of prev slatex::=char / j) #\space))
                      (let ((k (+ j 1)))
                        (if (not
                             (memq
                              (slatex::of prev slatex::=notab / k)
                              (list
                               &mid-comment
                               &mid-math
                               &end-math
                               &mid-string
                               &end-string)))
                          (begin
                            (if (eq?
                                 (slatex::of prev slatex::=tab / k)
                                 &void-tab)
                              (slatex::setf
                                (slatex::of prev slatex::=tab / k)
                                &set-tab))
                            (slatex::setf
                              (slatex::of curr slatex::=tab / k)
                              &move-tab)))))
                     (else 'anything-else?))))))
              (slatex::remove-some-tabs prev remove-tabs-from))))
        (if (not slatex::*intext?*) (slatex::add-some-tabs curr))
        (slatex::clean-init-spaces curr)
        (slatex::clean-inner-spaces curr)))))

(define slatex::add-some-tabs
  (lambda (line)
    (let loop ((i 1) (succ-parens? #f))
      (let ((c (slatex::of line slatex::=char / i)))
        (cond
         ((char=? c #\newline) 'exit-loop)
         ((not (eq? (slatex::of line slatex::=notab / i) &void-notab))
          (loop (+ i 1) #f))
         ((char=? c #\[)
          (if (eq? (slatex::of line slatex::=tab / i) &void-tab)
            (slatex::setf (slatex::of line slatex::=tab / i) &set-tab))
          (loop (+ i 1) #f))
         ((char=? c #\()
          (if (eq? (slatex::of line slatex::=tab / i) &void-tab)
            (if (not succ-parens?)
              (slatex::setf (slatex::of line slatex::=tab / i) &set-tab)))
          (loop (+ i 1) #t))
         (else (loop (+ i 1) #f)))))))

(define slatex::remove-some-tabs
  (lambda (line i)
    (if i
      (let loop ((i i))
        (cond
         ((char=? (slatex::of line slatex::=char / i) #\newline) 'exit)
         ((eq? (slatex::of line slatex::=tab / i) &set-tab)
          (slatex::setf (slatex::of line slatex::=tab / i) &void-tab)
          (loop (+ i 1)))
         (else (loop (+ i 1))))))))

(define slatex::clean-init-spaces
  (lambda (line)
    (let loop ((i (slatex::of line slatex::=rtedge)))
      (cond
       ((< i 0) 'exit-loop)
       ((eq? (slatex::of line slatex::=tab / i) &move-tab)
        (let loop1 ((i (- i 1)))
          (cond
           ((< i 0) 'exit-loop1)
           ((memq
             (slatex::of line slatex::=space / i)
             (list &init-space &paren-space &bracket-space &quote-space))
            (slatex::setf
              (slatex::of line slatex::=space / i)
              &init-plain-space)
            (loop1 (- i 1)))
           (else (loop1 (- i 1))))))
       (else (loop (- i 1)))))))

(define slatex::clean-inner-spaces
  (lambda (line)
    (let loop ((i 0) (succ-inner-spaces? #f))
      (cond
       ((char=? (slatex::of line slatex::=char / i) #\newline) 'exit-loop)
       ((eq? (slatex::of line slatex::=space / i) &inner-space)
        (if (not succ-inner-spaces?)
          (slatex::setf (slatex::of line slatex::=space / i) &plain-space))
        (loop (+ i 1) #t))
       (else (loop (+ i 1) #f))))))

(define slatex::blank-line?
  (lambda (line)
    (let loop ((i 0))
      (let ((c (slatex::of line slatex::=char / i)))
        (cond
         ((char=? c #\space)
          (if (eq? (slatex::of line slatex::=notab / i) &void-notab)
            (loop (+ i 1))
            #f))
         ((char=? c #\newline)
          (let loop1 ((j (- i 1)))
            (if (not (<= j 0))
              (begin
                (slatex::setf (slatex::of line slatex::=space / i) &void-space)
                (loop1 (- j 1)))))
          #t)
         (else #f))))))

(define slatex::flush-comment-line?
  (lambda (line)
    (and (char=? (slatex::of line slatex::=char / 0) #\;)
         (eq? (slatex::of line slatex::=notab / 0) &begin-comment)
         (not (char=? (slatex::of line slatex::=char / 1) #\;)))))

(define slatex::display-tex-line
  (lambda (line)
    (cond
     (else
      (let loop ((i (if (slatex::flush-comment-line? line) 1 0)))
        (let ((c (slatex::of line slatex::=char / i)))
          (if (char=? c #\newline)
            (if (not (eq? (slatex::of line slatex::=tab / i) &void-tab))
              (newline slatex::*out*))
            (begin (write-char c slatex::*out*) (loop (+ i 1))))))))))

(define slatex::display-scm-line
  (lambda (line)
    (let loop ((i 0))
      (let ((c (slatex::of line slatex::=char / i)))
        (cond
         ((char=? c #\newline)
          (let ((notab (slatex::of line slatex::=notab / i))
                (tab (slatex::of line slatex::=tab / i)))
            (if (eq? notab &end-string) (display "}" slatex::*out*))
            (cond
             ((eq? tab &tabbed-crg-ret)
              (display "\\\\%" slatex::*out*)
              (newline slatex::*out*))
             ((eq? tab &plain-crg-ret) (newline slatex::*out*))
             ((eq? tab &void-tab)
              (write-char #\% slatex::*out*)
              (newline slatex::*out*)))))
         ((eq? (slatex::of line slatex::=notab / i) &begin-comment)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &mid-comment)
          (write-char c slatex::*out*)
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &begin-string)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (display "\\dt{" slatex::*out*)
          (if (char=? c #\space)
            (slatex::display-space
              (slatex::of line slatex::=space / i)
              slatex::*out*)
            (slatex::display-tex-char c slatex::*out*))
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &mid-string)
          (if (char=? c #\space)
            (slatex::display-space
              (slatex::of line slatex::=space / i)
              slatex::*out*)
            (slatex::display-tex-char c slatex::*out*))
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &end-string)
          (if (char=? c #\space)
            (slatex::display-space
              (slatex::of line slatex::=space / i)
              slatex::*out*)
            (slatex::display-tex-char c slatex::*out*))
          (write-char #\} slatex::*out*)
          (if slatex::*in-qtd-tkn*
            (set! slatex::*in-qtd-tkn* #f)
            (if slatex::*in-mac-tkn* (set! slatex::*in-mac-tkn* #f)))
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &begin-math)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &mid-math)
          (write-char c slatex::*out*)
          (loop (+ i 1)))
         ((eq? (slatex::of line slatex::=notab / i) &end-math)
          (write-char c slatex::*out*)
          (if slatex::*in-qtd-tkn*
            (set! slatex::*in-qtd-tkn* #f)
            (if slatex::*in-mac-tkn* (set! slatex::*in-mac-tkn* #f)))
          (loop (+ i 1)))
         ((char=? c #\space)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (slatex::display-space
            (slatex::of line slatex::=space / i)
            slatex::*out*)
          (loop (+ i 1)))
         ((char=? c #\')
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (if (or slatex::*in-qtd-tkn*
                  (> slatex::*in-bktd-qtd-exp* 0)
                  (and (pair? slatex::*bq-stack*)
                       (not
                        (slatex::of
                          (car slatex::*bq-stack*)
                          slatex::=in-comma))))
            #f
            (set! slatex::*in-qtd-tkn* #t))
          (loop (+ i 1)))
         ((char=? c #\`)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (if (or (null? slatex::*bq-stack*)
                  (slatex::of (car slatex::*bq-stack*) slatex::=in-comma))
            (set! slatex::*bq-stack*
              (cons
               (let ((f (slatex::make-bq-frame)))
                 (slatex::setf (slatex::of f slatex::=in-comma) #f)
                 (slatex::setf (slatex::of f slatex::=in-bq-tkn) #t)
                 (slatex::setf (slatex::of f slatex::=in-bktd-bq-exp) 0)
                 f)
               slatex::*bq-stack*)))
          (loop (+ i 1)))
         ((char=? c #\,)
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (if (not
               (or (null? slatex::*bq-stack*)
                   (slatex::of (car slatex::*bq-stack*) slatex::=in-comma)))
            (set! slatex::*bq-stack*
              (cons
               (let ((f (slatex::make-bq-frame)))
                 (slatex::setf (slatex::of f slatex::=in-comma) #t)
                 (slatex::setf (slatex::of f slatex::=in-bq-tkn) #t)
                 (slatex::setf (slatex::of f slatex::=in-bktd-bq-exp) 0)
                 f)
               slatex::*bq-stack*)))
          (if (char=? (slatex::of line slatex::=char / (+ i 1)) #\@)
            (begin (slatex::display-tex-char #\@ slatex::*out*) (loop (+ 2 i)))
            (loop (+ i 1))))
         ((memv c '(#\( #\[))
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (cond
           (slatex::*in-qtd-tkn*
            (set! slatex::*in-qtd-tkn* #f)
            (set! slatex::*in-bktd-qtd-exp* 1))
           ((> slatex::*in-bktd-qtd-exp* 0)
            (set! slatex::*in-bktd-qtd-exp* (+ slatex::*in-bktd-qtd-exp* 1))))
          (cond
           (slatex::*in-mac-tkn*
            (set! slatex::*in-mac-tkn* #f)
            (set! slatex::*in-bktd-mac-exp* 1))
           ((> slatex::*in-bktd-mac-exp* 0)
            (set! slatex::*in-bktd-mac-exp* (+ slatex::*in-bktd-mac-exp* 1))))
          (if (not (null? slatex::*bq-stack*))
            (let ((top (car slatex::*bq-stack*)))
              (cond
               ((slatex::of top slatex::=in-bq-tkn)
                (slatex::setf (slatex::of top slatex::=in-bq-tkn) #f)
                (slatex::setf (slatex::of top slatex::=in-bktd-bq-exp) 1))
               ((> (slatex::of top slatex::=in-bktd-bq-exp) 0)
                (slatex::setf
                  (slatex::of top slatex::=in-bktd-bq-exp)
                  (+ (slatex::of top slatex::=in-bktd-bq-exp) 1))))))
          (if (not (null? slatex::*case-stack*))
            (let ((top (car slatex::*case-stack*)))
              (cond
               ((slatex::of top slatex::=in-ctag-tkn)
                (slatex::setf (slatex::of top slatex::=in-ctag-tkn) #f)
                (slatex::setf (slatex::of top slatex::=in-bktd-ctag-exp) 1))
               ((> (slatex::of top slatex::=in-bktd-ctag-exp) 0)
                (slatex::setf
                  (slatex::of top slatex::=in-bktd-ctag-exp)
                  (+ (slatex::of top slatex::=in-bktd-ctag-exp) 1)))
               ((> (slatex::of top slatex::=in-case-exp) 0)
                (slatex::setf
                  (slatex::of top slatex::=in-case-exp)
                  (+ (slatex::of top slatex::=in-case-exp) 1))
                (if (= (slatex::of top slatex::=in-case-exp) 2)
                  (set! slatex::*in-qtd-tkn* #t))))))
          (loop (+ i 1)))
         ((memv c '(#\) #\]))
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (write-char c slatex::*out*)
          (if (> slatex::*in-bktd-qtd-exp* 0)
            (set! slatex::*in-bktd-qtd-exp* (- slatex::*in-bktd-qtd-exp* 1)))
          (if (> slatex::*in-bktd-mac-exp* 0)
            (set! slatex::*in-bktd-mac-exp* (- slatex::*in-bktd-mac-exp* 1)))
          (if (not (null? slatex::*bq-stack*))
            (let ((top (car slatex::*bq-stack*)))
              (if (> (slatex::of top slatex::=in-bktd-bq-exp) 0)
                (begin
                  (slatex::setf
                    (slatex::of top slatex::=in-bktd-bq-exp)
                    (- (slatex::of top slatex::=in-bktd-bq-exp) 1))
                  (if (= (slatex::of top slatex::=in-bktd-bq-exp) 0)
                    (set! slatex::*bq-stack* (cdr slatex::*bq-stack*)))))))
          (let loop ()
            (if (not (null? slatex::*case-stack*))
              (let ((top (car slatex::*case-stack*)))
                (cond
                 ((> (slatex::of top slatex::=in-bktd-ctag-exp) 0)
                  (slatex::setf
                    (slatex::of top slatex::=in-bktd-ctag-exp)
                    (- (slatex::of top slatex::=in-bktd-ctag-exp) 1))
                  (if (= (slatex::of top slatex::=in-bktd-ctag-exp) 0)
                    (slatex::setf (slatex::of top slatex::=in-case-exp) 1)))
                 ((> (slatex::of top slatex::=in-case-exp) 0)
                  (slatex::setf
                    (slatex::of top slatex::=in-case-exp)
                    (- (slatex::of top slatex::=in-case-exp) 1))
                  (if (= (slatex::of top slatex::=in-case-exp) 0)
                    (begin
                      (set! slatex::*case-stack* (cdr slatex::*case-stack*))
                      (loop))))))))
          (loop (+ i 1)))
         (else
          (slatex::display-tab
            (slatex::of line slatex::=tab / i)
            slatex::*out*)
          (loop (slatex::do-token line i))))))))

(define slatex::do-all-lines
  (lambda ()
    (let loop ((line1 slatex::*line1*) (line2 slatex::*line2*))
      (let* ((line2-paragraph? slatex::*latex-paragraph-mode?*)
             (more? (slatex::get-line line1)))
        (slatex::peephole-adjust line1 line2)
        ((if line2-paragraph?
           slatex::display-tex-line
           slatex::display-scm-line)
         line2)
        (if (not (eq? line2-paragraph? slatex::*latex-paragraph-mode?*))
          ((if slatex::*latex-paragraph-mode?*
             slatex::display-end-sequence
             slatex::display-begin-sequence)
           slatex::*out*))
        (if more? (loop line2 line1))))))

(define slatex::scheme2tex
  (lambda (inport outport)
    (set! slatex::*in* inport)
    (set! slatex::*out* outport)
    (set! slatex::*latex-paragraph-mode?* #t)
    (set! slatex::*in-qtd-tkn* #f)
    (set! slatex::*in-bktd-qtd-exp* 0)
    (set! slatex::*in-mac-tkn* #f)
    (set! slatex::*in-bktd-mac-exp* 0)
    (set! slatex::*case-stack* '())
    (set! slatex::*bq-stack* '())
    (let ((flush-line
            (lambda (line)
              (slatex::setf (slatex::of line slatex::=rtedge) 0)
              (slatex::setf (slatex::of line slatex::=char / 0) #\newline)
              (slatex::setf (slatex::of line slatex::=space / 0) &void-space)
              (slatex::setf (slatex::of line slatex::=tab / 0) &void-tab)
              (slatex::setf
                (slatex::of line slatex::=notab / 0)
                &void-notab))))
      (flush-line slatex::*line1*)
      (flush-line slatex::*line2*))
    (slatex::do-all-lines)))

(define slatex::do-token
  (let ((token-delims
          (list
           #\(
           #\)
           #\[
           #\]
           #\space
           slatex::*return*
           #\"
           #\'
           #\`
           #\newline
           #\,
           #\;)))
    (lambda (line i)
      (let loop ((buf '()) (i i))
        (let ((c (slatex::of line slatex::=char / i)))
          (cond
           ((char=? c #\\)
            (loop
             (cons (slatex::of line slatex::=char / (+ i 1)) (cons c buf))
             (+ i 2)))
           ((or (memv c token-delims) (memv c slatex::*math-triggerers*))
            (slatex::output-token (list->string (reverse! buf)))
            i)
           ((char? c)
            (loop (cons (slatex::of line slatex::=char / i) buf) (+ i 1)))
           (else
            (slatex::slatex-error
              'slatex::do-token
              "token contains non-char?"
              c))))))))

(define slatex::output-token
  (lambda (token)
    (if (not (null? slatex::*case-stack*))
      (let ((top (car slatex::*case-stack*)))
        (if (slatex::of top slatex::=in-ctag-tkn)
          (begin
            (slatex::setf (slatex::of top slatex::=in-ctag-tkn) #f)
            (slatex::setf (slatex::of top slatex::=in-case-exp) 1)))))
    (if (slatex::lassoc token slatex::special-symbols slatex::token=?)
      (begin
        (if slatex::*in-qtd-tkn*
          (set! slatex::*in-qtd-tkn* #f)
          (if slatex::*in-mac-tkn* (set! slatex::*in-mac-tkn* #f)))
        (display
          (cdr (slatex::lassoc token slatex::special-symbols slatex::token=?))
          slatex::*out*))
      (slatex::display-token
        token
        (cond
         (slatex::*in-qtd-tkn*
          (set! slatex::*in-qtd-tkn* #f)
          (cond
           ((equal? token "else") 'syntax)
           ((slatex::lmember token slatex::data-tokens slatex::token=?) 'data)
           ((slatex::lmember token slatex::constant-tokens slatex::token=?)
            'constant)
           ((slatex::lmember token slatex::variable-tokens slatex::token=?)
            'constant)
           ((slatex::lmember token slatex::keyword-tokens slatex::token=?)
            'constant)
           ((slatex::prim-data-token? token) 'data)
           (else 'constant)))
         ((> slatex::*in-bktd-qtd-exp* 0) 'constant)
         ((and (not (null? slatex::*bq-stack*))
               (not (slatex::of (car slatex::*bq-stack*) slatex::=in-comma)))
          'constant)
         (slatex::*in-mac-tkn*
          (set! slatex::*in-mac-tkn* #f)
          (slatex::set-keyword token)
          'syntax)
         ((> slatex::*in-bktd-mac-exp* 0) (slatex::set-keyword token) 'syntax)
         ((slatex::lmember token slatex::data-tokens slatex::token=?) 'data)
         ((slatex::lmember token slatex::constant-tokens slatex::token=?)
          'constant)
         ((slatex::lmember token slatex::variable-tokens slatex::token=?)
          'variable)
         ((slatex::lmember token slatex::keyword-tokens slatex::token=?)
          (cond
           ((slatex::token=? token "quote") (set! slatex::*in-qtd-tkn* #t))
           ((slatex::lmember token slatex::macro-definers slatex::token=?)
            (set! slatex::*in-mac-tkn* #t))
           ((slatex::lmember token slatex::case-and-ilk slatex::token=?)
            (set! slatex::*case-stack*
              (cons
               (let ((f (slatex::make-case-frame)))
                 (slatex::setf (slatex::of f slatex::=in-ctag-tkn) #t)
                 (slatex::setf (slatex::of f slatex::=in-bktd-ctag-exp) 0)
                 (slatex::setf (slatex::of f slatex::=in-case-exp) 0)
                 f)
               slatex::*case-stack*))))
          'syntax)
         ((slatex::prim-data-token? token) 'data)
         (else 'variable))
        slatex::*out*))
    (if (and (not (null? slatex::*bq-stack*))
             (slatex::of (car slatex::*bq-stack*) slatex::=in-bq-tkn))
      (set! slatex::*bq-stack* (cdr slatex::*bq-stack*)))))

(define slatex::directory-namestring
  (lambda (f)
    (let ((p (slatex::string-position-right slatex::*directory-mark* f)))
      (if p (substring f 0 (+ p 1)) ""))))

(define slatex::basename
  (lambda (f)
    (let ((p (slatex::string-position-right slatex::*directory-mark* f)))
      (if p (set! f (substring f (+ p 1) (string-length f))))
      (let ((p (slatex::string-position-right #\. f)))
        (if p (substring f 0 p) f)))))

(define slatex::*texinputs* "")

(define slatex::*texinputs-list* #f)

(define slatex::*path-separator*
  (cond
   ((eq? slatex::*operating-system* 'unix) #\:)
   ((eq? slatex::*operating-system* 'mac-os) (integer->char 0))
   ((memq slatex::*operating-system* '(windows os2 dos os2fat)) #\;)
   (else
    (slatex::slatex-error "Couldn't determine path separator character."))))

(define slatex::*directory-mark*
  (cond
   ((eq? slatex::*operating-system* 'unix) #\/)
   ((eq? slatex::*operating-system* 'mac-os) #\:)
   ((memq slatex::*operating-system* '(windows os2 dos os2fat)) #\\)
   (else (slatex::slatex-error "Couldn't determine directory mark."))))

(define slatex::*directory-mark-string*
  (list->string (list slatex::*directory-mark*)))

(define slatex::*file-hider*
  (cond
   ((memq slatex::*operating-system* '(windows os2 unix mac-os)) ".")
   ((memq slatex::*operating-system* '(dos os2fat)) "x")
   (else ".")))

(define slatex::path-to-list
  (lambda (p)
    (let loop ((p (string->list p)) (r (list "")))
      (let ((separator-pos (slatex::position-char slatex::*path-separator* p)))
        (if separator-pos
          (loop
           (list-tail p (+ separator-pos 1))
           (cons (list->string (slatex::sublist p 0 separator-pos)) r))
          (reverse! (cons (list->string p) r)))))))

(define slatex::find-some-file
  (lambda (path . files)
    (let loop ((path path))
      (if (null? path)
        #f
        (let ((dir (car path)))
          (let loop1 ((files
                       (if (or (string=? dir "") (string=? dir "."))
                         files
                         (map
                          (lambda (file)
                            (string-append
                              dir
                              slatex::*directory-mark-string*
                              file))
                          files))))
            (if (null? files)
              (loop (cdr path))
              (let ((file (car files)))
                (if (file-exists? file) file (loop1 (cdr files)))))))))))

(define slatex::file-extension
  (lambda (filename)
    (let ((i (slatex::string-position-right #\. filename)))
      (if i (substring filename i (string-length filename)) #f))))

(define slatex::full-texfile-name
  (lambda (filename)
    (let ((extn (slatex::file-extension filename)))
      (if (and extn (or (string=? extn ".sty") (string=? extn ".tex")))
        (slatex::find-some-file slatex::*texinputs-list* filename)
        (slatex::find-some-file
          slatex::*texinputs-list*
          (string-append filename ".tex")
          filename)))))

(define slatex::full-styfile-name
  (lambda (filename)
    (slatex::find-some-file
      slatex::*texinputs-list*
      (string-append filename ".sty"))))

(define slatex::full-clsfile-name
  (lambda (filename)
    (slatex::find-some-file
      slatex::*texinputs-list*
      (string-append filename ".cls"))))

(define slatex::full-scmfile-name
  (lambda (filename)
    (apply
     slatex::find-some-file
     slatex::*texinputs-list*
     filename
     (map
      (lambda (extn) (string-append filename extn))
      '(".scm" ".ss" ".s")))))

(define slatex::subjobname 'fwd)

(define slatex::primary-aux-file-count -1)

(define slatex::new-primary-aux-file
  (lambda (e)
    (set! slatex::primary-aux-file-count (+ slatex::primary-aux-file-count 1))
    (string-append
      slatex::*tex-calling-directory*
      slatex::*file-hider*
      "Z"
      (number->string slatex::primary-aux-file-count)
      slatex::subjobname
      e)))

(define slatex::new-secondary-aux-file
  (let ((n -1))
    (lambda (e)
      (set! n (+ n 1))
      (string-append
        slatex::*tex-calling-directory*
        slatex::*file-hider*
        "ZZ"
        (number->string n)
        slatex::subjobname
        e))))

(define slatex::new-aux-file
  (lambda e
    (let ((e (if (pair? e) (car e) "")))
      ((if slatex::*slatex-in-protected-region?*
         slatex::new-secondary-aux-file
         slatex::new-primary-aux-file)
       e))))

(define slatex::eat-till-newline
  (lambda (in)
    (let loop ()
      (let ((c (read-char in)))
        (cond
         ((eof-object? c) 'done)
         ((char=? c #\newline) 'done)
         (else (loop)))))))

(define slatex::read-ctrl-seq
  (lambda (in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex::slatex-error "read-ctrl-exp: \\ followed by eof."))
      (if (char-alphabetic? c)
        (list->string
          (reverse!
            (let loop ((s (list c)))
              (let ((c (peek-char in)))
                (cond
                 ((eof-object? c) s)
                 ((char-alphabetic? c) (read-char in) (loop (cons c s)))
                 ((char=? c #\%) (slatex::eat-till-newline in) (loop s))
                 (else s))))))
        (string c)))))

(define slatex::eat-tabspace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond
         ((eof-object? c) 'done)
         ((or (char=? c #\space) (char=? c slatex::*tab*))
          (read-char in)
          (loop))
         (else 'done))))))

(define slatex::eat-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond
         ((eof-object? c) 'done)
         ((char-whitespace? c) (read-char in) (loop))
         (else 'done))))))

(define slatex::eat-tex-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond
         ((eof-object? c) 'done)
         ((char-whitespace? c) (read-char in) (loop))
         ((char=? c #\%) (slatex::eat-till-newline in))
         (else 'done))))))

(define slatex::chop-off-whitespace
  (lambda (l)
    (slatex::ormapcdr (lambda (d) (if (char-whitespace? (car d)) #f d)) l)))

(define slatex::read-grouped-latexexp
  (lambda (in)
    (slatex::eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex::slatex-error
          "read-grouped-latexexp: ~\nExpected { but found eof."))
      (if (not (char=? c #\{))
        (slatex::slatex-error
          "read-grouped-latexexp: ~\nExpected { but found ~a."
          c))
      (slatex::eat-tex-whitespace in)
      (list->string
        (reverse!
          (slatex::chop-off-whitespace
            (let loop ((s '()) (nesting 0) (escape? #f))
              (let ((c (read-char in)))
                (if (eof-object? c)
                  (slatex::slatex-error
                    "read-groupted-latexexp: ~\nFound eof inside {...}."))
                (cond
                 (escape? (loop (cons c s) nesting #f))
                 ((char=? c #\\) (loop (cons c s) nesting #t))
                 ((char=? c #\%)
                  (slatex::eat-till-newline in)
                  (loop s nesting #f))
                 ((char=? c #\{) (loop (cons c s) (+ nesting 1) #f))
                 ((char=? c #\})
                  (if (= nesting 0) s (loop (cons c s) (- nesting 1) #f)))
                 (else (loop (cons c s) nesting #f)))))))))))

(define slatex::read-filename
  (let ((filename-delims
          (list
           #\{
           #\}
           #\[
           #\]
           #\(
           #\)
           #\#
           #\%
           #\\
           #\,
           #\space
           slatex::*return*
           #\newline
           slatex::*tab*
           #\\)))
    (lambda (in)
      (slatex::eat-tex-whitespace in)
      (let ((c (peek-char in)))
        (if (eof-object? c)
          (slatex::slatex-error
            "read-filename: ~\nExpected filename but found eof."))
        (if (char=? c #\{)
          (slatex::read-grouped-latexexp in)
          (list->string
            (reverse!
              (let loop ((s '()) (escape? #f))
                (let ((c (peek-char in)))
                  (cond
                   ((eof-object? c)
                    (if escape?
                      (slatex::slatex-error
                        "read-filename: ~\n\\ followed by eof.")
                      s))
                   (escape? (read-char in) (loop (cons c s) #f))
                   ((char=? c #\\) (read-char in) (loop (cons c s) #t))
                   ((memv c filename-delims) s)
                   (else (read-char in) (loop (cons c s) #f))))))))))))

(define slatex::read-schemeid
  (let ((schemeid-delims
          (list
           #\{
           #\}
           #\[
           #\]
           #\(
           #\)
           #\space
           slatex::*return*
           #\newline
           slatex::*tab*)))
    (lambda (in)
      (slatex::eat-whitespace in)
      (list->string
        (reverse!
          (let loop ((s '()) (escape? #f))
            (let ((c (peek-char in)))
              (cond
               ((eof-object? c) s)
               (escape? (read-char in) (loop (cons c s) #f))
               ((char=? c #\\) (read-char in) (loop (cons c s) #t))
               ((memv c schemeid-delims) s)
               (else (read-char in) (loop (cons c s) #f))))))))))

(define slatex::read-delimed-commaed-filenames
  (lambda (in lft-delim rt-delim)
    (slatex::eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex::slatex-error
          "read-delimed-commaed-filenames: ~\nExpected filename(s) but found eof."))
      (if (not (char=? c lft-delim))
        (slatex::slatex-error
          "read-delimed-commaed-filenames: ~\nLeft delimiter ~a not found."
          lft-delim))
      (let loop ((s '()))
        (slatex::eat-tex-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c)
            (slatex::slatex-error
              "read-delimed-commaed-filenames: ~\nFound eof inside filename(s)."))
          (if (char=? c rt-delim)
            (begin (read-char in) (reverse! s))
            (let ((s (cons (slatex::read-filename in) s)))
              (slatex::eat-tex-whitespace in)
              (let ((c (peek-char in)))
                (if (eof-object? c)
                  (slatex::slatex-error
                    "read-delimed-commaed-filenames: ~\nFound eof inside filename(s)."))
                (cond
                 ((char=? c #\,) (read-char in))
                 ((char=? c rt-delim) (void))
                 (else
                  (slatex::slatex-error
                    "read-delimed-commaed-filenames: ~\nBad filename(s) syntax.")))
                (loop s)))))))))

(define slatex::read-grouped-commaed-filenames
  (lambda (in) (slatex::read-delimed-commaed-filenames in #\{ #\})))

(define slatex::read-bktd-commaed-filenames
  (lambda (in) (slatex::read-delimed-commaed-filenames in #\[ #\])))

(define slatex::read-grouped-schemeids
  (lambda (in)
    (slatex::eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex::slatex-error
          "read-grouped-schemeids: ~\nExpected Scheme identifiers but found eof."))
      (if (not (char=? c #\{))
        (slatex::slatex-error
          "read-grouped-schemeids: ~\nExpected { but found ~a."
          c))
      (let loop ((s '()))
        (slatex::eat-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c)
            (slatex::slatex-error
              "read-grouped-schemeids:\nFound eof inside Scheme identifiers."))
          (if (char=? c #\})
            (begin (read-char in) (reverse! s))
            (loop (cons (slatex::read-schemeid in) s))))))))

(define slatex::eat-delimed-text
  (lambda (in lft-delim rt-delim)
    (slatex::eat-tex-whitespace in)
    (let ((c (peek-char in)))
      (if (eof-object? c)
        'exit
        (if (char=? c lft-delim)
          (let loop ()
            (let ((c (read-char in)))
              (if (eof-object? c)
                'exit
                (if (char=? c rt-delim) 'exit (loop))))))))))

(define slatex::eat-bktd-text
  (lambda (in) (slatex::eat-delimed-text in #\[ #\])))

(define slatex::eat-grouped-text
  (lambda (in) (slatex::eat-delimed-text in #\{ #\})))

(define slatex::ignore2 (lambda (i ii) 'void))

(define slatex::disable-slatex-temply
  (lambda (in)
    (set! slatex::*slatex-enabled?* #f)
    (set! slatex::*slatex-reenabler* (slatex::read-grouped-latexexp in))))

(define slatex::enable-slatex-again
  (lambda ()
    (set! slatex::*slatex-enabled?* #t)
    (set! slatex::*slatex-reenabler* "UNDEFINED")))

(define slatex::add-to-slatex-db
  (lambda (in categ)
    (if (memq categ '(keyword constant variable))
      (slatex::add-to-slatex-db-basic in categ)
      (slatex::add-to-slatex-db-special in categ))))

(define slatex::add-to-slatex-db-basic
  (lambda (in categ)
    (let ((setter
            (cond
             ((eq? categ 'keyword) slatex::set-keyword)
             ((eq? categ 'constant) slatex::set-constant)
             ((eq? categ 'variable) slatex::set-variable)
             (else
              (slatex::slatex-error
                "add-to-slatex-db-basic: ~\nUnknown category ~s."
                categ))))
          (ids (slatex::read-grouped-schemeids in)))
      (for-each setter ids))))

(define slatex::add-to-slatex-db-special
  (lambda (in what)
    (let ((ids (slatex::read-grouped-schemeids in)))
      (cond
       ((eq? what 'unsetspecialsymbol)
        (for-each slatex::unset-special-symbol ids))
       ((eq? what 'setspecialsymbol)
        (if (not (= (length ids) 1))
          (slatex::slatex-error
            "add-to-slatex-db-special: ~\n\\setspecialsymbol takes one arg exactly."))
        (let ((transl (slatex::read-grouped-latexexp in)))
          (slatex::set-special-symbol (car ids) transl)))
       (else
        (slatex::slatex-error
          "add-to-slatex-db-special: ~\nUnknown command ~s."
          what))))))

(define slatex::process-slatex-alias
  (lambda (in what which)
    (let ((triggerer (slatex::read-grouped-latexexp in)))
      (case which
        ((intext)
         (set! slatex::*intext-triggerers*
           (what triggerer slatex::*intext-triggerers* string=?)))
        ((resultintext)
         (set! slatex::*resultintext-triggerers*
           (what triggerer slatex::*resultintext-triggerers* string=?)))
        ((display)
         (set! slatex::*display-triggerers*
           (what triggerer slatex::*display-triggerers* string=?)))
        ((response)
         (set! slatex::*response-triggerers*
           (what triggerer slatex::*response-triggerers* string=?)))
        ((respbox)
         (set! slatex::*respbox-triggerers*
           (what triggerer slatex::*respbox-triggerers* string=?)))
        ((box)
         (set! slatex::*box-triggerers*
           (what triggerer slatex::*box-triggerers* string=?)))
        ((input)
         (set! slatex::*input-triggerers*
           (what triggerer slatex::*input-triggerers* string=?)))
        ((region)
         (set! slatex::*region-triggerers*
           (what triggerer slatex::*region-triggerers* string=?)))
        ((mathescape)
         (if (not (= (string-length triggerer) 1))
           (slatex::slatex-error
             "process-slatex-alias: ~\nMath escape should be character."))
         (set! slatex::*math-triggerers*
           (what (string-ref triggerer 0) slatex::*math-triggerers* char=?)))
        (else
         (slatex::slatex-error
           "process-slatex-alias:\nUnknown command ~s."
           which))))))

(define slatex::decide-latex-or-tex
  (lambda (latex?)
    (set! slatex::*latex?* latex?)
    (let ((pltexchk.jnk "pltexchk.jnk"))
      (if (file-exists? pltexchk.jnk) (delete-file pltexchk.jnk))
      (if (not slatex::*latex?*)
        (call-with-output-file
          pltexchk.jnk
          (lambda (outp) (display 'junk outp) (newline outp)))))))

(define slatex::process-include-only
  (lambda (in)
    (set! slatex::*include-onlys* '())
    (for-each
      (lambda (filename)
        (let ((filename (slatex::full-texfile-name filename)))
          (if filename
            (set! slatex::*include-onlys*
              (slatex::adjoin filename slatex::*include-onlys* string=?)))))
      (slatex::read-grouped-commaed-filenames in))))

(define slatex::process-documentstyle
  (lambda (in)
    (slatex::eat-tex-whitespace in)
    (if (char=? (peek-char in) #\[)
      (for-each
        (lambda (filename)
          (fluid-let
            ((slatex::*slatex-in-protected-region?* #f))
            (slatex::process-tex-file (string-append filename ".sty"))))
        (slatex::read-bktd-commaed-filenames in)))))

(define slatex::process-documentclass
  (lambda (in) (slatex::eat-bktd-text in) (slatex::eat-grouped-text in)))

(define slatex::process-case-info
  (lambda (in)
    (let ((bool (slatex::read-grouped-latexexp in)))
      (set! slatex::*slatex-case-sensitive?*
        (cond
         ((string-ci=? bool "true") #t)
         ((string-ci=? bool "false") #f)
         (else
          (slatex::slatex-error
            "process-case-info: ~\n\\schemecasesensitive's arg should be true or false.")))))))

(define slatex::seen-first-command? #f)

(define slatex::process-main-tex-file
  (lambda (filename)
    (display "SLaTeX v. ")
    (display slatex::*slatex-version*)
    (newline)
    (set! slatex::primary-aux-file-count -1)
    (set! slatex::*slatex-separate-includes?* #f)
    (if (or (not slatex::*texinputs-list*) (null? slatex::*texinputs-list*))
      (set! slatex::*texinputs-list*
        (if slatex::*texinputs*
          (slatex::path-to-list slatex::*texinputs*)
          '(""))))
    (let ((file-hide-file "xZfilhid.tex"))
      (if (file-exists? file-hide-file) (delete-file file-hide-file))
      (if (memq slatex::*operating-system* '(dos os2fat))
        (call-with-output-file
          file-hide-file
          (lambda (out) (display "\\def\\filehider{x}" out) (newline out)))))
    (display "typesetting code")
    (set! slatex::*tex-calling-directory*
      (slatex::directory-namestring filename))
    (set! slatex::subjobname (slatex::basename filename))
    (set! slatex::seen-first-command? #f)
    (slatex::process-tex-file filename)
    (display "done")
    (newline)))

(define slatex::dump-intext
  (lambda (in out)
    (let* ((write-char (if out write-char slatex::ignore2))
           (delim-char (begin (slatex::eat-whitespace in) (read-char in)))
           (delim-char (cond ((char=? delim-char #\{) #\}) (else delim-char))))
      (if (eof-object? delim-char)
        (slatex::slatex-error
          "dump-intext: Expected delimiting character ~\nbut found eof."))
      (let loop ()
        (let ((c (read-char in)))
          (if (eof-object? c)
            (slatex::slatex-error
              "dump-intext: Found eof inside Scheme code."))
          (if (char=? c delim-char)
            'done
            (begin (write-char c out) (loop))))))))

(define slatex::dump-display
  (lambda (in out ender)
    (slatex::eat-tabspace in)
    (let ((write-char (if out write-char slatex::ignore2))
          (ender-lh (string-length ender))
          (c (peek-char in)))
      (if (eof-object? c)
        (slatex::slatex-error
          "dump-display: Found eof inside displayed code."))
      (if (char=? c #\newline) (read-char in))
      (let loop ((i 0))
        (if (= i ender-lh)
          'done
          (let ((c (read-char in)))
            (if (eof-object? c)
              (slatex::slatex-error
                "dump-display: Found eof inside displayed code."))
            (if (char=? c (string-ref ender i))
              (loop (+ i 1))
              (let loop2 ((j 0))
                (if (< j i)
                  (begin (write-char (string-ref ender j) out) (loop2 (+ j 1)))
                  (begin (write-char c out) (loop 0)))))))))))

(define slatex::debug? #f)

(define slatex::process-tex-file
  (lambda (raw-filename)
    (if slatex::debug?
      (begin (display "begin ") (display raw-filename) (newline)))
    (let ((filename (slatex::full-texfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "[")
          (display raw-filename)
          (display "]")
          (flush-output))
        (call-with-input-file
          filename
          (lambda (in)
            (let ((done? #f))
              (let loop ()
                (if done?
                  'exit-loop
                  (begin
                    (let ((c (read-char in)))
                      (cond
                       ((eof-object? c) (set! done? #t))
                       ((char=? c #\%) (slatex::eat-till-newline in))
                       ((char=? c #\\)
                        (let ((cs (slatex::read-ctrl-seq in)))
                          (if (not slatex::seen-first-command?)
                            (begin
                              (set! slatex::seen-first-command? #t)
                              (slatex::decide-latex-or-tex
                                (or (string=? cs "documentstyle")
                                    (string=? cs "documentclass")
                                    (string=? cs "NeedsTeXFormat")))))
                          (cond
                           ((not slatex::*slatex-enabled?*)
                            (if (string=? cs slatex::*slatex-reenabler*)
                              (slatex::enable-slatex-again)))
                           ((string=? cs "slatexignorecurrentfile")
                            (set! done? #t))
                           ((string=? cs "slatexseparateincludes")
                            (if slatex::*latex?*
                              (set! slatex::*slatex-separate-includes?* #t)))
                           ((string=? cs "slatexdisable")
                            (slatex::disable-slatex-temply in))
                           ((string=? cs "begin")
                            (slatex::eat-tex-whitespace in)
                            (if (eqv? (peek-char in) #\{)
                              (let ((cs (slatex::read-grouped-latexexp in)))
                                (cond
                                 ((member cs slatex::*display-triggerers*)
                                  (slatex::trigger-scheme2tex
                                    'envdisplay
                                    in
                                    cs))
                                 ((member cs slatex::*response-triggerers*)
                                  (slatex::trigger-scheme2tex
                                    'envresponse
                                    in
                                    cs))
                                 ((member cs slatex::*respbox-triggerers*)
                                  (slatex::trigger-scheme2tex
                                    'envrespbox
                                    in
                                    cs))
                                 ((member cs slatex::*box-triggerers*)
                                  (slatex::trigger-scheme2tex 'envbox in cs))
                                 ((member cs slatex::*topbox-triggerers*)
                                  (slatex::trigger-scheme2tex
                                    'envtopbox
                                    in
                                    cs))
                                 ((member cs slatex::*region-triggerers*)
                                  (slatex::trigger-region
                                    'envregion
                                    in
                                    cs))))))
                           ((member cs slatex::*intext-triggerers*)
                            (slatex::trigger-scheme2tex 'intext in #f))
                           ((member cs slatex::*resultintext-triggerers*)
                            (slatex::trigger-scheme2tex 'resultintext in #f))
                           ((member cs slatex::*display-triggerers*)
                            (slatex::trigger-scheme2tex 'plaindisplay in cs))
                           ((member cs slatex::*response-triggerers*)
                            (slatex::trigger-scheme2tex 'plainresponse in cs))
                           ((member cs slatex::*respbox-triggerers*)
                            (slatex::trigger-scheme2tex 'plainrespbox in cs))
                           ((member cs slatex::*box-triggerers*)
                            (slatex::trigger-scheme2tex 'plainbox in cs))
                           ((member cs slatex::*topbox-triggerers*)
                            (slatex::trigger-scheme2tex 'plaintopbox in cs))
                           ((member cs slatex::*region-triggerers*)
                            (slatex::trigger-region 'plainregion in cs))
                           ((member cs slatex::*input-triggerers*)
                            (slatex::process-scheme-file
                              (slatex::read-filename in)))
                           ((string=? cs "input")
                            (let ((f (slatex::read-filename in)))
                              (if (not (string=? f ""))
                                (fluid-let
                                  ((slatex::*slatex-in-protected-region?* #f))
                                  (slatex::process-tex-file f)))))
                           ((string=? cs "usepackage")
                            (fluid-let
                              ((slatex::*slatex-in-protected-region?* #f))
                              (slatex::process-tex-file
                                (string-append
                                  (slatex::read-filename in)
                                  ".sty"))))
                           ((string=? cs "include")
                            (if slatex::*latex?*
                              (let ((f
                                     (slatex::full-texfile-name
                                       (slatex::read-filename in))))
                                (if (and f
                                         (or (eq? slatex::*include-onlys* 'all)
                                             (member
                                               f
                                               slatex::*include-onlys*)))
                                  (fluid-let
                                    ((slatex::*slatex-in-protected-region?*
                                       #f))
                                    (if slatex::*slatex-separate-includes?*
                                      (fluid-let
                                        ((slatex::subjobname
                                           (slatex::basename f))
                                         (slatex::primary-aux-file-count -1))
                                        (slatex::process-tex-file f))
                                      (slatex::process-tex-file f)))))))
                           ((string=? cs "includeonly")
                            (if slatex::*latex?*
                              (slatex::process-include-only in)))
                           ((string=? cs "documentstyle")
                            (if slatex::*latex?*
                              (slatex::process-documentstyle in)))
                           ((string=? cs "documentclass")
                            (if slatex::*latex?*
                              (slatex::process-documentclass in)))
                           ((string=? cs "schemecasesensitive")
                            (slatex::process-case-info in))
                           ((string=? cs "defschemetoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'intext))
                           ((string=? cs "undefschemetoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'intext))
                           ((string=? cs "defschemeresulttoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'resultintext))
                           ((string=? cs "undefschemeresulttoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'resultintext))
                           ((string=? cs "defschemeresponsetoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'response))
                           ((string=? cs "undefschemeresponsetoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'response))
                           ((string=? cs "defschemeresponseboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'respbox))
                           ((string=? cs "undefschemeresponseboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'respbox))
                           ((string=? cs "defschemedisplaytoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'display))
                           ((string=? cs "undefschemedisplaytoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'display))
                           ((string=? cs "defschemeboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'box))
                           ((string=? cs "undefschemeboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'box))
                           ((string=? cs "defschemetopboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'topbox))
                           ((string=? cs "undefschemetopboxtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'topbox))
                           ((string=? cs "defschemeinputtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'input))
                           ((string=? cs "undefschemeinputtoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'input))
                           ((string=? cs "defschemeregiontoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'region))
                           ((string=? cs "undefschemeregiontoken")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'region))
                           ((string=? cs "defschememathescape")
                            (slatex::process-slatex-alias
                              in
                              slatex::adjoin
                              'mathescape))
                           ((string=? cs "undefschememathescape")
                            (slatex::process-slatex-alias
                              in
                              slatex::delete
                              'mathescape))
                           ((string=? cs "setkeyword")
                            (slatex::add-to-slatex-db in 'keyword))
                           ((string=? cs "setconstant")
                            (slatex::add-to-slatex-db in 'constant))
                           ((string=? cs "setvariable")
                            (slatex::add-to-slatex-db in 'variable))
                           ((string=? cs "setspecialsymbol")
                            (slatex::add-to-slatex-db in 'setspecialsymbol))
                           ((string=? cs "unsetspecialsymbol")
                            (slatex::add-to-slatex-db
                              in
                              'unsetspecialsymbol)))))))
                    (loop)))))))))
    (if slatex::debug?
      (begin (display "end ") (display raw-filename) (newline)))))

(define slatex::process-scheme-file
  (lambda (raw-filename)
    (let ((filename (slatex::full-scmfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "process-scheme-file: ")
          (display raw-filename)
          (display " doesn't exist")
          (newline))
        (let ((aux.tex (slatex::new-aux-file ".tex")))
          (display ".")
          (flush-output)
          (if (file-exists? aux.tex) (delete-file aux.tex))
          (call-with-input-file
            filename
            (lambda (in)
              (call-with-output-file
                aux.tex
                (lambda (out)
                  (fluid-let
                    ((slatex::*intext?* #f)
                     (slatex::*code-env-spec* "ZZZZschemedisplay"))
                    (slatex::scheme2tex in out))))))
          (if slatex::*slatex-in-protected-region?*
            (set! slatex::*protected-files*
              (cons aux.tex slatex::*protected-files*)))
          (slatex::process-tex-file filename))))))

(define slatex::trigger-scheme2tex
  (lambda (typ in env)
    (let* ((aux (slatex::new-aux-file))
           (aux.scm (string-append aux ".scm"))
           (aux.tex (string-append aux ".tex")))
      (if (file-exists? aux.scm) (delete-file aux.scm))
      (if (file-exists? aux.tex) (delete-file aux.tex))
      (display ".")
      (flush-output)
      (call-with-output-file
        aux.scm
        (lambda (out)
          (cond
           ((memq typ '(intext resultintext)) (slatex::dump-intext in out))
           ((memq typ '(envdisplay envresponse envrespbox envbox envtopbox))
            (slatex::dump-display in out (string-append "\\end{" env "}")))
           ((memq
             typ
             '(plaindisplay plainresponse plainrespbox plainbox plaintopbox))
            (slatex::dump-display in out (string-append "\\end" env)))
           (else
            (slatex::slatex-error
              "trigger-scheme2tex: ~\n                          Unknown triggerer ~s."
              typ)))))
      (call-with-input-file
        aux.scm
        (lambda (in)
          (call-with-output-file
            aux.tex
            (lambda (out)
              (fluid-let
                ((slatex::*intext?* (memq typ '(intext resultintext)))
                 (slatex::*code-env-spec*
                   (cond
                    ((eq? typ 'intext) "ZZZZschemecodeintext")
                    ((eq? typ 'resultintext) "ZZZZschemeresultintext")
                    ((memq typ '(envdisplay plaindisplay)) "ZZZZschemedisplay")
                    ((memq typ '(envresponse plainresponse))
                     "ZZZZschemeresponse")
                    ((memq typ '(envrespbox plainrespbox))
                     "ZZZZschemeresponsebox")
                    ((memq typ '(envbox plainbox)) "ZZZZschemebox")
                    ((memq typ '(envtopbox plaintopbox)) "ZZZZschemetopbox")
                    (else
                     (slatex::slatex-error
                       "trigger-scheme2tex: ~\n                                   Unknown triggerer ~s."
                       typ)))))
                (slatex::scheme2tex in out))))))
      (if slatex::*slatex-in-protected-region?*
        (set! slatex::*protected-files*
          (cons aux.tex slatex::*protected-files*)))
      (if (memq
           typ
           '(envdisplay plaindisplay envbox plainbox envtopbox plaintopbox))
        (slatex::process-tex-file aux.tex))
      (delete-file aux.scm))))

(define slatex::trigger-region
  (lambda (typ in env)
    (let ((aux.tex (slatex::new-primary-aux-file ".tex"))
          (aux2.tex (slatex::new-secondary-aux-file ".tex")))
      (if (file-exists? aux2.tex) (delete-file aux2.tex))
      (if (file-exists? aux.tex) (delete-file aux.tex))
      (display ".")
      (flush-output)
      (fluid-let
        ((slatex::*slatex-in-protected-region?* #t)
         (slatex::*protected-files* '()))
        (call-with-output-file
          aux2.tex
          (lambda (out)
            (cond
             ((eq? typ 'envregion)
              (slatex::dump-display in out (string-append "\\end{" env "}")))
             ((eq? typ 'plainregion)
              (slatex::dump-display in out (string-append "\\end" env)))
             (else
              (slatex::slatex-error
                "trigger-region: ~\nUnknown triggerer ~s."
                typ)))))
        (slatex::process-tex-file aux2.tex)
        (set! slatex::*protected-files* (reverse! slatex::*protected-files*))
        (call-with-input-file
          aux2.tex
          (lambda (in)
            (call-with-output-file
              aux.tex
              (lambda (out) (slatex::inline-protected-files in out)))))
        (delete-file aux2.tex)))))

(define slatex::inline-protected-files
  (lambda (in out)
    (let ((done? #f))
      (let loop ()
        (if done?
          'exit-loop
          (begin
            (let ((c (read-char in)))
              (cond
               ((eof-object? c) (set! done? #t))
               ((or (char=? c slatex::*return*) (char=? c #\newline))
                (let ((c2 (peek-char in)))
                  (if (not (eof-object? c2)) (write-char c out))))
               ((char=? c #\%)
                (write-char c out)
                (newline out)
                (slatex::eat-till-newline in))
               ((char=? c #\\)
                (let ((cs (slatex::read-ctrl-seq in)))
                  (cond
                   ((string=? cs "begin")
                    (let ((cs (slatex::read-grouped-latexexp in)))
                      (cond
                       ((member cs slatex::*display-triggerers*)
                        (slatex::inline-protected 'envdisplay in out cs))
                       ((member cs slatex::*response-triggerers*)
                        (slatex::inline-protected 'envresponse in out cs))
                       ((member cs slatex::*respbox-triggerers*)
                        (slatex::inline-protected 'envrespbox in out cs))
                       ((member cs slatex::*box-triggerers*)
                        (slatex::inline-protected 'envbox in out cs))
                       ((member cs slatex::*topbox-triggerers*)
                        (slatex::inline-protected 'envtopbox in out cs))
                       ((member cs slatex::*region-triggerers*)
                        (slatex::inline-protected 'envregion in out cs))
                       (else
                        (display "\\begin{" out)
                        (display cs out)
                        (display "}" out)))))
                   ((member cs slatex::*intext-triggerers*)
                    (slatex::inline-protected 'intext in out #f))
                   ((member cs slatex::*resultintext-triggerers*)
                    (slatex::inline-protected 'resultintext in out #f))
                   ((member cs slatex::*display-triggerers*)
                    (slatex::inline-protected 'plaindisplay in out cs))
                   ((member cs slatex::*response-triggerers*)
                    (slatex::inline-protected 'plainresponse in out cs))
                   ((member cs slatex::*respbox-triggerers*)
                    (slatex::inline-protected 'plainrespbox in out cs))
                   ((member cs slatex::*box-triggerers*)
                    (slatex::inline-protected 'plainbox in out cs))
                   ((member cs slatex::*topbox-triggerers*)
                    (slatex::inline-protected 'plaintopbox in out cs))
                   ((member cs slatex::*region-triggerers*)
                    (slatex::inline-protected 'plainregion in out cs))
                   ((member cs slatex::*input-triggerers*)
                    (slatex::inline-protected 'input in out cs))
                   (else (display "\\" out) (display cs out)))))
               (else (write-char c out))))
            (loop)))))))

(define slatex::inline-protected
  (lambda (typ in out env)
    (cond
     ((eq? typ 'envregion)
      (display "\\begin{" out)
      (display env out)
      (display "}" out)
      (slatex::dump-display in out (string-append "\\end{" env "}"))
      (display "\\end{" out)
      (display env out)
      (display "}" out))
     ((eq? typ 'plainregion)
      (display "\\" out)
      (display env out)
      (slatex::dump-display in out (string-append "\\end" env))
      (display "\\end" out)
      (display env out))
     (else
      (let ((f (car slatex::*protected-files*)))
        (set! slatex::*protected-files* (cdr slatex::*protected-files*))
        (call-with-input-file
          f
          (lambda (in) (slatex::inline-protected-files in out)))
        (delete-file f))
      (cond
       ((memq typ '(intext resultintext))
        (display "{}" out)
        (slatex::dump-intext in #f))
       ((memq typ '(envrespbox envbox envtopbox))
        (if (not slatex::*latex?*) (display "{}" out))
        (slatex::dump-display in #f (string-append "\\end{" env "}")))
       ((memq typ '(plainrespbox plainbox plaintopbox))
        (display "{}" out)
        (slatex::dump-display in #f (string-append "\\end" env)))
       ((memq typ '(envdisplay envresponse))
        (slatex::dump-display in #f (string-append "\\end{" env "}")))
       ((memq typ '(plaindisplay plainresponse))
        (slatex::dump-display in #f (string-append "\\end" env)))
       ((eq? typ 'input) (slatex::read-filename in))
       (else
        (slatex::slatex-error
          "inline-protected: ~\nUnknown triggerer ~s."
          typ)))))))


)

