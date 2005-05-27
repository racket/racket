;; copyright by Paul Graunke June 2000 AD
(unit/sig dtd^
  (import dtd-ast^ entity-expander^ mzlib:function^ mzlib:string^)
  ;; Note: this library only supports a the subset of SGML used for HTML as descibed in
  ;; http://www.w3.org/TR/html401/intro/sgmltut.html
  
  ;; Spec = (listof (cons (listof Symbol) (listof Symbol)))
  
  ;; read-sgml-dtd : Input-port -> Dtd
  (define (read-sgml-dtd in)
    (parameterize ([read-case-sensitive #t])
      (let ([in (filter-comments in)])
        (let read-items ()
          (let skip-entity-refs ()
            (skip-whitespace in)
            (when (eq? (peek-char in) #\%)
              (skip-until (lambda (c) (eq? c #\;)) in)
              (skip-entity-refs)))
          (cond
            [(eof-object? (peek-char in)) null]
            [else
             (unless (and (eq? (read-char in) #\<)
                          (eq? (read-char in) #\!))
               (error 'read-sgml-dtd "unknown junk in dtd at ~a" (file-position in)))
             (case (peek-char in)
               [(#\>) (read-char in) (read-items)]
               [(#\[) (read-char in)
                (skip-whitespace in)
                (let ([uh (read-id in)])
                  (skip-until (lambda (c) (eq? c #\[)) in)
                  (cons (begin0 (make-thingy uh (read-until (lambda (c) (eq? c #\])) in))
                                (unless (and (eq? (read-char in) #\]) (eq? (read-char in) #\>))
                                  (error 'read-sgml-dtd "Invalid <![ uh [ whatever ]]> thingy.")))
                        (read-items)))]
               [else (cons (let ([name (read-id in)])
                             (case name
                               [(entity) (skip-until (lambda (c) (eq? c #\%)) in)
                                (skip-whitespace in)
                                (begin0 (make-entity-def (read-until char-whitespace? in); more here check case sensitivity ; (read-id in)
                                                         (begin (skip-until quote? in) (read-until quote? in)))
                                        (skip-until gt? in))]
                               [(element)
                                (make-element-def (read-names in)
                                                  (read-required/optional in)
                                                  (read-required/optional in)
                                                  (read-until gt? in))]
                               [(attlist)
                                (make-att-list (read-names in) (read-until gt? in))]
                               [else (error 'read-sgml-dtd "Unknown name: <!~s ...>" name)]))
                           (read-items))])])))))
  
  ;; quote? : Char -> Bool
  (define (quote? c) (eq? c #\"))
  ;; gt? : Char -> Bool
  (define (gt? c) (eq? c #\>))
  
  ;; read-names : Input-port -> (listof Symbol)
  (define (read-names in)
    (skip-whitespace in)
    (case (peek-char in)
      [(#\() (read-char in)
       (skip-whitespace in)
       (cons (read-id in)
             (let loop ()
               (skip-whitespace in)
               (case (read-char in)
                 [(#\| #\,) (skip-whitespace in) (cons (read-id in) (loop))]
                 [(#\)) null]               
                 [else (error 'read-names "unexpected character in element names '~a'" (read-line in))])))]
      [else (list (read-id in))]))
  
  ;; read-required/optional : Input-port -> Bool
  (define (read-required/optional in)
    (skip-whitespace in)
    (eq? (char-downcase (read-char in)) #\o))
  
  ;; read-id : Input-port -> Symbol
  (define (read-id in)
    (string->symbol
     (list->string
      (let loop ()
        (let ([c (peek-char in)])
          (if (and (not (eof-object? c))
                   (or (char-id? c)
                       ;; this is a hack - allowing % and ; characters accepts parameter entities.
                       (memq c '(#\% #\;))))
              (cons (char-downcase c) (begin (read-char in) (loop)))
              null))))))
  
  ;; char-id? : Char -> Bool
  ;; Note: the & is not legal, really, but SEC's Edgar format uses it. Yuck.
  ;; Nope. & messes up SGML dtds - remove it.
  (define (char-id? c)
    (or (char-alphabetic? c) (char-numeric? c) (memq c '(#\_ #\- #\: #\.))))
  
  ;; skip-whitespace : Input-port -> Void
  (define (skip-whitespace in) (skip-past char-whitespace? in))
  
  ;; skip-past : (Char -> Bool) Input-port -> Void
  (define (skip-past skip? in)
    (let loop ()
      (when (let ([c (peek-char in)])
              (and (not (eof-object? c)) (skip? c)))
        (read-char in)
        (loop))))
  
  ;; skip-until : (Char -> Bool) Input-port -> Void
  (define (skip-until delimiter? in)
    (let loop ()
      (unless (delimiter? (read-char in))
        (loop))))
  
  ;; read-until : (Char -> Bool) Input-port -> String
  ;; skips delimiter
  (define (read-until delimiter? in)
    (list->string
     (let loop ()
       (let ([c (read-char in)])
         (if (delimiter? c)
             null
             (cons c (loop)))))))
  
  ;; filter-comments : Input-port -> Input-port
  ;; Note: <!-- blah --> comments come out <!>, which need to be removed later.
  (define (filter-comments in)
    (make-input-port
     (lambda ()
       (let ([char (read-char in)])
         (if (and (eq? char #\-) (eq? (peek-char in) #\-))
             (let loop ()
               (if (and (eq? (read-char in) #\-) (eq? (read-char in) #\-))
                   (read-char in)
                   (loop)))
             char)))
     (lambda ()
       ;; more here - this is broken if the next char is #\- and starts a comment and more chars
       ;; aren't ready after the comment.
       (char-ready? in))
     (lambda () (close-input-port in))))
  
  ;; summarize-dtd : Dtd -> Spec
  (define (summarize-dtd dtd)
    (let ([expander
           (foldr (lambda (x acc)
                    (extend-entity-expander (entity-def-name x) (entity-def-value x) acc))
                  empty-entity-expander
                  (filter entity-def? dtd))]
          [hack-content ;: String -> (listof Symbol)
           (lambda (content)
             (let* ([stripped (filter (lambda (x) (not (char-whitespace? x))) (string->list content))]
                    [cludged (map (lambda (c)
                                    (cond
                                      [(char-id? c) c]
                                      [else #\space]))
                                  stripped)]
                    [symbols (read-from-string-all (list->string cludged))]
                    [nix (memq '- symbols)])
               (cond
                 [nix (filter (lambda (s) (not (memq s nix))) symbols)]
                 [(and (pair? symbols) (null? (cdr symbols)) (eq? (car symbols) 'empty)) null]
                 [else symbols])))])
      (merge-contents 
       (map (lambda (x)
              (cons (foldr (lambda (s acc)
                             (let ([in (open-input-string 
                                        (format "(~a)" (expand-entities expander (symbol->string s))))])
                               (append (read-names in) acc)))
                           null
                           (element-def-name x))
                    (quicksort (hack-content (expand-entities expander (element-def-content x)))
                               (lambda (a b)
                                 (string<=? (symbol->string a)
                                            (symbol->string b))))))
            (filter element-def? dtd)))))
  
  ;; merge-contents : Spec -> Spec
  (define merge-contents
    (letrec ([comb
              (lambda (x rst)
                (cond
                  [(null? rst) (list x)]
                  [else (cond
                          [(equal? (cdr x) (cdar rst))
                           (cons (cons (append (car x) (caar rst))
                                       (cdr x))
                                 (cdr rst))]
                          [else (cons (car rst) (comb x (cdr rst)))])]))])
      (lambda (lst)
        (cond
          [(null? lst) null]
          [else (comb (car lst)
                      (merge-contents (cdr lst)))])))))
