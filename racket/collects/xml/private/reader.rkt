#lang racket/base
(require racket/contract/base
         racket/list
         (only-in "structures.rkt" make-document make-prolog)
         (except-in (submod "structures.rkt" unsafe) make-document make-prolog))

(provide/contract
 [read-xml (() (input-port?) . ->* . document?)]
 [read-xml/document (() (input-port?) . ->* . document?)]
 [read-xml/element (() (input-port?) . ->* . element?)]
 [xml-count-bytes (parameter/c boolean?)]
 [read-comments (parameter/c boolean?)]
 [collapse-whitespace (parameter/c boolean?)]
 [exn:xml? (any/c . -> . boolean?)])

;; Start-tag ::= (make-start-tag Location Location Symbol (listof Attribute))
(define-struct (start-tag source) (name attrs))

;; End-tag ::= (make-end-tag Location Location Symbol)
(define-struct (end-tag source) (name))

;; Token ::= Contents | Start-tag | End-tag | Eof

(define xml-count-bytes (make-parameter #f))
(define read-comments (make-parameter #f))
(define collapse-whitespace (make-parameter #f))

;; read-xml : [Input-port] -> Document
(define read-xml
  (lambda ([in (current-input-port)])
    (let*-values ([(in pos) (positionify in)]
                  [(misc0 start) (read-misc in pos)])
      (make-document (make-prolog misc0 #f empty)
                     (read-xml-element-helper pos in start)
                     (let-values ([(misc1 end-of-file) (read-misc in pos)])
                       (unless (EOF? end-of-file)
                         (parse-error (list
                                       (make-srcloc
                                        (object-name in)
                                        #f
                                        #f
                                        (location-offset (source-start end-of-file))
                                        (- (location-offset (source-stop end-of-file))
                                           (location-offset (source-start end-of-file)))))
                                      "extra stuff at end of document ~e"
                                      end-of-file))
                       misc1)))))

;; read-xml : [Input-port] -> Document
(define (read-xml/document [in (current-input-port)])
  (let*-values ([(in pos) (positionify in)]
                [(misc0 start) (read-misc in pos)])
    (make-document (make-prolog misc0 #f empty)
                   (read-xml-element-helper pos in start)
                   empty)))

;; read-xml/element : [Input-port] -> Element
(define read-xml/element
  (lambda ([in (current-input-port)])
    (let-values ([(in pos) (positionify in)])
      (skip-space in)
      (read-xml-element-helper pos in (lex in pos)))))

;; read-xml-element-helper : Nat Iport Token -> Element
(define (read-xml-element-helper pos in start)
  (cond
    [(start-tag? start) (read-element start in pos)]
    [(element? start) start]
    [else
     (parse-error
      (list
       (make-srcloc
        (object-name in)
        #f
        #f
        ; XXX Some data structures should really be changed to be sources
        (if (source? start)
            (location-offset (source-start start))
            #f)
        (if (source? start)
            (- (location-offset (source-stop start))
               (location-offset (source-start start)))
            #f)))
      "expected root element - received ~e"
      (cond
        [(pcdata? start) (pcdata-string start)]
        [(EOF? start) eof]
        [else start]))]))

;; read-misc : Input-port (-> Location) -> (listof Misc) Token
(define (read-misc in pos)
  (let read-more ()
    (let ([x (lex in pos)])
      (cond
        [(p-i? x)
         (let-values ([(lst next) (read-more)])
           (values (cons x lst) next))]
        [(comment? x)
         (let-values ([(lst next) (read-more)])
           (if (read-comments)
               (values (cons x lst) next)
               (values lst next)))]
        [(and (pcdata? x) (andmap char-whitespace? (string->list (pcdata-string x))))
         (read-more)]
        [else (values null x)]))))

;; read-element : Start-tag Input-port (-> Location) -> Element
(define (read-element start in pos)
  (let ([name (start-tag-name start)]
        [a (source-start start)]
        [b (source-stop start)])
    (let read-content ([k (lambda (body end-loc)
                            (make-element
                             a end-loc name (start-tag-attrs start)
                             body))])
      (let ([x (lex in pos)])
        (cond
          [(EOF? x)
           (parse-error (list
                         (make-srcloc
                          (object-name in)
                          #f
                          #f
                          (location-offset (source-start start))
                          (- (location-offset (source-stop start))
                             (location-offset (source-start start)))))
                        "unclosed `~a' tag at [~a ~a]"
                        name
                        (format-source a)
                        (format-source b))]
          [(start-tag? x)
           (let ([next-el (read-element x in pos)])
             (read-content (lambda (body end-loc)
                             (k (cons next-el body)
                                end-loc))))]
          [(end-tag? x)
           (let ([end-loc (source-stop x)])
             (unless (eq? name (end-tag-name x))
               (parse-error
                (list
                 (make-srcloc (object-name in)
                              #f
                              #f
                              (location-offset a)
                              (- (location-offset b) (location-offset a)))
                 (make-srcloc (object-name in)
                              #f
                              #f
                              (location-offset (source-start x))
                              (- (location-offset end-loc) (location-offset (source-start x)))))
                "start tag `~a' at [~a ~a] doesn't match end tag `~a' at [~a ~a]"
                name
                (format-source a)
                (format-source b)
                (end-tag-name x)
                (format-source (source-start x))
                (format-source end-loc)))
             (k null end-loc))]
          [(entity? x) (read-content (lambda (body end-loc)
                                       (k (cons (expand-entity x) body)
                                          end-loc)))]
          [(comment? x) (if (read-comments)
                            (read-content (lambda (body end-loc) (k (cons x body) end-loc)))
                            (read-content k))]
          [else (read-content (lambda (body end-loc) (k (cons x body) end-loc)))])))))

;; expand-entity : Entity -> (U Entity Pcdata)
;; more here - allow expansion of user defined entities
(define (expand-entity x)
  (let ([expanded (default-entity-table (entity-text x))])
    (if expanded
        (make-pcdata (source-start x) (source-stop x) expanded)
        x)))

;; default-entity-table : Symbol -> (U #f String)
(define (default-entity-table name)
  (case name
    [(amp) "&"]
    [(lt) "<"]
    [(gt) ">"]
    [(quot) "\""]
    [(apos) "'"]
    [else #f]))

(define-struct (EOF source) ())

;; lex : Input-port (-> Location) -> (U Token special)
(define (lex in pos)
  (let ([c (peek-char-or-special in)])
    (cond
      [(eof-object? c) (read-char in) (EOF (pos) (pos))]
      [(eq? c #\&) (lex-entity in pos)]
      [(eq? c #\<) (lex-tag-cdata-pi-comment in pos)]
      [(not (char? c)) (read-char-or-special in)]
      [else (lex-pcdata in pos)])))

; lex-entity : Input-port (-> Location) -> Entity
; pre: the first char is a #\&
(define (lex-entity in pos)
  (let ([start (pos)])
    (read-char in)
    (let ([data (case (peek-char in)
                  [(#\#)
                   (read-char in)
                   (let ([n (case (peek-char in)
                              [(#\x) (read-char in)
                                     (string->number (read-until #\; in pos) 16)]
                              [else (string->number (read-until #\; in pos))])])
                     (unless (number? n)
                       (lex-error in pos "malformed numeric entity"))
                     (unless (valid-char? n)
                       (lex-error in pos "not a well-formed numeric entity (does not match the production for Char, see XML 4.1)"))
                     n)]
                  [else
                   (begin0
                     (lex-name in pos)
                     (unless (eq? (read-char in) #\;)
                       (lex-error in pos "expected ; at the end of an entity")))])])
      (make-entity start (pos) data))))

; lex-tag-cdata-pi-comment : Input-port (-> Location) -> Start-tag | Element | End-tag | Cdata | p-i | Comment
; pre: the first char is a #\<
(define (lex-tag-cdata-pi-comment in pos)
  (let ([start (pos)])
    (read-char in)
    (case (non-eof peek-char-or-special in pos)
      [(#\!)
       (read-char in)
       (case (non-eof peek-char in pos)
         [(#\-) (read-char in)
                (unless (eq? (read-char-or-special in) #\-)
                  (lex-error in pos "expected second - after <!-"))
                (let ([data (lex-comment-contents in pos)])
                  (unless (eq? (read-char in) #\>)
                    (lex-error in pos "expected > to end comment (\"--\" can't appear in comments)"))
                  ;(make-comment start (pos) data)
                  (make-comment data))]
         [(#\[) (read-char in)
                (unless (string=? (read-string 6 in) "CDATA[")
                  (lex-error in pos "expected CDATA following <["))
                (let ([data (lex-cdata-contents in pos)])
                  (make-cdata start (pos) (string-append "<![CDATA[" data "]]>")))]
         [else (skip-dtd in pos)
               (skip-space in)
               (unless (eq? (peek-char-or-special in) #\<)
                 (lex-error in pos "expected p-i, comment, or element after doctype"))
               (lex-tag-cdata-pi-comment in pos)])]
      [(#\?) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (let ([data (lex-pi-data in pos)])
                 (make-p-i start (pos) name data)))]
      [(#\/) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (unless (eq? (read-char-or-special in) #\>)
                 (lex-error in pos "expected > to close ~a's end tag" name))
               (make-end-tag start (pos) name))]
      [else ; includes 'special, but lex-name will fail in that case
       (let ([name (lex-name in pos)]
             [attrs (lex-attributes in pos)])
         (skip-space in)
         (case (read-char-or-special in)
           [(#\/)
            (unless (eq? (read-char in) #\>)
              (lex-error in pos "expected > to close empty element ~a" name))
            (make-element start (pos) name attrs null)]
           [(#\>) (make-start-tag start (pos) name attrs)]
           [else (lex-error in pos "expected / or > to close tag `~a'" name)]))])))

;; lex-attributes : Input-port (-> Location) -> (listof Attribute)
(define (lex-attributes in pos)
  (let* ([result-list
          (let loop ()
            (skip-space in)
            (cond [(name-start? (peek-char-or-special in))
                   (cons (lex-attribute in pos) (loop))]
                  [else null]))]
         [check-dup (check-duplicates result-list eq? #:key attribute-name)])
    (if check-dup
        (lex-error in pos "duplicated attribute name ~a" (attribute-name check-dup))
        result-list)))

;; lex-attribute : Input-port (-> Location) -> Attribute
(define (lex-attribute in pos)
  (let ([start (pos)]
        [name (lex-name in pos)])
    (skip-space in)
    (unless (eq? (read-char in) #\=)
      (lex-error in pos "expected = in attribute ~a" name))
    (skip-space in)
    ;; more here - handle entities and disallow "<"
    (let* ([delimiter (read-char-or-special in)]
           [value (case delimiter
                    [(#\' #\")
                     (define out (open-output-string))
                     (let read-more ()
                       (let ([c (non-eof peek-char-or-special in pos)])
                         (cond
                           [(eq? c 'special)
                            (lex-error in pos "attribute values cannot contain non-text values")]
                           [(eq? c delimiter)
                            (void (read-char in))]
                           [(eq? c #\&)
                            (let ([entity (expand-entity (lex-entity in pos))])
                              (cond
                                [(pcdata? entity)
                                 (write-string (pcdata-string entity) out)]
                                [(number? (entity-text entity))
                                 (write-char (integer->char (entity-text entity)) out)]
                                [else ;; more here - do something with user defined entities
                                 (void)]))
                            (read-more)]
                           [else
                            (write-char c out)
                            (void (read-char in))
                            (read-more)])))
                     (get-output-string out)]
                    [else (if (char? delimiter)
                              (lex-error in pos "attribute values must be in ''s or in \"\"s")
                              delimiter)])])
      (make-attribute start (pos) name value))))

;; skip-space : Input-port -> Void
;; deviation - should sometimes insist on at least one space
(define (skip-space in)
  (let loop ()
    (let ([c (peek-char-or-special in)])
      (when (and (char? c)
                 (char-whitespace? c))
        (read-char in)
        (loop)))))

;; lex-pcdata : Input-port (-> Location) -> Pcdata
;; deviation - disallow ]]> "for compatibility" with SGML, sec 2.4 XML spec
(define (lex-pcdata in pos)
  (define start (pos))
  (define data
    (let loop ([chars null])
      (let ([next (peek-char-or-special in)])
        (cond
          [(or (eof-object? next)
               (not (char? next))
               (eq? next #\&)
               (eq? next #\<))
           (apply string (reverse chars))]
          [(and (char-whitespace? next) (collapse-whitespace))
           (skip-space in)
           (loop (cons #\space chars))]
          [else
           (loop (cons (read-char in) chars))]))))
  (make-pcdata start (pos) data))

;; lex-name : Input-port (-> Location) -> Symbol
(define (lex-name in pos)
  (define c (non-eof read-char-or-special in pos))
  (unless (name-start? c)
    (lex-error in pos "expected name, received ~e" c))
  (let loop ([chars (list c)])
    (define c (non-eof peek-char-or-special in pos))
    (cond
      [(eq? c 'special)
       (lex-error in pos "names cannot contain non-text values")]
      [(name-char? c)
       (loop (cons (read-char in) chars))]
      [else
       (string->symbol (apply string (reverse chars)))])))

;; skip-dtd : Input-port (-> Location) -> Void
(define (skip-dtd in pos)
  (let skip ()
    (case (non-eof read-char in pos)
      [(#\') (read-until #\' in pos) (skip)]
      [(#\") (read-until #\" in pos) (skip)]
      [(#\<)
       (case (non-eof read-char in pos)
         [(#\!) (case (non-eof read-char in pos)
                  [(#\-) (read-char in) (lex-comment-contents in pos) (read-char in) (skip)]
                  [else (skip) (skip)])]
         [(#\?) (lex-pi-data in pos) (skip)]
         [else (skip) (skip)])]
      [(#\>) (void)]
      [else (skip)])))

;; name-start? : Char -> Bool
(define (name-start? ch)
  (and (char? ch)
       (or (char-alphabetic? ch)
           (eq? ch #\_)
           (eq? ch #\:))))

;; name-char? : Char -> Bool
(define (name-char? ch)
  (and (char? ch)
       (or (name-start? ch)
           (char-numeric? ch)
           (eq? ch #\.)
           (eq? ch #\-))))

;; read-until : Char Input-port (-> Location) -> String
;; discards the stop character, too
(define (read-until char in pos)
  (let loop ([chars null])
    (define c
      (non-eof read-char in pos))
    (if (eq? c char)
        (apply string (reverse chars))
        (loop (cons c chars)))))

;; non-eof : (Input-port -> (U Char Eof)) Input-port (-> Location) -> Char
(define (non-eof f in pos)
  (let ([c (f in)])
    (cond
      [(eof-object? c) (lex-error in pos "unexpected eof")]
      [else c])))

;; gen-read-until-string : String -> (Input-port (-> Location) -> String)
(define (gen-read-until-string W)
  (define re (regexp (regexp-quote W)))
  (lambda (in pos)
    (define out (open-output-string))
    (unless (regexp-match-positions re in 0 #f out)
      (lex-error in pos "unexpected eof"))
    (get-output-string out)))

;; "-->" makes more sense, but "--" follows the spec.
(define lex-comment-contents (gen-read-until-string "--"))
(define lex-pi-data (gen-read-until-string "?>"))
(define lex-cdata-contents (gen-read-until-string "]]>"))

;; positionify : Input-port -> Input-port (-> Location)
; This function predates port-count-lines! and port-next-location.
; Otherwise I would have used those directly at the call sites.
(define (positionify in)
  (unless (xml-count-bytes)
    (port-count-lines! in))
  (values
   in
   (lambda ()
     (let-values ([(line column offset) (port-next-location in)])
       (make-location line column offset)))))

;; locs : (listof (list number number))
(define-struct (exn:xml exn:fail:read) ())

;; lex-error : Input-port String (-> Location) TST* -> alpha
;; raises a lexer error, using exn:xml
(define (lex-error in pos str . rest)
  (let* ([the-pos (pos)]
         [offset (location-offset the-pos)])
    (raise
     (make-exn:xml
      (format "read-xml: lex-error: at position ~a: ~a"
              (format-source the-pos)
              (apply format str rest))
      (current-continuation-marks)
      (list
       (make-srcloc (object-name in) #f #f offset 1))))))

;; parse-error : (listof srcloc) (listof TST) *-> alpha
;; raises a parsing error, using exn:xml
(define (parse-error src fmt . args)
  (raise (make-exn:xml (string-append "read-xml: parse-error: "
                                      (apply format fmt args))
                       (current-continuation-marks)
                       src)))

;; format-source : Location -> string
;; to format the source location for an error message
(define (format-source loc)
  (if (location? loc)
      (format "~a.~a/~a" (location-line loc) (location-char loc) (location-offset loc))
      (format "~a" loc)))
