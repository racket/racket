#lang racket/base
(require racket/contract
         racket/list
         racket/match
         "structures.rkt")

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
                  (make-cdata start (pos) (format "<![CDATA[~a]]>" data)))]
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
  (let* ([result_list 
          (let loop ()
            (skip-space in)
            (cond [(name-start? (peek-char-or-special in))
                   (cons (lex-attribute in pos) (loop))]
                  [else null]))]
         [check_dup (check-duplicates result_list (lambda (a b) (eq? (attribute-name a) (attribute-name b))))])
    (if check_dup
        (lex-error in pos "duplicated attribute name ~a" (attribute-name check_dup))
        result_list)))

;; lex-attribute : Input-port (-> Location) -> Attribute
(define (lex-attribute in pos)
  (let ([start (pos)]
        [name (lex-name in pos)])
    (skip-space in)
    (unless (eq? (read-char in) #\=)
      (lex-error in pos "expected = in attribute ~a" name))
    (skip-space in)
    ;; more here - handle entites and disallow "<"
    (let* ([delimiter (read-char-or-special in)]
           [value (case delimiter
                    [(#\' #\")
                     (list->string
                      (let read-more ()
                        (let ([c (non-eof peek-char-or-special in pos)])
                          (cond
                            [(eq? c 'special)
                             (lex-error in pos "attribute values cannot contain non-text values")]
                            [(eq? c delimiter) (read-char in) null]
                            [(eq? c #\&)
                             (let ([entity (expand-entity (lex-entity in pos))])
                               (if (pcdata? entity)
                                   (append (string->list (pcdata-string entity)) (read-more))
                                   ;; more here - do something with user defined entites
                                   (read-more)))]
                            [else (read-char in) (cons c (read-more))]))))]
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
;; deviation - disallow ]]> "for compatability" with SGML, sec 2.4 XML spec 
(define (lex-pcdata in pos)
  (let ([start (pos)]
        [data (let loop ()
                (let ([next (peek-char-or-special in)])
                  (cond
                    [(or (eof-object? next)
                         (not (char? next))
                         (eq? next #\&)
                         (eq? next #\<))
                     null]
                    [(and (char-whitespace? next) (collapse-whitespace))
                     (skip-space in)
                     (cons #\space (loop))]
                    [else (cons (read-char in) (loop))])))])
    (make-pcdata start
                 (pos)
                 (list->string data))))

;; lex-name : Input-port (-> Location) -> Symbol
(define (lex-name in pos)
  (let ([c (non-eof read-char-or-special in pos)])
    (unless (name-start? c)
      (lex-error in pos "expected name, received ~e" c))
    (string->symbol
     (list->string
      (cons c (let lex-rest ()
                (let ([c (non-eof peek-char-or-special in pos)])
                  (cond
                    [(eq? c 'special)
                     (lex-error in pos "names cannot contain non-text values")]
                    [(name-char? c)
                     (cons (read-char in) (lex-rest))]
                    [else null]))))))))

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
  (list->string
   (let read-more ()
     (let ([c (non-eof read-char in pos)])
       (cond
         [(eq? c char) null]
         [else (cons c (read-more))])))))

;; non-eof : (Input-port -> (U Char Eof)) Input-port (-> Location) -> Char
(define (non-eof f in pos)
  (let ([c (f in)])
    (cond
      [(eof-object? c) (lex-error in pos "unexpected eof")]
      [else c])))

;; gen-read-until-string : String -> Input-port (-> Location) -> String
;; uses Knuth-Morris-Pratt from
;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
;; discards stop from input
;; ---
;; Modified by Jay to look more like the version on Wikipedia after discovering a bug when parsing CDATA
;; The use of the hasheq table and the purely numeric code trades hash efficiency for stack/ec capture efficiency
(struct hash-string (port pos ht))
(define (hash-string-ref hs k)
  (match-define (hash-string port pos ht) hs)
  (hash-ref! ht k (lambda () (non-eof read-char port pos))))

(define (gen-read-until-string W)
  (define Wlen (string-length W))
  (define T (make-vector Wlen #f))
  (vector-set! T 0 -1)
  (vector-set! T 1 0)
  (let kmp-table ([pos 2] [cnd 0])
    (when (pos . < . Wlen)
      (cond
        [(char=? (string-ref W (sub1 pos)) (string-ref W cnd))
         (vector-set! T pos (add1 cnd))
         (kmp-table (add1 pos) (add1 cnd))]
        [(cnd . > . 0)
         (kmp-table pos (vector-ref T cnd))]
        [(zero? cnd)
         (vector-set! T pos 0)
         (kmp-table (add1 pos) 0)])))
  (lambda (S-as-port S-pos)
    (define S (hash-string S-as-port S-pos (make-hasheq)))
    (define W-starts-at
      (let kmp-search ([m 0] [i 0])
        (if (char=? (string-ref W i) (hash-string-ref S (+ m i)))
            (let ([i (add1 i)])
              (if (= i Wlen)
                  m
                  (kmp-search m i)))
            (let* ([Ti (vector-ref T i)]
                   [m (+ m i (* -1 Ti))])
              (if (Ti . > . -1)
                  (let ([i Ti])
                    (kmp-search m i))
                  (let ([i 0])
                    (kmp-search m i)))))))
    (list->string
     (for/list ([i (in-range 0 W-starts-at)])
       (hash-string-ref S i)))))

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
