#lang racket

;; todo?
;; -- export tokenization?

(require (rename-in lang/prim (first-order->higher-order f2h)))
(require (only-in net/sendurl send-url/file))

;; I am trying to use these lists to automate the documentation of the functions
;; but my scribble skills are insufficient and my time is running out. 

(module devices racket/base
  (provide *input-devices* *output-devices*)
  (define *input-devices*  `((stdin ,current-input-port)   (standard-in ,current-input-port)))
  (define *output-devices* `((stdout ,current-output-port) (standard-out ,current-output-port))))

(require (submod "." devices))

;;---------------------------------------------------------------------------------------------------
(provide simulate-file) ;; syntax (simulate-file reader string ...)

(provide
 ;; all reader functions consume the name of a file f:
 ;; -- f must be a file name (string) in the same folder as the program 
 
 ;; String -> String
 ;; read the specified file as a string
 read-file
 
 ;; String -> [Listof 1String]
 ;; read the specified file as a list of 1strings (characters)
 read-1strings
 
 ;; String -> [Listof String]
 ;; read the specified file as a list of strings, one per line
 read-lines
 
 ;; String -> [Listof String]
 ;; read the specified file as a list of white-space separated tokens
 read-words
 
 ;; String -> [Listof [Listof String]]
 ;; read the specified file as a list of lines, each line as a list of words
 read-words/line
 
 ;; String -> [Listof [Listof (Union Number String)]]
 ;; read the specified file as a list of lines, each line as a list of words and numbers 
 read-words-and-numbers/line
 
 ;; String -> [Listof [Listof (U Any)]]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the specified file as a list of lists---one per line---of values (Any)
 read-csv-file
 
 ;; String String -> String 
 ;; (write-file filename str) writes str to filename; 
 ;; produces the file name as a confirmation that the write succeeded 
 write-file)

#;
(provide 
 ;; [List-of Symbol]
 ;;  symbols that redirect input from an input-port 
 *input-devives*
 
 ;; [List-of Symbol]
 ;; symbols that redirect output from a output-port 
 *output-devives*)

;; ---------------------------------------------------------------------------------------------------
;; reading simple X-expressions via HTML/XML files 

(provide-higher-order-primitive
 ;; String ([Listof Any] -> X) -> [Listof X]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the specified file as a file of comma-separated values, apply the second
 ;; argument to each row, i.e., list of CSV on one line 
 read-csv-file/rows (_ row-processor))
(provide 
 ;; String -> Nothing 
 ;; send the specified file name as a URL to default browser 
 (rename-out [send-url/file show])
 
 ;; --------------------------------------------------------------------------------------------------
 ;; data definition 
 
 ;   Xexpr.v3 is one of: 
 ;   -- Symbol 
 ;   -- String 
 ;   -- Number 
 ;   -- (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v3]))
 ;   -- (cons Symbol [List-of Xexpr.v3])
 ;
 ;   Attribute is:
 ;      (list Symbol String)
 ;   (list 'a "some text") is called an a-Attribute and "some text" is a's value.
 
 ;; --------------------------------------------------------------------------------------------------
 ;; function names and purpose statements 
 
 ;; Any -> Boolean 
 ;; is the given value an Xexpr.v3? 
 ;; effect: display bad piece if x is not an Xexpr.v3
 xexpr?
 
 ;; String -> Xexpr.v3
 ;; given a file name, retrieve the first X(HT)ML element
 
 ;; the X(HT)ML element does not contain any strings as elements other than whitespace, 
 ;; and all whitespace between embedded elements is eliminated 
 read-plain-xexpr
 
 ;; String -> Xexpr.v3
 ;; given a file name, retrieve the first X(HT)ML element
 read-xexpr
 
 ;; String -> Boolean 
 ;; true, if the url points to a legitimate web page; 
 ;; false, if this url returns a '404'
 ;; raises an exception if the url doesn't get caught by an active server 
 url-exists? 
 
 ;; String -> [Maybe Xexpr.v3]
 ;; given a URL, retrieve the first X(HT)ML element, false if the web page isn't found
 ;; read HTML as XML (if possible) 
 ;; effect: signals an error in case of network problems or if there is no element 
 read-xexpr/web
 
 ;; String -> [Maybe Xexpr.v3]
 ;; given a URL, find web site and retrieve the first XML element, false if the web page isn't found 
 ;; read HTML as XML (if possible) 
 ;; effect: signals an error in case of network problems or if there is no element 
 
 ;; the XML element does not contain any strings as elements other than whitespace, 
 ;; and all whitespace between embedded elements is eliminated 
 read-plain-xexpr/web
 
 ;; String -> [Listof String]
 ;; produce the list of (fully resolve) .html references in a elements from url
 url-html-neighbors 
 
 ;; Xexpr.v3 -> String
 ;; turn the X-expression into a string 
 xexpr-as-string)

;; ---------------------------------------------------------------------------------------------------
;; exported functions 

(require htdp/error
         (for-syntax racket/base syntax/parse)
         "private/csv/csv.rkt"
         ;; --- xml/html 
         net/url
         (except-in xml/xml xexpr?)
         html
         srfi/13)

(module+ test
  (require rackunit))

(define-syntax-rule
  (def-reader (name f s ...) body ...)
  (define (name f s ...)
    (check-input-file f 'name)
    (let ()
      body ...)))

(def-reader (read-file f)
  (list->string (read-chunks f read-char drop-last-newline)))

(def-reader (read-1strings f)
  (map string (read-chunks f read-char drop-last-newline)))

(def-reader (read-lines f)
  (read-chunks f *read-line reverse))

(def-reader (read-words f)
  (read-words/line/internal f append))

(def-reader (read-words/line f)
  (read-words/line/internal f cons))

(def-reader (read-words-and-numbers/line f)
  ;; String [Listof [Listof (U String Number)]] -> [Listof [Listof (U String Number)]]
  (define (tease-out-numbers line1 r)
    (cons (for/list ((t (in-list line1))) (or (string->number t) t)) r))
  (read-words/line/internal f tease-out-numbers))

(def-reader (read-csv-file f)
  (read-csv-file/func f))

(def-reader (read-csv-file/rows f row)
  (check-proc 'read-csv-file row 1 "one argument" "row")
  (read-csv-file/func f row))

(define (*read-line)
  (read-line (current-input-port) 'any))

;; ---------------------------------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; writer 

(define (write-file f str)
  (check-output-file f 'write-file)
  (check-arg 'write-file (string? str) "string" "second" str)
  (define (wt) (printf "~a" str))
  (define device (assq f *output-devices*))
  (if device 
      (parameterize ((current-output-port [(cadr device)])) (wt))
      (with-output-to-file f wt #:mode 'text #:exists 'replace))
  f)

;; -----------------------------------------------------------------------------
;; auxiliaries 

;; String [String [Listof [Listof X]] -> Y] -> Y
(define (read-words/line/internal f combine)
  (define lines (read-chunks f *read-line (lambda (x) x)))
  (foldl (lambda (f r)
           (define fst (filter (compose not (curry string=? "")) (split f)))
           (combine fst r))
         '() lines))

;; String (-> X) ([Listof X] -> [Listof X]) -> [Listof X]
;; read a file as a list of X where process-accu is applied to accu when eof
(define (read-chunks f read-chunk process-accu) 
  (define (rd)
    (let loop ([accu '()])
      (define nxt (read-chunk))
      (if (eof-object? nxt) (process-accu accu) (loop (cons nxt accu)))))
  (read-from-file-or-device f rd))

;; String [-> X] -> X
(define (read-from-file-or-device f rd)
  (define device (assq f *input-devices*))
  (if device 
      (parameterize ((current-input-port [(cadr device)])) (rd))
      (with-input-from-file f #:mode 'text rd)))

(define (read-csv-file/func f [row (lambda (x) x)])
  (define (reader o)
    (csv->list
     (make-csv-reader o '((strip-leading-whitespace?  . #t)
                          (strip-trailing-whitespace? . #t)))))
  (define device (assq f *input-devices*))
  (map row
       (if device 
           (reader [(cadr device)])
           (call-with-input-file f #:mode 'text reader))))

;; [Listof Char] -> [Listof Char]
(define (drop-last-newline accu)
  (reverse (if (and (pair? accu) (char=? (car accu) #\newline)) (cdr accu) accu)))

;; effect: ensure that f is a file in current directory or report error for t
(define (check-input-file f t)
  (define d? (assq f *input-devices*))
  (check-arg t (or (string? f) d?) (error-message (map car *input-devices*)) "first" f)
;; ---------------------------------------------------------------------------------------------------
  (unless (or d? (file-exists? f))
    (error t "file ~s not found in ~s [the program's folder]"
      f
      (path->string (current-directory))))
  f)

;; effect: ensure that f is a file in current directory or report error for t
(define (check-output-file f t)
  (define d? (assq f *output-devices*))
  (check-arg t (or (string? f) d?) (error-message (map car *output-devices*)) "first" f))

;; [Listof Symbol] -> String 
(define (error-message los)
  (string-append "string or one of: " (string-join (map ~e los) ", ")))

;; split : String [Regexp] -> [Listof String]
;; splits a string into a list of substrings using the given delimiter
;; (white space by default)
;;ELI: This shouldn't be needed now, it can use `string-split' as is
;; (also, the trimming doesn't make sense if the pattern is not a
;; space--?)
(define (split str [ptn #rx"[ ]+"])
  (regexp-split ptn (string-trim str)))

;; split-lines : String -> Listof[String]
;; splits a string with newlines into a list of lines
(define (split-lines str)
  (map string-trim (split str "\r*\n")))

;; ---------------------------------------------------------------------------------------------------
;; implementation 

;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------
;; exported functions 

(require htdp/error
         (for-syntax racket/base syntax/parse)
         ;; --- xml/html 
         net/url
         (except-in xml/xml xexpr?)
         html
         srfi/13)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (xexpr? x0)
  (define tag (gensym))
  
  ;; Any -> Boolean 
  (define (xexpr-aux? x)
    (cond
      [(string? x) #t]
      [(symbol? x) #t]
      [(number? x) #t]
      [(and (cons? x) (symbol? (first x)))
       (define body (rest x))
       (or (and (list-of-attributes? (first body)) (list-of-xexpr? (rest body))) 
           (list-of-xexpr? body))]
      [else (raise (cons tag x))]))
  
  ;; Any -> Boolean 
  (define (list-of-attributes? xs)
    (and (list? xs)
         (for/and ((x xs))
           (and (list? x) (= (length x) 2) (symbol? (first x)) (string? (second x))))))
  
  ;; Any -> Boolean 
  (define (list-of-xexpr? xs)
    (and (or (list? xs) (raise (cons tag xs)))
         (andmap xexpr-aux? xs)))
  
  ;; -- IN -- 
  (with-handlers (((lambda (x) (and (cons? x) (eq? (car x) tag)))
                   (lambda (x) 
                     (pretty-print `(,(cdr x) occurs in ,x0))
                     #f)))
    (xexpr-aux? x0)))

(define (url-html-neighbors u)
  (define x (read-xexpr/web u))
  (if x (url-html-neighbors-aux u x) '()))

(define (url-exists? url:string)
  (retrieve 'url-exists? url:string (lambda (_ h) (not (404? h)))))

(def-reader (read-plain-xexpr f)
  (read-xexpr-aux 'read-plain-xexpr f fix-up))

(def-reader (read-xexpr f)
  (read-xexpr-aux 'read-xexpr f values))

(define (read-xexpr/web url:string)
  (read-xexpr/web-aux 'read-xexpr/web url:string values))

(define (read-plain-xexpr/web url:string)
  (read-xexpr/web-aux 'read-xexpr/web url:string fix-up))

(define (xexpr-as-string x) 
  (check-arg 'xexpr->string (and (pair? x) (xexpr? x)) 'xexpr "first" x)
  (string-trim (call-with-output-string (curry display-xml/content (xexpr->xml x)))))

;; ---------------------------------------------------------------------------------------------------
;; Symbol String [XML -> XML] -> Xexpr
;; read an Xexpr from a file that contains at least one XHTML element 
(define (read-xexpr-aux tag f fix-up)
  (first-element tag fix-up (read-from-file-or-device f read-html-as-xml)))

;; Symbol String [XML -> XML] -> Xexpr 
;; read an Xexpr from a URL that contains 
(define (read-xexpr/web-aux tag url:string fix-up)
  (retrieve 
   tag
   url:string
   (lambda (url h)
     (if (404? h)
         #f
         (first-element tag fix-up (call/input-url url get-impure-port read-html-as-xml))))))

;; Symbol [XML -> XML] [List-of X] -> Xexpr 
;; picks out the XML elements from raw, ensures there is one, and fixes up the first one as an Xexpr
(define (first-element tag fix-up raw)
  (define ele (filter element? raw))
  (if (empty? ele)
      #f ; (error tag "no XHTML element found")
      (check-result tag xexpr? 'xexpr (xml->xexpr (fix-up (first ele))))))

(define fix-up (eliminate-whitespace '() (lambda (x) #t)))

(module+ test
  (define test-file
    #<< eos
<?xml version='1.0' encoding='UTF-8'?>
<osm>
  <bound box="41.91100,-71.74800,42.16100,-71.61800" origin="0.40.1"/>
  <node id="64028225" version="1" timestamp="2007-10-11T01:54:28Z" uid="8609" user="ewedistrict"
    changeset="95861" lat="42.049639" lon="-71.734509">
    <tag k="attribution" v="Office of Geographic and Environmental Information (MassGIS)"/>
    <tag k="created_by" v="JOSM"/>
    <tag k="source" v="massgis_import_v0.1_20071010205040"/>
  </node>
</osm>
    
 eos
    )
  
  (check-equal? 
   (with-input-from-string test-file (lambda () (read-xexpr-aux 'read-xexpr 'stdin fix-up)))
   '(osm
     ()
     (bound ((box "41.91100,-71.74800,42.16100,-71.61800") (origin "0.40.1")))
     (node
      ((changeset "95861")
       (id "64028225")
       (lat "42.049639")
       (lon "-71.734509")
       (timestamp "2007-10-11T01:54:28Z")
       (uid "8609")
       (user "ewedistrict")
       (version "1"))
      (tag
       ((k "attribution")
        (v "Office of Geographic and Environmental Information (MassGIS)")))
      (tag ((k "created_by") (v "JOSM")))
      (tag ((k "source") (v "massgis_import_v0.1_20071010205040")))))))

;; ---------------------------------------------------------------------------------------------------
;; Symbol String [URL String -> X] -> X 
;; retrieve the first text line from the url represented by url:string, hand url and line to consumer
(define (retrieve tag url:string consumer)
  (define URL (string->url url:string))
  (with-handlers ([exn:fail:network? 
                   (lambda (e) 
                     (define msg (format "working url, details:\n ~a" (exn-message e)))
                     (check-arg tag #f msg "" url:string))])
    (consumer URL (call/input-url URL get-impure-port read-line))))

;; ---------------------------------------------------------------------------------------------------
;; String -> Boolean 
;; does the string contain "404"
(define (404? s) (pair? (regexp-match "404" s)))

;; ---------------------------------------------------------------------------------------------------
;; String Xexpr -> [Listof String]
;; retrieve the domain-local neighbors of url that point to HTML files 

(module+ test 
  (check-equal? (url-html-neighbors-aux
                 "http://fun.com/"
                 '(div 
                   (a "hello")
                   (a ((href " one.html")) "world")
                   (a ((href "Papers/ignore.pdf")) "world")
                   (img ((alt "two")))
                   (img ((alt "three") (src "ignore.jpg")))))
                '("http://fun.com/one.html")))

(define (url-html-neighbors-aux u xexpr)
  (define url (string->url u))
  (for/fold ((result '())) ((e (xexpr-elements xexpr 'a)))
    (cond
      [(and (cons? (rest e)) (loa? (second e)))
       (define html-targets
         (for/fold ((htmls '())) ((attributes-of-a-element (second e)))
           (cond
             [(symbol=? (first attributes-of-a-element) 'href)
              (define value:str (string-trim-both (second attributes-of-a-element)))
              (define value:url (combine-url/relative url value:str))
              (if (not (url-ends-in-html? value:url)) 
                  htmls 
                  (cons (url->string value:url) htmls))]
             [else htmls])))
       (append html-targets result)]
      [else result])))

;; ---------------------------------------------------------------------------------------------------
;; URL -> Boolean 
;; does the url end in html? 

(module+ test
  (check-true (url-ends-in-html? (string->url "seconds.html")))
  (check-false (url-ends-in-html? (string->url "seconds.pdf"))))

(define (url-ends-in-html? u)
  (define q (reverse (map path/param-path (url-path u))))
  (and (cons? q) (pair? (regexp-match ".html$" (first q)))))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr Symbol -> [Listof Xexpr]
;; retrieve all elements whose tag is 'tag'

(module+ test 
  (check-equal? (xexpr-elements '(p () (p ((align "center")) "hello") (a)) 'p) 
                (list '(p () (p ((align "center")) "hello") (a))
                      '(p ((align "center")) "hello"))))

(define (xexpr-elements x0 tag)
  (define (K- fst rst) rst)
  (xexpr-abs x0 
             '() 
             K-
             K- 
             append 
             (lambda (e loa rst) (if (symbol=? (first e) tag) (cons e rst) rst))
             (lambda (e rst) (if (symbol=? (first e) tag) (cons e rst) rst))))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr
;; [Attribute -> X]
;; Y 
;; [String Y -> Y] 
;; [Xexpr Y -> Y]
;; [Xexpr [Listof X] Z -> W] 
;; [Symbol Z -> W] 
;; -> 
;; W

;; traverse X-expression and combine results 
(define (xexpr-abs x0 body0 attr-combine string-combine xexpr-combine loa-combine plain-combine)
  ;; Xexpr -> W
  (define (f-xexpr x)
    (cond
      [(and (cons? (rest x)) (loa? (second x))) 
       (loa-combine x (f-loa (second x)) (f-xbody (rest (rest x))))]
      [else (plain-combine x (f-xbody (rest x)))]))
  ;; Xbody -> Z
  (define (f-xbody x)
    (cond
      [(empty? x) body0]
      [(string? (first x)) (string-combine (first x) (f-xbody (rest x)))]
      [(cons? (first x)) (xexpr-combine (f-xexpr (first x)) (f-xbody (rest x)))]
      [else (f-xbody (rest x))]))
  ;; LOA -> [Listof X]
  (define (f-loa x)
    (cond
      [(empty? x) '()]
      [else (attr-combine (first x) (f-loa (rest x)))]))
  ;; -- IN -- 
  (f-xexpr x0))

;; String or (cons Symbol Y) or empty or (cons (list Symbol String) Any) --> Boolean
;; is the given value a loa, possibly in front of some other Xexpr elements 
(define (loa? x)
  (or (empty? x) (and (cons? x) (cons? (first x)))))



;; ---------------------------------------------------------------------------------------------------
;; tester 

(define-syntax (simulate-file stx)
  (syntax-case stx ()
    [(simulate-file) 
     (raise-syntax-error #f "expects at least one sub-expression" stx)]
    [(simulate-file reader str ...) #'(simulate-file/proc (f2h reader) str ...)]))

(define (simulate-file/proc reader . los)
  (define _1 (check-proc "simulate-file" reader 1 "reader" "one argument"))
  (define _2 
    (andmap 
     (lambda (f)
       (check-arg "simulate-file" (string? f) "sequence of strings" "" f))
     los))
  (define t (make-temporary-file "drracket-temporary-file-~a"))
  (dynamic-wind 
   (lambda ()
     (with-output-to-file t 
       (lambda () (for-each displayln los))
       #:mode 'text
       #:exists 'replace))
   (lambda () 
     (reader (path->string t)))
   (lambda ()
     (delete-file t))))
