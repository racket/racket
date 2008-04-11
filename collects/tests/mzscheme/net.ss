
(load-relative "loadtest.ss")

(Section 'net)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; url.ss tests
;;

(require net/url
	 net/uri-codec
         mzlib/string
         )

(test "%Pq" uri-decode "%Pq")
(test "%P" uri-decode "%P")
(test "a=hel%2Blo+%E7%88%B8" alist->form-urlencoded '((a . "hel+lo \u7238")))
(test '((a . "hel+lo \u7238")) form-urlencoded->alist (alist->form-urlencoded '((a . "hel+lo \u7238"))))
(test "a=hel%2Blo&b=good-bye" alist->form-urlencoded '((a . "hel+lo") (b . "good-bye")))
(let* ([alist   '((a . "hel+lo") (b . "good-bye"))]
       [ampstr  "a=hel%2Blo&b=good-bye"]
       [semistr "a=hel%2Blo;b=good-bye"])
  (define (test:alist<->str mode str)
    (parameterize ([current-alist-separator-mode
                    (or mode (current-alist-separator-mode))])
      (test str alist->form-urlencoded alist)
      (test alist form-urlencoded->alist str)))
  (test:alist<->str #f ampstr) ; the default
  (test:alist<->str 'amp         ampstr)
  (test:alist<->str 'amp-or-semi ampstr)
  (test:alist<->str 'semi        semistr)
  (test:alist<->str 'semi-or-amp semistr))
(test '((x . "foo") (y . "bar") (z . "baz"))
      form-urlencoded->alist "x=foo&y=bar;z=baz")
(parameterize ([current-alist-separator-mode 'semi])
  (test '((a . "hel+lo&b=good-bye")) form-urlencoded->alist
	(parameterize ([current-alist-separator-mode 'amp])
	  (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))))
(parameterize ([current-alist-separator-mode 'amp])
  (test '((a . "hel+lo;b=good-bye")) form-urlencoded->alist
	(parameterize ([current-alist-separator-mode 'semi])
	  (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))))
(test "aNt=Hi" alist->form-urlencoded '((aNt . "Hi")))
(test '((aNt . "Hi")) form-urlencoded->alist (alist->form-urlencoded '((aNt . "Hi"))))
(test "aNt=Hi" alist->form-urlencoded (form-urlencoded->alist "aNt=Hi"))

(test 'amp-or-semi current-alist-separator-mode)
(err/rt-test (current-alist-separator-mode 'bad))

;; Test the current-proxy-servers parameter can be set
(parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
  (test '(("http" "proxy.com" 3128)) current-proxy-servers))

(let ([with-censor (load-relative "censor.ss")])
  (with-censor
   (lambda ()
     ;; Test all ASCII chars
     (let ([p (let loop ([n 0])
		(if (= n 128)
                  null
                  (let ([s (string (char-downcase (integer->char n)))])
                    (cons (cons (string->symbol s) s)
                          (loop (add1 n))))))])
       (test p form-urlencoded->alist (alist->form-urlencoded p))
       (let ([l (apply string-append (map cdr p))])
         (test l uri-decode (uri-encode l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tests adapted from Noel Welsh's original test suite
;;

(let ()
  (define-syntax (for stx)
    (syntax-case stx (code)
      [(_ (i from to) e)
       (and (identifier? (syntax code))
            (number? (syntax-e (syntax from)))
            (number? (syntax-e (syntax to))))
       (syntax (let loop ([i from])
                 e
                 (unless (= i to)
                   (loop (+ i 1)))))]))

  (test "hello" uri-encode "hello")
  (test "hello%20there" uri-encode "hello there")

  (let ((pad (lambda (str)
               (if (= (string-length str) 1)
                   (string-append "0" str)
                   str))))
    (for (code 0 127)
         (if (or (= code 45) (= code 33) (= code 95)
                 (= code 46) (= code 126) (= code 42)
                 (= code 39) (= code 40) (= code 41) 
                 (and (<= 48 code) (<= code 57))   ; 0-9
                 (and (<= 65 code) (<= code 90))   ; A-Z
                 (and (<= 97 code) (<= code 122))) ; a-z
             (test (string (integer->char code)) uri-encode (string (integer->char code)))
             (test (string-append "%" (pad (string-upcase (number->string code 16))))
                   uri-encode
                   (string (integer->char code))))))

  (test "" alist->form-urlencoded '())
  (test "key=hello+there" alist->form-urlencoded '((key . "hello there")))
  (test "key1=hi&key2=hello" alist->form-urlencoded '((key1 . "hi") (key2 . "hello")))
  (test "key1=hello+there" alist->form-urlencoded '((key1 . "hello there")))

  (test "hello" uri-decode "hello")
  (test "hello there" uri-decode "hello%20there")

  (let* ((pad (lambda (str)
                (if (= (string-length str) 1)
                    (string-append "0" str)
                    str)))
         (uppercase (lambda (str)
                      (string-uppercase! str)
                      str))
         (lowercase (lambda (str)
                      (string-lowercase! str)
                      str))
         (hexcode (lambda (code)
                    (string-append "%"
                                   (pad (number->string code 16))))))

    ;; each of the next three of these were going from 0 to 255 in Noel's
    ;; original test suite. Those fail here, however.

    (for (code 0 127)
         (test (string (integer->char code)) uri-decode (uppercase (hexcode code))))
    (for (code 0 127)
         (test (string (integer->char code)) uri-decode (lowercase (hexcode code)))))

  (for (code 0 127)
       (test (string (integer->char code)) uri-decode (string (integer->char code))))

  ;; form-urlencoded->alist
  (test '() form-urlencoded->alist "")
  (test '((key . "value")) form-urlencoded->alist "key=value")
  (test '((key . "hello there")) form-urlencoded->alist "key=hello+there")
  (test '((key . "a value")) form-urlencoded->alist "key=a%20value")
  (test '((key . #f))  form-urlencoded->alist "key")
  (test '((key1 . "value 1") (key2 . "value 2")) form-urlencoded->alist "key1=value+1&key2=value+2"))

;;
;; end Noel's original tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (test-s->u vec str)
    (test vec string->url/vec str)
    (test str url/vec->string vec))

  (define (string->url/vec str) (url->vec (string->url str)))
  (define (url/vec->string vec) (url->string (vec->url vec)))

  (define (test-c-u/r expected base relative)
    (define (combine-url/relative-vec x y)
      (url->vec (combine-url/relative (vec->url x) y)))
    (define (->vec x) (url->vec (if (string? x) (string->url x) x)))
    (test (->vec expected) combine-url/relative-vec (->vec base) relative))

  (define (vec->url vec)
    (make-url (vector-ref vec 0)
              (vector-ref vec 1)
              (vector-ref vec 2)
              (vector-ref vec 3)
              (vector-ref vec 4)
              (map (lambda (x)
                     (let ([lst (vector->list x)])
                       (make-path/param (car lst) (cdr lst))))
                   (vector-ref vec 5))
              (vector-ref vec 6)
              (vector-ref vec 7)))

  (define (url->vec url)
    (vector (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (url-path-absolute? url)
            (map (lambda (x) (list->vector (cons (path/param-path x) (path/param-param x))))
                 (url-path url))
            (url-query url)
            (url-fragment url)))

  (test-s->u #(#f #f #f #f #t (#("")) () #f)
             "/")
  (test-s->u #(#f #f #f #f #f () () #f)
             "")
  (test-s->u #("http" #f #f #f #t (#("")) () #f)
             "http:/")

  (test-s->u #("http" #f "" #f #t (#("")) () #f)
             "http:///")

  (test-s->u #("http" #f "www.drscheme.org" #f #f () () #f)
             "http://www.drscheme.org")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("")) () #f)
             "http://www.drscheme.org/")

  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) () #f)
             "http://www.drscheme.org/a/b/c")
  (test-s->u #("http" "robby" "www.drscheme.org" #f #t (#("a") #("b") #("c")) () #f)
             "http://robby@www.drscheme.org/a/b/c")
  (test-s->u #("http" #f "www.drscheme.org" 8080 #t (#("a") #("b") #("c")) () #f)
             "http://www.drscheme.org:8080/a/b/c")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) () "joe")
             "http://www.drscheme.org/a/b/c#joe")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tim . "")) #f)
             "http://www.drscheme.org/a/b/c?tim=")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tim . "")) "joe")
             "http://www.drscheme.org/a/b/c?tim=#joe")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tim . "tim")) "joe")
             "http://www.drscheme.org/a/b/c?tim=tim#joe")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tam . "tom")) "joe")
             "http://www.drscheme.org/a/b/c?tam=tom#joe")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tam . "tom") (pam . "pom")) "joe")
             "http://www.drscheme.org/a/b/c?tam=tom&pam=pom#joe")
  (parameterize ([current-alist-separator-mode 'semi])
    (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tam . "tom") (pam . "pom")) "joe")
               "http://www.drscheme.org/a/b/c?tam=tom;pam=pom#joe"))
  (parameterize ([current-alist-separator-mode 'amp])
    (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((tam . "tom") (pam . "pom")) "joe")
               "http://www.drscheme.org/a/b/c?tam=tom&pam=pom#joe"))
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c" "b")) () #f)
             "http://www.drscheme.org/a/b/c;b")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a" "x") #("b") #("c" "b")) () #f)
             "http://www.drscheme.org/a;x/b/c;b")

  ;; test unquoting for %
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((ti#m . "")) "jo e")
             "http://www.drscheme.org/a/b/c?ti%23m=#jo%20e")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a " " a") #(" b ") #(" c ")) () #f)
             "http://www.drscheme.org/a%20;%20a/%20b%20/%20c%20")
  (test-s->u #("http" "robb y" "www.drscheme.org" #f #t (#("")) () #f)
             "http://robb%20y@www.drscheme.org/")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("%a") #("b/") #("c")) () #f)
             "http://www.drscheme.org/%25a/b%2F/c")

  ;; test the characters that need to be encoded in paths vs those that do not need to
  ;; be encoded in paths
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a:@!$&'()*+,=z") #("/?#[];") #("")) () #f)
             "http://www.drscheme.org/a:@!$&'()*+,=z/%2F%3F%23%5B%5D%3B/")

  (test-s->u #("http" #f "www.drscheme.org" #f #t (#(".") #("..") #(same) #(up) #("...") #("abc.def")) () #f)
             "http://www.drscheme.org/%2e/%2e%2e/./../.../abc.def")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("." "") #(".." "") #(same "") #(up "") #("..." "") #("abc.def" "")) () #f)
             "http://www.drscheme.org/%2e;/%2e%2e;/.;/..;/...;/abc.def;")

  ;; test other scheme identifiers
  (test-s->u #("blah" #f "www.foo.com" #f #t (#("")) () #f)
             "blah://www.foo.com/")
  (test-s->u #("blah99" #f "www.foo.com" #f #t (#("")) () #f)
             "blah99://www.foo.com/")
  (test-s->u #("blah+" #f "www.foo.com" #f #t (#("")) () #f)
             "blah+://www.foo.com/")
  (test-s->u #("a+b-c456.d" #f "www.foo.com" #f #t (#("")) () #f)
             "a+b-c456.d://www.foo.com/")

  ;; a colon and other junk (`sub-delims') can appear in usernames
  (test #("http" "x:!$&'()*+,;=y" "www.drscheme.org" #f #t (#("a")) () #f)
        string->url/vec
        "http://x:!$&'()*+,;=y@www.drscheme.org/a")
  ;; a colon and atsign can appear in absolute paths
  (test-s->u #(#f #f #f #f #t (#("x:@y") #("z")) () #f)
             "/x:@y/z")
  ;; and in relative paths as long as it's not in the first element
  (test-s->u #(#f #f #f #f #f (#("x") #("y:@z")) () #f)
             "x/y:@z")

  ;; test bad schemes
  (err/rt-test (string->url "://www.foo.com/") url-exception?)
  (err/rt-test (string->url "9://www.foo.com/") url-exception?)
  (err/rt-test (string->url "9a://www.foo.com/") url-exception?)
  (err/rt-test (string->url "a*b://www.foo.com/") url-exception?)
  (err/rt-test (string->url "a b://www.foo.com/") url-exception?)

  ;; test file: urls
  (test-s->u #("file" #f "" #f #t (#("abc") #("def.html")) () #f)
             "file:///abc/def.html")
  (test "file:///abc/def.html" url->string (string->url "file:///abc/def.html"))
  (parameterize ([file-url-path-convention-type 'unix])
    (test "file://a/b" url->string (string->url "file://a/b")))

  (parameterize ([file-url-path-convention-type 'unix])
    (test-s->u #("file" #f "localhost" #f #t (#("abc") #("def.html")) () #f)
               "file://localhost/abc/def.html"))

  ;; test files: urls with colons, and the different parsing on Windows
  (test-s->u #("file" #f "localhost" 123 #t (#("abc") #("def.html")) () #f)
             "file://localhost:123/abc/def.html")
  (parameterize ([file-url-path-convention-type 'unix])
    ;; different parse for file://foo:/...
    (test #("file" #f "foo" #f #t (#("abc") #("def.html")) () #f)
          string->url/vec
          "file://foo:/abc/def.html"))
  (parameterize ([file-url-path-convention-type 'windows])
    (test #("file" #f "" #f #t (#("foo:") #("abc") #("def.html")) () #f)
        string->url/vec
        "file://foo:/abc/def.html")
    (test #("file" #f "" #f #t (#("c:") #("abc") #("def.html")) () #f)
        string->url/vec
        "file://c:/abc/def.html")
    (test #("file" #f "" #f #t (#("") #("d") #("c") #("abc") #("def.html")) () #f)
        string->url/vec
        "file:\\\\d\\c\\abc\\def.html"))

  (parameterize ([file-url-path-convention-type 'unix])
    ;; but no effect on http://foo:/...
    (test #("http" #f "foo" #f #t (#("abc") #("def.html")) () #f)
          string->url/vec
          "http://foo:/abc/def.html"))
  (parameterize ([file-url-path-convention-type 'windows])
    (test #("http" #f "foo" #f #t (#("abc") #("def.html")) () #f)
          string->url/vec
          "http://foo:/abc/def.html"))

  (test "file:///c:/a/b"
        url->string (path->url (bytes->path #"c:\\a\\b" 'windows)))    
  (test "file:///c:/a/b"
        url->string (path->url (bytes->path #"\\\\?\\c:\\a\\b" 'windows)))

  (test #"/a/b/c" path->bytes
        (url->path (path->url (bytes->path #"/a/b/c" 'unix)) 'unix))
  (test #"a/b/c" path->bytes
        (url->path (path->url (bytes->path #"a/b/c" 'unix)) 'unix))
  (test #"c:\\a\\b" path->bytes
        (url->path (path->url (bytes->path #"c:/a/b" 'windows)) 'windows))
  (test #"a\\b" path->bytes
        (url->path (path->url (bytes->path #"a/b" 'windows)) 'windows))
  (test #"\\\\d\\c\\a" path->bytes
        (url->path (path->url (bytes->path #"//d/c/a" 'windows)) 'windows))
  (test #"c:\\a\\b" path->bytes
        (url->path (path->url (bytes->path #"\\\\?\\c:\\a\\b" 'windows)) 'windows))
  (test #"\\\\d\\c\\a\\b" path->bytes
        (url->path (path->url (bytes->path #"\\\\?\\UNC\\d\\c\\a\\b" 'windows)) 'windows))
  (test #"\\\\?\\c:\\a/x\\b" path->bytes
        (url->path (path->url (bytes->path #"\\\\?\\c:\\a/x\\b" 'windows)) 'windows))
  (test #"\\\\?\\UNC\\d\\c\\a/x\\b" path->bytes
        (url->path (path->url (bytes->path #"\\\\?\\UNC\\d\\\\c\\a/x\\b" 'windows)) 'windows))
    
  ;; see PR8809 (value-less keys in the query part)
  (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . #f)) #f)
             "http://foo.bar/baz?ugh")
  (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . "")) #f)
             "http://foo.bar/baz?ugh=")
  (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . #f) (x . "y") (|1| . "2")) #f)
             "http://foo.bar/baz?ugh&x=y&1=2")
  (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . "") (x . "y") (|1| . "2")) #f)
             "http://foo.bar/baz?ugh=&x=y&1=2")
  (parameterize ([current-alist-separator-mode 'amp])
    (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . #f) (x . "y") (|1| . "2")) #f)
             "http://foo.bar/baz?ugh&x=y&1=2"))
  (parameterize ([current-alist-separator-mode 'semi])
    (test-s->u #("http" #f "foo.bar" #f #t (#("baz")) ((ugh . #f) (x . "y") (|1| . "2")) #f)
             "http://foo.bar/baz?ugh;x=y;1=2"))

  ;; test case sensitivity
  (test #("http" "ROBBY" "www.drscheme.org" 80 #t (#("INDEX.HTML" "XXX")) ((T . "P")) "YYY")
        string->url/vec
        "HTTP://ROBBY@WWW.DRSCHEME.ORG:80/INDEX.HTML;XXX?T=P#YYY")

  (test-s->u #("mailto" #f #f #f #f (#("robby@plt-scheme.org")) () #f)
             "mailto:robby@plt-scheme.org")

  (test #("http" #f "www.drscheme.org" #f #f () ((bar . "馨慧")) #f)
        string->url/vec
        "http://www.drscheme.org?bar=馨慧")

  (test #("http" #f "www.drscheme.org" #f #f () ((bár . "é")) #f)
        string->url/vec
        "http://www.drscheme.org?bár=é")

  (test-c-u/r "http://www.drscheme.org"
              (make-url #f #f #f #f #f '() '() #f)
              "http://www.drscheme.org")

  (test-c-u/r "http://www.drscheme.org"
              "http://www.drscheme.org"
              "")

  (test-c-u/r "http://www.mzscheme.org"
              "http://www.drscheme.org/"
              "http://www.mzscheme.org")

  (test-c-u/r "http://www.drscheme.org/index.html"
              "http://www.drscheme.org/"
              "index.html")
  (test-c-u/r "http://www.drscheme.org/index.html"
              "http://www.drscheme.org/"
              "/index.html")
  (test-c-u/r "http://www.drscheme.org/index.html"
              "http://www.drscheme.org/a/b/c/"
              "/index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/index.html"
              "http://www.drscheme.org/a/b/c"
              "index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/c/index.html"
              "http://www.drscheme.org/a/b/c/"
              "index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/d/index.html"
              "http://www.drscheme.org/a/b/c"
              "d/index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/c/d/index.html"
              "http://www.drscheme.org/a/b/c/"
              "d/index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/index.html"
              "http://www.drscheme.org/a/b/c/"
              "../index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/c/index.html"
              "http://www.drscheme.org/a/b/c/"
              "./index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/c/%2e%2e/index.html"
              "http://www.drscheme.org/a/b/c/"
              "%2e%2e/index.html")
  (test-c-u/r "http://www.drscheme.org/a/index.html"
              "http://www.drscheme.org/a/b/../c/"
              "../index.html")

  (test-c-u/r "http://www.drscheme.org/a/b/c/d/index.html"
              "http://www.drscheme.org/a/b/c/d/index.html#ghijkl"
              "index.html")
  (test-c-u/r "http://www.drscheme.org/a/b/c/d/index.html#abcdef"
              "http://www.drscheme.org/a/b/c/d/index.html#ghijkl"
              "#abcdef")

  (test-c-u/r "file:///a/b/c/d/index.html"
              "file:///a/b/c/"
              "d/index.html")
  (test-c-u/r "file:///a/b/d/index.html"
              "file:///a/b/c"
              "d/index.html")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; tests from rfc 3986
  ;;

  (for-each
   (λ (line) (test-c-u/r (caddr line) "http://a/b/c/d;p?q" (car line)))
   '(("g:h"           =  "g:h")
     ("g"             =  "http://a/b/c/g")
     ("./g"           =  "http://a/b/c/g")
     ("g/"            =  "http://a/b/c/g/")
     ("/g"            =  "http://a/g")
     ("//g"           =  "http://g")
     ("?y"            =  "http://a/b/c/d;p?y")
     ("g?y"           =  "http://a/b/c/g?y")
     ("#s"            =  "http://a/b/c/d;p?q#s")
     ("g#s"           =  "http://a/b/c/g#s")
     ("g?y#s"         =  "http://a/b/c/g?y#s")
     (";x"            =  "http://a/b/c/;x")
     ("g;x"           =  "http://a/b/c/g;x")
     ("g;x?y#s"       =  "http://a/b/c/g;x?y#s")
     (""              =  "http://a/b/c/d;p?q")
     ("."             =  "http://a/b/c/")
     ("./"            =  "http://a/b/c/")
     (".."            =  "http://a/b/")
     ("../"           =  "http://a/b/")
     ("../g"          =  "http://a/b/g")
     ("../.."         =  "http://a/")
     ("../../"        =  "http://a/")
     ("../../g"       =  "http://a/g")

     ;; abnormal examples follow

     ("../../../g"    =  "http://a/g")
     ("../../../../g" =  "http://a/g")

     ("/./g"          =  "http://a/g")
     ("/../g"         =  "http://a/g")
     ("g."            =  "http://a/b/c/g.")
     (".g"            =  "http://a/b/c/.g")
     ("g.."           =  "http://a/b/c/g..")
     ("..g"           =  "http://a/b/c/..g")

     ("./../g"        =  "http://a/b/g")
     ("./g/."         =  "http://a/b/c/g/")
     ("g/./h"         =  "http://a/b/c/g/h")
     ("g/../h"        =  "http://a/b/c/h")
     ("g;x=1/./y"     =  "http://a/b/c/g;x=1/y")
     ("g;x=1/../y"    =  "http://a/b/c/y")

     ("g?y/./x"       =  "http://a/b/c/g?y/./x")
     ("g?y/../x"      =  "http://a/b/c/g?y/../x")
     ("g#s/./x"       =  "http://a/b/c/g#s/./x")
     ("g#s/../x"      =  "http://a/b/c/g#s/../x")
     ("http:g"        =  "http:g")         ; for strict parsers

     ))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; a few tests of head.ss -- JBC, 2006-07-31
;;

(require net/head)

(test (void) validate-header "From: me@here.net\r\n\r\n")
(test (void) validate-header #"From: me@here.net\r\n\r\n")
(test (void) validate-header "From: a\r\nTo: b\r\nResent-to: qrv@erocg\r\n\r\n")
(test (void) validate-header #"From: a\r\nTo: b\r\nResent-to: qrv@erocg\r\n\r\n")
(err/rt-test (validate-header "From: a\r\nTo: b\r\nMissingTrailingrn: qrv@erocg\r\n") exn:fail?)
(err/rt-test (validate-header #"From: a\r\nTo: b\r\nMissingTrailingrn: qrv@erocg\r\n") exn:fail?)
(err/rt-test (validate-header "From: a\r\nnocolon inthisline\r\n\r\n") exn:fail?)
(err/rt-test (validate-header #"From: a\r\nnocolon inthisline\r\n\r\n") exn:fail?)
(err/rt-test (validate-header "From: a\r\nMissingReturn: och\n\r\n" exn:fail?))
(err/rt-test (validate-header #"From: a\r\nMissingReturn: och\n\r\n" exn:fail?))
(err/rt-test (validate-header "From: a\r\nSpacein Fieldname: och\r\n\r\n" exn:fail?))
(err/rt-test (validate-header #"From: a\r\nSpacein Fieldname: och\r\n\r\n" exn:fail?))

(define test-header "From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n")
(define test-header/bytes #"From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n")

(test "abc" extract-field "From" test-header)
(test #"abc" extract-field #"From" test-header/bytes)
(test "field is\r\n continued" extract-field "To" test-header)
(test #"field is\r\n continued" extract-field #"To" test-header/bytes)
(test "zoo\r\n continued" extract-field "Another" test-header)
(test #"zoo\r\n continued" extract-field #"Another" test-header/bytes)

(test "From: def\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field "From" "def" test-header)
(test #"From: def\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field #"From" #"def" test-header/bytes)
(test "To: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field "From" #f test-header)
(test #"To: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field #"From" #f test-header/bytes)

(test "From: abc\r\nTo: qrs\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field "To" "qrs" test-header)
(test #"From: abc\r\nTo: qrs\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field #"To" #"qrs" test-header/bytes)
(test "From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field "To" #f test-header)
(test #"From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
      replace-field #"To" #f test-header/bytes)

(test "From: abc\r\nTo: field is\r\n continued\r\nAnother: abc\r\n def\r\n\r\n"
      replace-field "Another" "abc\r\n def" test-header)
(test #"From: abc\r\nTo: field is\r\n continued\r\nAnother: abc\r\n def\r\n\r\n"
      replace-field #"Another" #"abc\r\n def" test-header/bytes)
(test "From: abc\r\nTo: field is\r\n continued\r\n\r\n"
      replace-field "Another" #f test-header)
(test #"From: abc\r\nTo: field is\r\n continued\r\n\r\n"
      replace-field #"Another" #f test-header/bytes)

(test "From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
      remove-field "To" test-header)
(test #"From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
      remove-field #"To" test-header/bytes)

(test `(("From" . "abc")
       ("To" . "field is\r\n continued")
       ("Another" . "zoo\r\n continued"))
      extract-all-fields test-header)
(test `((#"From" . #"abc")
       (#"To" . #"field is\r\n continued")
       (#"Another" . #"zoo\r\n continued"))
      extract-all-fields test-header/bytes)

(test "From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\nAthird: data\r\n\r\n"
      append-headers test-header "Athird: data\r\n\r\n")
(test #"From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\nAthird: data\r\n\r\n"
      append-headers test-header/bytes #"Athird: data\r\n\r\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cookie tests --- JBM, 2006-12-01

(require net/cookie)

;; cookie-test : (cookie -> cookie) string -> test
(define (cookie-test fn  expected)
  (test expected
        (λ (c) (print-cookie (fn c)))
        (set-cookie "a" "b")))

;; RC = "reverse curry"
(define (RC f arg2) (λ (arg1) (f arg1 arg2)))
;; o = compose
(define-syntax o
    (syntax-rules ()
      [(o f) f]
      [(o f g h ...)
       (λ (x) (o/* x f g h ...))]))
(define-syntax o/*
  (syntax-rules ()
    [(o/* x) x]
    [(o/* x f g ...)
     (f (o/* x g ...))]))

;; test the most basic functionality
(cookie-test (λ (x) x) "a=b; Version=1")

;; test each modifier individually
(cookie-test (RC cookie:add-comment "set+a+to+b") "a=b; Comment=set+a+to+b; Version=1")
(cookie-test (RC cookie:add-comment "a comment with spaces") "a=b; Comment=\"a comment with spaces\"; Version=1")
(cookie-test (RC cookie:add-comment "the \"risks\" involved in waking")
             "a=b; Comment=\"the \\\"risks\\\" involved in waking\"; Version=1")
(cookie-test (RC cookie:add-comment "\"already formatted\"")
             "a=b; Comment=\"already formatted\"; Version=1")
(cookie-test (RC cookie:add-comment "\"problematic \" internal quote\"")
             "a=b; Comment=\"\\\"problematic \\\" internal quote\\\"\"; Version=1")
(cookie-test (RC cookie:add-comment "contains;semicolon")
             "a=b; Comment=\"contains;semicolon\"; Version=1")
(cookie-test (RC cookie:add-domain ".example.net") "a=b; Domain=.example.net; Version=1")
(cookie-test (RC cookie:add-max-age 100) "a=b; Max-Age=100; Version=1")
(cookie-test (RC cookie:add-path "/whatever/wherever/") "a=b; Path=\"/whatever/wherever/\"; Version=1")
(cookie-test (RC cookie:add-path "a+path") "a=b; Path=a+path; Version=1")
(cookie-test (RC cookie:add-path "\"/already/quoted/\"") "a=b; Path=\"/already/quoted/\"; Version=1")
(cookie-test (RC cookie:secure #t) "a=b; Secure; Version=1")
(cookie-test (RC cookie:secure #f) "a=b; Version=1")
(cookie-test (RC cookie:version 12) "a=b; Version=12")

;; test combinations
(cookie-test (o (RC cookie:add-comment "set+a+to+b")
                (RC cookie:add-domain ".example.net"))
             "a=b; Comment=set+a+to+b; Domain=.example.net; Version=1")
(cookie-test (o (RC cookie:add-max-age 300)
                (RC cookie:secure #t))
             "a=b; Max-Age=300; Secure; Version=1")
(cookie-test (o (RC cookie:add-path "/whatever/wherever/")
                (RC cookie:version 10)
                (RC cookie:add-max-age 20))
             "a=b; Max-Age=20; Path=\"/whatever/wherever/\"; Version=10")

;; test error cases
(define-syntax cookie-error-test
  (syntax-rules ()
    [(cookie-error-test e)
     (thunk-error-test (λ () (e (set-cookie "a" "b"))) #'e cookie-error?)]))

(cookie-error-test (RC cookie:add-comment "illegal character #\000"))
(cookie-error-test (RC cookie:add-max-age -10))
(cookie-error-test (RC cookie:add-domain "doesntstartwithadot.example.com"))
(cookie-error-test (RC cookie:add-domain "bad domain.com"))
(cookie-error-test (RC cookie:add-domain ".bad-domain;com"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other net tests
;;

(require net/base64
	 net/qp
	 mzlib/port)

(define tricky-strings
  (let ([dir (collection-path "tests" "mzscheme")])
    (list (make-bytes 200 32)
	  (make-bytes 200 9)
	  (make-bytes 200 (char->integer #\x))
	  (list->bytes
	   (let loop ([i 0])
	     (if (= i 256)
		 null
		 (cons i (loop (add1 i))))))
	  ;; Something that doesn't end with a LF:
	  (bytes-append
	   (with-input-from-file (build-path dir "net.ss") (lambda () (read-bytes 500)))
	   #"xxx")
	  ;; CRLF:
	  (regexp-replace
	   #rx#"\n"
	   (with-input-from-file (build-path dir "net.ss") (lambda () (read-bytes 500)))
	   #"\r\n"))))

(define (check-same encode decode port line-rx max-w)
  (let ([p (open-output-bytes)])
    (copy-port port p)
    (let ([bytes (get-output-bytes p)]
	  [r (open-output-bytes)])
      (encode (open-input-bytes bytes) r)
      (let ([p (open-input-bytes (get-output-bytes r))])
	(let loop ()
	  (let ([l (read-bytes-line p 'any)])
	    (unless (eof-object? l)
	      (unless (<= (bytes-length l) max-w)
		(test encode "line too long" l))
	      (let ([m (regexp-match-positions line-rx l)])
		(unless (and m
			     (= (bytes-length l) (cdar m)))
		  (test encode 'bad-line l)))
	      (loop))))
	(let ([q (open-output-bytes)])
	  (decode (open-input-bytes (get-output-bytes r)) q)
	  (unless (equal? (get-output-bytes q) bytes)
	    (with-output-to-file "/tmp/x0" (lambda () (display (get-output-bytes r))) 'truncate)
	    (with-output-to-file "/tmp/x1" (lambda () (display (get-output-bytes q))) 'truncate)
	    (with-output-to-file "/tmp/x2" (lambda () (display bytes)) 'truncate)
	    (error 'decode "failed")))))))

(define (check-same-file encode decode file line-rx max-w)
  ;; This "test" is really just a progress report:
  (test #t list? (list file encode))
  (call-with-input-file file
    (lambda (p)
      (check-same encode decode p line-rx max-w))))

(define (check-same-all encode decode line-rx max-w)
  (for-each (lambda (tricky-string)
	      (check-same encode decode 
			  (open-input-bytes tricky-string)
			  line-rx max-w))
	    tricky-strings)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed 17)
    (let ([dir (collection-path "tests" "mzscheme")])
      (for-each (lambda (p)
		  (when (regexp-match #rx"[.]ss$" (path->string p))
		    (unless (or (positive? (random 10)) ; check random 1/10 of files
				(equal? (path->string p) "flat.ss"))
		      (check-same-file encode decode (build-path dir p) line-rx max-w))))
		(directory-list dir)))))

(check-same-all (lambda (i o) (qp-encode-stream i o))
		qp-decode-stream
		#rx#"^(|[\t \41-\176]*[\41-\176]+)$"
		76)

(check-same-all base64-encode-stream 
		base64-decode-stream
		#rx#"^[0-9a-zA-Z+=/]*$"
		72)

(report-errs)
