#lang racket
(require net/url-string tests/eli-tester
         (only-in net/uri-codec current-alist-separator-mode))

(define (url->vec url)
  (vector
   (url-scheme url)
   (url-user url)
   (url-host url)
   (url-port url)
   (url-path-absolute? url)
   (map (lambda (x)
          (list->vector (cons (path/param-path x) (path/param-param x))))
        (url-path url))
   (url-query url)
   (url-fragment url)))

(define (vec->url vec)
  (make-url
   (vector-ref vec 0)
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

(define (string->url/vec str) (url->vec (string->url str)))
(define (url/vec->string vec) (url->string (vec->url vec)))

(define (test-s->u vec str)
  (test (string->url/vec str) => vec
        (url/vec->string vec) => str))

(define (test-c-u/r expected base relative)
  (define (combine-url/relative-vec x y)
    (url->vec (combine-url/relative (vec->url x) y)))
  (define (->vec x) (url->vec (if (string? x) (string->url x) x)))
  (test (combine-url/relative-vec (->vec base) relative)
        => (->vec expected)))

(provide tests)
(module+ main (test do (tests)))
(define (tests)
  (test-s->u #(#f #f #f #f #t (#("")) () #f)
             "/")
  (test-s->u #(#f #f #f #f #f () () #f)
             "")

  (test-s->u #("http" #f #f #f #t (#("")) () #f)
             "http:/")

  (test-s->u #("http" #f "" #f #t (#("")) () #f)
             "http:///")

  (test-s->u #("http" #f "www.drscheme.org" #f #t () () #f)
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

  ;; Allow empty port spec
  (test #("http" #f "www.drscheme.org" #f #t (#("a")) () #f)
        url->vec
        (string->url "http://www.drscheme.org:/a"))

  ;; Test IPv6 literal addresses, which are written with "[...]" around
  ;; an address that contains ":"s:
  (test-s->u #("http" #f "::1" #f #t (#("a") #("b") #("c")) () "joe")
             "http://[::1]/a/b/c#joe")
  ;; This random address came from the Wikipedia page in IPv6:
  (test-s->u #("http" #f "2001:0db8:85a3:0000:0000:8a2e:0370:7334" #f #t (#("a") #("b") #("c")) () "joe")
             "http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/a/b/c#joe")
  (test "http://[::1]/a/b/c#joe" url->string (string->url "http://[::1]/a/b/c#joe"))
  (test "http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/a/b/c#joe"
        url->string
        (string->url "http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/a/b/c#joe"))

  ;; test unquoting for %
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a") #("b") #("c")) ((ti#m . "")) "jo e")
             "http://www.drscheme.org/a/b/c?ti%23m=#jo%20e")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a " " a") #(" b ") #(" c ")) () #f)
             "http://www.drscheme.org/a%20;%20a/%20b%20/%20c%20")
  (test-s->u #("http" "robb y" "www.drscheme.org" #f #t (#("")) () #f)
             "http://robb%20y@www.drscheme.org/")
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("%a") #("b/") #("c")) () #f)
             "http://www.drscheme.org/%25a/b%2F/c")
  (test-s->u #("http" "robby:password" "www.drscheme.org" #f #t (#("")) () #f)
             "http://robby:password@www.drscheme.org/")
  (test "robby:password" (lambda (x) (url-user (string->url x))) "http://robby%3apassword@www.drscheme.org/")

  ;; test the characters that need to be encoded in paths vs those that do not need to
  ;; be encoded in paths
  (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a:@!$&'()*+,=z") #("/?#[];") #("")) () "@!$&'()*+,=z")
             "http://www.drscheme.org/a:@!$&'()*+,=z/%2F%3F%23%5B%5D%3B/#%40!%24%26'()*%2B%2C%3Dz")
  (parameterize ([current-url-encode-mode 'unreserved])
    (test-s->u #("http" #f "www.drscheme.org" #f #t (#("a:@!$&'()*+,=z") #("/?#[];") #("")) () "@!$&'()*+,=z")
               "http://www.drscheme.org/a:@%21$&%27%28%29%2A+,=z/%2F%3F%23%5B%5D%3B/#%40%21%24%26%27%28%29%2A%2B%2C%3Dz"))

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
  (test
   (string->url "://www.foo.com/")    =error> url-exception?
   (string->url "9://www.foo.com/")   =error> url-exception?
   (string->url "9a://www.foo.com/")  =error> url-exception?
   (string->url "a*b://www.foo.com/") =error> url-exception?
   (string->url "a b://www.foo.com/") =error> url-exception?)

  ;; test relatve path after host
  (test
   (string->url "http://www.foo.com:oops/") =error> url-exception?
   (string->url "http://www.foo.com:0oops/") =error> url-exception?

   ;; Empty "relative" path is ok with host:
   (url->string (url "http" #f "www.drscheme.org" #f #f (list) (list) #f))
   => "http://www.drscheme.org"
   ;; Non-empty "relative" path is not ok with host:
   (url->string (url "http" #f "www.drscheme.org" #f #f (list (path/param "a" (list))) (list) #f))
   =error> exn:fail:contract?)

  ;; test file: urls
  (test-s->u #("file" #f "" #f #t (#("abc") #("def.html")) () #f)
             "file:///abc/def.html")
  (test (url->string (string->url "file:///abc/def.html"))
        => "file:///abc/def.html")
  (parameterize ([file-url-path-convention-type 'unix])
    (test (url->string (string->url "file://a/b"))
          => "file://a/b")
    (test-s->u #("file" #f "localhost" #f #t (#("abc") #("def.html")) () #f)
               "file://localhost/abc/def.html"))

  ;; test files: urls with colons, and the different parsing on Windows
  (test-s->u #("file" #f "localhost" 123 #t (#("abc") #("def.html")) () #f)
             "file://localhost:123/abc/def.html")
  (parameterize ([file-url-path-convention-type 'unix])
    ;; different parse for file://foo:/...
    (test (string->url/vec "file://foo:/abc/def.html")
          => #("file" #f "foo" #f #t (#("abc") #("def.html")) () #f)))
  (parameterize ([file-url-path-convention-type 'windows])
    (test (string->url/vec "file://foo:/abc/def.html")
          => #("file" #f "foo" #f #t (#("abc") #("def.html")) () #f)
          (string->url/vec "file://foo/abc/def.html")
          => #("file" #f "foo" #f #t (#("abc") #("def.html")) () #f)
          (string->url/vec "file:///foo/abc/def.html")
          => #("file" #f "" #f #t (#("foo") #("abc") #("def.html")) () #f)
          (string->url/vec "file://c:/abc/def.html")
          => #("file" #f "" #f #t (#("c:") #("abc") #("def.html")) () #f)
          (string->url/vec "fILe://C:/abc/def.html")
          => #("file" #f "" #f #t (#("C:") #("abc") #("def.html")) () #f)
          (string->url/vec "file:c:/abc/def.html")
          => #("file" #f #f #f #t (#("c:") #("abc") #("def.html")) () #f)
          (string->url/vec "file:/c:/abc/def.html")
          => #("file" #f #f #f #t (#("c:") #("abc") #("def.html")) () #f)
          (string->url/vec "file:\\\\d\\c\\abc\\def.html")
          => #("file" #f "d" #f #t (#("c") #("abc") #("def.html")) () #f)
          ;; Despite parsing as an "absolute" URL, will convert to a relative
          ;; Windows path:
          (string->url/vec "file:///x/y")
          => #("file" #f "" #f #t (#("x") #("y")) () #f)
          ;; No path-element decoding when special parsing is triggered:
          (string->url/vec "file://c:/a%20b")
          => #("file" #f "" #f #t (#("c:") #("a%20b")) () #f)
          (string->url/vec "file:\\\\\\\\d\\c\\a%20b")
          => #("file" #f "" #f #t (#("") #("d") #("c") #("a%20b")) () #f)
          ;; Path-element decoding applies for proper URL encodings:
          (string->url/vec "file:///c:/a%20b")
          => #("file" #f "" #f #t (#("c:") #("a b")) () #f)
          (string->url/vec "file://d/c/a%20b")
          => #("file" #f "d" #f #t (#("c") #("a b")) () #f)))
          

  (parameterize ([file-url-path-convention-type 'unix])
    ;; but no effect on http://foo:/...
    (test (string->url/vec "http://foo:/abc/def.html")
          => #("http" #f "foo" #f #t (#("abc") #("def.html")) () #f)))
  (parameterize ([file-url-path-convention-type 'windows])
    (test (string->url/vec "http://foo:/abc/def.html")
          => #("http" #f "foo" #f #t (#("abc") #("def.html")) () #f)))

  (test (url->string (path->url (bytes->path #"/a/b" 'unix)))
        => "file:///a/b"
        (url->string (path->url (bytes->path #"/a/b/" 'unix)))
        => "file:///a/b/"
        (url->string (path->url (bytes->path #"c:\\a\\b" 'windows)))
        => "file:///c:/a/b"
        (url->string (path->url (bytes->path #"c:\\a\\b\\" 'windows)))
        => "file:///c:/a/b/"
        (url->string (path->url (bytes->path #"\\\\?\\c:\\a\\b" 'windows)))
        => "file:///c:/a/b"
        (url->string (path->url (bytes->path #"\\\\?\\c:\\a\\b\\" 'windows)))
        => "file:///c:/a/b/")

  (test
   (path->bytes (url->path (path->url (bytes->path #"/a/b/c" 'unix)) 'unix))
   => #"/a/b/c"
   (path->bytes (url->path (path->url (bytes->path #"/a/b/c/" 'unix)) 'unix))
   => #"/a/b/c/."
   (path->bytes (url->path (path->url (bytes->path #"a/b/c" 'unix)) 'unix))
   => #"a/b/c"
   (path->bytes (url->path (path->url (bytes->path #"a/b/c/" 'unix)) 'unix))
   => #"a/b/c/."
   (path->bytes (url->path (path->url (bytes->path #"c:/a/b" 'windows)) 'windows))
   => #"c:\\a\\b"
   (path->bytes (url->path (path->url (bytes->path #"c:/a/b/" 'windows)) 'windows))
   => #"c:\\a\\b\\."
   (path->bytes (url->path (path->url (bytes->path #"a/b" 'windows)) 'windows))
   => #"a\\b"
   (path->bytes (url->path (path->url (bytes->path #"a/b/" 'windows)) 'windows))
   => #"a\\b\\."
   (path->bytes (url->path (path->url (bytes->path #"//d/c/a" 'windows)) 'windows))
   => #"\\\\d\\c\\a"
   (path->bytes (url->path (path->url (bytes->path #"//d/c/a/" 'windows)) 'windows))
   => #"\\\\d\\c\\a\\."
   (path->bytes (url->path (path->url (bytes->path #"\\\\?\\c:\\a\\b" 'windows)) 'windows))
   => #"c:\\a\\b"
   (path->bytes (url->path (path->url (bytes->path #"\\\\?\\UNC\\d\\c\\a\\b" 'windows)) 'windows))
   => #"\\\\d\\c\\a\\b"
   (path->bytes (url->path (path->url (bytes->path #"\\\\?\\c:\\a/x\\b" 'windows)) 'windows))
   => #"\\\\?\\c:\\a/x\\b"
   (path->bytes (url->path (path->url (bytes->path #"\\\\?\\UNC\\d\\\\c\\a/x\\b" 'windows)) 'windows))
   => #"\\\\?\\UNC\\d\\c\\a/x\\b" 
   ;; Supoprt proper encoding of UNC paths:
   (path->bytes (url->path (vec->url #("file" #f "m" #f #t (#("d")) () #f)) 'windows))
   => #"\\\\m\\d\\"
   (path->bytes (url->path (vec->url #("file" #f "m" #f #t (#("d") #("x")) () #f)) 'windows))
   => #"\\\\m\\d\\x"
   ;; Supoprt old encoding of UNC paths:
   (path->bytes (url->path (vec->url #("file" #f "" #f #t (#("") #("m") #("d")) () #f)) 'windows))
   => #"\\\\m\\d\\"
   (path->bytes (url->path (vec->url #("file" #f "" #f #t (#("") #("m") #("d") #("x")) () #f)) 'windows))
   => #"\\\\m\\d\\x")
   

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
  (test (string->url/vec
         "HTTP://ROBBY@WWW.DRSCHEME.ORG:80/INDEX.HTML;XXX?T=P#YYY")
        => #("http" "ROBBY" "www.drscheme.org" 80 #t (#("INDEX.HTML" "XXX")) ((T . "P")) "YYY"))

  (test-s->u #("mailto" #f #f #f #f (#("robby@racket-lang.org")) () #f)
             "mailto:robby@racket-lang.org")

  ;; The following two tests are not really correct: they rely on the URL
  ;; decoding silently passing un-encoded text as is instead of barfing.  (Eg,
  ;; using these URLs in a browser and then copy-pasting it from the address
  ;; should get you a properly encoded string instead.)
  (test (string->url/vec "http://www.drscheme.org?bar=馨慧")
        #("http" #f "www.drscheme.org" #f #f () ((bar . "馨慧")) #f))
  (test (string->url/vec "http://www.drscheme.org?bár=é")
        => #("http" #f "www.drscheme.org" #f #t () ((bár . "é")) #f))

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

  ;; tests from rfc 3986
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

  (test (relative-path->relative-url-string "a")
        => "a")
  (test (relative-path->relative-url-string "a/")
        => "a/")
  (test (relative-path->relative-url-string "a/b")
        => "a/b")
  (test (relative-path->relative-url-string (build-path "a" 'up "b"))
        => "a/../b")
  (test (relative-path->relative-url-string (build-path "a" 'same "b" 'up))
        => "a/./b/../")
  (test (relative-path->relative-url-string (build-path "a&c;" 'same "b"))
        => "a%26c%3B/./b")
  (test (relative-path->relative-url-string (bytes->path '#"\\\\?\\REL\\a\\b/c\\d" 'windows))
        => "a/b%2Fc/d")

  )

(module+ test (require (submod ".." main))) ; for raco test & drdr
