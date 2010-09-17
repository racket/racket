#lang at-exp s-exp "shared.rkt"

(define -all-people- ; "First Last" bib-name|#f responsible-for|#f url
  '(["Shriram Krishnamurthi" #f "brown"
     "http://www.cs.brown.edu/~sk/"]
    ["Jay McCarthy" #f "byu"
     "http://faculty.cs.byu.edu/~jay/"]
    ["John Clements" #f "calpoly"
     "http://users.csc.calpoly.edu/~clements/"]
    ["Matthias Felleisen" #f "neu"
     "http://www.ccs.neu.edu/home/matthias/"]
    ["Robby Findler" "Robert Bruce Findler" "northwestern"
     "http://www.eecs.northwestern.edu/~robby/"]
    ["Matthew Flatt" #f "utah"
     "http://www.cs.utah.edu/~mflatt/"]
    ["Kathi Fisler" #f "wpi"
     "http://web.cs.wpi.edu/~kfisler/"]))

(define -all-places- ; "Name" "Location, ST" url pubs-url|#
  '(["Brown University" "Providence, RI"
     "http://www.cs.brown.edu/research/plt/"
     "http://www.cs.brown.edu/~sk/Publications/Papers/"]
    ["Brigham Young University" "Provo, UT"
     "http://faculty.cs.byu.edu/~jay/"
     #f]
    ["California Polytechnic State University" "San Luis Obispo, CA"
     "http://users.csc.calpoly.edu/~clements/"
     #f]
    ["Northeastern University" "Boston, MA"
     "http://www.ccs.neu.edu/scheme/"
     "http://www.ccs.neu.edu/scheme/pubs/"]
    ["Northwestern University" "Evanston, IL"
     "http://plt.eecs.northwestern.edu/"
     "http://www.eecs.northwestern.edu/~robby/pubs/"]
    ["University of Utah" "Salt Lake City, UT"
     "http://www.cs.utah.edu/plt/"
     "http://www.cs.utah.edu/plt/publications/"]
    ["Worcester Polytechnic Institute" "Worcester, MA"
     "http://web.cs.wpi.edu/~kfisler/"
     #f]))

(require racket/list racket/string)

(provide (struct-out person) (struct-out place))
(struct person (name bib-name responsible-for url edu search-string))
(struct place  (name location url pubs-url edu search-string))

;; list of all people and places, and functions to search them with some
;; (unique) word
(provide all-people all-places find-person find-place)
(define-values (all-people all-places find-person find-place)
  (let ()
    (define (make maker url-pos args-list)
      (for/list ([args (in-list args-list)])
        (let ([url (list-ref args url-pos)]
              [search (string-downcase (string-join (filter values args)
                                                    "; "))]
              [rx #rx"^http://[^/]*[.]([^.]+)[.]edu/"])
          (apply maker `(,@args
                         ,(cadr (or (regexp-match rx url)
                                    (error 'person/place
                                           "no X.edu found in ~a" url)))
                         ,search)))))
    (define people (make person 3 -all-people-))
    (define places (make place  2 -all-places-))
    (unless (equal? places (remove-duplicates places #:key place-edu))
      (error 'places "found multiple places with the same X.edu url"))
    (unless (equal? (sort (map place-edu places) string<?)
                    (sort (map person-responsible-for people) string<?))
      (error 'places/people "no direct mapping between `edu' of places and ~a"
             "responsible-for of people"))
    (define (finder name all get-search-string)
      (let ([t (make-hasheq)])
        (lambda (x)
          (let ([x (if (string? x) (string->symbol x) x)])
            (hash-ref!
             t x
             (lambda ()
               (let* ([rx (regexp-quote (string-downcase (symbol->string x)))]
                      [rx (pregexp (string-append "\\b" rx "\\b"))])
                 (let loop ([found #f] [l all])
                   (cond
                     [(null? l) (or found (error name "couldn't find: ~.a" x))]
                     [(not (regexp-match? rx (get-search-string (car l))))
                      (loop found (cdr l))]
                     [found (error name "ambiguous search: ~.a" x)]
                     [else (loop (car l) (cdr l))])))))))))
    (values people places
            (finder 'find-person people person-search-string)
            (finder 'find-place  places place-search-string))))

(provide people)
(define people
  @page[#:part-of 'community]{
    @p{“PLT” refers to the group that is the core of the Racket development
       team.  PLT consists of numerous people distributed across several
       different universities in the USA:
       @(ul (map (lambda (p)
                   @li{@a[href: (place-url p)]{
                         @(place-name p), @(place-location p)}})
                 all-places))}
    @h4{Affiliates}
    @p{We work with
       @ul*{@~ The crew at @a[href: "http://untyped.com"]{Untyped}
            @~ Dorai Sitaram, GTE Labs
            @~ Mike Sperber, Universität Tübingen}
       In particular, please check out the Racket-related work being done at
       @|-schematics|.}
    @h4{And ...}
    @p{Finally, Racket is supported by an band of volunteers who contribute not
       only code and documentation but also infectious enthusiasm—too many to
       name but whose help and encouragement make this fun and worthwhile.}})
