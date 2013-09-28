#lang meta/web

(require "resources.rkt")

(define (make-all place person)
  ;; The first person in a place is the one responsible for it
  (list
   (place ; -------------------------------------------------------------------
    'neu "Northeastern University"
    #:location "Boston, MA"
    #:url "http://www.ccs.neu.edu/racket/"
    #:pubs "http://www.ccs.neu.edu/racket/pubs/"
    (person 'matthias "Matthias Felleisen"
            #:url "http://www.ccs.neu.edu/home/matthias/")
    (person 'eli "Eli Barzilay"
            #:url "http://barzilay.org/"))
   (place ; -------------------------------------------------------------------
    'utah "University of Utah"
    #:location "Salt Lake City, UT"
    #:url "http://www.cs.utah.edu/plt/"
    #:pubs "http://www.cs.utah.edu/plt/publications/"
    (person 'mflatt "Matthew Flatt"
            #:url "http://www.cs.utah.edu/~mflatt/"))
   (place ; -------------------------------------------------------------------
    'nwu "Northwestern University"
    #:location "Evanston, IL"
    #:url "http://plt.eecs.northwestern.edu/"
    #:pubs "http://www.eecs.northwestern.edu/~robby/pubs/"
    (person 'robby "Robby Findler"
            #:bibname "Robert Bruce Findler"
            #:url "http://www.eecs.northwestern.edu/~robby/"))
   (place ; -------------------------------------------------------------------
    'brown "Brown University"
    #:location "Providence, RI"
    #:url "http://www.cs.brown.edu/research/plt/"
    #:pubs "http://www.cs.brown.edu/~sk/Publications/Papers/"
    (person 'shriram "Shriram Krishnamurthi"
            #:url "http://www.cs.brown.edu/~sk/"))
   (place ; -------------------------------------------------------------------
    'byu "Brigham Young University"
    #:location "Provo, UT"
    #:url "http://faculty.cs.byu.edu/~jay/"
    #:pubs "http://faculty.cs.byu.edu/~jay/home/#(part._pubs)"
    (person 'jay "Jay McCarthy"
            #:url "http://faculty.cs.byu.edu/~jay/"))
   (place ; -------------------------------------------------------------------
    'calpoly "California Polytechnic State University"
    #:location "San Luis Obispo, CA"
    #:url "http://users.csc.calpoly.edu/~clements/"
    #:pubs "http://www.brinckerhoff.org/clements/papers/"
    (person 'clements "John Clements"
            #:url "http://users.csc.calpoly.edu/~clements/"))
   (place ; -------------------------------------------------------------------
    'wpi "Worcester Polytechnic Institute"
    #:location "Worcester, MA"
    #:url "http://web.cs.wpi.edu/~kfisler/"
    (person 'kathi "Kathi Fisler"
            #:url "http://web.cs.wpi.edu/~kfisler/"))))

;; ----------------------------------------------------------------------------

(struct person (nick name url bibname place) #:prefab)
(struct place  (nick name location url pubs people) #:prefab)

(provide (struct-out place)  all-places find-place
         (struct-out person) all-people find-person)

(define-values (all-places find-place all-people find-person)
  (let ()
    (define places0
      (make-all
       ;; make a place
       (λ (nick name #:location loc #:url url #:pubs [pubs #f] . people)
         (place nick name loc url pubs people))
       ;; make a person
       (λ (nick name #:url url #:bibname [bibname name])
         (person nick name url bibname (make-placeholder #f)))))
    (for* ([place (in-list places0)]
           [person (in-list (place-people place))])
      (placeholder-set! (person-place person) place))
    (define places (make-reader-graph places0))
    (define people (append-map place-people places))
    (when (ormap (λ (p) (null? (place-people p))) places)
      (error 'places "all places should have people in them"))
    (define (make-finder what xs get-nick)
      (define t (make-hasheq))
      (for ([x (in-list xs)])
        (hash-update! t (get-nick x)
                      (λ (old) (if old (error what "got duplicate nicks") x))
                      #f))
      (λ (nick) (or (hash-ref t nick #f)
                    (error what "nick not found: ~s" nick))))
    (values places (make-finder 'places places place-nick)
            people (make-finder 'people people person-nick))))

(provide people)
(define people
  @page[#:window-title "Racket People: The PLT Group" #:part-of 'community
        #:description "PLT: the group that is the Racket development team."]{
    @p{“PLT” refers to the group that is the core of the Racket development
       team.  PLT consists of numerous people distributed across several
       different universities in the USA:
       @(ul (map (λ (p) @li{@a[href: (place-url p)]{
                              @(place-name p), @(place-location p)}})
                 all-places))}
    @p{Also, Racket is supported by a band of volunteers who contribute not
       only code and documentation but also infectious enthusiasm—too many to
       name but whose help and encouragement make this fun and worthwhile.}})
