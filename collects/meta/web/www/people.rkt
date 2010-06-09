#lang at-exp s-exp "shared.rkt"

;;TODO: combine the info in outreach+research.rkt into a list of structs here
(define places
  (ul (map (lambda (p)
             (let-values ([(place-name place-url person-name person-url)
                           (apply values p)])
               @li{@a[href: place-url]{@place-name}}))
           '(("Brown University, Providence, RI"
              "http://www.cs.brown.edu/research/plt/"
              "Shriram Krishnamurthi"
              "http://www.cs.brown.edu/~sk/")
             ("Brigham Young University, Provo, UT"
              "http://faculty.cs.byu.edu/~jay/"
              "Jay McCarthy"
              "http://faculty.cs.byu.edu/~jay/")
             ("California Polytechnic State University, San Luis Obispo, CA"
              "http://users.csc.calpoly.edu/~clements/"
              "John Clements"
              "http://users.csc.calpoly.edu/~clements/")
             ("Northeastern University, Boston, MA"
              "http://www.ccs.neu.edu/scheme/"
              "Matthias Felleisen"
              "http://www.ccs.neu.edu/home/matthias/")
             ("Northwestern University, Evanston, IL"
              "http://plt.eecs.northwestern.edu/"
              "Robert Bruce Findler"
              "http://www.eecs.northwestern.edu/~robby/")
             ("University of Utah, Salt Lake City, UT"
              "http://www.cs.utah.edu/plt/"
              "Matthew Flatt"
              "http://www.cs.utah.edu/~mflatt/")
             ("Worcester Polytechnic Institute"
              "http://web.cs.wpi.edu/~kfisler/"
              "Kathi Fisler"
              "http://web.cs.wpi.edu/~kfisler/")))))

(provide people)
(define people
  @page{
    @p{@|ldquo|PLT@|rdquo| refers to the group that is the core of the Racket
       development team.  PLT consists of numerous people distributed across
       several different universities in the USA: @places}
    @h4{Affiliates}
    @p{We work with
       @ul{@li{The crew at @a[href: "http://untyped.com"]{Untyped}}
           @li{Dorai Sitaram, GTE Labs}
           @li{Mike Sperber, Universität Tübingen}}
       In particular, please check out the Racket-related work being done at
       @|-schematics|.}
    @h4{And ...}
    @p{Finally, Racket is supported by an band of volunteers who contribute not
       only code and documentation but also infectious enthusiasm@|mdash|too
       many to name but whose help and encouragement make this fun and
       worthwhile.}})
