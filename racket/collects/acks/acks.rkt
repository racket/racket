#lang racket/base

(provide get-general-acks
         get-authors)

(require racket/match
         racket/string)

(define (get-authors)
  (get-general-acks))

;; The names are sorted alphabetically by last name.
;; After a modification, run `racket acks.rkt` to get the list of sorted names,
;; and copy-and-paste it here.
;;
;; Last name is automatically extracted from a name,
;; which might not work correctly on all names.
;; Use @ to manually mark the last name.
(define raw-names
  '("Jesse Alama"
    "Claire Alvis"
    "Leif Andersen"
    "Ross Angle"
    "Yavuz Arkun"
    "Michael Ballantyne"
    "Ian Barland"
    "Eli Barzilay"
    "Gann Bierner"
    "Stephen Bloch"
    "William J. Bowman"
    "Marc Burns"
    "Matthew Butterick"
    "Filipe Cabecinhas"
    "Wing Hei Chan"
    "Stephen Chang"
    "Richard Cleis"
    "John Clements"
    "Richard Cobbe"
    "Greg Cooper"
    "Ryan Culpepper"
    "Stephen @De Gabrielle"
    "Christos Dimoulas"
    "Eric Dobson"
    "Joel Dueck"
    "Carl Eastlund"
    "Moy Easwaran"
    "Will Farr"
    "Matthias Felleisen"
    "Dan Feltey"
    "Burke Fetscher"
    "Michael Filonenko"
    "Robby Findler"
    "Jacqueline Firth"
    "Kathi Fisler"
    "Cormac Flanagan"
    "Matthew Flatt"
    "Spencer Florence"
    "Fred Fu"
    "Tony Garnock-Jones"
    "Dionna Amalie Glaze"
    "Sebastian Good"
    "Paul Graunke"
    "Kathy Gray"
    "Ben Greenman"
    "Dan Grossman"
    "Arjun Guha"
    "Dave Gurnell"
    "Tobias Hammer"
    "Alex Harsányi"
    "William Hatch"
    "Bruce Hauman"
    "Greg Hendershott"
    "Dave Herman"
    "Blake Johnson"
    "Andrew Kent"
    "Alexis King"
    "Casey Klein"
    "Alex Knauth"
    "Geoffrey S. Knauth"
    "D. Ben Knoble"
    "Mark Krentel"
    "Shriram Krishnamurthi"
    "Mario Latendresse"
    "Xiangqi Li"
    "Guillaume Marceau"
    "Gustavo Massaccesi"
    "Paulo Matos"
    "Jacob Matthews"
    "Jay McCarthy"
    "Philip McGrath"
    "Mike T. McHenry"
    "Philippe Meunier"
    "Cameron Moy"
    "Max S. New"
    "Laurent Orseau"
    "Scott Owens"
    "Pavel Panchekha"
    "David T. Pierson"
    "Bogdan Popa"
    "Sorawee Porncharoenwase"
    "Jon Rafkind"
    "Jamie Raymond"
    "Grant Rettke"
    "Paul Schlie"
    "Dorai Sitaram"
    "Francisco Solsona"
    "Sarah Spall"
    "Mike Sperber"
    "Vincent St-Amour"
    "Paul Stansifer"
    "Paul Steckler"
    "Stevie Strickland"
    "James Swaine"
    "Jens Axel Søgaard"
    "Asumu Takikawa"
    "Kevin Tew"
    "Sam Tobin-Hochstadt"
    "Neil Toronto"
    "Milo Turner"
    "Dale Vaillancourt"
    "Neil @Van Dyke"
    "David @Van Horn"
    "Anton @van Straaten"
    "Dimitris Vyzovitis"
    "Stephanie Weirich"
    "Noel Welsh"
    "Adam Wick"
    "yjqww6"
    "Danny Yoo"
    "Shu-Hung You"
    "Jon Zeppieri"
    "ChongKai Zhu"))

;; strip the last name marker away
(define names (map (λ (s) (string-replace s "@" "")) raw-names))

(define (get-general-acks)
  (string-append
   "The following individuals contributed to the implementation"
   " and documentation of Racket: "
   (format-names names)
   "."))

;; The length of names should be longer than 2 for this to make sense.
(define (format-names names)
  (string-join names ", " #:before-last ", and "))

(define (extract-last name)
  (match name
    [(pregexp #px" @(.+)$" (list _ last)) last]
    [(pregexp #px" ([^ ]+)$" (list _ last)) last]
    [_ name]))

(define (get-sorted-names names)
  (sort names string-ci<? #:key extract-last))

(module+ main
  (require racket/pretty)
  (pretty-write `(define raw-names ',(get-sorted-names raw-names))))

(module+ test
  (define (check-equal? actual expected)
    (unless (equal? actual expected)
      (error 'acks "not equal\nfirst: ~s\nsecond: ~s" actual expected)))

  (check-equal? (extract-last "Andries @van Dam") "van Dam")
  (check-equal? (extract-last "Andries van Dam") "Dam")
  (check-equal? (extract-last "Oscar @De La Hoya") "De La Hoya")
  (check-equal? (extract-last "Oscar De La Hoya") "Hoya")

  (check-equal? (format-names '("Évariste Galois"
                                "Pierre-Simon Laplace"
                                "Pierre de Fermat"))
                "Évariste Galois, Pierre-Simon Laplace, and Pierre de Fermat")

  ;; main test to make sure the names are sorted correctly
  (check-equal? (get-sorted-names raw-names) raw-names))
