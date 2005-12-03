(module acks mzscheme
  (provide get-general-acks
           get-translating-acks
           get-authors)
  
  (define (get-authors)
    (string-append
     "DrScheme was written by "
     "John Clements, "
     "Matthias Felleisen, "
     "Robby Findler, "
     "Paul Graunke, "
     "Matthew Flatt, "
     "Shriram Krishnamurthi, "
     "and "
     "Paul Steckler."))
  
  (define (get-general-acks)
    (string-append
     "Thanks to "
     "Ian Barland, "
     "Eli Barzilay, "
     "Gann Bierner, "
     "Richard Cobbe, "
     "Moy Easwaran, "
     "Kathi Fisler, "
     "Cormac Flanagan, "
     "Sebastian Good, "
     "Kathy Gray, "
     "Bruce Hauman, "
     "Mark Krentel, "
     "Mario Latendresse, "
     "Scott Owens, "
     "Jamie Raymond, "
     "Paul Schlie, "
     "Dorai Sitaram, "
     "Mike Sperber, "
     "Francisco Solsona, "
     "Neil W. Van Dyke, "
     "Anton van Straaten, "
     "Stephanie Weirich, "
     "Noel Welsh, "
     "and "
     "Adam Wick "
     "for contributions of prototypes, libraries, testing, and criticism of PLT documentation."))

  (define (get-translating-acks)
    (string-append
     "Thanks to "
     "ChongKai Zhu, "
     "Ian Barland, "
     "Biep Durieux, "
     "Tim Hanson, "
     "Chihiro Kuraya, "
     "Philippe Meunier, "
     "Jens Axel Søgaard, "
     "Francisco Solsona, "
     "Mike Sperber, "
     "Reini Urban, "
     "and "
     "Paolo Zoppetti "
     "for their help translating DrScheme's GUI to other languages.")))
