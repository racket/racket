(module acks mzscheme
  (provide get-general-acks
           get-translating-acks
           get-authors)
  
  (define (get-authors)
    (get-general-acks))
  
  (define (get-general-acks)
    (string-append
     "The following individuals contributed to the implementation"
     " of DrScheme and associated tools: "
     "Ian Barland, "
     "Eli Barzilay, "
     "Gann Bierner, "
     "John Clements, "
     "Richard Cobbe, "
     "Greg Cooper, "
     "Ryan Culpepper, "
     "Carl Eastlund, "
     "Moy Easwaran, "
     "Matthias Felleisen, "
     "Robby Findler, "
     "Kathi Fisler, "
     "Cormac Flanagan, "
     "Matthew Flatt, "
     "Sebastian Good, "
     "Paul Graunke, "
     "Kathy Gray, "
     "Dan Grossman, "
     "Bruce Hauman, "
     "Dave Herman, "
     "Mark Krentel, "
     "Shriram Krishnamurthi, "
     "Mario Latendresse, "
     "Guillaume Marceau, "
     "Jacob Matthews, "
     "Jay McCarthy, "
     "Philippe Meunier, "
     "Scott Owens, "
     "Jamie Raymond, "
     "Paul Schlie, "
     "Dorai Sitaram, "
     "Mike Sperber, "
     "Paul Steckler, "
     "Jens Axel Søgaard, "
     "Francisco Solsona, "
     "Sam Tobin-Hochstadt, "
     "Neil W. Van Dyke, "
     "Anton van Straaten, "
     "Dale Vaillancourt, "
     "Stephanie Weirich, "
     "Noel Welsh, "
     "Adam Wick, "
     "and "
     "ChongKai Zhu."))

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
