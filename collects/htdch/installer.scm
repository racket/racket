(module installer mzscheme
  (require (prefix geo: (lib "installer.ss" "htdch" "geometry"))
           (prefix color: (lib "installer.ss" "htdch" "colors"))
           (prefix draw: (lib "installer.ss" "htdch" "draw"))
           (prefix idraw: (lib "installer.ss" "htdch" "idraw"))
           (prefix graph: (lib "installer.ss" "htdch" "graphics"))
           )
  (provide installer)

  (define (installer plthome)
    (geo:installer plthome)
    (color:installer plthome)
    (draw:installer plthome)
    (idraw:installer plthome)
    (graph:installer plthome))

  )
