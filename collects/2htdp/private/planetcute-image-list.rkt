#lang racket/base
(provide images name->filename)

(define (name->filename img)
  (define-values (_ new-chars)
    (for/fold ([cap-next? #t]
               [chars '()])
      ([c (in-string (format "~a" img))])
      (values (equal? #\- c)
              (cons (cond
                      [(equal? c #\-) #\space]
                      [cap-next? (char-upcase c)]
                      [else c])
                    chars))))
  (format "~a.png" (apply string (reverse new-chars))))

(define images
  '(brown-block
    character-boy
    character-cat-girl
    character-horn-girl
    character-pink-girl
    character-princess-girl
    chest-closed
    chest-lid
    chest-open
    dirt-block
    door-tall-closed
    door-tall-open
    enemy-bug
    gem-blue
    gem-green
    gem-orange
    grass-block
    heart
    key
    plain-block
    ramp-north
    ramp-south
    ramp-west
    roof-east
    roof-north-east
    roof-north-west
    roof-north
    roof-south-east
    roof-south-west
    roof-south
    roof-west
    shadow-east
    shadow-north-east
    shadow-north-west
    shadow-north
    shadow-side-west
    shadow-south-east
    shadow-south-west
    shadow-south
    shadow-west
    stone-block-tall
    stone-block
    tree-short
    tree-tall
    tree-ugly
    wall-block-tall
    wall-block
    water-block
    window-tall
    wood-block))