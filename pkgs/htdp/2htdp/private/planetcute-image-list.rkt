#lang racket/base
(provide images name->filename)

(define (name->filename img)
  (format "~a.png" img))

(define images
  '((Characters
     character-boy
     character-cat-girl
     character-horn-girl
     character-pink-girl
     character-princess-girl
     enemy-bug
     speech-bubble)

    (Items
     chest-closed
     chest-lid
     chest-open
     gem-blue
     gem-green
     gem-orange
     heart
     key
     rock
     selector
     tree-short
     tree-tall
     tree-ugly
     yellow-star)

    (Blocks
     brown-block
     dirt-block
     grass-block
     plain-block
     stone-block-tall
     stone-block
     wall-block-tall
     wall-block
     water-block
     wood-block)

    (Ramps
     ramp-east
     ramp-north
     ramp-south
     ramp-west)

    (Buildings
     door-tall-closed
     door-tall-open
     roof-east
     roof-north-east
     roof-north-west
     roof-north
     roof-south-east
     roof-south-west
     roof-south
     roof-west
     window-tall)

    (Shadows
     shadow-east
     shadow-north-east
     shadow-north-west
     shadow-north
     shadow-side-west
     shadow-south-east
     shadow-south-west
     shadow-south
     shadow-west)))
