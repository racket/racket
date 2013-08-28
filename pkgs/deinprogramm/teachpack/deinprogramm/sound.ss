(module sound mzscheme
  (require (lib "mred.ss" "mred"))

  (define (play-sound-file file)
    (play-sound file #f)
    (void))

  (define (background-play-sound-file file)
    (play-sound file #t)
    (void))

  (provide play-sound-file
           background-play-sound-file))
