(module init '#%kernel
  (#%require "../private/top-int.rkt")

  (#%provide (all-from '#%kernel)
             #%top-interaction))
