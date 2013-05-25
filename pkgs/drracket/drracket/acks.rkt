#lang racket/base
(require acks/acks)

(provide get-general-acks
         get-translating-acks
         get-authors)

(define (get-translating-acks)
  (string-append
   "Thanks to "
   "Ian Barland, "
   "Biep Durieux, "
   "Tim Hanson, "
   "Chihiro Kuraya, "
   "Philippe Meunier, "
   "Irina Mintiy, "
   "Sergey Semerikov, "
   "Francisco Solsona, "
   "Mike Sperber, "
   "Jens Axel Søgaard, "
   "Reini Urban, "
   "ChongKai Zhu, "
   "and "
   "Paolo Zoppetti "
   "for their help translating DrRacket's GUI to other languages."))
