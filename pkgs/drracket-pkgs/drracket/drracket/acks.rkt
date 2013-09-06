#lang racket/base
(require acks/acks)

(provide get-general-acks
         get-translating-acks
         get-authors)

(define (get-translating-acks)
  (string-append
   "Thanks to "
   "Sung Gyeong Bae, "
   "Ian Barland, "
   "Yujeong Cho, "
   "Jae sung Chung, "
   "Biep Durieux, "
   "Tim Hanson, "
   "Jieung Kim, "
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
