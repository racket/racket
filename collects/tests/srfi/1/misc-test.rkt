;;;
;;; <misc-test.rkt> ---- Misc list procedure tests
;;; Time-stamp: <05/12/16 21:15:50 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; SRFI-1 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-1 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-1; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Originally created by:

;; John David Stone
;; Department of Mathematics and Computer Science
;; Grinnell College
;; stone@math.grin.edu

(module misc-test
  mzscheme
  (require rackunit)
  (require (all-except srfi/1/misc append! reverse!)
   (rename srfi/1/misc s:append! append!)
   (rename srfi/1/misc s:reverse! reverse!))

  (provide misc-tests)

  (define misc-tests
    (test-suite
     "Miscellaneous list procedures tests"

     ;; ZIP

     (test-case
      "zip:all-lists-empty"
      (check-true (null? (zip '() '() '() '() '()))))

     (test-case
      "zip:one-list"
      (check-equal? (zip '(Pisces Puppis Reticulum))
                     '((Pisces) (Puppis) (Reticulum))))

     (test-case
      "zip:two-lists"
      (check-equal? (zip '(Sagitta Sagittarius Scorpio Scutum Serpens)
                          '(Sextans Taurus Telescopium Triangulum Tucana))
                     '((Sagitta Sextans)
                       (Sagittarius Taurus)
                       (Scorpio Telescopium)
                       (Scutum Triangulum)
                       (Serpens Tucana))))

     (test-case
      "zip:short-lists"
      (check-equal? (zip '(Vela) '(Virgo) '(Volens) '(Vulpecula))
                     '((Vela Virgo Volens Vulpecula))))

     (test-case
      "zip:several-lists"
      (check-equal? (zip '(actinium aluminum americium antimony argon)
                          '(arsenic astatine barium berkeleium beryllium)
                          '(bismuth boron bromine cadmium calcium)
                          '(californium carbon cerium cesium chlorine)
                          '(chromium cobalt copper curium dysprosium)
                          '(einsteinium erbium europium fermium fluorine)
                          '(francium gadolinium gallium germanium gold))
                     '((actinium arsenic bismuth californium
                                 chromium einsteinium francium)
                       (aluminum astatine boron carbon cobalt
                                 erbium gadolinium)
                       (americium barium bromine cerium copper
                                  europium gallium)
                       (antimony berkeleium cadmium cesium curium
                                 fermium germanium)
                       (argon beryllium calcium chlorine
                              dysprosium fluorine gold))))

     ;; UNZIP2

     (test-case
      "unzip2:empty-list-of-lists"
      (let-values (((firsts seconds) (unzip2 '())))
        (check-true (and (null? firsts) (null? seconds)))))

     (test-case
      "unzip2:singleton-list-of-lists"
      (let-values (((firsts seconds) (unzip2 '((retriever rottweiler)))))
        (check-true (and (equal? firsts '(retriever))
                          (equal? seconds '(rottweiler))))))

     (test-case
      "unzip2:longer-list-of-lists"
      (let-values (((firsts seconds)
                    (unzip2 '((saluki samoyed)
                              (shipperke schnauzer)
                              (setter shepherd)
                              (skye spaniel)
                              (spitz staghound)))))
        (check-true (and (equal? firsts '(saluki shipperke setter skye spitz))
                          (equal? seconds '(samoyed schnauzer shepherd spaniel
                                                    staghound))))))

     (test-case
      "unzip2:lists-with-extra-elements"
      (let-values (((firsts seconds)
                    (unzip2 '((terrier turnspit vizsla wiemaraner)
                              (whippet wolfhound)
                              (bells bones bongo carillon celesta)
                              (chimes clappers conga)))))
        (check-true (and (equal? firsts '(terrier whippet bells chimes))
                          (equal? seconds
                                  '(turnspit wolfhound bones clappers))))))

     ;; UNZIP3

     (test-case
      "unzip3:empty-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '())))
        (check-true (and (null? firsts) (null? seconds) (null? thirds)))))

     (test-case
      "unzip3:singleton-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((cymbals gamelan glockenspiel)))))
        (check-true (and (equal? firsts '(cymbals))
                          (equal? seconds '(gamelan))
                          (equal? thirds '(glockenspiel))))))

     (test-case
      "unzip3:longer-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((gong handbells kettledrum)
                              (lyra maraca marimba)
                              (mbira membranophone metallophone)
                              (nagara naker rattle)
                              (sizzler snappers tabor)))))
        (check-true (and (equal? firsts '(gong lyra mbira nagara sizzler))
                          (equal? seconds '(handbells maraca membranophone naker
                                                      snappers))
                          (equal? thirds '(kettledrum marimba metallophone rattle
                                                      tabor))))))

     (test-case
      "unzip3:lists-with-extra-elements"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((tambourine timbrel timpani tintinnabula tonitruone)
                              (triangle vibraphone xylophone)
                              (baccarat banker bezique bingo bridge canasta)
                              (casino craps cribbage euchre)))))
        (check-true (and (equal? firsts '(tambourine triangle baccarat casino))
                          (equal? seconds '(timbrel vibraphone banker craps))
                          (equal? thirds
                                  '(timpani xylophone bezique cribbage))))))

     ;; UNZIP4

     (test-case
      "unzip4:empty-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '())))
        (check-true (and (null? firsts)
                          (null? seconds)
                          (null? thirds)
                          (null? fourths)))))

     (test-case
      "unzip4:singleton-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((fantan faro gin hazard)))))
        (check-true (and (equal? firsts '(fantan))
                          (equal? seconds '(faro))
                          (equal? thirds '(gin))
                          (equal? fourths '(hazard))))))

     (test-case
      "unzip4:longer-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((hearts keno loo lottery)
                              (lotto lowball monte numbers)
                              (ombre picquet pinball pinochle)
                              (poker policy quinze romesteq)
                              (roulette rum rummy skat)))))
        (check-true (and (equal? firsts '(hearts lotto ombre poker roulette))
                          (equal? seconds '(keno lowball picquet policy rum))
                          (equal? thirds '(loo monte pinball quinze rummy))
                          (equal? fourths
                                  '(lottery numbers pinochle romesteq skat))))))

     (test-case
      "unzip4:lists-with-extra-elements"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((adamant agate alexandrite amethyst aquamarine
                                       beryl)
                              (bloodstone brilliant carbuncle carnelian)
                              (chalcedony chrysoberyl chrysolite chrysoprase
                                          citrine coral demantoid)
                              (diamond emerald garnet girasol heliotrope)))))
        (check-true (and (equal? firsts '(adamant bloodstone chalcedony diamond))
                          (equal? seconds '(agate brilliant chrysoberyl emerald))
                          (equal? thirds
                                  '(alexandrite carbuncle chrysolite garnet))
                          (equal? fourths
                                  '(amethyst carnelian chrysoprase girasol))))))

     ;; UNZIP5

     (test-case
      "unzip5:empty-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '())))
        (check-true
         (and (null? firsts)
              (null? seconds)
              (null? thirds)
              (null? fourths)
              (null? fifths)))))

     (test-case
      "unzip5:singleton-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((hyacinth jacinth jade jargoon jasper)))))
        (lambda (firsts seconds thirds fourths fifths)
          (and (equal? firsts '(hyacinth))
               (equal? seconds '(jacinth))
               (equal? thirds '(jade))
               (equal? fourths '(jargoon))
               (equal? fifths '(jasper))))))

     (test-case
      "unzip5:longer-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((kunzite moonstone morganite onyx opal)
                              (peridot plasma ruby sapphire sard)
                              (sardonyx spinel star sunstone topaz)
                              (tourmaline turquoise zircon Argus basilisk)
                              (Bigfoot Briareus bucentur Cacus Caliban)))))
        (check-true
         (and (equal? firsts
                      '(kunzite peridot sardonyx tourmaline Bigfoot))
              (equal? seconds
                      '(moonstone plasma spinel turquoise Briareus))
              (equal? thirds '(morganite ruby star zircon bucentur))
              (equal? fourths '(onyx sapphire sunstone Argus Cacus))
              (equal? fifths '(opal sard topaz basilisk Caliban))))))

     (test-case
      "unzip5:lists-with-extra-elements"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((centaur Cerberus Ceto Charybdis chimera cockatrice
                                       Cyclops)
                              (dipsas dragon drake Echidna Geryon)
                              (Gigantes Gorgon Grendel griffin Harpy hippocampus
                                        hippocentaur hippocerf)
                              (hirocervus Hydra Kraken Ladon manticore Medusa)))))
        (check-true
         (and (equal? firsts '(centaur dipsas Gigantes hirocervus))
              (equal? seconds '(Cerberus dragon Gorgon Hydra))
              (equal? thirds '(Ceto drake Grendel Kraken))
              (equal? fourths '(Charybdis Echidna griffin Ladon))
              (equal? fifths '(chimera Geryon Harpy manticore))))))

     ;; APPEND!

     (test-case
      "append!:no-arguments"
      (check-true (null? (s:append!))))

     (test-case
      "append!:one-argument"
      (check-equal? (s:append! (list 'mermaid 'merman 'Minotaur))
                     '(mermaid merman Minotaur)))

     (test-case
      "append!:several-arguments"
      (check-equal?
       (s:append! (list 'nixie 'ogre 'ogress 'opinicus)
                  (list 'Orthos)
                  (list 'Pegasus 'Python)
                  (list 'roc 'Sagittary 'salamander 'Sasquatch 'satyr)
                  (list 'Scylla 'simurgh 'siren))
       '(nixie ogre ogress opinicus Orthos Pegasus
               Python roc Sagittary salamander Sasquatch
               satyr Scylla simurgh siren)))

     (test-case
      "append!:some-null-arguments"
      (check-equal?
       (s:append! (list) (list) (list 'Sphinx 'Talos 'troll) (list)
                  (list 'Typhoeus) (list) (list) (list))
       '(Sphinx Talos troll Typhoeus)))

     (test-case
      "append!:all-null-arguments"
      (check-true (null? (s:append! (list) (list) (list) (list) (list)))))

     ;; APPEND-REVERSE

     (test-case
      "append-reverse:first-argument-null"
      (check-equal? (append-reverse '() '(Typhon unicorn vampire werewolf))
                     '(Typhon unicorn vampire werewolf)))

     (test-case
      "append-reverse:second-argument-null"
      (check-equal? (append-reverse '(windigo wivern xiphopagus yeti zombie) '())
                     '(zombie yeti xiphopagus wivern windigo)))

     (test-case
      "append-reverse:both-arguments-null"
      (check-true (null? (append-reverse '() '()))))

     (test-case
      "append-reverse:neither-argument-null"
      (check-equal?
       (append-reverse '(Afghanistan Albania Algeria Andorra)
                       '(Angola Argentina Armenia))
       '(Andorra Algeria Albania Afghanistan Angola
                 Argentina Armenia)))

     ;; APPEND-REVERSE!

     (test-case
      "append-reverse!:first-argument-null"
      (check-equal? (append-reverse! (list)
                                      (list 'Australia 'Austria 'Azerbaijan))
                     '(Australia Austria Azerbaijan)))

     (test-case
      "append-reverse!:second-argument-null"
      (check-equal? (append-reverse! (list 'Bahrain 'Bangladesh 'Barbados
                                            'Belarus 'Belgium)
                                      (list))
                     '(Belgium Belarus Barbados Bangladesh Bahrain)))

     (test-case
      "append-reverse!:both-arguments-null"
      (check-true (null? (append-reverse! (list) (list)))))

     (test-case
      "append-reverse!:neither-argument-null"
      (check-equal? (append-reverse! (list 'Belize 'Benin 'Bhutan 'Bolivia)
                                      (list 'Bosnia 'Botswana 'Brazil))
                     '(Bolivia Bhutan Benin Belize Bosnia Botswana Brazil)))

     ;; REVERSE!

     (test-case
      "reverse!:empty-list"
      (check-true (null? (s:reverse! (list)))))

     (test-case
      "reverse!:singleton-list"
      (check-equal? (s:reverse! (list 'Brunei))
                     '(Brunei)))

     (test-case
      "reverse!:longer-list"
      (check-equal? (s:reverse! (list 'Bulgaria 'Burundi 'Cambodia 'Cameroon
                                       'Canada))
                     '(Canada Cameroon Cambodia Burundi Bulgaria)))

     ))
  )

;;; misc-test.rkt ends here
