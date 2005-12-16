;;;
;;; <misc-test.ss> ---- Misc list procedure tests
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

  (require
   (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
   (all-except (lib "misc.ss" "srfi" "1") append! reverse!)
   (rename (lib "misc.ss" "srfi" "1") s:append! append!)
   (rename (lib "misc.ss" "srfi" "1") s:reverse! reverse!))

  (provide misc-tests)

  (define misc-tests
    (make-test-suite
     "Miscellaneous list procedures tests"

     ;; ZIP

     (make-test-case
      "zip:all-lists-empty"
      (assert-true (null? (zip '() '() '() '() '()))))

     (make-test-case
      "zip:one-list"
      (assert-equal? (zip '(Pisces Puppis Reticulum))
                     '((Pisces) (Puppis) (Reticulum))))

     (make-test-case
      "zip:two-lists"
      (assert-equal? (zip '(Sagitta Sagittarius Scorpio Scutum Serpens)
                          '(Sextans Taurus Telescopium Triangulum Tucana))
                     '((Sagitta Sextans)
                       (Sagittarius Taurus)
                       (Scorpio Telescopium)
                       (Scutum Triangulum)
                       (Serpens Tucana))))

     (make-test-case
      "zip:short-lists"
      (assert-equal? (zip '(Vela) '(Virgo) '(Volens) '(Vulpecula))
                     '((Vela Virgo Volens Vulpecula))))

     (make-test-case
      "zip:several-lists"
      (assert-equal? (zip '(actinium aluminum americium antimony argon)
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

     (make-test-case
      "unzip2:empty-list-of-lists"
      (let-values (((firsts seconds) (unzip2 '())))
        (assert-true (and (null? firsts) (null? seconds)))))

     (make-test-case
      "unzip2:singleton-list-of-lists"
      (let-values (((firsts seconds) (unzip2 '((retriever rottweiler)))))
        (assert-true (and (equal? firsts '(retriever))
                          (equal? seconds '(rottweiler))))))

     (make-test-case
      "unzip2:longer-list-of-lists"
      (let-values (((firsts seconds)
                    (unzip2 '((saluki samoyed)
                              (shipperke schnauzer)
                              (setter shepherd)
                              (skye spaniel)
                              (spitz staghound)))))
        (assert-true (and (equal? firsts '(saluki shipperke setter skye spitz))
                          (equal? seconds '(samoyed schnauzer shepherd spaniel
                                                    staghound))))))

     (make-test-case
      "unzip2:lists-with-extra-elements"
      (let-values (((firsts seconds)
                    (unzip2 '((terrier turnspit vizsla wiemaraner)
                              (whippet wolfhound)
                              (bells bones bongo carillon celesta)
                              (chimes clappers conga)))))
        (assert-true (and (equal? firsts '(terrier whippet bells chimes))
                          (equal? seconds
                                  '(turnspit wolfhound bones clappers))))))

     ;; UNZIP3

     (make-test-case
      "unzip3:empty-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '())))
        (assert-true (and (null? firsts) (null? seconds) (null? thirds)))))

     (make-test-case
      "unzip3:singleton-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((cymbals gamelan glockenspiel)))))
        (assert-true (and (equal? firsts '(cymbals))
                          (equal? seconds '(gamelan))
                          (equal? thirds '(glockenspiel))))))

     (make-test-case
      "unzip3:longer-list-of-lists"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((gong handbells kettledrum)
                              (lyra maraca marimba)
                              (mbira membranophone metallophone)
                              (nagara naker rattle)
                              (sizzler snappers tabor)))))
        (assert-true (and (equal? firsts '(gong lyra mbira nagara sizzler))
                          (equal? seconds '(handbells maraca membranophone naker
                                                      snappers))
                          (equal? thirds '(kettledrum marimba metallophone rattle
                                                      tabor))))))

     (make-test-case
      "unzip3:lists-with-extra-elements"
      (let-values (((firsts seconds thirds)
                    (unzip3 '((tambourine timbrel timpani tintinnabula tonitruone)
                              (triangle vibraphone xylophone)
                              (baccarat banker bezique bingo bridge canasta)
                              (casino craps cribbage euchre)))))
        (assert-true (and (equal? firsts '(tambourine triangle baccarat casino))
                          (equal? seconds '(timbrel vibraphone banker craps))
                          (equal? thirds
                                  '(timpani xylophone bezique cribbage))))))

     ;; UNZIP4

     (make-test-case
      "unzip4:empty-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '())))
        (assert-true (and (null? firsts)
                          (null? seconds)
                          (null? thirds)
                          (null? fourths)))))

     (make-test-case
      "unzip4:singleton-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((fantan faro gin hazard)))))
        (assert-true (and (equal? firsts '(fantan))
                          (equal? seconds '(faro))
                          (equal? thirds '(gin))
                          (equal? fourths '(hazard))))))

     (make-test-case
      "unzip4:longer-list-of-lists"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((hearts keno loo lottery)
                              (lotto lowball monte numbers)
                              (ombre picquet pinball pinochle)
                              (poker policy quinze romesteq)
                              (roulette rum rummy skat)))))
        (assert-true (and (equal? firsts '(hearts lotto ombre poker roulette))
                          (equal? seconds '(keno lowball picquet policy rum))
                          (equal? thirds '(loo monte pinball quinze rummy))
                          (equal? fourths
                                  '(lottery numbers pinochle romesteq skat))))))

     (make-test-case
      "unzip4:lists-with-extra-elements"
      (let-values (((firsts seconds thirds fourths)
                    (unzip4 '((adamant agate alexandrite amethyst aquamarine
                                       beryl)
                              (bloodstone brilliant carbuncle carnelian)
                              (chalcedony chrysoberyl chrysolite chrysoprase
                                          citrine coral demantoid)
                              (diamond emerald garnet girasol heliotrope)))))
        (assert-true (and (equal? firsts '(adamant bloodstone chalcedony diamond))
                          (equal? seconds '(agate brilliant chrysoberyl emerald))
                          (equal? thirds
                                  '(alexandrite carbuncle chrysolite garnet))
                          (equal? fourths
                                  '(amethyst carnelian chrysoprase girasol))))))

     ;; UNZIP5

     (make-test-case
      "unzip5:empty-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '())))
        (assert-true
         (and (null? firsts)
              (null? seconds)
              (null? thirds)
              (null? fourths)
              (null? fifths)))))

     (make-test-case
      "unzip5:singleton-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((hyacinth jacinth jade jargoon jasper)))))
        (lambda (firsts seconds thirds fourths fifths)
          (and (equal? firsts '(hyacinth))
               (equal? seconds '(jacinth))
               (equal? thirds '(jade))
               (equal? fourths '(jargoon))
               (equal? fifths '(jasper))))))

     (make-test-case
      "unzip5:longer-list-of-lists"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((kunzite moonstone morganite onyx opal)
                              (peridot plasma ruby sapphire sard)
                              (sardonyx spinel star sunstone topaz)
                              (tourmaline turquoise zircon Argus basilisk)
                              (Bigfoot Briareus bucentur Cacus Caliban)))))
        (assert-true
         (and (equal? firsts
                      '(kunzite peridot sardonyx tourmaline Bigfoot))
              (equal? seconds
                      '(moonstone plasma spinel turquoise Briareus))
              (equal? thirds '(morganite ruby star zircon bucentur))
              (equal? fourths '(onyx sapphire sunstone Argus Cacus))
              (equal? fifths '(opal sard topaz basilisk Caliban))))))

     (make-test-case
      "unzip5:lists-with-extra-elements"
      (let-values (((firsts seconds thirds fourths fifths)
                    (unzip5 '((centaur Cerberus Ceto Charybdis chimera cockatrice
                                       Cyclops)
                              (dipsas dragon drake Echidna Geryon)
                              (Gigantes Gorgon Grendel griffin Harpy hippocampus
                                        hippocentaur hippocerf)
                              (hirocervus Hydra Kraken Ladon manticore Medusa)))))
        (assert-true
         (and (equal? firsts '(centaur dipsas Gigantes hirocervus))
              (equal? seconds '(Cerberus dragon Gorgon Hydra))
              (equal? thirds '(Ceto drake Grendel Kraken))
              (equal? fourths '(Charybdis Echidna griffin Ladon))
              (equal? fifths '(chimera Geryon Harpy manticore))))))

     ;; APPEND!

     (make-test-case
      "append!:no-arguments"
      (assert-true (null? (s:append!))))

     (make-test-case
      "append!:one-argument"
      (assert-equal? (s:append! (list 'mermaid 'merman 'Minotaur))
                     '(mermaid merman Minotaur)))

     (make-test-case
      "append!:several-arguments"
      (assert-equal?
       (s:append! (list 'nixie 'ogre 'ogress 'opinicus)
                  (list 'Orthos)
                  (list 'Pegasus 'Python)
                  (list 'roc 'Sagittary 'salamander 'Sasquatch 'satyr)
                  (list 'Scylla 'simurgh 'siren))
       '(nixie ogre ogress opinicus Orthos Pegasus
               Python roc Sagittary salamander Sasquatch
               satyr Scylla simurgh siren)))

     (make-test-case
      "append!:some-null-arguments"
      (assert-equal?
       (s:append! (list) (list) (list 'Sphinx 'Talos 'troll) (list)
                  (list 'Typhoeus) (list) (list) (list))
       '(Sphinx Talos troll Typhoeus)))

     (make-test-case
      "append!:all-null-arguments"
      (assert-true (null? (s:append! (list) (list) (list) (list) (list)))))

     ;; APPEND-REVERSE

     (make-test-case
      "append-reverse:first-argument-null"
      (assert-equal? (append-reverse '() '(Typhon unicorn vampire werewolf))
                     '(Typhon unicorn vampire werewolf)))

     (make-test-case
      "append-reverse:second-argument-null"
      (assert-equal? (append-reverse '(windigo wivern xiphopagus yeti zombie) '())
                     '(zombie yeti xiphopagus wivern windigo)))

     (make-test-case
      "append-reverse:both-arguments-null"
      (assert-true (null? (append-reverse '() '()))))

     (make-test-case
      "append-reverse:neither-argument-null"
      (assert-equal?
       (append-reverse '(Afghanistan Albania Algeria Andorra)
                       '(Angola Argentina Armenia))
       '(Andorra Algeria Albania Afghanistan Angola
                 Argentina Armenia)))

     ;; APPEND-REVERSE!

     (make-test-case
      "append-reverse!:first-argument-null"
      (assert-equal? (append-reverse! (list)
                                      (list 'Australia 'Austria 'Azerbaijan))
                     '(Australia Austria Azerbaijan)))

     (make-test-case
      "append-reverse!:second-argument-null"
      (assert-equal? (append-reverse! (list 'Bahrain 'Bangladesh 'Barbados
                                            'Belarus 'Belgium)
                                      (list))
                     '(Belgium Belarus Barbados Bangladesh Bahrain)))

     (make-test-case
      "append-reverse!:both-arguments-null"
      (assert-true (null? (append-reverse! (list) (list)))))

     (make-test-case
      "append-reverse!:neither-argument-null"
      (assert-equal? (append-reverse! (list 'Belize 'Benin 'Bhutan 'Bolivia)
                                      (list 'Bosnia 'Botswana 'Brazil))
                     '(Bolivia Bhutan Benin Belize Bosnia Botswana Brazil)))

     ;; REVERSE!

     (make-test-case
      "reverse!:empty-list"
      (assert-true (null? (s:reverse! (list)))))

     (make-test-case
      "reverse!:singleton-list"
      (assert-equal? (s:reverse! (list 'Brunei))
                     '(Brunei)))

     (make-test-case
      "reverse!:longer-list"
      (assert-equal? (s:reverse! (list 'Bulgaria 'Burundi 'Cambodia 'Cameroon
                                       'Canada))
                     '(Canada Cameroon Cambodia Burundi Bulgaria)))

     ))
  )

;;; misc-test.ss ends here
