;;;
;;; <search-test.ss> ---- List searching functions tests
;;; Time-stamp: <05/12/16 21:16:26 noel>
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

(module search-test
  mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (all-except (lib "search.ss" "srfi" "1") member))

  (provide search-tests)

  (define search-tests
    (make-test-suite
     "List search tests"

     ;; FIND

     (make-test-case
      "find:in-null-list"
      (assert-true (not (find (lambda (x) #t) '()))))

     (make-test-case
      "find:in-singleton-list"
      (assert-eq? (find (lambda (x) #t) '(Aurora))
                  'Aurora))

     (make-test-case
      "find:not-in-singleton-list"
      (assert-true (not (find (lambda (x) #f) '(Austinville)))))

     (make-test-case
      "find:at-front-of-longer-list"
      (assert-eq?
       (find (lambda (x) #t) '(Avery Avoca Avon Ayrshire Badger))
       'Avery))

     (make-test-case
      "find:in-middle-of-longer-list"
      (assert =
              (find even? '(149 151 153 155 156 157 159))
              156))

     (make-test-case
      "find:at-end-of-longer-list"
      (assert =
              (find even? '(161 163 165 167 168))
              168))

     (make-test-case
      "find:not-in-longer-list"
      (assert-true
       (not
        (find (lambda (x) #f)
              '(Bagley Bailey Badwin Balfour Balltown)))))

;;; FIND-TAIL

     (make-test-case
      "find-tail:in-null-list"
      (assert-true (not (find-tail (lambda (x) #t) '()))))

     (make-test-case
      "find-tail:in-singleton-list"
      (let ((source '(Ballyclough)))
        (assert-eq?
         (find-tail (lambda (x) #t) source)
         source)))

     (make-test-case
      "find-tail:not-in-singleton-list"
      (assert-true (not (find-tail (lambda (x) #f) '(Bancroft)))))

     (make-test-case
      "find-tail:at-front-of-longer-list"
      (let ((source '(Bangor Bankston Barney Barnum Bartlett)))
        (assert-eq?
         (find-tail (lambda (x) #t) source)
         source)))

     (make-test-case
      "find-tail:in-middle-of-longer-list"
      (let ((source '(169 171 173 175 176 177 179)))
        (assert-eq?
         (find-tail even? source)
         (cddddr source))))

     (make-test-case
      "find-tail:at-end-of-longer-list"
      (let ((source '(181 183 185 187 188)))
        (assert-eq?
         (find-tail even? source)
         (cddddr source))))

     (make-test-case
      "find-tail:not-in-longer-list"
      (assert-true
       (not
        (find-tail (lambda (x) #f)
                   '(Batavia Bauer Baxter Bayard Beacon)) )))

;;; ANY

     (make-test-case
      "any:in-one-null-list"
      (assert-true (not (any values '()))))

     (make-test-case
      "any:in-one-singleton-list"
      (assert-equal? (any vector '(Beaconsfield)) '#(Beaconsfield)))

     (make-test-case
      "any:not-in-one-singleton-list"
      (assert-true (not (any (lambda (x) #f) '(Beaman)))))

     (make-test-case
      "any:at-beginning-of-one-longer-list"
      (assert-equal?
       (any vector '(Beaver Beaverdale Beckwith Bedford Beebeetown))
       '#(Beaver)))

     (make-test-case
      "any:in-middle-of-one-longer-list"
      (assert =
              (any (lambda (x) (and (odd? x) (+ x 189)))
                   '(190 192 194 196 197 198 200))
              386))

     (make-test-case
      "any:at-end-of-one-longer-list"
      (assert =
              (any (lambda (x) (and (odd? x) (+ x 201)))
                   '(202 204 206 208 209))
              410))

     (make-test-case
      "any:not-in-one-longer-list"
      (assert-true
       (not (any (lambda (x) #f)
                 '(Beech Belinda Belknap Bellefountain Bellevue)))))

     (make-test-case
      "any:in-several-null-lists"
      (assert-true
       (not (any vector '() '() '() '() '()))))

     (make-test-case
      "any:in-several-singleton-lists"
      (assert-equal?
       (any vector
            '(Belmond)
            '(Beloit)
            '(Bennett)
            '(Benson)
            '(Bentley))
       '#(Belmond Beloit Bennett Benson Bentley)))

     (make-test-case
      "any:not-in-several-singleton-lists"
      (assert-true
       (not
        (any (lambda arguments #f)
             '(Benton)
             '(Bentonsport)
             '(Berea)
             '(Berkley)
             '(Bernard)))))

     (make-test-case
      "any:at-beginning-of-several-longer-lists"
      (assert-equal?
       (any vector
            '(Berne Bertram Berwick Bethesda Bethlehem Bettendorf
                    Beulah)
            '(Bevington Bidwell Bingham Birmingham Bladensburg
                        Blairsburg Blairstown)
            '(Blakesburg Blanchard Blencoe Bliedorn Blockton
                         Bloomfield Bloomington)
            '(Bluffton Bode Bolan Bonair Bonaparte Bondurant Boone)
            '(Booneville Botany Botna Bouton Bowsher Boxholm Boyd))
       '#(Berne Bevington Blakesburg Bluffton Booneville)))

     (make-test-case
      "any:in-middle-of-several-longer-lists"
      (assert =
              (any (lambda arguments
                     (let ((sum (apply + arguments)))
                       (and (odd? sum) (+ sum 210))))
                   '(211 212 213 214 215 216 217)
                   '(218 219 220 221 222 223 224)
                   '(225 226 227 228 229 230 231)
                   '(232 233 234 235 236 237 238)
                   '(240 242 244 246 247 248 250))
              1359))

     (make-test-case
      "any:at-end-of-several-longer-lists"
      (assert =
              (any (lambda arguments
                     (let ((sum (apply + arguments)))
                       (and (even? sum) (+ sum 210))))
                   '(252 253 254 255 256 257 258)
                   '(259 260 261 262 263 264 265)
                   '(266 267 268 269 270 271 272)
                   '(273 274 275 276 277 278 279)
                   '(281 283 285 287 289 291 292))
              1576))

     (make-test-case
      "any:not-in-several-longer-lists"
      (assert-true
       (not
        (any (lambda arguments #f)
             '(Boyden Boyer Braddyville Bradford Bradgate Brainard
                      Brandon)
             '(Brayton Brazil Breda Bridgewater Brighton Bristol
                       Bristow)
             '(Britt Bromley Brompton Bronson Brooklyn Brooks
                     Brookville)
             '(Browns Brownville Brunsville Brushy Bryant Bryantsburg
                      Buchanan)
             '(Buckeye Buckhorn Buckingham Bucknell Budd Buffalo
                       Burchinal)))))

     (make-test-case
      "any:not-in-lists-of-unequal-length"
      (assert-true
       (not (any (lambda arguments #f)
                 '(Burdette Burlington Burnside Burt)
                 '(Bushville Bussey)
                 '(Buxton Cairo Calamus)
                 '(Caledonia Clahoun Callender Calmar Caloma Calumet)))))

;;; EVERY

     (make-test-case
      "every:in-one-null-list"
      (assert-true (every values '())))

     (make-test-case
      "every:in-one-singleton-list"
      (assert-equal?
       (every vector '(Camanche))
       '#(Camanche)))

     (make-test-case
      "every:not-in-one-singleton-list"
      (assert-true
       (not (every (lambda (x) #f) '(Cambria)))))

     (make-test-case
      "every:failing-at-beginning-of-one-longer-list"
      (assert-true
       (not
        (every (lambda (x) #f)
               '(Cambridge Cameron Canby Canton Cantril)) )))

     (make-test-case
      "every:failing-in-middle-of-one-longer-list"
      (assert-true
       (not
        (every (lambda (x) (and (even? x) (+ x 293)))
               '(294 296 298 300 301 302 304)))))

     (make-test-case
      "every:failing-at-end-of-one-longer-list"
      (assert-true
       (not
        (every (lambda (x) (and (even? x) (+ x 305)))
               '(306 308 310 312 313)))))

     (make-test-case
      "every:in-one-longer-list"
      (assert-equal?
       (every vector
              '(Carbon Carbondale Carl Carlisle Carmel))
       '#(Carmel)))

     (make-test-case
      "every:in-several-null-lists"
      (assert-true
       (every vector '() '() '() '() '())))

     (make-test-case
      "every:in-several-singleton-lists"
      (assert-equal?
       (every vector
              '(Carnarvon)
              '(Carnes)
              '(Carney)
              '(Carnforth)
              '(Carpenter))
       '#(Carnarvon Carnes Carney Carnforth Carpenter)))

     (make-test-case
      "every:not-in-several-singleton-lists"
      (assert-true
       (not
        (every (lambda arguments #f)
               '(Carroll)
               '(Carrollton)
               '(Carrville)
               '(Carson)
               '(Cartersville)))))

     (make-test-case
      "every:failing-at-beginning-of-several-longer-lists"
      (assert-true
       (not
        (every (lambda arguments #f)
               '(Cascade Casey Castalia Castana Cattese Cedar
                         Centerdale)
               '(Centerville Centralia Ceres Chapin Chariton
                             Charleston Charlotte)
               '(Chatsworth Chautauqua Chelsea Cheney Cherokee Chester
                            Chickasaw)
               '(Chillicothe Churchtown Churchville Churdan Cincinnati
                             Clare Clarence)
               '(Clarinda Clarion Clark Clarkdale Clarksville Clayton
                          Clearfield))
        )))

     (make-test-case
      "every:failing-in-middle-of-several-longer-lists"
      (assert-true
       (not
        (every (lambda arguments
                 (let ((sum (apply + arguments)))
                   (and (odd? sum) (+ sum 314))))
               '(315 316 317 318 319 320 321)
               '(322 323 324 325 326 327 328)
               '(329 330 331 332 333 334 335)
               '(336 337 338 339 340 341 342)
               '(343 345 347 349 350 351 353))
        )))

     (make-test-case
      "every:failing-at-end-of-several-longer-lists"
      (assert-true
       (not
        (every (lambda arguments
                 (let ((sum (apply + arguments)))
                   (and (odd? sum) (+ sum 354))))
               '(355 356 357 358 359 360 361)
               '(362 363 364 365 366 367 368)
               '(369 370 371 372 373 374 375)
               '(376 377 378 379 380 381 382)
               '(383 385 387 389 391 393 394))
        )))

     (make-test-case
      "every:in-several-longer-lists"
      (assert-equal?
       (every vector
              '(Cleghorn Clemons Clermont Cleves Cliffland Climax
                         Clinton)
              '(Clio Clive Cloverdale Clucas Clutier Clyde Coalville)
              '(Coburg Coggon Coin Colesburg Colfax Collett Collins)
              '(Colo Columbia Colwell Commerce Communia Competine
                     Concord)
              '(Conesville Confidence Cono Conover Conrad Conroy
                           Consol))
       '#(Clinton Coalville Collins Concord Consol)))

     (make-test-case
      "every:in-lists-of-unequal-length"
      (assert-equal?
       (every vector
              '(Conway Cool Cooper Coppock)
              '(Coralville Corley)
              '(Cornelia Cornell Corning)
              '(Correctionville Corwith Corydon Cosgrove Coster
                                Cotter))
       '#(Cool Corley Cornell Corwith)))

;;; LIST-INDEX

     (make-test-case
      "list-index:in-one-null-list"
      (assert-true
       (not (list-index (lambda (x) #t) '()))))

     (make-test-case
      "list-index:in-one-singleton-list"
      (assert-true
       (zero?
        (list-index (lambda (x) #t) '(Cottonville)))))

     (make-test-case
      "list-index:not-in-one-singleton-list"
      (assert-true
       (not (list-index (lambda (x) #f) '(Coulter)))))

     (make-test-case
      "list-index:at-front-of-one-longer-list"
      (assert-true
       (zero?
        (list-index (lambda (x) #t)
                    '(Covington Craig Cranston Crathorne
                                Crawfordsville)))))
     (make-test-case
      "list-index:in-middle-of-one-longer-list"
      (list-index even? '(395 397 399 401 402 403 405))
      (lambda (result) (= result 4)))

     (make-test-case
      "list-index:at-end-of-one-longer-list"
      (assert =
              (list-index odd? '(406 408 410 412 414 415))
              5))

     (make-test-case
      "list-index:not-in-one-longer-list"
      (assert-true
       (not
        (list-index (lambda (x) #f)
                    '(Crescent Cresco Creston Crocker Crombie)))))

     (make-test-case
      "list-index:in-several-null-lists"
      (assert-true
       (not (list-index (lambda arguments #t) '() '() '() '() '()))))

     (make-test-case
      "list-index:in-several-singleton-lists"
      (assert-true
       (zero?      (list-index (lambda arguments #t)
                               '(Cromwell)
                               '(Croton)
                               '(Cumberland)
                               '(Cumming)
                               '(Curlew)))))

     (make-test-case
      "list-index:not-in-several-singleton-lists"
      (assert-true
       (not      (list-index (lambda arguments #f)
                             '(Cushing)
                             '(Cylinder)
                             '(Dahlonega)
                             '(Dalby)
                             '(Dale)))))

     (make-test-case
      "list-index:at-front-of-several-longer-lists"
      (assert-true
       (zero? (list-index (lambda arguments #t)
                          '(Dallas Dana Danbury Danville Darbyville
                                   Davenport Dawson)
                          '(Dayton Daytonville Dean Decorah Dedham Deerfield
                                   Defiance)
                          '(Delaware Delhi Delmar Deloit Delphos Delta
                                     Denhart)
                          '(Denison Denmark Denova Denver Depew Derby Devon)
                          '(Dewar Dexter Diagonal Dickens Dickieville Dike
                                  Dillon)))))

     (make-test-case
      "list-index:in-middle-of-several-longer-lists"
      (assert =
              (list-index (lambda arguments (odd? (apply + arguments)))
                          '(416 417 418 419 420 421 422)
                          '(423 424 425 426 427 428 429)
                          '(430 431 432 433 434 435 436)
                          '(437 438 439 440 441 442 443)
                          '(444 446 448 450 451 452 454))
              4))

     (make-test-case
      "list-index:at-end-of-several-longer-lists"
      (assert =
              (list-index (lambda arguments (even? (apply + arguments)))
                          '(455 456 457 458 459 460)
                          '(461 462 463 464 465 466)
                          '(467 468 469 470 471 472)
                          '(473 474 475 476 477 478)
                          '(479 481 483 485 487 488))
              5))

     (make-test-case
      "list-index:not-in-several-longer-lists"
      (assert-true
       (not
        (list-index (lambda arguments #f)
                    '(Dinsdale Dixon Dodgeville Dolliver Donahue
                               Donnan Donnelley)
                    '(Donnellson Doon Dorchester Doris Douds Dougherty
                                 Douglas)
                    '(Doney Dows Drakesville Dresden Dubuque Dudley
                            Dumfries)
                    '(Dumont Dunbar Duncan Duncombe Dundee Dunkerton
                             Dunlap)
                    '(Durango Durant Durham Dutchtown Dyersville
                              Dysart Earlham)))))

     ))
  )

;;; search-test.ss ends here
