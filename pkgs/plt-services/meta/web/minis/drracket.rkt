#lang meta/web

(define-context "drracket")

;; This just shows an ascii logo and redirects to the main page.  Thanks
;; to Can Burak Cilingir for both the logo and the drracket.org domain
;; name.

(define index
  (page #:title "DrRacket" #:extra-headers (lazy headers) (center-div logo)))

(require (only-in "../www/all.rkt" main))

(define (logo)
  (define (text->lines text)
    (cond [(null? text) '()]
          [(null? (cdr text))
           (if (equal? "\n" (car text)) '() text)]
          [(equal? "\n" (cadr text))
           (cons (car text) (text->lines (cddr text)))]
          [else
           (text->lines (cons (string-append (car text) (cadr text))
                              (cddr text)))]))
  (define (do-line chars colors)
    (let loop ([chars (map (λ (c) (if (eq? #\space c) nbsp c))
                           (string->list chars))]
               [colors (string->list colors)])
      (let* ([class (car colors)]
             [n (for/or ([color (in-list (cdr colors))] [i (in-naturals 1)])
                  (and (not (equal? class color)) i))]
             [block (if n (take chars n) chars)]
             [block (if (eq? #\space class)
                      block (span class: (list "c" class) block))])
        (if n
          (cons block (loop (drop chars n) (drop colors n)))
          (list block)))))
  (pre style: "font-family: monospace, fixed; font-weight: bold;"
    (add-newlines (map do-line (text->lines char-matrix)
                               (text->lines color-matrix)))))

(define (headers)
  (list
   (style/inline type: 'text/css
     (add-newlines (map (λ (c) @list{.c@(car c) {
                                       color: #@(cadr c);
                                       background-color: #@(caddr c);
                                     }})
                        colors)))
   (meta http-equiv: "refresh"
         content: (list "2;URL=" (url-of main)))))

(define char-matrix
  @list|{
                           88XSS%
                    .888;@88@:88@XXS%tt
                8@Xt      tX88X X88@XS%%t:.
              8%:..        ::t@88;88@XX%%t;:..
            @t.            8: :X@ %888XX%%t;:. :
          8 .   . .        8   .S8%888@XS%%t;:. .S
         @                      .:8S888@XStt;;:. .;
        X.   .              8.   :% ;888XS%tt;::. .t
       @:  .   . . .        8%    t@8X88@X%%;;::.  :t
      8: .                  %     .SX%88@XStt;::.. .;S
     8S      .      .      S      .%S8888XS%t;::.. .:tX
     8; .  .    .     .    S   8;  t@8@X@XS%t;::.. .:;S
    @@:  .   .     .      8.8   8..%@8X8@XStt;:... .:;%@
    @X:    .     .     .  ;8     8XS88@X88%%;::..  ..;%@
    @@: .     .     .   X;.%      888Xtt88St;::..  .:;tX
    @8;    .          .X..X        ;@St; .8;;:..  ..:;%@
    X8S. .      .  .  8  8        ;S8;;.  8X::.  ...;t%@
    %@@%.    .       X  ;        %%% :. ;tX%;:.  ..:;tS8
    tX8X;.       .  8. .X      .S@888@.:t%%@;:  ..::t%S8
    :%X8Xt:.  .   .8 . 8      ;X8888X8:;%S@X t...::;tS@@
     ;%@88%t;:.. . ;  S     :S@8888XXXX%SX888::.::;t%X8
     .;tX@8@XS%ttt:t%X   .tX@8888@XS%%SS@@88888.:;t%S8X
       :t%X8888@@:8888 S@888888XXS%t;tSX88888@@t;t%S@8
        :;t%S@88t%%% ;8888@@@SS%%t;;:::;8@88@XX @%X@8
        . :;t%S:....8@@@XSSS%%tt;:::....S8@8XS X S@8
         :. .:@t%;:.8%%%%ttt;;:::....   ;SXS X   8S
          t:. 88X@8@;;;;;;;:::...   ..:.:8 X    S8
            8X@88X%::.::::...   ...:::;tt;.  .S8
              88X:.:.........::::;;;tt%%SS@;8@
                8SS%tt;;;;;;t;tttt%%SSX@88X
                    @@XXSSSSSSSSXX@@888@
                          @@8888XS}|)

(define colors
  '([a "000" "00a"]
    [b "000" "555"]
    [c "000" "a00"]
    [d "00a" "000"]
    [e "00a" "555"]
    [f "00a" "55f"]
    [g "0aa" "555"]
    [h "555" "000"]
    [i "555" "00a"]
    [j "555" "a00"]
    [k "555" "aaa"]
    [l "55f" "00a"]
    [m "55f" "aaa"]
    [n "a00" "000"]
    [o "a00" "555"]
    [p "a00" "f55"]
    [q "a0a" "555"]
    [r "a0a" "55f"]
    [s "a0a" "a00"]
    [t "a0a" "aaa"]
    [u "a0a" "f55"]
    [v "a50" "555"]
    [w "a50" "a00"]
    [x "a50" "aaa"]
    [y "a50" "f55"]
    [z "aaa" "555"]
    [0 "aaa" "f55"]
    [1 "aaa" "fff"]
    [2 "f55" "a00"]
    [3 "f55" "aaa"]
    [4 "f5f" "f55"]
    [5 "fff" "aaa"]))

(define color-matrix
  @list|{
                           ifllll
                    qktx555155kflllllllg
                2pppppppp5111155fllllllllll
              pppppppppppp511111kffllllllllllv
            ppppppppppppppp1111115ffllllllllllla
          jpppppppppppppppp0111111rfflllllllllliaa
         ppppppppppppppppppp1111111ffllllllllllllaa
        pppppppppppppppppppp11111115ffllllllllllliaa
       ppppppppppppppppppppp11111111mfflllllllllllaaa
      pppppppppppppppppppppp111111115fflllllllllllaaaa
     2ppppppppppppppppppppp31111111115flllllllllllaaaaa
     pppppppppppppppppppppp115p1111111kfllllllllllaaaaa
    2ppppppppppppppppppppp111ppp1111155fllllllllllaaaaaa
    2pppppppppppppppppppp511ppppp1111555ifllllllllaaaaaa
    2ppppppppppppppppppp3114pppppp111555kllllllllaaaaaaa
    2pppppppppppppppppp3113pppppppp155555qfllllllaaaaaaa
    2ppppppppppppppppp0111pppppppppk155555illllliaaaaaaa
    22ppppppppppppppp3111ppppppppppp555kkkklillaaaaaaaaa
    222ppppppppppppp01113pppppppppppp5kkkkkklilaaaaaaaaa
    2222ppppppppppp01111pppppppppp222kkkkkkkqliaaaaaaaad
     2222pppppppppp1111ppppppppp222222kkkkkkkqlaaaaaaaa
     22222pppppppu1111yppppppp22222222okkkkkzkiiaaaaaad
      222222ppppy1511kppppp222222222222zkkzzzzziaaaaad
       22222222255555p22222222222222222skzzzzzzgiaaad
        c2222225555k222222222222222222w2okzzzzgzgaad
         cc222zkkkkqp222222222222222jjjwjzzzqzqqved
          ccjqkkkkk222222222222222wjcccccjqzqqqqbh
            ozzzzzj2j2j2j2j2w22jccccccccccqvqqbh
              ojjjccccjcccccccccccccccccccjgbh
                cccccccccccccccccccccccccnn
                    cccccccccccccccccnnn
                          nncnnnnn}|)
