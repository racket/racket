#lang racket/base
(require rackunit
         (only-in racket/sandbox with-limits)
         openssl)

;; Test that (ssl-connect _ 443 'secure) does connect to well-known sites
;; and that it rejects connections to some known insecure sites.

(module+ test
  (run-tests (list racket-hosts)
             (list bad-hosts)))

(module+ main
  (run-tests (list racket-hosts popular-hosts)
             (list bad-hosts more-bad-hosts)))

;; ------------------------------------------------------------

(define-logger test)

(define TIMEOUT-SEC 3)

;; Racket sites
(define racket-hosts
  '(racket-lang.org
    www.racket-lang.org
    docs.racket-lang.org
    download.racket-lang.org
    mirror.racket-lang.org
    pkgs.racket-lang.org
    planet.racket-lang.org
    pre-release.racket-lang.org
    htdp.org
    ))

;; Popular sites
(define popular-hosts
  '(;; https://www.similarweb.com/top-websites/
    google.com
    youtube.com
    facebook.com
    twitter.com
    instagram.com
    baidu.com
    wikipedia.org
    yandex.ru
    yahoo.com
    whatsapp.com
    amazon.com
    netflix.com
    live.com
    yahoo.co.jp
    reddit.com
    tiktok.com
    vk.com
    office.com
    discord.com
    zoom.us
    linkedin.com
    naver.com
    twitch.tv
    bing.com
    roblox.com
    mail.ru
    duckduckgo.com
    qq.com
    pinterest.com
    bilibili.com
    microsoft.com
    msn.com
    news.yahoo.co.jp
    fandom.com
    login.microsoftonline.com
    ebay.com
    samsung.com
    google.com.br
    globo.com
    accuweather.com
    ok.ru
    docomo.ne.jp
    weather.com
    bbc.co.uk
    amazon.co.jp

    ;; https://www.similarweb.com/top-websites/category/law-and-government/
    ;; (Maybe likely to use different CAs from commercial sites.)
    gov.uk
    service.gov.uk
    dpboss.net
    gosuslugi.ru
    www.cowin.gov.in
    www.turkiye.gov.tr
    www.japanpost.jp
    noticiasconcursos.com.br
    correios.com.br
    irs.gov
    canada.ca
    royalmail.com
    europa.eu
    gov.br
    mos.ru
    acesso.gov.br
    argentina.gob.ar
    ny.gov
    sarkariresult.com
    ssa.gov
    va.gov

    ;; Assorted other international sites
    ;; https://www.similarweb.com/top-websites/$COUNTRY
    google.fr
    orange.fr
    amazon.fr
    leboncoin.fr
    legossip.net
    doctolib.fr
    www.programme-tv.net
    lefigaro.fr
    francetvinfo.fr
    credit-agricole.fr
    www.ouest-france.fr
    google.de
    amazon.de
    bild.de
    ebay.de
    t-online.de
    ebay-kleinanzeigen.de
    web.de
    gmx.net
    dhl.de
    spiegel.de
    paypal.com
    focus.de
    seznam.cz
    novinky.cz
    idnes.cz
    stream.cz
    super.cz
    google.cz
    sport.cz
    blesk.cz
    aktualne.cz
    centrum.cz
    alza.cz
    iprima.cz
    heureka.cz
    bazos.cz
    rakuten.co.jp
    google.co.jp
    auone.jp
    fc2.com
    www.pixiv.net
    syosetu.com
    blog.jp
    line.me
    nicovideo.jp
    shopee.vn
    24h.com.vn
    google.com.vn
    kenh14.vn
    ))

(define (test-good-host host)
  (test-case (format "good host ~a" host)
    (define hostname (format "~a" host))
    (define-values (in out)
      (with-limits TIMEOUT-SEC #f
        (ssl-connect hostname 443 'secure)))
    (close-output-port out)
    (close-input-port in)
    (log-test-debug "ok, connected to ~a" host)))

(define bad-hosts
  '(;; Invalid certificate
    expired.badssl.com
    wrong.host.badssl.com
    self-signed.badssl.com
    untrusted-root.badssl.com
    ;; revoked.badssl.com
    ))

(define more-bad-hosts
  '(;; Insufficient security level
    rc4-md5.badssl.com
    rc4.badssl.com
    3des.badssl.com
    null.badssl.com
    sha1-2017.badssl.com
    sha1-intermediate.badssl.com
    dh480.badssl.com
    dh512.badssl.com
    ;; dh1024.badssl.com
    ;; dh2048.badssl.com
    ;; dh-small-subgroup.badssl.com
    ;; dh-composite.badssl.com
    ))

(define (test-bad-host bad-host)
  (test-case (format "bad host ~a" bad-host)
    (define hostname (format "~a" bad-host))
    (check-exn #rx"connect failed"
               (lambda ()
                 (with-limits TIMEOUT-SEC #f
                   (ssl-connect hostname 443 'secure))))
    (log-test-debug "ok, rejected ~a" bad-host)))

(define (run-tests good-hostss bad-hostss)
  (for* ([hosts good-hostss] [host hosts])
    (test-good-host host))
  (for* ([bad-hosts bad-hostss] [bad-host bad-hosts])
    (test-bad-host bad-host)))
