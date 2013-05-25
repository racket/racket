#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/ticks.rkt")
(provide
 (contract-out (struct pre-tick ([value real?] [major? boolean?]))
               (struct (tick pre-tick) ([value real?] [major? boolean?] [label string?]))
               (struct ticks ([layout ticks-layout/c] [format ticks-format/c])))
 24h-descending-date-ticks-formats 12h-descending-date-ticks-formats
 24h-descending-time-ticks-formats 12h-descending-time-ticks-formats
 us-currency-scales uk-currency-scales eu-currency-scales
 us-currency-formats uk-currency-formats eu-currency-formats
 ticks-layout/c ticks-format/c
 no-ticks-layout no-ticks-format no-ticks
 (activate-contract-out ticks-default-number
                        ticks-mimic ticks-scale ticks-add linear-scale
                        linear-ticks-layout linear-ticks-format linear-ticks
                        log-ticks-layout log-ticks-format log-ticks
                        date-ticks-formats date-ticks-layout date-ticks-format date-ticks
                        time-ticks-formats time-ticks-layout time-ticks-format time-ticks
                        bit/byte-ticks-format bit/byte-ticks
                        currency-ticks-scales currency-ticks-formats
                        currency-ticks-format currency-ticks
                        fraction-ticks-format fraction-ticks
                        collapse-ticks
                        contour-ticks format-tick-labels))
