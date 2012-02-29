#lang typed/racket/base
(require "../private/utils.rkt")
(require/typed/provide 
 racket/date
 [date->seconds (case-> (date -> Integer)
                        (date Any -> Integer))]
 [date->string (case-> (date -> String)
                       (date Any -> String))]
 [current-date (-> date)]
 [date-display-format (Parameterof (U 'american 'chinese 'german 'indian 
                                      'irish 'iso-8601 'rfc2822 'julian))]
 [find-seconds (case-> (Integer Integer Integer Integer Integer Integer -> Integer)
                       (Integer Integer Integer Integer Integer Integer Any -> Integer))]
 [date->julian/scalinger (date -> Integer)]
 [julian/scalinger->string (Integer -> String)])
