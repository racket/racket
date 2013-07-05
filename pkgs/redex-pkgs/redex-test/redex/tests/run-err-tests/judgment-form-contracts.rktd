(#rx"input q at position 1"
 ([judgment (ctc-fail q s)])
 (judgment-holds judgment))
(#rx"output q at position 2"
 ([judgment (ctc-fail a s)])
 (judgment-holds judgment))
(#rx"input q at position 1"
 ([judgment (ctc-fail b s)])
 (judgment-holds judgment))
(#rx"output q at position 2"
 ([judgment (ctc-fail c s)])
 (judgment-holds judgment))
