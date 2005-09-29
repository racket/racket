(append (map interface? (list Color<%> Posn<%> ColorPosn<%>))
        (map class? (list PosnC% ColorC% ColorPosnC%))
        (map mixin? (list $ColorPosnC-mixin)))
