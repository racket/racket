(append (map interface? (list MovingPoint<%>
                              Point3D<%>
                              Point<%>
                              ColorPoint<%>
                              Color<%>))
        (map class? (list ColorC%
                          Point3DC%
                          ColorPointC%
                          ColorMovingPointC%
                          MovingColorPointC%
                          PointC%
                          MovingPointC%))
        (map mixin? (list makeMobile-mixin
                          addColor-mixin
                          $Point3DC-mixin)))
