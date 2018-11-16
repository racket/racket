set BUILD_LEVEL=cgc
call build.bat

..\..\RacketCGC.exe -O "info@compiler/cm" -l- setup --boot ../setup-go.rkt ../build/compiled ^
                    ignored ../build/ignored.d ^
                    csbuild.rkt ^
                    --racketcs-suffix "" --pull ^
                    -- --depth 1

..\..\Racket.exe -N "raco" -l- setup
