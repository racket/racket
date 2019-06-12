set BUILD_LEVEL=3m
call build.bat

..\..\Racket3m.exe -O "info@compiler/cm" -l- setup --boot ../setup-go.rkt ../build/compiled ^
                  ignored ../build/ignored.d ^
                  csbuild.rkt ^
                  --racketcs-suffix "" --pull ^
                  -- --depth 1

..\..\Racket.exe -N "raco" -l- setup
