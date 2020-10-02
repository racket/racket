set BUILD_LEVEL=bc
call build.bat

..\..\RacketBC.exe -O "info@compiler/cm" -l- setup --boot ../setup-go.rkt ../build/compiled ^
                  ignored ../build/ignored.d ^
                  csbuild.rkt ^
                  --racketcs-suffix "" --pull

..\..\Racket.exe -N "raco" -l- setup
