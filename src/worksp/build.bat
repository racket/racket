cd racket
devenv racket.sln /Build Release
cd ..\gracket
devenv gracket.sln /Build Release
cd ..

cd gc2
..\..\..\racketcgc -cu make.rkt
cd ..

..\..\racket -cu ..\gracket\get-libs.rkt ..\gracket ..\..\lib

cd mzstart
devenv mzstart.sln /Build Release
cd ..\mrstart
devenv mrstart.sln /Build Release
cd ..

cd mzcom
devenv mzcom.sln /Build Release
cd ..\libmysterx
devenv libmysterx.sln /Build Release
cd ..

cd libmysterx
..\..\..\racket -cu xform.rkt
cd ..

cd mzcom
..\..\..\racket -cu xform.rkt
cd ..

cd mzcom
devenv mzcom.sln /Build 3m
cd ..\libmysterx
devenv libmysterx.sln /Build 3m
cd ..

..\..\racket -l setup -N "raco setup"
