cd racket
devenv racket.sln /Build Release
cd ..\gracket
devenv gracket.sln /Build Release
cd ..

cd gc2
..\..\..\racketcgc -cu make.ss
cd ..

cd mzcom
devenv mzcom.sln /Build Release
cd ..\libmysterx
devenv libmysterx.sln /Build Release
cd ..

cd libmysterx
..\..\..\racket -cu xform.ss
cd ..

cd mzcom
..\..\..\racket -cu xform.ss
cd ..

cd mzcom
devenv mzcom.sln /Build 3m
cd ..\libmysterx
devenv libmysterx.sln /Build 3m
cd ..

cd mzstart
devenv mzstart.sln /Build Release
cd ..\mrstart
devenv mrstart.sln /Build Release
cd ..

..\..\racket -l setup -N "rico setup"
