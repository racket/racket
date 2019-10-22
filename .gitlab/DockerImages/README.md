# Docker images for Gitlab CI

In order to speed up building and testing of Racket, we have prepared some Docker images.
Currently pushed to DockerHub under the pmatos/racket-ci repo. This is then used in the CI
configuration with:
```
	image: "pmatos/racket-ci:testdeps" # for testing jobs
```
	
or:
```
	image: "pmatos/racket-ci:builddeps" # for build jobs
```
	
The Makefile does the build/push automatically but you need to be logged in - use `docker login`.
