# Docker images for CI

In order to speed up building and testing of Racket, we have prepared some Docker images.
Currently pushed to DockerHub under the racket/racket-ci repo. This is then used in the CI
configuration with:
```
	image: "racket/racket-ci:testdeps" # for testing jobs
```
	
or:
```
	image: "racket/racket-ci:builddeps" # for build jobs
```

The images are automatically build and uploaded on all commits by Docker Hub.

To update manually, use the Makefile in this directory, but you need to be logged in - use `docker login`.

