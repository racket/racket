# Docker images for CI

In order to speed up building and testing of Racket, we have prepared some Docker images.
Currently pushed to DockerHub under the racket/racket-ci repo. This is then used in the CI
configuration with:
```
	image: "racket/racket-ci:latest"
```

The image is automatically built and uploaded on all relevant commits through GitHub Actions.

