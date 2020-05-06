all: push-builddeps push-testdeps

.PHONY: push-builddeps push-testdeps build-testdeps build-testdeps

push-builddeps: build-builddeps
	docker push pmatos/racket-ci:builddeps

build-builddeps: Dockerfile.builddeps
	docker build --file Dockerfile.builddeps --tag pmatos/racket-ci:builddeps .

push-testdeps: build-testdeps
	docker push pmatos/racket-ci:testdeps

build-testdeps: Dockerfile.testdeps
	docker build --file Dockerfile.testdeps --tag pmatos/racket-ci:testdeps .

