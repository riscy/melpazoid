DOCKERID = emacs
PACKAGE_NAME ?= NONE

.PHONY: run
run:
	python melpazoid.py

.PHNY: pytest
pytest:
	pytest --doctest-modules

.PHONY: term
test: image
	@docker run -it --rm --network none ${DOCKERID}

.PHONY: term
term: image
	docker run -it --rm --entrypoint=/bin/bash ${DOCKERID}

.PHONY: image
image:
	@docker build --build-arg PACKAGE_NAME --quiet --tag ${DOCKERID} -f Dockerfile .
