IMAGE_NAME = melpazoid

.PHONY: run
run:
	python melpazoid/melpazoid.py

.PHONY: test
test: image
	@docker run --rm --network none ${IMAGE_NAME}

.PHONY: term
term: image
	docker run -it --rm --entrypoint=/bin/bash ${IMAGE_NAME}

.PHONY: image
image:
	@docker build --build-arg PACKAGE_MAIN --quiet \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .

.PHONY: test-melpazoid
test-melpazoid:
	mypy --warn-return-any melpazoid
	pytest --doctest-modules
	black -S --check .
