IMAGE_NAME = melpazoid

.PHONY: run
run:
	python melpazoid/melpazoid.py

.PHONY: test
test: image
	@docker run -it --rm --network none ${IMAGE_NAME}

.PHONY: term
term: image
	docker run -it --rm --entrypoint=/bin/bash ${IMAGE_NAME}

.PHONY: image
image:
	@docker build --build-arg PACKAGE_MAIN --quiet \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .
