IMAGE_NAME = melpazoid
DOCKER ?= docker
DOCKER_OPTIONS = --cap-drop all --security-opt=no-new-privileges --pids-limit=5
DOCKER_OUTPUT = --quiet  # e.g. '--progress=plain' xor '--quiet'
BUILD_OPTIONS = --load

.PHONY: run
run:
	python3 melpazoid/melpazoid.py

# https://cheatsheetseries.owasp.org/cheatsheets/Docker_Security_Cheat_Sheet.html
.PHONY: test
test: image
	@$(DOCKER) run --rm --network=none ${DOCKER_OPTIONS} ${IMAGE_NAME}

.PHONY: term
term: image
	$(DOCKER) run -it --rm --entrypoint=/bin/bash ${DOCKER_OPTIONS} ${IMAGE_NAME}

.PHONY: image
image:
	@$(DOCKER) build --build-arg PACKAGE_MAIN ${BUILD_OPTIONS} ${DOCKER_OUTPUT} \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .

.PHONY: test-melpazoid
test-melpazoid:
	mypy --strict --non-interactive --install-types melpazoid
	pytest --doctest-modules --durations=5
	black -S --check .
