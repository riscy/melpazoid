IMAGE_NAME = melpazoid
DOCKER ?= docker
DOCKER_OPTIONS = --cap-drop all --security-opt=no-new-privileges --pids-limit=50
DOCKER_OUTPUT ?= --progress=plain  # e.g. '--progress=plain' xor '--quiet'

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
	@$(DOCKER) build --build-arg PACKAGE_MAIN ${DOCKER_OUTPUT} \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .

.PHONY: test-melpazoid
test-melpazoid:
	rm -rf _requirements.el
	mypy --strict --non-interactive --install-types melpazoid
	pytest --doctest-modules --durations=5
	ruff check . --extend-select=ISC001
	ruff format --check .
