IMAGE_NAME = melpazoid
DOCKER_OPTIONS = --cap-drop all --security-opt=no-new-privileges --pids-limit=5

.PHONY: run
run:
	python3 melpazoid/melpazoid.py

# https://cheatsheetseries.owasp.org/cheatsheets/Docker_Security_Cheat_Sheet.html
.PHONY: test
test: image
	@docker run --rm --network=none ${DOCKER_OPTIONS} ${IMAGE_NAME}

.PHONY: term
term: image
	docker run -it --rm --entrypoint=/bin/bash ${DOCKER_OPTIONS} ${IMAGE_NAME}

.PHONY: image
image:
	@docker build --build-arg PACKAGE_MAIN --quiet \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .

.PHONY: test-melpazoid
test-melpazoid:
	mypy --strict --non-interactive --install-types melpazoid
	pytest --doctest-modules --durations=5
	black -S --check .
