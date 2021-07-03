IMAGE_NAME = melpazoid

.PHONY: run
run:
	python melpazoid/melpazoid.py

# https://cheatsheetseries.owasp.org/cheatsheets/Docker_Security_Cheat_Sheet.html
.PHONY: test
test: image
	@docker run --rm --cap-drop all --network=none --security-opt=no-new-privileges ${IMAGE_NAME}

.PHONY: term
term: image
	docker run -it --rm --entrypoint=/bin/bash ${IMAGE_NAME}

.PHONY: image
image:
	@docker build --build-arg PACKAGE_MAIN --quiet \
		--tag ${IMAGE_NAME} -f docker/Dockerfile .

.PHONY: test-melpazoid
test-melpazoid:
	mypy melpazoid || yes | mypy --install-types && mypy melpazoid
	pytest --doctest-modules
	black -S --check .
