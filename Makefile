SHELL=/bin/bash -o pipefail
export PATH := node_modules/.bin:$(PATH)

test:
	elm-verify-examples
	elm-test