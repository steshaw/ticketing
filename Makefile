token := $(shell cat github-token)

elm.min.js: elm.js
	uglifyjs < $< > $@

elm.js: Ticketing.elm Pagination.elm Token.elm
	elm make Ticketing.elm --yes --output $@

Token.elm: github-token
	generate-token-module
