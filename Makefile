.PHONY: test build alias

test:
	clojure -M:dev:kaocha

build:
	clojure -T:build uber

repl:
	clj -A:clj:nrepl -m nrepl.cmdline
