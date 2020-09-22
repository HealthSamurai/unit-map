.PHONY: repl test

repl:
	rm -rf .cpcache/ && clojure -A:test:nrepl

test:
	clj -A:test:kaocha

ci-test:
	clojure -A:test:kaocha:ci
