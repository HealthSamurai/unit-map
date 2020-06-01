.PHONY: repl test

repl:
	rm -rf .cpcache/ && DEBUG=true clj -A:test:nrepl

test:
	clj -A:test:kaocha

ci-test:
	clojure -A:test:kaocha:ci
