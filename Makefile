.PHONY: repl test

repl:
	rm -rf .cpcache/ && DEBUG=true clj -A:test:nrepl

test:
	clj -A:test:kaocha
