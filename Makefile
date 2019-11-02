.PHONY: all clean exec test test1

DUNE       := dune
DIFF       := meld
EXECUTABLE := driver.exe
ARGS       :=

all: driver

lib:
	$(DUNE) build src/lib/tigris.lib

driver:
	$(DUNE) build src/bin/driver.exe

exec:
	rlwrap $(DUNE) exec src/bin/$(EXECUTABLE) -- $(ARGS)

test:
	#$(DUNE) runtest --diff-command $(DIFF)
	$(DUNE) runtest

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean

test1: all
	echo "(1 + 2 * 10) * 2" | $(DUNE) exec src/bin/$(EXECUTABLE)
