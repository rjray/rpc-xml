# Simple makefile to create method files from the inputs

MAKEMETHOD := make_method
METHODS := linux.proc.cpuinfo linux.proc.meminfo
XPL_FILES := $(METHODS:=.xpl)

%.xpl : %.code %.help %.base
	$(MAKEMETHOD) --base=$*

all:	$(XPL_FILES)

clean:
	rm -f $(XPL_FILES)
