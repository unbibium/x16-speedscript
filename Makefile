TARGET = target

all: c64-spsc x16-spsc

c64-spsc: x16-spsc.s
	cl65 -o $(TARGET)/c64-spsc.prg -t c64 --listing $(TARGET)/c64-spsc.lst -C c64-asm.cfg c64-spsc.s

x16-spsc: x16-spsc.s
	cl65 -o $(TARGET)/x16-spsc.prg -t c64 --listing $(TARGET)/spsc.lst -C c64-asm.cfg x16-spsc.s

$(shell  mkdir -p $(TARGET))
