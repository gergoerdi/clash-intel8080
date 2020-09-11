all: alpha-basic1000.a80.com

%.com: %.hex
	objcopy --gap-fill 0 --input-target=ihex --output-target=binary $< $@

