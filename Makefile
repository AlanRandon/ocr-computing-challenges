.PHONY: new

new:
	@if [ -z ${name} ]; then echo 'USAGE: make new name=<NAME>'; exit 1; fi
	mkdir ${name}
	echo -e "(executable\n (public_name ocr_computing_challenges)\n (name main)\n (libraries core ppx_jane)\n (preprocess (pps ppx_jane)))" > ${name}/dune
	echo -e 'open Core\n\nlet () = Stdio.print_endline "Hello World!"' > ${name}/main.ml
