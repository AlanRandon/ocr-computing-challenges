.PHONY: new

new:
	@if [ -z ${name} ]; then echo 'USAGE: make new name=<NAME>'; exit 1; fi
	mkdir ${name}
	echo -e "(executable\n (public_name ocr_computing_challenges)\n (name main))" > ${name}/dune
	echo 'let () = print_endline "Hello World!"' > ${name}/main.ml
