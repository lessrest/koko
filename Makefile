build:
	docker build --rm -t lessrest/koko .
shell:
	docker run -it --rm lessrest/koko bash
