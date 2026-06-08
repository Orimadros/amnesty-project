IMAGE := amazon-amnesty:dev

.PHONY: docker-build docker-boot docker-run all \
	build analysis 01_build 02_analysis 01_car 02_vtn 02_vtn_car 03_lavoura full clean-stamps

docker-build:
	docker build -t $(IMAGE) .

docker-boot:
	-docker run --rm -it -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash

docker-run:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash -c "$(CMD)"

all:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk all

build:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk build

analysis:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk analysis

01_build:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 01_build

02_analysis:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 02_analysis

01_car:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 01_car

02_vtn:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 02_vtn

02_vtn_car:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 02_vtn_car

03_lavoura:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk 03_lavoura

full:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk full

clean-stamps:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk clean-stamps
