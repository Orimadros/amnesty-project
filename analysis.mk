SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

R_SCRIPT ?= Rscript

VTN_DIR := code/01_build/02_vtn
CAR_SCRIPT := code/01_build/01_car/0_build_car_layers_from_raw.R

STAMP_DIR := build/stamps

VTN_YEARS := 2015 2016 2017 2018 2019 2021 2022
VTN_PRECLEAN_YEARS := 2019 2021 2022

VTN_PRECLEAN_OUT := $(addprefix data/preclean/vtn_,$(addsuffix .rds,$(VTN_PRECLEAN_YEARS)))
VTN_CLEAN_OUT := $(addprefix data/clean/vtn/vtn_,$(addsuffix _clean.rds,$(VTN_YEARS)))
VTN_REGION_OUT := $(addprefix data/clean/vtn_IHS/vtn_region_,$(addsuffix .csv,$(VTN_YEARS)))
VTN_TO_FIX := $(addprefix data/input/manual/vtn/unmatched_,$(addsuffix _TO_FIX.csv,$(VTN_YEARS)))

MUNI_CLEAN := data/clean/muni_division_2015.Rdata
IHS_REGIONS := data/clean/IHS_regions_divison.Rdata
CAR_DIR := data/intermediate/car
CAR_ELIGIBLE := $(CAR_DIR)/car_eligible_cleaned.shp
CAR_INELIGIBLE := $(CAR_DIR)/car_ineligible_cleaned.shp
CAR_LEGAL := $(CAR_DIR)/already_treated.shp

VTN_CAR_MATCH_SCRIPT := $(VTN_DIR)/6_match_car_IHSregion.R
VTN_CAR_REGION_SCRIPT := $(VTN_DIR)/7_vtn_car_IHSregion_merge.R
VTN_CAR_PARCEL_SCRIPT := $(VTN_DIR)/8_vtn_car_merge.R

VTN_PRECLEAN_STAMP := $(STAMP_DIR)/vtn_preclean.stamp
VTN_CLEAN_STAMP := $(STAMP_DIR)/vtn_clean.stamp
VTN_CORR_STAMP := $(STAMP_DIR)/vtn_corrections.stamp
VTN_REGION_STAMP := $(STAMP_DIR)/vtn_region_csv.stamp
VTN_CAR_IHS_STAMP := $(STAMP_DIR)/vtn_car_ihs.stamp
VTN_CAR_REGION_STAMP := $(STAMP_DIR)/vtn_car_regions.stamp
VTN_CAR_PARCEL_STAMP := $(STAMP_DIR)/vtn_car_parcels.stamp

.PHONY: help all \
	build analysis \
	01_build 02_analysis \
	01_car 02_vtn 02_vtn_car 03_lavoura \
	vtn vtn_car car lavoura full clean-stamps

help:
	@echo "Targets:"
	@echo "  make -f analysis.mk all         # current stable pipeline (VTN 0-5)"
	@echo "  make -f analysis.mk 01_build    # build subtree aliases"
	@echo "  make -f analysis.mk 02_analysis # analysis subtree alias"
	@echo "  make -f analysis.mk 01_car      # run CAR scaffold"
	@echo "  make -f analysis.mk 02_vtn      # run VTN 0-5"
	@echo "  make -f analysis.mk 02_vtn_car  # run VTN steps 6-8 (CAR-dependent)"
	@echo "  make -f analysis.mk 03_lavoura  # run lavoura subtree (stub)"
	@echo "  make -f analysis.mk full        # run build + analysis aliases"
	@echo "  make -f analysis.mk clean-stamps"

all: vtn

build: 01_build

analysis: 02_analysis

01_build: 01_car 02_vtn 02_vtn_car 03_lavoura

02_analysis:
	@echo "No analysis DAG wired yet under code/02_analysis."

01_car: car

02_vtn: vtn

02_vtn_car: vtn_car

03_lavoura: lavoura

vtn: $(MUNI_CLEAN) $(VTN_PRECLEAN_OUT) $(VTN_CLEAN_OUT) $(VTN_CORR_STAMP) $(VTN_REGION_OUT) $(IHS_REGIONS)

vtn_car: $(VTN_CAR_IHS_STAMP) $(VTN_CAR_REGION_STAMP) $(VTN_CAR_PARCEL_STAMP)

car:
	$(R_SCRIPT) "$(CAR_SCRIPT)"

lavoura:
	@echo "No lavoura DAG wired yet under code/01_build/03_lavoura."

full: 01_build 02_analysis

clean-stamps:
	rm -rf "$(STAMP_DIR)"

$(STAMP_DIR):
	mkdir -p "$@"

$(MUNI_CLEAN): $(VTN_DIR)/0_clean_muni_boundaries.R
	$(R_SCRIPT) "$(VTN_DIR)/0_clean_muni_boundaries.R"
	@test -f "$@"

$(VTN_PRECLEAN_STAMP): $(VTN_DIR)/1_pre_clean_vtn_2019-22.R | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_DIR)/1_pre_clean_vtn_2019-22.R"
	@date > "$@"

$(VTN_PRECLEAN_OUT): $(VTN_PRECLEAN_STAMP)
	@test -f "$@"

$(VTN_CLEAN_STAMP): $(VTN_DIR)/2_clean_vtn_data.R $(MUNI_CLEAN) $(VTN_PRECLEAN_OUT) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_DIR)/2_clean_vtn_data.R"
	@date > "$@"

$(VTN_CLEAN_OUT): $(VTN_CLEAN_STAMP)
	@test -f "$@"

$(VTN_CORR_STAMP): $(VTN_DIR)/3_unmatched_vtn_correction.R $(VTN_CLEAN_OUT) $(VTN_TO_FIX) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_DIR)/3_unmatched_vtn_correction.R"
	@date > "$@"

$(VTN_REGION_STAMP): $(VTN_DIR)/4_clean_ihs_markit_Tomas.R $(VTN_CORR_STAMP) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_DIR)/4_clean_ihs_markit_Tomas.R"
	@date > "$@"

$(VTN_REGION_OUT): $(VTN_REGION_STAMP)
	@test -f "$@"

$(IHS_REGIONS): $(VTN_DIR)/5_create_region_shapefile.R $(MUNI_CLEAN)
	$(R_SCRIPT) "$(VTN_DIR)/5_create_region_shapefile.R"
	@test -f "$@"

$(VTN_CAR_IHS_STAMP): $(VTN_CAR_MATCH_SCRIPT) $(IHS_REGIONS) $(CAR_ELIGIBLE) $(CAR_INELIGIBLE) $(CAR_LEGAL) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_CAR_MATCH_SCRIPT)"
	@date > "$@"

$(VTN_CAR_REGION_STAMP): $(VTN_CAR_REGION_SCRIPT) $(VTN_CAR_IHS_STAMP) $(VTN_REGION_OUT) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_CAR_REGION_SCRIPT)"
	@date > "$@"

$(VTN_CAR_PARCEL_STAMP): $(VTN_CAR_PARCEL_SCRIPT) $(VTN_CAR_IHS_STAMP) $(VTN_REGION_OUT) $(IHS_REGIONS) | $(STAMP_DIR)
	$(R_SCRIPT) "$(VTN_CAR_PARCEL_SCRIPT)"
	@date > "$@"
