SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

R_SCRIPT ?= Rscript
R_PROJ := code/patricio_preach_tomas_work
DAG_DIR := build/dag
STAMP_DIR := $(DAG_DIR)/stamps

VTN_YEARS := 2015 2016 2017 2018 2019 2021 2022
VNP_YEARS_PRE := 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
VNP_YEARS_POST := 2016 2017 2018 2019 2020 2021 2022
VNP_YEARS_ALL := $(VNP_YEARS_PRE) $(VNP_YEARS_POST)
LAV_YEARS := 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
PARCEL_CATS := eligible ineligible legal

VTN_PRECLEAN := \
	$(R_PROJ)/data/preclean/vtn_2019.rds \
	$(R_PROJ)/data/preclean/vtn_2021.rds \
	$(R_PROJ)/data/preclean/vtn_2022.rds

VTN_CLEAN := $(addprefix $(R_PROJ)/data/clean/vtn/vtn_,$(addsuffix _clean.rds,$(VTN_YEARS)))
VTN_TO_FIX := $(addprefix $(R_PROJ)/data/clean/analysis_vtn/unmatched_,$(addsuffix _TO_FIX.csv,$(VTN_YEARS)))
VTN_CORR_STAMP := $(R_PROJ)/data/clean/vtn/.vtn_unmatched_corrections.stamp
VTN_REGION := $(addprefix $(R_PROJ)/data/clean/vtn_IHS/vtn_region_,$(addsuffix .csv,$(VTN_YEARS)))
VTN_VS_REGION := $(foreach y,$(VTN_YEARS),$(R_PROJ)/data/output/vtn_vs_car_regions/$(y)/car_vtn_region_$(y).csv $(R_PROJ)/data/output/vtn_vs_car_regions/$(y)/car_missing_region_$(y).csv)

VTN_PARCELS_REGION := $(foreach y,$(VTN_YEARS),$(foreach c,$(PARCEL_CATS),$(R_PROJ)/data/output/vtn_parcels_region/$(y)/$(c)_car_vtn_region_$(y).csv $(R_PROJ)/data/output/vtn_parcels_region/$(y)/$(c)_missing_vtn_region_$(y).csv))

VTN_WIDE := \
	$(R_PROJ)/data/output/parcels_VTN_wide/eligible_parcels_vtn_wide.rds \
	$(R_PROJ)/data/output/parcels_VTN_wide/eligible_parcels_vtn_wide.csv \
	$(R_PROJ)/data/output/parcels_VTN_wide/ineligible_parcels_vtn_wide.rds \
	$(R_PROJ)/data/output/parcels_VTN_wide/ineligible_parcels_vtn_wide.csv \
	$(R_PROJ)/data/output/parcels_VTN_wide/legal_parcels_vtn_wide.rds \
	$(R_PROJ)/data/output/parcels_VTN_wide/legal_parcels_vtn_wide.csv

VNP_CLEAN := \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt.csv \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt.rds \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt_pre2015.csv \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt_pre2015.rds

VNP_REGION_POST := $(foreach y,$(VNP_YEARS_POST),$(R_PROJ)/data/output/vnp_vs_car_regions/$(y)/car_vnp_$(y).csv $(R_PROJ)/data/output/vnp_vs_car_regions/$(y)/car_missing_$(y).csv)
VNP_REGION_PRE := $(foreach y,$(VNP_YEARS_PRE),$(R_PROJ)/data/output/vnp_pre2015_vs_car_regions/$(y)/car_vnp_$(y).csv $(R_PROJ)/data/output/vnp_pre2015_vs_car_regions/$(y)/car_missing_$(y).csv)
VNP_REGION_ONLY := $(VNP_REGION_POST) $(VNP_REGION_PRE)

VNP_PARCELS := $(foreach y,$(VNP_YEARS_ALL),$(foreach c,$(PARCEL_CATS),$(R_PROJ)/data/output/vnp_parcels_region/$(y)/$(c)_car_vnp_region_$(y).csv $(R_PROJ)/data/output/vnp_parcels_region/$(y)/$(c)_missing_vnp_region_$(y).csv))

LAV_MASTER := $(R_PROJ)/data/output/fnp_lavoura_2002_2017_with_state.csv
LAV_FNP_REGION := $(foreach y,$(LAV_YEARS),$(R_PROJ)/data/output/fnp_vs_car_regions/$(y)/car_fnp_$(y).csv $(R_PROJ)/data/output/fnp_vs_car_regions/$(y)/car_missing_$(y).csv)
LAV_REGION := $(foreach y,$(LAV_YEARS),$(foreach c,$(PARCEL_CATS),$(R_PROJ)/data/output/lavoura_parcels_region/$(y)/$(c)_car_lavoura_region_$(y).csv $(R_PROJ)/data/output/lavoura_parcels_region/$(y)/$(c)_missing_lavoura_region_$(y).csv))

NB_LAV_WIDE := \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/eligible_parcels_all/eligible_parcel_nb_lavoura_wide.rds \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/eligible_parcels_all/eligible_parcel_nb_lavoura_wide.csv \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/ineligible_parcels_all/ineligible_parcel_nb_lavoura_wide.rds \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/ineligible_parcels_all/ineligible_parcel_nb_lavoura_wide.csv \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/legal_parcels_all/legal_parcel_nb_lavoura_wide.rds \
	$(R_PROJ)/data/output/parcels_NB_Lavoura/legal_parcels_all/legal_parcel_nb_lavoura_wide.csv

CAR_PARCEL_RDATA := \
	$(R_PROJ)/data/output/eligible_car.Rdata \
	$(R_PROJ)/data/output/ineligible_car.Rdata \
	$(R_PROJ)/data/output/legal_car.Rdata

CAR_IHS_RDATA := \
	$(R_PROJ)/data/output/car_vtn_IHS_breakdown/eligible_car_IHS.Rdata \
	$(R_PROJ)/data/output/car_vtn_IHS_breakdown/ineligible_car_IHS.Rdata \
	$(R_PROJ)/data/output/car_vtn_IHS_breakdown/legal_car_IHS.Rdata

VTN_PRECLEAN_STAMP := $(STAMP_DIR)/vtn_preclean.stamp
VTN_CLEAN_STAMP := $(STAMP_DIR)/vtn_clean.stamp
VTN_REGION_STAMP := $(STAMP_DIR)/vtn_region.stamp
CAR_IHS_STAMP := $(STAMP_DIR)/car_ihs_match.stamp
VTN_VS_REGION_STAMP := $(STAMP_DIR)/vtn_vs_region.stamp
VTN_PARCELS_STAMP := $(STAMP_DIR)/vtn_parcels.stamp
VTN_WIDE_STAMP := $(STAMP_DIR)/vtn_wide.stamp
VNP_CLEAN_STAMP := $(STAMP_DIR)/vnp_clean.stamp
VNP_REGION_POST_STAMP := $(STAMP_DIR)/vnp_region_post.stamp
VNP_REGION_PRE_STAMP := $(STAMP_DIR)/vnp_region_pre.stamp
VNP_PARCELS_STAMP := $(STAMP_DIR)/vnp_parcels.stamp
LAV_MASTER_STAMP := $(STAMP_DIR)/lavoura_master.stamp
LAV_REGION_STAMP := $(STAMP_DIR)/lavoura_region.stamp
NB_LAV_WIDE_STAMP := $(STAMP_DIR)/nb_lavoura_wide.stamp

.PHONY: help dag canonical-dag legacy-dag all \
	vtn-pipeline nb-pipeline lavoura-pipeline \
	audit-dag dangling deprecated report

all: dag

dag: canonical-dag legacy-dag report

help:
	@echo "Targets:"
	@echo "  make canonical-dag   # canonical Tomas VTN+NB+Lavoura DAG (file targets)"
	@echo "  make legacy-dag      # legacy branches tracked as external-path DAG nodes"
	@echo "  make report          # classify ALL repo files: active/deprecated/dangling"
	@echo "  make dangling        # print only dangling files"
	@echo "  make deprecated      # print deprecated files"

canonical-dag: vtn-pipeline nb-pipeline lavoura-pipeline

legacy-dag:
	@echo "Legacy DAG branch (external-path / non-portable scripts) includes:"
	@echo "  code/0_deforestation_rules.cpp -> code/1_mapbiomas.R -> code/2_empirics.R -> code/3_policy1.R"
	@echo "  miseEnPlace/multas.R -> miseEnPlace/multas_updated.R / multas_RegsFE.R"
	@echo "  fines_robustness/Multas-AvisosMatchingV3.R -> fines_robustness_sutva.do"
	@echo "  clustering_missing_regions/clustering_missing_regions.R"
	@echo "These scripts rely on absolute paths outside this repo and are intentionally not auto-executed here."

vtn-pipeline: $(VTN_REGION) $(VTN_VS_REGION) $(VTN_PARCELS_REGION) $(VTN_WIDE)

nb-pipeline: $(VNP_CLEAN) $(VNP_REGION_ONLY) $(VNP_PARCELS)

lavoura-pipeline: $(LAV_MASTER) $(LAV_FNP_REGION) $(LAV_REGION) $(NB_LAV_WIDE)

# -----------------------------------------------------------------------------
# Canonical Tomas chain (executable where external inputs are available)
# -----------------------------------------------------------------------------

$(R_PROJ)/data/clean/muni_division_2015.Rdata: $(R_PROJ)/code/Tomas_VTN_processing/0_clean_muni_boundaries.R
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/0_clean_muni_boundaries.R

$(VTN_PRECLEAN_STAMP): $(R_PROJ)/code/Tomas_VTN_processing/1_pre_clean_vtn_2019-22.R | $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/1_pre_clean_vtn_2019-22.R
	@date > $@

$(VTN_PRECLEAN): $(VTN_PRECLEAN_STAMP)
	@test -f "$@"

$(VTN_CLEAN_STAMP): $(R_PROJ)/code/Tomas_VTN_processing/2_clean_vtn_data.R $(R_PROJ)/data/clean/muni_division_2015.Rdata $(VTN_PRECLEAN) | $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/2_clean_vtn_data.R
	@date > $@

$(VTN_CLEAN): $(VTN_CLEAN_STAMP)
	@test -f "$@"

$(VTN_CORR_STAMP): $(R_PROJ)/code/Tomas_VTN_processing/3_unmatched_vtn_correction.R $(VTN_CLEAN) $(VTN_TO_FIX)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/3_unmatched_vtn_correction.R
	@mkdir -p $(@D)
	@date > $@

$(VTN_REGION_STAMP): $(R_PROJ)/code/Tomas_VTN_processing/4_clean_ihs_markit_Tomas.R $(VTN_CORR_STAMP) | $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/4_clean_ihs_markit_Tomas.R
	@date > $@

$(VTN_REGION): $(VTN_REGION_STAMP)
	@test -f "$@"

$(R_PROJ)/data/clean/IHS_regions_divison.Rdata: $(R_PROJ)/code/Tomas_VTN_processing/5_create_region_shapefile.R $(R_PROJ)/data/clean/muni_division_2015.Rdata
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/5_create_region_shapefile.R

$(R_PROJ)/data/clean/all_car_regions.Rdata \
$(CAR_IHS_RDATA) \
$(CAR_PARCEL_RDATA) \
$(R_PROJ)/data/output/car_vtn_IHS_breakdown/eligible_IHS.csv \
$(R_PROJ)/data/output/car_vtn_IHS_breakdown/ineligible_IHS.csv \
$(R_PROJ)/data/output/car_vtn_IHS_breakdown/legal_IHS.csv: \
	$(CAR_IHS_STAMP)
	@test -f "$@"

$(CAR_IHS_STAMP): \
	$(R_PROJ)/code/Tomas_VTN_processing/6_match_car_IHSregion.R \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/6_match_car_IHSregion.R
	@if [ -f "$(R_PROJ)/all_car_regions.Rdata" ]; then \
		cp -f "$(R_PROJ)/all_car_regions.Rdata" "$(R_PROJ)/data/clean/all_car_regions.Rdata"; \
	elif [ ! -f "$(R_PROJ)/data/clean/all_car_regions.Rdata" ]; then \
		echo "Missing all_car_regions.Rdata after script 6"; exit 1; \
	fi
	@for c in eligible ineligible legal; do \
		src="$(R_PROJ)/data/output/car_vtn_IHS_breakdown/$${c}_car_IHS.Rdata"; \
		dst="$(R_PROJ)/data/output/$${c}_car.Rdata"; \
		if [ -f "$$src" ]; then \
			cp -f "$$src" "$$dst"; \
		elif [ ! -f "$$dst" ]; then \
			echo "Missing CAR parcel file: $$src and $$dst"; exit 1; \
		fi; \
	done
	@date > $@

$(VTN_VS_REGION_STAMP): \
	$(R_PROJ)/code/Tomas_VTN_processing/7_vtn_car_IHSregion_merge.R \
	$(R_PROJ)/data/clean/all_car_regions.Rdata \
	$(VTN_REGION) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/7_vtn_car_IHSregion_merge.R
	@date > $@

$(VTN_VS_REGION): $(VTN_VS_REGION_STAMP)
	@test -f "$@"

$(VTN_PARCELS_STAMP): \
	$(R_PROJ)/code/Tomas_VTN_processing/8_vtn_car_merge.R \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	$(CAR_PARCEL_RDATA) \
	$(VTN_REGION) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/8_vtn_car_merge.R
	@date > $@

$(VTN_PARCELS_REGION): $(VTN_PARCELS_STAMP)
	@test -f "$@"

$(VTN_WIDE_STAMP): \
	$(R_PROJ)/code/Tomas_VTN_processing/9_joining_vtn.R \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	$(CAR_PARCEL_RDATA) \
	$(VTN_REGION) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_VTN_processing/9_joining_vtn.R
	@date > $@

$(VTN_WIDE): $(VTN_WIDE_STAMP)
	@test -f "$@"

$(VNP_CLEAN_STAMP): \
	$(R_PROJ)/code/Tomas_NB_processing/1.0_tomas_task5.R \
	$(R_PROJ)/code/Tomas_NB_processing/2.0_tomas_task5.R \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/1.0_tomas_task5.R
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/2.0_tomas_task5.R
	@date > $@

$(VNP_CLEAN): $(VNP_CLEAN_STAMP)
	@test -f "$@"

$(VNP_REGION_POST_STAMP): \
	$(R_PROJ)/code/Tomas_NB_processing/3.0_tomas_task5.R \
	$(R_PROJ)/data/clean/all_car_regions.Rdata \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt.rds \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/3.0_tomas_task5.R
	@date > $@

$(VNP_REGION_POST): $(VNP_REGION_POST_STAMP)
	@test -f "$@"

$(VNP_REGION_PRE_STAMP): \
	$(R_PROJ)/code/Tomas_NB_processing/4.0_tomas_task5.R \
	$(R_PROJ)/data/clean/all_car_regions.Rdata \
	$(R_PROJ)/data/clean/vnp/city_region_yearly_pt_pre2015.rds \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/4.0_tomas_task5.R
	@date > $@

$(VNP_REGION_PRE): $(VNP_REGION_PRE_STAMP)
	@test -f "$@"

$(VNP_PARCELS_STAMP): \
	$(R_PROJ)/code/Tomas_NB_processing/5.0_tomas_task5.R \
	$(R_PROJ)/code/Tomas_NB_processing/6.0_tomas_task5.R \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	$(CAR_PARCEL_RDATA) \
	$(VNP_CLEAN) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/5.0_tomas_task5.R
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_NB_processing/6.0_tomas_task5.R
	@date > $@

$(VNP_PARCELS): $(VNP_PARCELS_STAMP)
	@test -f "$@"

$(LAV_MASTER_STAMP): \
	$(R_PROJ)/code/Tomas_Lavoura_processing_NB_merge/1.match_lavoura_data.R \
	$(R_PROJ)/data/clean/all_car_regions.Rdata \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_Lavoura_processing_NB_merge/1.match_lavoura_data.R
	@date > $@

$(LAV_MASTER) $(LAV_FNP_REGION): $(LAV_MASTER_STAMP)
	@test -f "$@"

$(LAV_REGION_STAMP): \
	$(R_PROJ)/code/Tomas_Lavoura_processing_NB_merge/2.match_lavoura_data.R \
	$(LAV_MASTER) \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	$(CAR_PARCEL_RDATA) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_Lavoura_processing_NB_merge/2.match_lavoura_data.R
	@date > $@

$(LAV_REGION): $(LAV_REGION_STAMP)
	@test -f "$@"

$(NB_LAV_WIDE_STAMP): \
	$(R_PROJ)/code/Tomas_Lavoura_processing_NB_merge/3.match_lavoura_data.R \
	$(LAV_MASTER) \
	$(VNP_CLEAN) \
	$(R_PROJ)/data/clean/IHS_regions_divison.Rdata \
	$(CAR_PARCEL_RDATA) \
	| $(STAMP_DIR)
	cd $(R_PROJ) && $(R_SCRIPT) code/Tomas_Lavoura_processing_NB_merge/3.match_lavoura_data.R
	@date > $@

$(NB_LAV_WIDE): $(NB_LAV_WIDE_STAMP)
	@test -f "$@"

# -----------------------------------------------------------------------------
# Full-repo DAG accounting: classify ALL files into active/reference/deprecated/
# external-path scripts/dangling.
# -----------------------------------------------------------------------------

$(DAG_DIR):
	mkdir -p $(DAG_DIR)

$(STAMP_DIR): | $(DAG_DIR)
	mkdir -p $(STAMP_DIR)

$(DAG_DIR)/all_files.txt: | $(DAG_DIR)
	LC_ALL=C find . -type f ! -path './.git/*' | sed 's|^./||' | sort > $@

$(DAG_DIR)/active_files.txt: | $(DAG_DIR)
	{ \
		echo "code/0_deforestation_rules.cpp"; \
		echo "code/1_mapbiomas.R"; \
		echo "code/2_empirics.R"; \
		echo "code/3_policy1.R"; \
		echo "code/x_aggregate_infractions.R"; \
		echo "clustering_missing_regions/clustering_missing_regions.R"; \
		echo "fines_robustness/Multas-AvisosMatchingV1.R"; \
		echo "fines_robustness/Multas-AvisosMatchingV2.R"; \
		echo "fines_robustness/Multas-AvisosMatchingV3.R"; \
		echo "fines_robustness/fines_robustness_sutva.do"; \
		echo "miseEnPlace/multas.R"; \
		echo "miseEnPlace/multas_updated.R"; \
		echo "miseEnPlace/multas_RegsFE.R"; \
		echo "miseEnPlace/multas_test_Guilherme.R"; \
		echo "miseEnPlace/empirics_amazon_final.do"; \
		echo "lit/amazon/create_SicarMuniOverlap_variables_COMMENTS.R"; \
		find clustering_missing_regions -type f; \
		find fines_robustness -type f; \
		find miseEnPlace -type f ! -path 'miseEnPlace/archive/*'; \
		find $(R_PROJ)/code/Tomas_VTN_processing -type f -name '*.R'; \
		find $(R_PROJ)/code/Tomas_NB_processing -type f -name '*.R'; \
		find $(R_PROJ)/code/Tomas_Lavoura_processing_NB_merge -type f -name '*.R'; \
		find $(R_PROJ)/data/clean -type f; \
		find $(R_PROJ)/data/preclean -type f; \
		find $(R_PROJ)/data/output -type f; \
		find $(R_PROJ)/data/muni_division_2015 -type f; \
		find $(R_PROJ)/plots -type f; \
		echo "$(R_PROJ)/all_car_regions.Rdata"; \
	} | sed 's|^./||' | sort -u > $@

$(DAG_DIR)/reference_files.txt: | $(DAG_DIR)
	{ \
		find docs -type f 2>/dev/null; \
		find lit -type f 2>/dev/null; \
		echo "QUESTIONS_LEGACY.md"; \
		echo "Makefile"; \
		find $(DAG_DIR) -type f 2>/dev/null; \
	} | sed 's|^./||' | sort -u > $@

$(DAG_DIR)/deprecated_files.txt: | $(DAG_DIR)
	{ \
		find code/working -type f 2>/dev/null; \
		find $(R_PROJ)/code/tomas_task_1 -type f 2>/dev/null; \
		find $(R_PROJ)/code/tomas_task_2 -type f 2>/dev/null; \
		find $(R_PROJ)/code/tomas_task_3 -type f 2>/dev/null; \
		find $(R_PROJ)/code/tomas_task_4 -type f 2>/dev/null; \
		find $(R_PROJ)/code/preach -type f 2>/dev/null; \
		echo "$(R_PROJ)/code/graph_missing.R"; \
		find $(R_PROJ)/data/output/car_vnp_IHS_breakdown -type f 2>/dev/null; \
		find $(R_PROJ)/data/output/CAR_muni_breakdown -type f 2>/dev/null; \
		find $(R_PROJ)/data/output/car_vtn_muni_breakdown -type f 2>/dev/null; \
		find $(R_PROJ)/data/output/vtn_vs_car_muni -type f 2>/dev/null; \
		find $(R_PROJ)/data/output/vtn_parcels -type f 2>/dev/null; \
		find $(R_PROJ)/data/output/parcels_NB_Lavoura/ilegal_parcels_all -type f 2>/dev/null; \
		find $(R_PROJ)/data/output -type f -name '*ilegal*' 2>/dev/null; \
		find $(R_PROJ)/cleaned_data -type f 2>/dev/null; \
		find $(R_PROJ)/docs -type f 2>/dev/null; \
		find $(R_PROJ)/renv -type f 2>/dev/null; \
		echo "$(R_PROJ)/renv.lock"; \
		echo "$(R_PROJ)/null.lintr"; \
		find miseEnPlace/archive -type f 2>/dev/null; \
		echo "subprojects/permutations/create_permuations.R"; \
		find 'subprojects/BH replication' -type f 2>/dev/null; \
		echo "subprojects/BH replication.zip"; \
		echo "patricio/tinyScript.R"; \
		find patricio -type f 2>/dev/null; \
		echo "code/.Rapp.history"; \
		find $(R_PROJ)/data/Meso_stuff -type f 2>/dev/null; \
	} | sed 's|^./||' | sort -u > $@

$(DAG_DIR)/external_path_scripts.txt: | $(DAG_DIR)
	find . -type f \( -name '*.R' -o -name '*.do' -o -name '*.cpp' -o -name '*.py' -o -name '*.sh' \) \
		! -path './$(R_PROJ)/renv/*' \
	| while read -r f; do \
		if LC_ALL=C grep -Eq '(/Users/|D:/|C:/|~/|Dropbox|Downloads|Documents|Volumes)' "$$f"; then \
			echo "$${f#./}"; \
		fi; \
	done | sort -u > $@

$(DAG_DIR)/classified_files.txt: $(DAG_DIR)/all_files.txt $(DAG_DIR)/active_files.txt $(DAG_DIR)/reference_files.txt $(DAG_DIR)/deprecated_files.txt $(DAG_DIR)/external_path_scripts.txt
	cat $(DAG_DIR)/active_files.txt $(DAG_DIR)/reference_files.txt $(DAG_DIR)/deprecated_files.txt $(DAG_DIR)/external_path_scripts.txt \
		| sort -u > $(DAG_DIR)/classified_files_raw.txt
	comm -23 $(DAG_DIR)/all_files.txt $(DAG_DIR)/classified_files_raw.txt > $(DAG_DIR)/dangling_files.txt
	cat $(DAG_DIR)/all_files.txt | while read -r f; do \
		if grep -Fxq "$$f" $(DAG_DIR)/deprecated_files.txt; then c=deprecated; \
		elif grep -Fxq "$$f" $(DAG_DIR)/active_files.txt; then c=active_dag; \
		elif grep -Fxq "$$f" $(DAG_DIR)/reference_files.txt; then c=reference; \
		elif grep -Fxq "$$f" $(DAG_DIR)/external_path_scripts.txt; then c=external_path_script; \
		else c=dangling; fi; \
		printf '%s\t%s\n' "$$c" "$$f"; \
	done > $@

report: $(DAG_DIR)/classified_files.txt
	@echo "DAG classification written to $(DAG_DIR)/classified_files.txt"
	@echo "Counts:"
	@printf "  active_dag:           %s\n" "$$(grep -c '^active_dag' $(DAG_DIR)/classified_files.txt || true)"
	@printf "  reference:            %s\n" "$$(grep -c '^reference' $(DAG_DIR)/classified_files.txt || true)"
	@printf "  deprecated:           %s\n" "$$(grep -c '^deprecated' $(DAG_DIR)/classified_files.txt || true)"
	@printf "  scripts_with_external_paths: %s\n" "$$(wc -l < $(DAG_DIR)/external_path_scripts.txt)"
	@printf "  dangling:             %s\n" "$$(grep -c '^dangling' $(DAG_DIR)/classified_files.txt || true)"
	@echo ""
	@echo "External-path scripts list: $(DAG_DIR)/external_path_scripts.txt"
	@echo ""
	@echo "Top dangling files (first 40):"
	@sed -n '1,40p' $(DAG_DIR)/dangling_files.txt

dangling: $(DAG_DIR)/classified_files.txt
	@cat $(DAG_DIR)/dangling_files.txt

deprecated: $(DAG_DIR)/classified_files.txt
	@cat $(DAG_DIR)/deprecated_files.txt

audit-dag: report
