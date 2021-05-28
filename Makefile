.PHONY: all

ADMINS := $(shell seq 1 13)
FIGURE_FITS := $(addsuffix .pdf, $(addprefix outputs/fit_plots/fit_res_on_, $(ADMINS)))
COUNTERFACTUAL_IDS := $(shell seq 1 7)
COUNTERFACTUAL_RUNS := $(addsuffix .rds, $(addprefix outputs/simulations/counterfactual_, $(COUNTERFACTUAL_IDS)))
# $(info VAR="$(COUNTERFACTUAL)")

all: data/processed/m_fits_all.rds\
		 data/processed/m_fits_off.rds\
		 outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf\
		 $(FIGURE_FITS)\
		 data/processed/counterfactual_params.rds\
		 $(COUNTERFACTUAL_RUNS)\
		 outputs/simulations/counterfactual_all_simulations.rds

data/processed/input_bf_only.rds: R/clean_and_produce_BF_data.R data/raw/input.RData data/raw/clinical.rds data/raw/subnational_resistance.rds 
	Rscript $<

data/processed/m_fits_on.rds: R/fit_m_resistance_on.R data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds
	Rscript $<
	
data/processed/m_fits_off.rds: data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds R/fit_m_resistance_off.R
	Rscript R/fit_m_resistance_off.R
	
data/processed/m_fits_cascades_olyset.rds: R/fit_m_cascades_olyset.R data/raw/prevalence_olyset_mira.rds data/processed/input_bf_only.rds data/raw/itn.rds
	Rscript $<
	
data/processed/m_fits_all.rds: R/fit_m_combine.R data/processed/m_fits_on.rds data/processed/m_fits_cascades_olyset.rds data/raw/prevalence_olyset_mira.rds data/raw/monthly_prevalence.rds
	Rscript $<

# note $< gives first prerequisite; $* allows access to %; $@ allows access to target
$(FIGURE_FITS): outputs/fit_plots/fit_res_on_%.pdf: R/plot_fits_non_cascades.R data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds data/processed/m_fits_on.rds
	Rscript $< $* -o $@
	
outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf: R/plot_fits_cascades.R data/raw/monthly_prevalence.rds data/raw/prevalence_olyset_mira.rds data/processed/input_bf_only.rds data/processed/m_fits_all.rds
	Rscript $<
	
data/processed/counterfactual_params.rds: R/counterfactual_parameter_sets.R data/processed/input_bf_only.rds
	Rscript $<

$(COUNTERFACTUAL_RUNS): outputs/simulations/counterfactual_%.rds: R/counterfactual.R
	Rscript $< $*

outputs/simulations/counterfactual_all_simulations.rds: R/counterfactual_simulations_process.R $(COUNTERFACTUAL_RUNS)
	Rscript $<
