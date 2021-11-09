.PHONY: all

ADMINS := $(shell seq 1 39)
FIGURE_FITS := $(addsuffix .pdf, $(addprefix outputs/fit_plots/fit_res_on_, $(ADMINS)))
COUNTERFACTUAL_IDS := $(shell seq 1 312)
COUNTERFACTUAL_RUNS := $(addsuffix .rds, $(addprefix outputs/simulations/counterfactual_, $(COUNTERFACTUAL_IDS)))
PROJECTION_IDS := $(shell seq 1 390)
PROJECTION_RUNS := $(addsuffix .rds, $(addprefix outputs/simulations/projection_, $(PROJECTION_IDS)))
# $(info VAR="$(COUNTERFACTUAL)")

all: data/processed/m_fits_all.rds\
		 data/processed/m_fits_off.rds\
		 outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf\
		 $(FIGURE_FITS)\
		 data/processed/counterfactual_params.rds\
		 $(COUNTERFACTUAL_RUNS)\
		 outputs/simulations/counterfactual_all_simulations.rds\
		 outputs/counterfactual_plots/scenarios.pdf\
		 outputs/counterfactual_plots/interventions.pdf\
		 outputs/counterfactual_plots/cases_absolute.pdf\
		 outputs/counterfactual_plots/cases_percentage.pdf\
		 outputs/counterfactual_plots/cases_admin1_percentage.pdf\
		 data/processed/projection_params.rds\
		 $(PROJECTION_RUNS)\
		 outputs/simulations/projection_all_simulations.rds\
		 outputs/projection_plots/scenarios.pdf\
		 outputs/projection_plots/scenarios_upper.pdf\
		 outputs/projection_plots/scenarios_lower.pdf\
		 outputs/counterfactual_plots/age_prevalence_cascades.pdf\
		 outputs/data/processed/cascades_actual_fitted_age_prevalence.rds\
		 outputs/counterfactual_plots/map_vs_old_itn.pdf\
		 data/processed/input_upper.rds

data/processed/input_bf_only.rds: R/clean_and_produce_BF_data.R\
	data/raw/input.RData\
	data/raw/clinical.rds\
	data/raw/subnational_resistance.rds 
	Rscript $<
	
data/processed/compare_map_old_itns.rds: R/compare_map_itn.R\
	data/raw/SSA_ITN_use_adm1.csv\
	data/processed/input_bf_only.rds
	Rscript $<

outputs/counterfactual_plots/map_vs_old_itn.pdf: R/plot_map_itn.R\
	data/processed/compare_map_old_itns.rds
	Rscript $<

data/processed/input_mean.rds: R/process_map_itn.R\
	data/processed/compare_map_old_itns.rds\
	data/processed/input_bf_only.rds
	Rscript $<

data/processed/input_lower.rds: data/processed/input_mean.rds
data/processed/input_upper.rds: data/processed/input_mean.rds

data/processed/m_fits_on.rds: R/fit_m_resistance_on.R\
	data/raw/monthly_prevalence.rds\
	data/processed/input_mean.rds\
	data/processed/input_lower.rds\
	data/processed/input_upper.rds
	Rscript $<
	
data/processed/m_fits_off.rds: R/fit_m_resistance_off.R\
	data/raw/monthly_prevalence.rds\
	data/processed/input_mean.rds\
	data/processed/input_lower.rds\
	data/processed/input_upper.rds
	Rscript $<
	
data/processed/m_fits_cascades_olyset.rds: R/fit_m_cascades_olyset.R\
	data/raw/prevalence_olyset_mira.rds\
	data/processed/input_mean.rds\
	data/raw/itn.rds
	Rscript $<
	
data/processed/m_fits_all.rds: R/fit_m_combine.R\
	data/processed/m_fits_on.rds\
	data/processed/m_fits_cascades_olyset.rds\
	data/raw/prevalence_olyset_mira.rds\
	data/raw/monthly_prevalence.rds
	Rscript $<

# note $< gives first prerequisite; $* allows access to %; $@ allows access to target
$(FIGURE_FITS): outputs/fit_plots/fit_res_on_%.pdf: R/plot_fits_non_cascades.R\
	data/raw/monthly_prevalence.rds\
	data/processed/input_mean.rds\
	data/processed/input_lower.rds\
	data/processed/input_upper.rds\
	data/processed/m_fits_on.rds
	Rscript $< $* -o $@
	
outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf: R/plot_fits_cascades.R\
	data/raw/monthly_prevalence.rds\
	data/raw/prevalence_olyset_mira.rds\
	data/processed/input_mean.rds\
	data/processed/m_fits_all.rds
	Rscript $<

data/processed/cascades_fit_prevalence_1.rds: outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf
data/processed/cascades_fit_prevalence_2.rds: outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf
	
data/processed/counterfactual_params.rds: R/counterfactual_parameter_sets.R\
	data/processed/input_mean.rds
	Rscript $<

$(COUNTERFACTUAL_RUNS): outputs/simulations/counterfactual_%.rds: R/counterfactual.R\
	data/processed/counterfactual_params.rds\
	data/raw/monthly_prevalence.rds\
	data/processed/input_mean.rds\
	data/processed/input_lower.rds\
	data/processed/input_upper.rds
	Rscript $< $*

outputs/simulations/counterfactual_all_simulations.rds: R/counterfactual_simulations_process.R\
	$(COUNTERFACTUAL_RUNS)
	Rscript $<
	
outputs/counterfactual_plots/scenarios.pdf: R/plot_counterfactuals.R\
	outputs/simulations/counterfactual_all_simulations.rds
	Rscript $<
	
outputs/counterfactual_plots/scenarios_lower.pdf: outputs/counterfactual_plots/scenarios.pdf
outputs/counterfactual_plots/scenarios_upper.pdf: outputs/counterfactual_plots/scenarios.pdf

outputs/counterfactual_plots/interventions.pdf: R/plot_interventions.R\
	data/processed/input_mean.rds
	Rscript $<
	
data/processed/intervention_coverages_whole_burkina.rds: outputs/counterfactual_plots/interventions.pdf
	
outputs/counterfactual_plots/cases_absolute.pdf: R/counterfactual_cases.R\
	outputs/simulations/counterfactual_all_simulations.rds\
	data/raw/population_projections.RData\
	data/raw/wmr_cases_2010_2018.csv
	Rscript $<

outputs/counterfactual_plots/cases_percentage.pdf: outputs/counterfactual_plots/cases_absolute.pdf
outputs/counterfactual_plots/cases_admin1_percentage.pdf: outputs/counterfactual_plots/cases_percentage.pdf
outputs/simulations/counterfactual_cases_absolute.rds: outputs/counterfactual_plots/cases_percentage.pdf

outputs/counterfactual_plots/age_prevalence_cascades.pdf: R/plot_age_dependent_prevalence_cascades.R\
	data/raw/prevalence_age_cascades.rds\
	outputs/simulations/counterfactual_all_simulations.rds
	Rscript $<

outputs/data/processed/cascades_actual_fitted_age_prevalence.rds: outputs/counterfactual_plots/age_prevalence_cascades.pdf

data/processed/projection_params.rds: R/projection_parameter_sets.R\
	data/processed/input_mean.rds
	Rscript $<

$(PROJECTION_RUNS): outputs/simulations/projection_%.rds: R/projection.R\
	data/processed/projection_params.rds
	Rscript $< $*
	
outputs/simulations/projection_all_simulations.rds: R/projection_simulations_process.R\
	$(PROJECTION_RUNS)
	Rscript $<

outputs/projection_plots/scenarios.pdf: R/plot_projections.R\
	outputs/simulations/projection_all_simulations.rds
	Rscript $<

outputs/projection_plots/scenarios_lower.pdf: outputs/projection_plots/scenarios.pdf
outputs/projection_plots/scenarios_upper.pdf: outputs/projection_plots/scenarios.pdf