all: data/processed/m_fits_all.rds data/processed/m_fits_off.rds outputs/fit_plots/fit_res_on_%.pdf

data/processed/input_bf_only.rds: data/raw/input.RData data/raw/clinical.rds data/raw/subnational_resistance.rds R/clean_and_produce_BF_data.R
	Rscript R/clean_and_produce_BF_data.R

data/processed/m_fits_on.rds: data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds R/fit_m_resistance_on.R
	Rscript R/fit_m_resistance_on.R
	
data/processed/m_fits_off.rds: data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds R/fit_m_resistance_off.R
	Rscript R/fit_m_resistance_off.R
	
data/processed/m_fits_cascades_olyset.rds: data/raw/prevalence_olyset_mira.rds data/processed/input_bf_only.rds data/raw/itn.rds R/fit_m_cascades_olyset.R
	Rscript R/fit_m_cascades_olyset.R
	
data/processed/m_fits_all.rds: data/processed/m_fits_on.rds data/processed/m_fits_cascades_olyset.rds data/raw/prevalence_olyset_mira.rds data/raw/monthly_prevalence.rds R/fit_m_combine.R
	Rscript R/fit_m_combine.R
	
outputs/fit_plots/fit_res_on_%.pdf: data/raw/monthly_prevalence.rds data/processed/input_bf_only.rds data/processed/m_fits_on.rds R/plot_fits_non_cascades.R
	Rscript R/plot_fits_non_cascades.R
	
outputs/fit_plots/fit_res_on_cascades_inc_olyset.pdf: data/raw/monthly_prevalence.rds data/raw/prevalence_olyset_mira.rds data/processed/input_bf_only.rds data/processed/m_fits_all.rds R/plot_fits_cascades.R
	Rscript R/plot_fits_cascades.R

# clean:
	# rm -f data/processed/input_bf_only.rds
