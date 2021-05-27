all: data/processed/input_bf_only.rds

data/processed/input_bf_only.rds: data/raw/input.RData data/raw/clinical.rds data/raw/subnational_resistance.rds R/clean_and_produce_BF_data.R
	Rscript R/clean_and_produce_BF_data.R

clean:
	rm -f data/processed/input_bf_only.rds
