run_site <- function(run, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                     interventions, seasonality, resistance, total_M, target, draw = 0){
  
  # Interventions
  itn <- mlgts:::itn_flexible_input(0:31, interventions$llin, num_runs = 3)
  irs <- mlgts::irs_flexible_input(0:31, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:31, interventions$smc, interventions$smc_rounds)
  tx <- mlgts::treat_flexible_input(0:31, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 31), rep(0, 31))
  rtss <- mlgts::epi_pev_flexible_input(0:31, interventions$rtss)
  
  # LLIN resistance params
  resistance <- resistance$resistance
  resistance <- c(resistance, resistance[length(resistance)])
  
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance, 
                                                 interventions$net_type)
  
  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance, 
                                                  interventions$irs_compound)
  
  options = paste("num_people 10000 final_run 31 itn_usage 1 num_runs 3 add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)
  
  site <- mlgts::site_create(vectors, seasonality, total_M)
  
  # Run the model
  name <- paste0(NAME_0, "_", NAME_1, "_", NAME_2, "_", ur, "_", draw)
  name <- gsub(" ", "", name)
  model_out <- mlgts::launch(name, site = site, demog = demog, pop = pop, options = options, draw = draw)
  
  # Basic output formatting
  output <- model_out$output
  output$run <- run
  output$Continent <- Continent
  output$ISO <- ISO
  output$NAME_0 <- NAME_0
  output$NAME_1 <- NAME_1
  output$NAME_2 <- NAME_2
  output$ur <- ur
  output$draw <- draw
  output$t <- output$year + 2000
  output$year <- floor(output$year + 2000)
  output$month <- c(rep(1:12, 31), 1)
  
  # Save output
  output_address <- paste0(run, "/output/raw_model_output/", name, ".csv")
  write.csv(output, output_address, row.names = FALSE)
}