fit_site <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                     interventions, seasonality, resistance, target, param_a=0.89,
                     param_b=0.47, num_runs=3, num_people=10000,
                     beta_1=-1.431, beta_2=5.603){

  # Interventions
  itn <- mlgts:::itn_flexible_input(0:18, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:18, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:18, interventions$smc, interventions$smc_rounds)
  tx <- mlgts::treat_flexible_input(0:18, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:18, interventions$rtss)

  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:19],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b, beta_1, beta_2)

  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:19],
                                                  interventions$irs_compound)

  options = paste(paste0("num_people ", num_people, " final_run 19 itn_usage 1 num_runs ", num_runs), "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)

  site <- mlgts::site_create(vectors, seasonality)

  f1 <- mlgts::fit_m(variable = "clin_inc_all_smooth",
                     target = target$target,
                     # These rows are the average inc over years 2016, 2017 and 2018
                     rows = c(205, 217, 229),
                     maxiter = 20,
                     tolerance = 0.01,
                     extendInt = "upX",
                     interval = c(0.1, 50),
                     site = site, demog = demog, pop = pop, options = options)

  name <- paste(Continent, ISO, NAME_0, NAME_1, NAME_2, ur, sep = "_")
  write.csv(data.frame(Continent = Continent,
                       ISO = ISO,
                       NAME_0 = NAME_0,
                       NAME_1 = NAME_1,
                       NAME_2 = NAME_2,
                       ur = ur,
                       total_M = f1),
            paste0("outputs/total_M/", name, ".csv"),
            row.names= FALSE)

  return(f1)
}

fit_site_prev <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                     interventions, seasonality, resistance, target, param_a=0.89,
                     param_b=0.47, num_runs=3, num_people=10000, rows_=c(205, 217, 229),
                     beta_1=-1.431, beta_2=5.603){

  # Interventions
  itn <- mlgts:::itn_flexible_input(0:18, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:18, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:18, interventions$smc, interventions$smc_rounds)
  tx <- mlgts::treat_flexible_input(0:18, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:18, interventions$rtss)

  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:19],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b,
                                                 beta_1, beta_2)

  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:19],
                                                  interventions$irs_compound)

  options = paste(paste0("num_people ", num_people, " final_run 19 itn_usage 1 num_runs ", num_runs), "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)

  site <- mlgts::site_create(vectors, seasonality)

  f1 <- mlgts::fit_m(variable = "prev_0_5_smooth",
                     target = target$target,
                     # These rows are the average inc over years 2016, 2017 and 2018
                     rows = rows_,
                     maxiter = 20,
                     tolerance = 0.01,
                     extendInt = "upX",
                     interval = c(0.1, 50),
                     site = site, demog = demog, pop = pop, options = options)

  name <- paste(Continent, ISO, NAME_0, NAME_1, NAME_2, ur, sep = "_")
  write.csv(data.frame(Continent = Continent,
                       ISO = ISO,
                       NAME_0 = NAME_0,
                       NAME_1 = NAME_1,
                       NAME_2 = NAME_2,
                       ur = ur,
                       total_M = f1),
            paste0("outputs/total_M/", name, ".csv"),
            row.names= FALSE)

  return(f1)
}

fit_site_prev_2_10 <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                          interventions, seasonality, resistance, target, param_a=0.89,
                          param_b=0.47, num_runs=3, num_people=10000, rows_=c(205, 217, 229),
                          beta_1=-1.431, beta_2=5.603){

  # Interventions
  itn <- mlgts:::itn_flexible_input(0:18, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:18, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:18, interventions$smc, interventions$smc_rounds)
  tx <- mlgts::treat_flexible_input(0:18, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:18, interventions$rtss)

  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:19],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b,
                                                 beta_1,
                                                 beta_2)

  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:19],
                                                  interventions$irs_compound)

  options = paste(paste0("num_people ", num_people, " final_run 19 itn_usage 1 num_runs ", num_runs), "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)

  site <- mlgts::site_create(vectors, seasonality)

  f1 <- mlgts::fit_m(variable = "prev_2_10_smooth",
                     target = target$target,
                     # These rows are the average inc over years 2016, 2017 and 2018
                     rows = rows_,
                     maxiter = 20,
                     tolerance = 0.01,
                     extendInt = "upX",
                     interval = c(0.1, 50),
                     site = site, demog = demog, pop = pop, options = options)

  name <- paste(Continent, ISO, NAME_0, NAME_1, NAME_2, ur, sep = "_")
  write.csv(data.frame(Continent = Continent,
                       ISO = ISO,
                       NAME_0 = NAME_0,
                       NAME_1 = NAME_1,
                       NAME_2 = NAME_2,
                       ur = ur,
                       total_M = f1),
            paste0("outputs/total_M/", name, ".csv"),
            row.names= FALSE)

  return(f1)
}

f_run_model <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                        interventions, seasonality, resistance, target,
                        param_a=0.89, param_b=0.47, num_runs=3, num_people=10000,
                        lower_smc_age=0.5,
                        upper_smc_age=5){
  # Interventions
  itn <- mlgts:::itn_flexible_input(0:18, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:18, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:18, interventions$smc, interventions$smc_rounds,
                                    lower_smc_age,
                                    upper_smc_age)
  tx <- mlgts::treat_flexible_input(0:18, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:18, interventions$rtss)

  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:19],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b)

  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:19],
                                                  interventions$irs_compound)

  options = paste(paste0("num_people ", num_people, " final_run 19 itn_usage 1 num_runs ", num_runs),
                  "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)

  site <- mlgts::site_create(vectors, seasonality)

  options <- paste(paste("total_M", total_M, sep = ' '), options, sep = ' ')
  temp <- suppressMessages(launch(name = 'Fitting_output',
                                  options = options,
                                  site = site, demog = demog, pop = pop))
  return(temp)
}

f_run_model_longer <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                        interventions, seasonality, resistance, target,
                        param_a=0.89, param_b=0.47, num_runs=3, num_people=10000,
                        beta_1=-1.431, beta_2=5.603, lower_smc_age,
                        upper_smc_age){
  # Interventions
  itn <- mlgts:::itn_flexible_input(0:25, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:25, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:25, interventions$smc, interventions$smc_rounds,
                                    interventions$lower_smc_age, interventions$upper_smc_age)
  tx <- mlgts::treat_flexible_input(0:25, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:25, interventions$rtss)

  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:26],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b,
                                                 beta_1, beta_2)

  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:26],
                                                  interventions$irs_compound)

  options = paste(paste0("num_people ", num_people, " final_run 26 itn_usage 1 num_runs ", num_runs), "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)

  site <- mlgts::site_create(vectors, seasonality)

  options <- paste(paste("total_M", total_M, sep = ' '), options, sep = ' ')
  temp <- suppressMessages(launch(name = 'Fitting_output',
                                  options = options,
                                  site = site, demog = demog, pop = pop))
  return(temp)
}

f_run_model_now <- function(total_M, Continent, ISO, NAME_0, NAME_1, NAME_2, ur, vectors, demog, pop,
                               interventions, seasonality, resistance, target,
                               param_a=0.89, param_b=0.47, num_runs=3, num_people=10000,
                               beta_1=-1.431, beta_2=5.603, lower_smc_age,
                               upper_smc_age){
  # Interventions
  itn <- mlgts:::itn_flexible_input(0:20, interventions$llin, num_runs = num_runs)
  irs <- mlgts::irs_flexible_input(0:20, interventions$irs)
  smc <- mlgts::smc_flexible_input2(0:20, interventions$smc, interventions$smc_rounds,
                                    interventions$lower_smc_age, interventions$upper_smc_age)
  tx <- mlgts::treat_flexible_input(0:20, interventions$tx * (1 - interventions$prop_act), interventions$tx * interventions$prop_act, rep(0, 19), rep(0, 19))
  rtss <- mlgts::epi_pev_flexible_input(0:20, interventions$rtss)
  
  # LLIN resistance params
  lparam <- mlgts::itn_resistance_flexible_input(interventions$year - 2000,
                                                 resistance$resistance[1:21],
                                                 interventions$net_type,
                                                 param_a,
                                                 param_b,
                                                 beta_1, beta_2)
  
  # IRS resistance params
  iparam <- mlgts:::irs_resistance_flexible_input(interventions$year - 2000,
                                                  resistance$resistance[1:21],
                                                  interventions$irs_compound)
  
  options = paste(paste0("num_people ", num_people, " final_run 21 itn_usage 1 num_runs ", num_runs), "add", itn, irs, smc, tx, rtss,
                  mlgts::bionomics_string(vectors), lparam, iparam)
  
  site <- mlgts::site_create(vectors, seasonality)
  
  options <- paste(paste("total_M", total_M, sep = ' '), options, sep = ' ')
  temp <- suppressMessages(launch(name = 'Fitting_output',
                                  options = options,
                                  site = site, demog = demog, pop = pop))
  return(temp)
}
