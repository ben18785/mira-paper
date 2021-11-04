f_likelihood <- function(a_adminLevel1, result, annual){
  result$year <- result$year + 2000
  result$year1 <- floor(result$year)
  result <- result %>%
    group_by(year1) %>%
    mutate(month=seq_along(year1)) %>%
    mutate(year=floor(year)) %>%
    filter(year <= 2018)
  aShortDF <- annual %>%
    filter(adminLevel1==a_adminLevel1) %>%
    filter(!is.na(prevalence))
  lAgeRange <- unique(aShortDF$age_range_r[!is.na(aShortDF$age_range_r)])
  lAgeRange1 <- gsub("-", "_", lAgeRange)
  lNames <- map_chr(lAgeRange1, ~paste0("prev_", .))
  col_nums <- match(c("year", "month", lNames), colnames(result))

  # iterate through age ranges and years and determine log-likelihood
  aShortDF$prevalence_sim <- NA
  k <- 1
  for(i in 1:length(lAgeRange)){
    aTempDF <- aShortDF %>%
      filter(age_range_r==lAgeRange[i])
    res <- result[, c(col_nums[1:2], col_nums[2 + i])]
    for(j in 1:nrow(aTempDF)){
      res1 <- res %>%
        filter(year==aTempDF$year[j],
               month==aTempDF$month[j])
      phi <- res1[1, 3][[1]]
      aShortDF$prevalence_sim[k] <- phi
      k <- k + 1
    }
  }
  f_log_likelihood <- function(kappa) {
    len <- nrow(aShortDF)
    lLogLikelihood <- vector(length = len)
    for(i in 1:len) {
      lLogLikelihood[i] <- dbetabinom(aShortDF$positive[i],
                                      size = aShortDF$size[i],
                                      alpha = aShortDF$prevalence_sim[i] * kappa,
                                      beta = (1 - aShortDF$prevalence_sim[i]) * kappa,
                                      log = T)
    }
    return(-sum(lLogLikelihood))
  }
  opt <- optim(5, f_log_likelihood, lower = 1, upper = 100, method="Brent")

  return(list(kappa=opt$par, log_likelihood=opt$value))
}

dbetabinom <- function(x, size, alpha, beta, log=FALSE){
  log_pdf <- lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta)
  if(!log)
    return(exp(log_pdf))
  else
    return(log_pdf)
}

f_run_negative_loglikelihood <- function(m, NAME_1_, annual,
                                         temp_input, num_people_=10000) {
  a_df <- temp_input %>%
    filter(NAME_1==NAME_1_) %>%
    mutate(total_M=m) %>%
    mutate(num_people=num_people_)
  temp <- purrr::pmap(a_df, f_run_model)
  outputs <- temp[[1]]$output
  fit <- f_likelihood(NAME_1_, outputs, annual)
  print(paste0("m = ", m, ", log-likelihood = ", fit$log_likelihood))
  return(fit$log_likelihood)
}

# f_run_negative_loglikelihood(15, "Cascades", annual, temp_input, num_people_ = 1000)

f_find_m <- function(NAME_1_, annual, temp_input, num_people_ = 10000) {
  f <- function(m) f_run_negative_loglikelihood(m, NAME_1_, annual,
                                                temp_input, num_people_)
  fit <- optim(10, f, lower = 0.1, upper = 400, method = "Brent",
               control = list(maxit = 20,
                              reltol=10^(-3)))
  return(list(m=fit$par, log_likelihood_negative=fit$value))
}


f_plot_specific_simple <- function(NAME_1, outputs, annual){
  temp <- outputs
  temp$year <- temp$year + 2000
  temp$date1 <- temp$year
  temp$year1 <- floor(temp$year)
  temp <- temp %>%
    group_by(year1) %>%
    mutate(month=seq_along(year1)) %>%
    mutate(year=floor(year)) %>%
    filter(year <= 2018)
  aShortDF <- annual %>%
    filter(adminLevel1==NAME_1)
  temp <- left_join(temp, aShortDF, by = c("year", "month"))

  lAgeRange <- temp$age_range_r
  lAgeRange1 <- gsub("-", "_", lAgeRange)
  lNames <- map_chr(lAgeRange1, ~paste0("prev_", .))
  temp$col <- match(lNames, colnames(temp))
  # temp <- temp[, c(c(1, match("month", colnames(temp)), match("date1", colnames(temp)), 50:ncol(temp)), unique(temp$col[!is.na(temp$col)]))]
  aLookup <- as.data.frame(table(annual$age_range_r)) %>%
    rename(age_range=Var1) %>%
    mutate(age_range_r=age_range) %>%
    left_join(annual %>% ungroup() %>%
                select(age_range_r, lower_age_r, upper_age_r) %>%
                unique(), by = "age_range_r") %>%
    mutate(middle=0.5 * (lower_age_r + upper_age_r)) %>%
    arrange(middle) %>%
    mutate(num=seq_along(age_range))
  annual$age_range_r <- factor(annual$age_range_r)
  annual$age_range_r <- fct_relevel(annual$age_range_r,
                                    as.character(aLookup$age_range_r))
  n <- n_distinct(annual$age_range_r)
  col <- setNames(hcl(seq(15,375,length=n+1)[1:n], 100, 65), levels(annual$age_range_r))

  temp1 <- select(temp %>% ungroup(), date1, contains("prev_")) %>%
    select(-contains("smooth")) %>%
    select(-contains("prev_all")) %>%
    select(-contains("prev_2_10")) %>%
    melt(id.vars="date1") %>%
    rename(age_range=variable)
  temp1$age_range <- gsub("prev_", "", temp1$age_range)
  temp1$age_range <- gsub("_", "-", temp1$age_range)
  temp1$num <- aLookup$age_range[match(temp1$age_range, aLookup$age_range)]
  temp1 <- temp1 %>%
    filter(!is.na(num))
  lAgeRange2 <- lAgeRange1[!is.na(lAgeRange1)] %>% unique()
  lAgeRange2 <- gsub("_", "-", lAgeRange2)
  temp1 <- temp1 %>%
    filter(age_range%in%lAgeRange2)
  temp <- temp %>%
    ungroup() %>%
    mutate(lower=qbeta(0.025, 1 + positive, 1 + size - positive),
           upper=qbeta(0.975, 1 + positive, 1 + size - positive)) %>%
    mutate(age_range_r=as.factor(age_range_r))
  temp$age_range_r=fct_relevel(temp$age_range_r,
                               as.character(aLookup$age_range_r[aLookup$age_range_r%in%unique(temp$age_range_r)]))

  ggplot(filter(temp, !is.na(age_range_r)), aes(x=date1)) +
    geom_pointrange(aes(y=prevalence, colour=age_range_r,
                        ymin=lower, ymax=upper)) +
    geom_line(data=filter(temp1, !is.na(age_range)), aes(y=value, colour=age_range)) +
    theme_classic() +
    scale_color_manual("Age range", values=col, drop=T) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ylab("Prevalence") +
    xlab("Year") +
    theme(axis.text = element_text(colour="black", size=14),
          axis.title = element_text(colour="black", size=14),
          title = element_text(colour="black", size=16),
          legend.text = element_text(colour="black", size=14))
}

remove_post_2018_interventions <- function(df_input) {
  df_int <- df_input$interventions[[1]] %>% 
    filter(year <= 2018)
  df_input$interventions[[1]] <- df_int
  df_input
}


fit_all_m <- function(resistance_off, temp_mean, temp_lower, temp_upper,
                  num_people=20000) {
  m_results <- matrix(nrow = nrow(temp_mean) * 3,
                      ncol = 4)
  k <- 1
  itn_level <- c("mean", "lower", "upper")
  for(j in seq(1, 3, 1)) {
    if(j == 1) {
      temp_input <- temp_mean
    }else if(j == 2) {
      temp_input <- temp_lower
    } else {
      temp_input <- temp_upper
    }
    for(i in 1:1) {
      temp <- temp_input[i, ]
      
      # remove all interventions > 2018
      temp <- remove_post_2018_interventions(temp)
      
      if(resistance_off) {
        res <- temp$resistance[[1]]
        res$resistance <- 0
        temp$resistance[[i]] <- res
      }
      
      print(temp$NAME_1)
      test <- f_find_m(temp$NAME_1, annual, temp, num_people)
      m_results[k, ] <- c(temp$NAME_1, itn_level[j], test$m, -test$log_likelihood_negative)
    }
  }
  colnames(m_results) <- c("NAME_1", "itn_scenario", "m", "log_likelihood")
  m_results <- as.data.frame(m_results)
  m_results
}
