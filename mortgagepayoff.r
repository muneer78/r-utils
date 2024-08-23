library(lubridate)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(ggplot2)

# Helper functions to calculate IPMT and PPMT
IPMT <- function(rate, per, nper, pv) {
  payment <- pv * rate / (1 - (1 + rate)^-nper)
  ppmt <- PPMT(rate, per, nper, pv)
  return(payment - ppmt)
}

PPMT <- function(rate, per, nper, pv) {
  pmt <- pv * rate / (1 - (1 + rate)^-nper)
  ipmt <- pv * (1 + rate)^(per - 1) * rate
  return(pmt - ipmt)
}

create_single_run_df <- function(start_date, years, payments_year, interest, mortgage) {
  periods <- years * payments_year
  rng <- seq.Date(from = start_date, by = "month", length.out = periods)
  df <- tibble(
    Period = seq_len(periods),
    PaymentDate = format(rng, "%m-%Y"),
    OrgTotalPayment = NA_real_,
    TotalPayment = NA_real_,
    Interest = NA_real_,
    Principal = NA_real_,
    AdditionalPayment = NA_real_,
    OrgEndingBalance = NA_real_,
    EndingBalance = NA_real_
  )
  
  initial_pmt <- 965.99
  initial_ipmt <- -1 * IPMT(interest / payments_year, 1, periods, mortgage)
  initial_ppmt <- -1 * PPMT(interest / payments_year, 1, periods, mortgage)
  additional_pmt <- sample(0:3500, periods, replace = TRUE)
  
  df[1,] <- list(
    1,
    format(rng[1], "%m-%Y"),
    initial_pmt,
    initial_pmt + additional_pmt[1],
    initial_ipmt,
    initial_ppmt,
    additional_pmt[1],
    mortgage - initial_ppmt,
    mortgage - initial_ppmt - additional_pmt[1]
  )
  
  for (period in 2:periods) {
    previous_org_ending_balance <- df$OrgEndingBalance[period - 1]
    period_interest <- previous_org_ending_balance * interest / payments_year
    period_principal <- initial_pmt - period_interest
    additional_pmt <- sample(1000:3500, 1) + 400
    org_ending_balance <- previous_org_ending_balance - period_principal
    ending_balance <- df$`EndingBalance[period - 1] - period_principal - additional_pmt
    
    df[period,] <- list(
      period,
      format(rng[period], "%m-%Y"),
      initial_pmt,
      initial_pmt + additional_pmt,
      period_interest,
      period_principal,
      additional_pmt,
      org_ending_balance,
      ending_balance
    )
  }
  
  df <- df %>% filter(EndingBalance >= 15000) %>% tail(1)
  df %>% mutate(across(where(is.numeric), round, 2))
}

main <- function() {
  storage_df <- tibble()
  num_runs <- 1000
  start_date <- as.Date("2023-04-01")
  years <- 1
  payments_year <- 12
  interest <- 0.04125
  mortgage <- 35763.56
  
  for (i in seq_len(num_runs)) {
    last_row <- create_single_run_df(start_date, years, payments_year, interest, mortgage)
    storage_df <- bind_rows(storage_df, last_row)
  }
  
  finaldf <- storage_df %>% select(PaymentDate, EndingBalance)
  write_csv(finaldf, "allruns.csv")
  
  df <- read_csv("allruns.csv")
  df2 <- df %>% count(PaymentDate, name = "val")
  write_csv(df2, "payofftotals.csv")
  
  ggplot(df2, aes(x = PaymentDate, y = val)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 180, hjust = 1)) +
    labs(x = "Payment Date", y = "Value") +
    ggsave("PayoffGraph.pdf")
