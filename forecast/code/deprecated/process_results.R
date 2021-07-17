library(covidcast)
library(dplyr)
library(tibble)

train_types = c('honest', 'dishonest')
idx = 2
train_type = train_types[idx]

results = readRDS(sprintf('results/results_%s.RDS', train_type))
actuals = readRDS('actuals.RDS')

results = results %>% left_join (
    actuals,
    by=c('geo_value', 'target_end_date'),
  )

print(results %>% group_by (
    forecaster,
  ) %>% summarize (
    count = n()
  ))

results %>% summarize (
    wis_na = sum(is.na(wis)),
    ae_na = sum(is.na(ae)),
    actual_na = sum(is.na(actual)),
  ) %>% print

# drop unnecessary columns
results = results %>% select (
    -target_end_date,
  )

dim(results)

results %>% group_by (
    forecaster,
  ) %>% summarize (
    count = n(),
  )

# create google subset forecaster that inherits AR3 where missing
gs_results = results %>% filter (
    forecaster == 'AR3GSSAA3_Subset',
  )
AR3_results = results %>% filter (
    forecaster == 'AR3',
  )
inherited_results = AR3_results %>% left_join (
    gs_results %>% select(
        -forecaster,
        -actual,
      ) %>% rename (
        wis_gs = wis,
        ae_gs = ae,
      ),
    by=c('geo_value', 'ahead', 'forecast_date')
  ) %>% mutate (
    forecaster = 'AR3_GSSAA3_Subset_InheritAR3',
    wis = coalesce(wis_gs, wis),
    ae = coalesce(ae_gs, ae),
  ) %>% select (
    -wis_gs,
    -ae_gs,
  )
results = bind_rows(results, inherited_results)

results %>% group_by (
    forecaster,
  ) %>% summarize (
    count = n(),
  )
    
na_results = results %>% group_by (
    forecaster,
  ) %>% summarize (
    na_wis = sum(is.na(wis)),
    na_ae = sum(is.na(ae)),
  )
print(na_results)

dim(results)

min_time = min(results$forecast_date)
max_time = max(results$forecast_date)
# get forecast_date case rates
cases_df = covidcast_signal(data_source = "jhu-csse",
                            signal = "confirmed_7dav_incidence_prop",
								 start_day = min_time, end_day = max_time,
                 as_of=lubridate::today(),
								 geo_type = "hrr")
cases_df = tibble(cases_df)
cases_df_old = cases_df
cases_df = cases_df %>% transmute (
    geo_value = as.character(geo_value),
    forecast_date = time_value,
    case_rate = pmax(0, value) + 1,
  )
# integrity check
mean(is.na(cases_df$case_rate))

results_with_cases = results %>% left_join (
    cases_df,
    by=c('geo_value','forecast_date')
  ) %>% rename (
    forecast_date_case_rate=case_rate,
  )

dim(results)

results_with_strawman = results_with_cases %>% left_join (
    results_with_cases %>% filter (
      forecaster == 'Baseline'
    ) %>% select (
      ahead, geo_value, wis, ae, forecast_date,
    ) %>% rename (
      strawman_wis = wis,
      strawman_ae = ae,
    ),
    by=c('ahead', 'geo_value', 'forecast_date')
  )

results
results_with_cases
results_with_strawman

results_base = results

results = results_with_strawman %>% mutate (
    relative_wis = wis / strawman_wis,
    relative_ae =  ae / strawman_ae,
    scaled_wis_fd = wis / forecast_date_case_rate,
    #scaled_wis_td = wis / target_date_case_rate,
    strawman_scaled_wis_fd = strawman_wis / forecast_date_case_rate,
    #strawman_scaled_wis_td = strawman_wis / target_date_case_rate,
    scaled_centered_wis_fd = scaled_wis_fd - strawman_scaled_wis_fd,
    #scaled_centered_wis_td = scaled_wis_td - strawman_scaled_wis_td,
    scaled_ae_fd = ae / forecast_date_case_rate,
    #scaled_ae_td = ae / target_date_case_rate,
    strawman_scaled_ae_fd = strawman_ae / forecast_date_case_rate,
    #strawman_scaled_ae_td = strawman_ae / target_date_case_rate,
    scaled_centered_ae_fd = scaled_ae_fd - strawman_scaled_ae_fd,
    #scaled_centered_ae_td = scaled_ae_td - strawman_scaled_ae_td,
  ) %>% select (
    -strawman_scaled_wis_fd,
    -strawman_scaled_ae_fd,
  )

# Sanity check:
# Taking mean of scaled_centered_wis_fd per forecaster should be equal to
# taking mean of scaled_wis_fd, strawman_scaled_wis_fd, and then taking diff

results %>% filter (
    ahead == 7,
  ) %>% group_by (
    forecaster,
  ) %>% summarize (
    m = mean(scaled_centered_wis_fd),
  )

xx =  results %>% filter (
    ahead == 7,
  ) %>% group_by (
    forecaster,
  ) %>% summarize (
    mean_scaled_wis_fd = mean(scaled_wis_fd),
  ) %>% pull (
    mean_scaled_wis_fd
  )
xx - xx[length(xx)]

print(results %>% group_by (
    forecaster,
#    geo_value,
    forecast_date,
  ) %>% summarize (
    n_na = sum(is.na(wis)),
  ) %>% filter (
    n_na > 0
  ), n=100)

saveRDS(results, sprintf('results/processed_results_%s.RDS', train_type))
