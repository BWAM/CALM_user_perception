supplemental_counts_df <- bind_rows(
  primary_lmas,
  primary_smas
) |> 
  group_by(program,primary_value, primary_metric, supplemental_metric) |> 
  count(supplemental_value,supplemental_metric, primary_metric) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))

supplemental_counts_pwl <- bind_rows(
  primary_lmas_mr,
  primary_smas_mr
) |> 
  group_by(program,primary_value, primary_metric, supplemental_metric) |> 
  count(supplemental_value,supplemental_metric, primary_metric) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))

supplemental_counts_pwl_raw <- bind_rows(
  primary_lmas_mr,
  primary_smas_mr
)
