primary_counts_df <- bind_rows(
  primary_lmas,
  primary_smas
) |> 
  count(primary_value, program, primary_metric) |> 
  group_by(program, primary_metric) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))