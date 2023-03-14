supplemental_counts_df <- bind_rows(
  supplemental_lmas,
  supplemental_smas
) |> 
  count(supplemental_value,
        program, supplemental_metric, primary_metric, primary_value) |> 
  group_by(program, supplemental_metric, primary_metric) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))