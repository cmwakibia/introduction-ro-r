#### 3. Render the Report with Different Parameters

# You can render the report with different parameters using the `rmarkdown::render` function in an R script. For example, generating reports for both rural and urban regions with household and school interventions:
  

library(rmarkdown)

# Define the list of parameter sets
params_list <- list(
  list(region = "rural", intervention = "household", year = 2023),
  list(region = "rural", intervention = "school", year = 2023),
  list(region = "urban", intervention = "household", year = 2023),
  list(region = "urban", intervention = "school", year = 2023)
)

# Loop through the parameters and render the reports
for (i in seq_along(params_list)) {
  params <- params_list[[i]]
  output_file <- paste0("report_", params$region, "_", params$intervention, ".html")
  
  render("foundational-learning.Rmd", params = params, output_file = output_file)
}
