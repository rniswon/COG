library(targets)
# library(tarchetypes)

tar_option_set(
  packages = c(
    "Template"
  ),
  memory = "transient"
)

options(clustermq.scheduler = "multiprocess")
# future::plan(future.callr::callr)

source("0-config.R")


list(
  # tar_target(
  #   a_file,
  #   file.path(a_folder, "a_file.csv"),
  #   format = "file"
  # ),
  # tar_target(
  #   a_data_object,
  #   a_command(
  #     a_file,
  #     an_argument
  #   ),
  #   format = "feather"
  # ),
  # tar_target(
  #   a_model_object,
  #   a_command(
  #     a_data_object,
  #     an_argument
  #   )
  # )
  tar_target(
    session_details,
    session_details()
  )
)
