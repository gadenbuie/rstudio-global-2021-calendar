
available_timezones <- function() {
  available_timezones <- OlsonNames()
  tz_list <- list()
  for (tz in available_timezones) {
    if (grepl("/", tz, fixed = TRUE)) {
      tz_split <- strsplit(tz, "/", fixed = TRUE)[[1]]
      tz_group <- tz_split[1]
      tz_name <- paste(tz_split[-1], collapse = "/")
      names(tz) <- gsub("_", " ", tz_name)
      tz_list[[tz_group]] <- c(tz_list[[tz_group]], tz)
    } else {
      tz_list[["Other"]] <- c(tz_list[["Other"]], tz)
    }
  }
  tz_list[sort(names(tz_list))]
}
