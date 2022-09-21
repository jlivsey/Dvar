# Setup Launch scripts for DVar sim study

# Define file names you want
launchFiles <- c(
  "launch01.R",
  "launch02.R",
  "launch03.R",
  "launch04.R",
  "launch05.R",
  "launch06.R",
  "launch07.R",
  "launch08.R",
  "launch09.R",
  "launch10.R",
  "launch11.R",
  "launch12.R"
)

for(i in seq_along(launchFiles)){

  x <- readLines("launch-script.R")

  # New lines
  new_lines <- character(2)
  new_lines[1] <- sprintf("WLidx <- WLlist[[%d]]", i)
  new_lines[2] <- sprintf("saveFileName <- \"results%02d.RData\"", i)

  # Find lines to replace
  replace_idx <- grep(pattern = "#REPLACEME", x = x)

  # replace lines
  x[replace_idx] <- new_lines

  f <- launchFiles[i]
  # Overwrite old run.R file
  writeLines(text = x, f)
}
