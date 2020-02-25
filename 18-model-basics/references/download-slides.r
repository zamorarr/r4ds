files <- c(
  "https://github.com/rstudio-education/master-the-tidyverse/raw/master/slides/07-Models.pdf",
  "https://github.com/rstudio-education/master-the-tidyverse/raw/master/exercises/07-Models.Rmd",
  "https://github.com/rstudio-education/master-the-tidyverse/raw/master/solutions/07-Models-solutions.Rmd",
  "https://github.com/rstudio-education/master-the-tidyverse-instructors/raw/master/powerpoints/07-Models.pptx"
)

for (f in files) {
  path <- file.path("18-model-basics/references", basename(f))
  if (!file.exists(path)) {
    download.file(f, path)
  }
}
