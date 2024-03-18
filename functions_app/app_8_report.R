
report_path <- tempfile(fileext = ".Rmd")
pdf_path <- tempfile(fileext = ".pdf")
image_path <- tempfile(fileext = ".RData")
print(image_path)
file.copy("functions_report/report.Rmd", report_path, overwrite = TRUE)
render_report <- function(input, output, params) {rmarkdown::render(input, output_file = output, params = params, envir = new.env(parent = globalenv()))}
