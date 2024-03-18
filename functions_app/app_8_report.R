
tempdir_path <- tempdir()
time <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_path <- paste0(tempdir_path, "/",time,"_report.rmd")
pdf_path <- paste0(tempdir_path, "/",time,"_report.pdf")
file.copy("functions_report/report.Rmd", report_path, overwrite = TRUE)

render_report <- function(my_params) {
  withProgress(message = 'Preparing PDF...', max = 1,  {

    if(file.exists(pdf_path)){file.remove(pdf_path)}
    rmarkdown::render(input = "functions_report/report.Rmd", #report_path,
                      output_dir = tempdir_path,
                      output_file = paste0("/",time,"_report.pdf"),
                      params = my_params
    , envir = new.env(parent = globalenv()))
    })
    }
