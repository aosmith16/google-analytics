render_report = function(startdate, enddate) {
     rmarkdown::render(
          "ga_report.Rmd", 
          params = list(
               startdate = startdate,
               enddate = enddate
          ),
          output_file = here::here("reports", paste0(enddate, "_GA_report", ".pdf") )
     )
}
render_report(startdate = "2020-06-05", enddate = "2020-06-11")
