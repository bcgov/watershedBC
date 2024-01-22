conn <- pool::dbPool(
  drv = RPostgres::dbDriver("PostgreSQL"),
  dbname = Sys.getenv("aw_dbname"),
  host = Sys.getenv("aw_host"),
  port = Sys.getenv("aw_port"),
  user = Sys.getenv("aw_user"),
  password = Sys.getenv("aw_password")
)
