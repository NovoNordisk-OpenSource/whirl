
# SQLite

db <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "test.sqllite")

DBI::dbListTables(db)

DBI::dbDisconnect(db)

p <- callr::r_session$new()
p

start_strace(p$get_pid(), "strace.log")

do_something <- function() {
  db <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "test.sqllite")

  DBI::dbWriteTable(db, "mtcars", mtcars, append = TRUE)

  DBI::dbDisconnect(db)
}

p$kill()

# Postgres

db <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "postgres",
  user = "postgres",
  password = "admin",
  port = 32768,
  host = "localhost"
)

DBI::dbListTables(db)

DBI::dbDisconnect(db)

do_something <- function() {

  readLines("_pkgdown.yml")

  db <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "postgres",
    user = "postgres",
    password = "admin",
    port = 32768,
    host = "localhost"
  )

  readLines("_pkgdown.yml")

  DBI::dbReadTable(db, "mtcars")

  DBI::dbWriteTable(db, "mtcars4", mtcars, append = TRUE)

  readLines("_pkgdown.yml")

  DBI::dbDisconnect(db)
}

p <- callr::r_session$new()
p

start_strace(p$get_pid(), "strace.log")

p$run(do_something)

p$kill()

