# Code to create the credential files

library(keyring)

credentials_df <- data.frame(
  user = c("username", "nick"), # mandatory
  password = c("password","12345"), # mandatory
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE)

key_set("BSD_dashboard_key", "Kaiser-Myer-Olkin")

create_db(
  credentials_data = credentials_df,
  sqlite_path = "C:/Users/cassidy.nicholas/OneDrive - IS/building-standards-dashboard/admin/logins.sqlite", # will be created
  passphrase = key_get("BSD_dashboard_key", "Kaiser-Myer-Olkin")
)


saveRDS(credentials_df, "C:/Users/cassidy.nicholas/OneDrive - IS/building-standards-dashboard/admin/credentials.rds")
