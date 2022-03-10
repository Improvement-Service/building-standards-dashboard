# Code to create the credential files

credentials_df <- data.frame(
  user = c("username"), # mandatory
  password = c("password"), # mandatory
  stringsAsFactors = FALSE)

saveRDS(credentials_df, "C:/Users/cassidy.nicholas/OneDrive - IS/building-standards-dashboard/admin/credentials.rds")
