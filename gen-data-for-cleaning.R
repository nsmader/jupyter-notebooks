#------------------------------------------------------------------------------#
#
### Generate Data for Cleaning
#
#------------------------------------------------------------------------------#

# This script generates fake data with a set of features that help demonstrate
# data cleaning features

#------------------------------------------------------------------------------#
### Set meta parameters --------------------------------------------------------
#------------------------------------------------------------------------------#

rm(list = ls())
setwd("~/GitHub/knitr-sandbox/")
library(dplyr)
library(data.table)


set.seed(60637)
n <- 3000
years <- 2014:2016

progs <- c("Reading for Tots", "Media Lab")
sitetype <- c("CBO", "Branch")
sites_CBO <- paste(sample(state.name, 3), c("Center", "Academy", "Center"))
sites_Branch <- c("Aiello", "Baraga", "De Bow", "Goldberg", "Hasan", "Korczak",
                  "Moccasin", "Nelson", "Thuis", "Wang", "Wieboldt") # These are sample names of Chicago playlot parks
activities_Tots <- c("Reading Group", "Story Writing")
activities_Lab <- c("Sound/Recording", "Video", "Computer Animation", "Gaming")

#------------------------------------------------------------------------------#
### Generate basic data --------------------------------------------------------
#------------------------------------------------------------------------------#

# Ids and ages
ids <- round(runif(n = n, min = 1e5, max = 1e6-1), 0)
ages <- sample(6:16, n, replace = TRUE, prob = (16:6+5)^2)
df <- data.frame(id = ids, age = ages)

# Sample multiple times
df <- rbind(df, df[sample(1:nrow(df), round(n*(length(years)-1), 0), replace = TRUE),])
n <- nrow(df)
df <- within(df, {
  year = sample(years, n, replace = TRUE, prob = c(9, 10, 11))
  age <- age + year - min(years)
})

# Programs and site types

agenudge <- round(rnorm(n), 0)
nudgedage <- df$age + agenudge
progs_i <- progs[(nudgedage<10)+1]
sitetype_i <- sample(sitetype, n, replace = TRUE, prob = c(0.1, 0.9))

# Activities in programs, sites in site types

activities_i <- sapply(progs_i,
                       function(p) {
                         switch(p,
                                "Reading for Tots" = sample(activities_Tots, 1, prob = c(0.45, 0.55)),
                                "Media Lab" = sample(activities_Lab, 1, prob = c(0.3, 0.4, 0.1, 0.2)))
                         
                       })
sites_i <- sapply(sitetype_i,
                  function(s) {
                    switch(s,
                           "CBO"    = sample(sites_CBO,    1, prob = runif(length(sites_CBO))),
                           "Branch" = sample(sites_Branch, 1, prob = runif(length(sites_Branch))))
                  })

# Multiple years with appropriately advancing age and only sometimes continuation
df <- data.frame(df, prog = progs_i, sitetype = sitetype_i,
                 site = sites_i, activity = activities_i,
                 stringsAsFactors = FALSE)

#------------------------------------------------------------------------------#
### Build in desired features for cleaning -------------------------------------
#------------------------------------------------------------------------------#

# Ensure duplicates of IDs, including for certain programs within years, to require aggregation/deduplication

df <- rbind(df,
            df[sample(1:nrow(df), round(nrow(df)/2, 0)),])


# Generate different notions of dosage

n <- nrow(df)
df <- within(df, {
  minutes <- ifelse(prog == "Reading for Tots", (rnorm(n = n, mean = age, sd = 2)/2)^2, NA)
  sessions <- ifelse(prog == "Media Lab", rpois(n, age), NA)
  credits <- ifelse(prog == "Media Lab" & activity %in% activities_Lab[1:2], rpois(n, sessions/10), NA)
})
summary(df)

# Generate extra miscellaneous columns (to be dropped), and mess with capitalization

df <- within(df, {
  QR4576 <- sample(c(143691, 23956, "ZY21309", "WGA239"), n, replace = TRUE)
  QR3292 <- sample(c(3496, 239601, 35960, "IW91xa"),      n, replace = TRUE)
  QR3962 <- sample(c(43269, 1329, 10329, "OS439"),        n, replace = TRUE)
  QR1329 <- sample(c(02965, 321096, 3294, "OQ396"),       n, replace = TRUE)
})

# Perform some data reshaping

df_Tots <- subset(df, grepl("Tot", prog)) %>%
  select(-one_of("sessions", "credits", "prog")) %>%
  select(-starts_with("QR")) %>%
  melt(id.vars = c("id", "age", "year", "sitetype", "site", "minutes")) %>%
  within(value <- paste0(value, " - Minutes")) %>%
  dcast(id + age + year + sitetype + site ~ value,
        value.var = "minutes", fun.aggregate = sum)
df_Lab  <- subset(df, grepl("Lab", prog), select = -minutes)

# Separate files across years, and ensure asymmetric offerings and inconsistent names

dfs <- NULL
for (p in c("Tots", "Lab")){  
  d <- get(paste0("df_", p))
  for (y in years){
    
    d_y <- subset(d, year == y)
    if (p == "Lab"){
      if (y == years[1]){
        d_y <- select(d_y, -one_of("credits", "QR4576"))
      } else if (y == years[2]) {
        d_y$activity <- tolower(d_y$activity)
        d_y <- select(d_y, -QR3292)
      } else if (y == years[3]) {
        d_y$activity <- toupper(gsub(" |/", "__", d_y$activity))  
        d_y <- select(d_y, -one_of("QR3962", "QR1329"))
      }
    }
    
    fname <- paste0(p, "Data_", y)
    assign(fname, d_y)
    dfs <- c(dfs, fname)
  }
}

save(list = dfs, file = "data-for-cleaning.Rda")
