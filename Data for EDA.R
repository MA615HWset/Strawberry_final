rm(list = ls())
library(knitr)  
library(kableExtra)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)

strawberry <- read_csv("strawberries25_v3.csv", col_names = TRUE, show_col_types = FALSE )

strawberry <- strawberry |> 
  filter(`Geo Level`== "NATIONAL" | `Geo Level`== "STATE")


drop_one_value_col <- function(df, prt_val = FALSE){ 
  # browser()
  df_id <- ensym(df)
  if(prt_val){
    msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
    print(msg)}
  ## takes whole dataframe
  dropc <- NULL
  val <- NULL
  ## test each column for a single value
  for(i in 1:dim(df)[2]){   
    if(dim(distinct(df[,i]))[1] == 1){
      dropc <- c(dropc, i)
      val <- c(val, df[1,i])
    }
  } 
  
  if(prt_val){
    if(is.null(dropc)){
      print("No columns dropped")
      return(df)}else{
        print("Columns dropped:")
        # print(colnames(df)[drop])
        print(unlist(val))
        df <- df[, -1*dropc]
        return(df)
      }
  }
  df <- df[, -1*dropc]
  return(df)
}


## use the function

strawberry <- strawberry |> drop_one_value_col(prt_val = FALSE)

straw_sur <- strawberry |> filter(Program=="SURVEY")

straw_sur <- straw_sur |> drop_one_value_col()


straw_sur1 <- straw_sur |>  separate_wider_delim(cols = `Data Item`,
                                                 delim = ", ",
                                                 names = c("straw",
                                                           "mkt",
                                                           "measure",
                                                           "other"
                                                 ),
                                                 too_many = "merge",
                                                 too_few = "align_start")


straw_sur2 <- straw_sur1 |> separate_wider_delim(cols = "straw", 
                                                 delim = " - ",
                                                 names = c("straw",
                                                           "more"),
                                                 too_many = "merge",
                                                 too_few = "align_start"
)

shift_loc <- function(df, col_name, dat_name, num_col, num_shift){
  # browser()
  col_num = which(colnames(df) == col_name)
  row_num = which(df[,col_num] == dat_name)  ## calcs a vector of rows
  
  for(k in 1:length(row_num)){
    d = rep(0,num_col) ## storage for items to be moved
    for(i in 1:num_col){
      d[i] = df[row_num[k], col_num + i - 1]
    }
    for(i in 1:num_col){
      ra = row_num[k]
      cb = col_num + i - 1
      df[ra, cb] <-  NA
    }
    for(j in 1:num_col){
      rc = row_num[k]
      cd = col_num + j - 1 + num_shift
      df[rc, cd] = d[j]
    }
  }
  return(df)
}

straw_sur2 %<>% shift_loc("more", "PRICE RECEIVED", 2, 1 )

straw_sur2 %<>% shift_loc("more", "ACRES HARVESTED", 1, 1 )

straw_sur2 %<>% shift_loc("more", "ACRES PLANTED", 1, 1 )

straw_sur2 %<>% shift_loc("more", "PRODUCTION", 2, 1 )

straw_sur2 %<>% shift_loc("more", "YIELD", 2, 1 )

straw_sur2 %<>% shift_loc("more", "APPLICATIONS", 3, 1 )

straw_sur2 %<>% shift_loc("more", "TREATED", 3, 1 )

straw_sur2 %<>% drop_one_value_col()


straw_sur2 <- straw_sur2 |>  
  separate_wider_delim(cols = Domain,
                       delim = ", ",
                       names = c("col1",
                                 "col2"),
                       
                       too_many = "merge",
                       too_few = "align_start")


# unique(straw_sur2$col1)

survey_d_total <- straw_sur2 |>  filter(col1 == "TOTAL")

survey_d_chem <- straw_sur2 |>  filter(col1 == "CHEMICAL")

survey_d_fert <- straw_sur2 |>  filter(col1 == "FERTILIZER")

##################### Total ###########################

survey_d_total %<>% drop_one_value_col()


survey_d_total %<>% shift_loc("measure", "MEASURED IN $ / CWT", 1, 1 )


survey_d_total %<>% shift_loc("measure", "MEASURED IN $", 1, 1 )


survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT", 1, 1 )

survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS", 1, 1 )


survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT / ACRE", 1, 1 )

survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS / ACRE", 1, 1 )


survey_d_total <- survey_d_total |>
  separate_wider_delim(cols = mkt,
                       delim = " - ",
                       names = c("col3",
                                 "col4"),
                       too_many = "merge",
                       too_few = "align_start")

survey_d_total %<>%
  select(-`State ANSI`)

survey_d_total <- survey_d_total |> 
  group_by(Year) |> 
  group_by(State) |>
  group_by(Period) |>
  group_by(col3)

survey_d_total <-  survey_d_total |>
  shift_loc(col_name = "col3", dat_name = "PRODUCTION", 2, 1)

survey_d_total <-  survey_d_total |>
  shift_loc(col_name = "col3", 
            dat_name = "PRICE RECEIVED", 2, 1)

survey_d_total <-  survey_d_total |>
  rename(market = col3, product_price = col4, summ = measure, measure = other)

h_index <- which(str_detect(survey_d_total$market, "ACRES HARVESTED") == TRUE)
survey_d_total$product_price[h_index] <- "HARVESTED"
survey_d_total$measure[h_index] <- "acres"
survey_d_total$market[h_index] <- NA


p_index <- which(str_detect(survey_d_total$market, "ACRES PLANTED") == TRUE)
survey_d_total$product_price[p_index] <- "PLANTED"
survey_d_total$measure[p_index] <- "Acres"
survey_d_total$market[p_index] <- NA

survey_d_total$measure <- str_replace(survey_d_total$measure, "MEASURED IN ", "")


y_index <- which(str_detect(survey_d_total$market, "YIELD") == TRUE)
survey_d_total$product_price[y_index] <- "YIELD"
survey_d_total$market[y_index] <- NA

ns_index <- which(str_detect(survey_d_total$market, "NOT SOLD") == TRUE)
survey_d_total$product_price[ns_index] <- "NOT SOLD"
survey_d_total$market[ns_index] <- NA

u_index <- which(str_detect(survey_d_total$market, "UTILIZED") == TRUE)
survey_d_total$product_price[u_index] <- "UTILIZED"
survey_d_total$market[u_index] <- NA

rm(ns_index, p_index, u_index, y_index, h_index, straw_sur2)

########################### Fertilizer ###########################

survey_d_fert <- survey_d_fert |> drop_one_value_col()

survey_d_fert <- survey_d_fert |> select(-`State ANSI`)

survey_d_fert <- survey_d_fert |>  
  separate_wider_delim(cols = mkt,
                       delim = " - ",
                       names = c("mk1",
                                 "mk2"),
                       too_many = "merge",
                       too_few = "align_start")

survey_d_fert$measure <- str_replace(survey_d_fert$measure, "MEASURED IN ", "")

survey_d_fert$`Domain Category` <- str_replace(survey_d_fert$`Domain Category`, "CHEMICAL, ", "")

survey_d_fert <- survey_d_fert |> rename(chem = `Domain Category`)

survey_d_fert <- survey_d_fert |>
  separate_wider_delim(cols = chem,
                       delim = ": ",
                       names = c("type",
                                 "chem_type"),
                       too_many = "merge",
                       too_few = "align_start")

survey_d_fert <- survey_d_fert |> 
  rename(chem_name = chem_type)

survey_d_fert$chem_name <- str_replace(survey_d_fert$chem_name, "^\\(", "")

survey_d_fert$chem_name <- str_replace(survey_d_fert$chem_name, "\\)$", "")

survey_d_fert <- survey_d_fert |> drop_one_value_col()

########################## Chemical ############################

survey_d_chem <- survey_d_chem |> drop_one_value_col()

survey_d_chem <- survey_d_chem |> select(-`State ANSI`)

survey_d_chem <- survey_d_chem |>  
  separate_wider_delim(cols = mkt,
                       delim = " - ",
                       names = c("mk1",
                                 "mk2"),
                       too_many = "merge",
                       too_few = "align_start")


survey_d_chem$measure <- str_replace(survey_d_chem$measure, "MEASURED IN ", "")

unique(survey_d_chem$`Domain Category`)

survey_d_chem$`Domain Category` <- str_replace(survey_d_chem$`Domain Category`, "CHEMICAL, ", "")

survey_d_chem <- survey_d_chem |> rename(chem = `Domain Category`)

survey_d_chem <- survey_d_chem |>
  separate_wider_delim(cols = chem,
                       delim = ": ",
                       names = c("type",
                                 "chem_type"),
                       too_many = "merge",
                       too_few = "align_start")

survey_d_chem <- survey_d_chem |> select(-col2)

survey_d_chem <- survey_d_chem |> 
  rename(chem_name = chem_type)

survey_d_chem$chem_name <- str_replace(survey_d_chem$chem_name, "^\\(", "")

survey_d_chem$chem_name <- str_replace(survey_d_chem$chem_name, "\\)$", "")

survey_d_chem <- survey_d_chem |>  
  separate_wider_delim(cols = chem_name,
                       delim = " = ",
                       names = c("chem_name",
                                 "chem_index"),
                       too_many = "error",
                       too_few = "align_start")

survey_d_chem$Value[survey_d_chem$Value == "(D)"] <- 0

survey_d_chem$Value <- as.numeric(survey_d_chem$Value)

survey_d_chem1 <- survey_d_chem %>%
  filter(!is.na(Value))

subset_data <- survey_d_chem1 %>%
  filter(measure == "LB / ACRE / APPLICATION")

################### Merging dangerous chemical ########

pesticide_data <- read.csv("california-top-50-pesticides-download-1731886436478.csv")  

pesticide_data$Chemical.Name <- toupper(pesticide_data$Chemical.Name)

california_data <- subset(subset_data, State == "CALIFORNIA")
merged_data <- merge(california_data, pesticide_data, by.x = "chem_name", by.y = "Chemical.Name")


merged_data <- merged_data[, !(names(merged_data) %in% c("Gross.Pounds", "App.Rate...lbs.a.i...acre.", "Acres.Planted","Acres.Treated"))]

dangerous_chemicals <- subset(merged_data, PAN.Bad.Actor == "Yes")
