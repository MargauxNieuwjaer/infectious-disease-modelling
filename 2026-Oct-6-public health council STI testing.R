#RESEARCH QUESTION: How does STI testing rate in one council compare
#with England and the other councils, and how has it changed over time?
#1. Where does our council sit?
#2. Has it changed over time?

#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("fingertipsR",
                 repos = c("https://ropensci.r-universe.dev",
                           "https://cloud.r-project.org"))

#DOWNLOAD LIBRARIES
library(tidyverse) #give us filter(), distinct(), write_csv()
library(janitor)
library(fingertipsR) #lets R talk to Fingertips website
                 
#CREATE CSV BACKUP COPY OF DOWNLOADED DATA USED 
dir.create("data", showWarnings = FALSE)
write_csv(sti, "data/sti_testing_raw.csv")                 

#ASK FINGERTIPS WHICH AREA TYPES EXIST FOR MY INDICATOR
area_types()                  # This funcion returns a table of area type IDs 
                              #find the ID for upper tier local authorities

#CREATE YOUR QUESTION
ids <- indicator_areatypes(IndicatorID = 91307) #indicator_areatypes is the question
#IndicatorID is what you're measuring ; 91307 = STI testing rate 
#AreaTypeID = 502 : where this is measured
#ids <- stores the answer in a box called ids

#PRINT IDS TO SEE AREA IDS ASSOCIATED WITH THE STI TESTING RATE
ids

#FIND OUT WITH THE NUMBERS ARE CALLED                   
area_types() |>       #get the full list of all area types
  filter(AreaTypeID %in% ids$AreaTypeID) |>  #keep only the ones that are in my box
  distinct(AreaTypeID, AreaTypeName)  #show each ID and name once 



sti <- fingertips_data(IndicatorID = 91307,
                       AreaTypeID  = 502)       # replace 502 with the ID you find above
write_csv(sti, "data/sti_testing_raw.csv")

sti <- clean_names(sti)
