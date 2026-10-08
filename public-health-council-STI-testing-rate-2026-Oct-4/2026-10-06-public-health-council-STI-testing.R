#RESEARCH QUESTION: How does STI testing rate in one council compare
#with England and the other councils, and how has it changed over time?
#1. Where does our council sit?
#2. Has it changed over time?

#INSTALL PACKAGES (only once, then leave them commented)
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("fingertipsR",
#                 repos = c("https://ropensci.r-universe.dev",
#                          "https://cloud.r-project.org"))

#LOAD LIBRARIES - run these every time you open R
library(tidyverse) #give us filter(), distinct(), write_csv()
library(janitor)
library(fingertipsR) #lets R talk to Fingertips website
                 

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

#CHOOSING THE AREA TYPE 
utla_id <- 502   #from the previous list, 502 is the upper tier councils (what we want)


#DOWNLOAD THE DATA
sti <- fingertips_data(IndicatorID = 91307,
                       AreaTypeID  = utla_id)       
#SAVE A COPY 
dir.create("data", showWarnings = FALSE)
write_csv(sti, "data/sti_testing_raw.csv")

#TIDY COLUMN NAMES 
sti <- clean_names(sti)

#LOOK AT WHAT WE HAVE 
glimpse(sti)
count(sti, area_type) #keep seperate to keep data distinct 
count(sti, timeperiod)
count(sti, sex, age) 

#NOTE: From the raw data file (see folder /data/), sex = Persons and ages = All ages
#the data tells us that there are 2,156 rows (2012-2025) with 154 rows per year
#that's 153 councils x 14 years
#NEXT STEP: choosing the council (Surrey) and makes sure it exists

my_council <- "Surrey"
sti |>
  filter(area_name == my_council) |> #== asks questions: are two things equal
  distinct(area_name, area_type)

#CHECK FOR MISSING VALUES 
sum(is.na(sti$value)) #counts blank values in the value column
#if prints 0, there are no gaps
sti |> filter(is.na(value)) |> count(timeperiod) #if more than 0, this shows 
#which years the gaps fall in 

#Because there is 1 gap in 2012, let's find out in which council this is 
sti |>
  filter(is.na(value)) |>
  select(area_name, area_type, timeperiod, count, denominator, valuenote, lower_ci95_0limit)
#area_name and timeperiod = which council and year
#count and denominator = if data behind rate is missing 
#valuenote = fingertips sometimes puts an explanation here, data not available?
#ONE MISSING VALUE: Isles of Scilly is a small council and 2012 its figure wasn't published 
#valuenote: "Value for Cornwall and Isles of Scilly combined"
#what to do if blank: leave it as NA

#CHECK SURREY HAS NO GAPS 
sti |>
  filter(area_name == my_council) |>
  summarise(missing = sum(is.na(value)))

#FIND THE LATEST YEAR AND ENGLANDS VALUE
latest <- max(sti$timeperiod_sortable)   # sti$timeperiod_sortable = the year column; max() picks the biggest = most recent; stored in a box called "latest"
latest 

#Latest year = 2025

england_value <- sti |>
  filter(area_type == "England", #keeps only the rows where area is England
  timeperiod_sortable == latest) |> # AND year is latest one
pull(value) #pulls the value column out as one plain number
england_value #print England's rate 

#England's testing rate for 2025 = 4,012 STI tests 
#for every 100,000 people living in England in 2025
#This is the benchmark for every council ; comparison point
#rate = (number of tests / population) x 100,000


#KEEP ONLY THE COUNCILS FOR 2025 (LATEST YEAR)
councils_latest <- sti |> 
  filter(area_type == "Counties & UAs (from Apr 2023)",  #keep only council rows (removes England) 
  timeperiod_sortable == latest) #...and latest year - 2025

nrow(councils_latest) #count the rows: expected 153 - 1 per council excluding Scilly

#MAKE A CLASSIFICATIO SYSTEM OF COUNCILS (HIGHER, SIMILAR, LOWER) VS ENGLAND 
councils_latest <- councils_latest |>
  mutate(  #mutate() = create a new column
    significance = case_when( #new column called significance"; case_when()
      #checks rules from top to bottom 
      lower_ci95_0limit > england_value ~ "Higher than England",
      lower_ci95_0limit < england_value ~ "Lower than England",
      TRUE ~ "Similar to England"
    )
  ) |>
  arrange(value) |> #sort the table from lowest rate to highest rate
  mutate(rank = row_number()) #new column "rank": 1 for the lowest council, 2 for the next, and so on

count(councils_latest, significance) #count how many councils fall into each label
count(councils_latest, comparedto_englandvalueorpercentiles) #cross check:
#Fingertips' own labels; should look broadly similar to yours 

#RESULTS: Fingertips have a different category : better, similar, worse;
#Higher than Englands = 49; Lower than England = 104

#DRAW A CATERPILLAR PLOT
my_row <- filter(councils_latest, area_name == my_council) #this creates a small
#row, so we can highlight the Surrey results 

ggplot(councils_latest,
       aes(x = rank,
           y = value,
           ymin = lower_ci95_0limit,
           ymax = upper_ci95_0limit,
           colour = significance)) +
  geom_pointrange(size = 0.2) +
  geom_hline(yintercept = england_value,
             linetype = "dashed") +
  geom_pointrange(data = my_row, #adds a clear black dot with Surrey data point
                  colour = "black",
                  size = 0.8) +
  geom_text(data = my_row, 
            aes(label = area_name),
            colour = "black",
            vjust = -2) +
  scale_colour_manual(values = c(
    "Higher than England" = "#D55E00",
      "Similar to England" = "#999999",
      "Lower than England" = "#0072B2"))+
  labs(title = "STI testing rate by council, 2025",
       subtitle = "Dashed line = England. Bars = 95% confidence interval",
       y = "Councils, ranked from lowest to highest",
       x = "Rate per 100,000",
       colour = NULL)+
  theme_minimal()+
  theme(axis.text.x = element_blank())
  
#PRINT SURREY SIMPLE VALUE
my_row$value

#SURREY VALUE WITH 95% CI AND CLASSIFICATION
my_row |>                                  #start from the Surrey row
  select(area_name, timeperiod, value,     #keep the council name, year and rate...
         lower_ci95_0limit,                #...plus the bottom of the confidence interval
         upper_ci95_0limit,                #...and the top of it
         significance)                     #...and our Higher/Similar/Lower label

#PULL OUT THE TWO YEARS WE COMPARE (2025 AND 2019)
surrey_2yr <- sti |>                                   # start from the full table; result goes in a box called "surrey_2yr"
  filter(area_name == my_council,                      # keep only Surrey...
         timeperiod %in% c("2019", "2025")) |>         # ...and only these two years (in quotes, because timeperiod is text)
  arrange(desc(timeperiod)) |>                         # sort so 2025 comes first and 2019 second
  select(area_name, timeperiod, count, denominator, value)   # keep only the columns we need

surrey_2yr                                             # print it: you should see exactly 2 rows

#RUN POISSON TEST TO ASK DIFF BETWEEN THE TWO RATES OR RANDOM
res <- poisson.test(x = surrey_2yr$count,              # x = the number of tests in each year (2025 first, then 2019)
                    T = surrey_2yr$denominator)        # T = the population each count comes from

res                                                    # print the full result

#PULL OUT NUMBERS YOU NEED
res$estimate                                # the rate ratio = 2025 rate ÷ 2019 rate (below 1 means 2025 is lower)
res$conf.int                                # the 95% confidence interval around that ratio
res$p.value                                 # the p-value (quote the interval first; if it's tiny, say "p < 0.001")

round((res$estimate - 1) * 100, 1)          # the ratio as a % change (e.g. 0.85 becomes -15%)
round((res$conf.int - 1) * 100, 1)          # the confidence interval as a % change

#My analysis of the results:
#2019: 36,952 tests ; 3,099 rate per 100,000
#2025: 30,186 tests; 2,417 rate per 100,000
#Surrey's STI testing rate fell from 3,099 per 100,000 in 2019 to 2,417 in 2025, a
#22% reduction (rate ratio 0.78, 95% CI 0.77 to 0.79, p<0.001), using a Poisson test
#comparing the two rates


