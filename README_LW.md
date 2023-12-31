This is my README file created for EPI590R final project

I have completed the specific objectives creating a descriptive statistics table,
a regression table, a histogram figure, using inline text and creating printed inline table output in the Quarto document. 


In order for the Quarto document to render correctly, please run the "FULL_CODE_LW" R script first in order to fill the environment with the necessary dataframes and objects. 


In my final project repository there are multiple files:
     - holds my quarto document "quarto_LW.qmd" with my code that renders a clean looking report following the specific objectives. 
     - renv.lock file that has the evidence of the packages i have used: dplyr, gtsummary, renv
     - top50.csv the orginal file that I read in and connected to my final project by using the here function
     
     
  
     
I am also saving my final code here as a second back up to the "FULL_CODE_LW"

here::here()
setwd("data")
getwd()

Loading the music data in:
library(readr)
data1 <- read_csv("/Users/lindseywalker/Documents/Documents/Documents/Documents/
                  RSPH/FALL 2023/EPI590R/final-project-LW/data/top50.csv")

install.packages("gtsummary")
library(gtsummary)

install.packages("dplyr")
library(dbplyr)

Create subset dataframe with only the variables i want to use
music <- data1 |> select(Beats.Per.Minute, Energy, Liveness, Danceability, 
                         Valence., Length., Acousticness.., Speechiness., Popularity, Genre)

tbl1 <- tbl_summary(
  music,
  by = Genre,
  label = list(Beats.Per.Minute ~ "Beats/ minute", Valence. ~ "Valence", 
               Length. ~ "Length", Acousticness.. ~ "Acousticness", Speechiness. ~ "Speechiness"),
  statistic = list(
    all_continuous() ~ "{mean}")) |>
  add_p() |>
  modify_header(label ~ "**Characteristic**")  |>
  modify_caption("**Table 1. Music Genre Characteristics**") |>
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6", "stat_7", 
                           "stat_8", "stat_9", "stat_10", "stat_11", "stat_12", "stat_13", "stat_14", 
                           "stat_15", "stat_16", "stat_17", "stat_18", "stat_19", "stat_20", "stat_21") 
                         ~ "**Music Genre, n=50**") |>
  add_overall() 
tbl1

create functions to use for later inline text inside using the data
pan_pop <- inline_text(tbl1, variable = "Beats.Per.Minute", column = "panamanian pop")
can_pop <- inline_text(tbl1, variable = "Beats.Per.Minute", column = "canadian pop")


Distribution of the continuous outcome: beats per minute:
hist(music$Beats.Per.Minute)

Create inline stats to then report in the manuscript:
mean <- list(n=nrow(music),
             mean_bpm = mean(music$Beats.Per.Minute))
             
There were `r mean$n` songs with a mean of `r mean$mean_bpm` per minute. 
@fig-hist shows the majority of songs have 80-100 beats per minute.

## Regression

Create linear regression models with Beats per minute as continuous outcome
 Model 1: looking at music characteristics that have to do with how the music is composed: acousticness, speechiness, liveness, length
 
linear_model <- lm(Beats.Per.Minute ~ Liveness + Acousticness.. + Speechiness. + Length.,
                   data = music)  
tbl_composed <- tbl_regression(
  linear_model,
  intercept = TRUE
)

Second model looks at music characteristics that have to do with how the sound is interpretted: danceability, popularity, 
linear_model2 <- lm(Beats.Per.Minute ~ Danceability + Popularity + Energy + Valence. + Length.,
                    data = music)
tbl_interp <- tbl_regression(
  linear_model2,
  intercept = TRUE
)

Combined the 2 linear regerssion models and create well formatted regression table:
tblreg <- tbl_merge(list(tbl_composed, tbl_interp),
          tab_spanner = c("**Music Composure Model**", "**Music Interpretation Model**"))
tblreg


install.packages("renv")
renv::init()
Used renv to see my loaded packages in there from this project: dpylr, renv, gtsummary



