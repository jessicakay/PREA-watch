# PREA watch
# github.com/jessicakay

# setup - run once per machine

install.packages("googlesheets")
install.packages("reshape2")
install.packages("dplyr")

# startup - run on load

library(reshape2)
library(dplyr)
fileDir<-"~/Downloads/PREA Report.xlsx"

original_state<-readxl::read_xlsx(fileDir,sheet = 2)
originalcounty<-readxl::read_xlsx(fileDir,sheet = 4)
temp_state<-as.data.frame(original_state)
tempcounty<-as.data.frame(originalcounty)

# data cleaning 

colnames(temp_state)[1]<-"variable"

e<-tolower(colnames(temp_state))
colnames(temp_state)<-e


var_names <- c(
  "Inmate on inmate sex acts",
  "Inmate on inmate sex abuse",
  "Inmate on inmate sexual harrassment",
  "Staff sexual misconduct",
  "Staff-inamte sexual harassment"
  )

state_data <- as.data.frame(
  temp_state %>%
    select(variable, substantiated, pending, unfounded, unsubstantiated) %>%
    filter(variable %in% var_names)
)

View(state_data)