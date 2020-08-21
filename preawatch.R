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

temp_state$variable[which(str_detect(temp_state$variable,"inamte"))]<-
  gsub("inamte","inmate",
  temp_state$variable[which(str_detect(temp_state$variable,"inamte"))]
  )

var_names <- c(
  "Inmate on inmate sex acts",
  "Inmate on inmate sex abuse",
  "Inmate on inmate sexual harrassment",
  "Staff sexual misconduct",
  "Staff-inmate sexual harassment"
  )

state_data <- as.data.frame(
  temp_state %>%
    select(variable, substantiated, pending, unfounded, unsubstantiated) %>%
    filter(variable %in% var_names | str_detect(variable, "[[:digit:]]+") == TRUE)
)

# a<-as.data.frame(str_extract_all(state_data[yearRows,][1],"[[:digit:]]+"))

state_data[yearRows,]$variable # list of header rows

state_data <-state_data %>% 
  mutate(year = case_when(
    str_detect(state_data$variable, "[[:digit:]]+") == TRUE ~  
      str_extract(state_data$variable, "[[:digit:]]+")))

# enumerate all headers that appear as observations

yearRows<-which(str_detect(state_data$variable,"[[:digit:]]+")==TRUE) 

# extract facility names

yearFacs<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")

state_data$facility<-"facility"
state_data[yearRows+1,]$year<-state_data[yearRows,]$year 
state_data[yearRows+2,]$year<-state_data[yearRows,]$year 
state_data[yearRows+3,]$year<-state_data[yearRows,]$year
state_data[yearRows+4,]$year<-state_data[yearRows,]$year
state_data[yearRows+1,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+2,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+3,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+4,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")


# future loop to cut down on redundant code
# 
# i<0
# for(i in dim(state_data)[1]){
#  if(str_detect(state_data$year[i],"[[:digit:]]+")==TRUE){
#    state_data$variable[i+1]<-"a"
#  }
#  i<-i+1
# }

View(
    state_data %>% 
  select(colnames(state_data)) %>%
  melt(id.vars=c("year","facility"),
       measure.vars=c("substantiated","pending","unfounded","unsubstantiated"))
)