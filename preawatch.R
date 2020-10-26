# PREA watch
# github.com/jessicakay

# setup - run once per machine

install.packages("googlesheets")
install.packages("reshape2")
install.packages("dplyr")

# startup - run on load

library(reshape2)
library(dplyr)
library(stringr)

attribution <- "github.com/jessicakay/PREA-watch"

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
  "Inmate on inmate sexual harassment",
  "Staff sexual misconduct",
  "Staff-inmate sexual harassment",
  "Average Daily Population"
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
state_data$adp<-"adp"
state_data[yearRows+1,]$year<-state_data[yearRows,]$year 
state_data[yearRows+2,]$year<-state_data[yearRows,]$year 
state_data[yearRows+3,]$year<-state_data[yearRows,]$year
state_data[yearRows+4,]$year<-state_data[yearRows,]$year
state_data[yearRows+5,]$year<-state_data[yearRows,]$year
state_data[yearRows+6,]$year<-state_data[yearRows,]$year
state_data[yearRows,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+1,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+2,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+3,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+4,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+5,]$adp<-state_data[yearRows+6,]$substantiated
state_data[yearRows+1,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+2,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+3,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+4,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+5,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data[yearRows+6,]$facility<-str_extract(state_data[yearRows,]$variable,"[[:alpha:]]+\\(?\\s[[:alpha:]]+")
state_data$substantiated[str_detect(state_data$substantiated,"201[[:digit:]]")]<-NA


  

new_df<-state_data[-c(yearRows,yearRows+6),] # remove header rows

# convert string to numeric data

new_df$substantiated<-as.numeric(new_df$substantiated)
new_df$pending<-as.numeric(new_df$pending)
new_df$unfounded<-as.numeric(new_df$unfounded)
new_df$unsubstantiated<-as.numeric(new_df$unsubstantiated)
new_df$adp<-as.numeric(new_df$adp)


new_df[c(2,3,4,5)]<-str_extract_all(new_df[c(2,3,4,5)],"[[:digit:]]+")

new_df$row_total<-as.numeric(new_df$substantiated)+as.numeric(new_df$pending)+
  as.numeric(new_df$unfounded)+as.numeric(new_df$unsubstantiated)

new_df$total_reports[yearRows]<-new_df$row_total[yearRows] # add all by year



  
# render data in long format

View(
    state_data %>% 
  select(colnames(state_data)) %>%
  melt(id.vars=c("year","facility"),
       measure.vars=c("substantiated","pending","unfounded","unsubstantiated"))
)

library(ggplot2)

png("~/Documents/GitHub/PREA-watch/souza.png",width = 800, height = 1000)
gridExtra::grid.arrange(
new_df %>%
  select(total,year) %>%
  ggplot()+
  stat_boxplot(geom='errorbar',aes(x=year,y=total),width=0.3)+
  geom_boxplot(aes(x=year,y=total))+
  labs(title = "Reporting outliers",subtitle = "Distribution of total reports per facility, stratified by year")+
  geom_hline(yintercept = as.numeric(max(as.numeric(new_df$total),na.rm = TRUE)),color="red",linetype="dashed")+
  annotate(geom="text",x=3,y=28,label="Souza Baranowski",color="red")+
  geom_point(x=3,y=29,shape=1,size=10,color="red"),

new_df %>% filter(variable=="Staff sexual misconduct") %>% select(total) %>%
ggplot(aes(x=total))+
  geom_histogram()+
  geom_point(x=29,y=2,shape=8,size=3,color="red")+
labs(subtitle = "Distribution of reports year year",caption = attribution)+
  xlab("number of reports")+
  ylab("frequency")
)
dev.off()

new_df %>% select(total)

new_df %>% 
  select(substantiated,pending,unfounded,unsubstantiated,variable) %>%
  ggplot()+
  geom_point(aes(x=substantiated,y=unsubstantiated))
