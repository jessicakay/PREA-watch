# PREA watch
# github.com/jessicakay

# setup - run once per machine

install.packages("googlesheets")
install.packages("reshape2")

# startup - run on load

library(reshape2)
fileDir<-"~/Downloads/PREA Report.xlsx"

original_state<-readxl::read_xlsx(fileDir,sheet = 2)
originalcounty<-readxl::read_xlsx(fileDir,sheet = 4)
temp_state<-as.data.frame(original_state)
tempcounty<-as.data.frame(originalcounty)

# data cleaning 
colnames(temp_state)[1]<-"variable"
colnames(temp_state)