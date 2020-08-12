# PREA watch
# github.com/jessicakay

install.packages("googlesheets")
install.packages("reshape2")
library(reshape2)

original_state<-readxl::read_xlsx("~/Downloads/PREA Report.xlsx",sheet = 2)

temp_state<-as.data.frame(original_state)

colnames(temp_state)[1]<-"variable"
colnames(temp_state)