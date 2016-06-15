
library(dplyr)
setwd('/Users/VirginiaSaulnier/Desktop/Hw1_412')
all_content = readLines("311_Service_Requests_-_Alley_Lights_Out.csv")
skip_first = all_content[-1]

alleylights= read.csv(textConnection(skip_first),header=T, fill = TRUE )
colnames(alleylights) <- c("Request","Status","Completion", "Number","Type", "Address", "Zip", "1","2","Ward","District","Community", "Latitude","Longitude","Location")
names(alleylights)

#alleylights$Request <- format(alleylights$Request, format="%m/%d/%y")
#alleylights$Completion <- format(alleylights$Completion, format="%m/%d/%y")


# based on variable values
#newalley <- alleylights[ which(alleylights$Date >'01/01/2016' & alleylights$Status == 'completed') ]
#alley16 <- select(alleylights,Request == '2016')
alley16 <- alleylights[grep("2016", alleylights$Request), ]
alley16$date_diff <- as.Date(as.character(alley16$Completion),format="%m/%d/%y")-
  as.Date(as.character(alley16$Request), format="%m/%d/%y")
#district_diff <-  c(alley16$date_diff, alley16$district)
service_ave = alley16 %>%
  group_by(District) %>%
  summarise(avg = mean(date_diff, na.rm=TRUE)) %>%
  arrange(avg)
group_by(alley16, District)
