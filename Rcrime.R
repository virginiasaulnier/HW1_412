
library(dplyr)

readin <- function(wd,file,skip){
  setwd(wd)
  dataframe = readLines(file)
  if (skip == "TRUE")
  {
    dataframe = dataframe[-1]
  }
  
  data = read.csv(textConnection(dataframe),header=T, fill = TRUE )
  
  return(data)
}

alleylights <- readin('/Users/VirginiaSaulnier/Desktop/412_hw1','311_Service_Requests_-_Alley_Lights_Out.csv',TRUE)
crime <- readin('/Users/VirginiaSaulnier/Desktop/412_hw1','Crimes2016.csv',FALSE)

colnames(alleylights) <- c("Request","Status","Completion", "Number","Type", "Address", "Zip", "1","2","Ward","District","Community", "Latitude","Longitude","Location")

alley16 <- alleylights[grep("2016", alleylights$Request), ]
alley16$date_diff <- as.Date(as.character(alley16$Completion),format="%m/%d/%y")-
  as.Date(as.character(alley16$Request), format="%m/%d/%y")


service_ave = alley16 %>%
  group_by(District) %>%
  summarise(repairtime = round(mean(date_diff, na.rm=TRUE),digits = 1)) %>%
  arrange(District) 
group_by(alley16, District)

crime_counts = crime %>%
  group_by(District) %>%
  summarise(crimetotal=length(Primary.Type)) %>%
arrange(District)

results = full_join(crime_counts, service_ave, by = "District")%>%
  arrange(crimetotal)

#colMax <- function(data) sapply(data, max, na.rm = TRUE)
dat$MAX <- apply(results[,-1],1,max,na.rm=TRUE) 

cat("The district with the slowest repair time is the number of crimes this year was")
cat("The district with the quickest repair time is the number of crimes this year was",dat$MAX)
