
library(dplyr)
#create a function to read in files, also has ability to skip extra header line, uses headers as column titles and returns data
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

alleylights <- readin('/Users/VirginiaSaulnier/Desktop/412_hw1','311_Service_Requests_-_Alley_Lights_Out.csv',TRUE)#read in alley lights file using funtion readin
crime <- readin('/Users/VirginiaSaulnier/Desktop/412_hw1','Crimes2016.csv',FALSE)#read in crime info using funtion readin
population <- readin('/Users/VirginiaSaulnier/Desktop/412_hw1','District_Population.csv',FALSE)
colnames(alleylights) <- c("Request","Status","Completion", "Number","Type", "Address", "Zip", "1","2","Ward","District","Community", "Latitude","Longitude","Location")

alley16 <- alleylights[grep("2016", alleylights$Request), ]#filter out 2016
alley16$date_diff <- as.Date(as.character(alley16$Completion),format="%m/%d/%y")-
  as.Date(as.character(alley16$Request), format="%m/%d/%y")#get date diff between date service was requested and date completed create new column to store


service_ave = alley16 %>%#create new database(service_ave)with repair time average by district
  group_by(District) %>%
  summarise(repairtime = round(mean(date_diff, na.rm=TRUE),digits = 1)) %>%
  arrange(District) 
group_by(alley16, District)

crime_counts = crime %>% #create new database(crime_counts) using the number of crimes per district, calculated by counting the instance of crimes(rows) per district
  group_by(District) %>%
  summarise(crimetotal=length(Primary.Type)) %>%
arrange(District)

results = full_join(crime_counts, service_ave, by = "District")%>% #combine three data sets and arrage by crime total
  arrange(crimetotal)
results = full_join(results, population, by = "District")%>% #combine three data sets and arrage by crime total
  arrange(crimetotal)
results$crimerate <- as.numeric(results$crimetotal,na.rm=TRUE)/as.numeric(results$population,na.rm=TRUE)
plot(results$crimetotal, results$repairtime)
#below is the start of print out info, will add if I have time

#dat$MAX <- apply(results[,-1],1,max,na.rm=TRUE) 

#cat("The district with the slowest repair time is the number of crimes this year was")
#cat("The district with the quickest repair time is the number of crimes this year was",dat$MAX)
