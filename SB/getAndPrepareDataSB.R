# to rebuild library:
# install.packages("devtools")
# devtools::install_github('Conte-Ecology/westBrookData/getWBData')

library(getWBData)

cdSB <- createCoreData(sampleType=c("electrofishing","stationaryAntenna","portableAntenna","acoustic"), 
                             whichDrainage = "stanley",
                             columnsToAdd=c("sampleNumber","river","riverMeter","survey","readerID","comment",'observedLength','observedWeight')
                             ) %>%  filter(!is.na(tag)) %>% # for now
  addTagProperties( columnsToAdd=c("cohort","species","dateEmigrated","sex","species")) 

#%>% #,'familyId') ) %>%
  #dplyr::filter( species %in% c( "bkt","bnt","ats" ), area %in% c("trib","inside","below","above","smolt trap") ) %>% 
 # createCmrData( maxAgeInSamples=20, inside=F) %>% 
#  addSampleProperties() #%>%
#  addEnvironmental() #%>%
 # addKnownZ() %>%
#  fillSizeLocation()

############# 2_prepare data

library(dplyr)

# install dev version to fix NA problem with lag()
#if (packageVersion("devtools") < 1.6) {
#  install.packages("devtools")
#}
#devtools::install_github("hadley/lazyeval")
#devtools::install_github("hadley/dplyr")

# sample 2.5 = fyke net. not sure if we should keep it - prob with obs model
#cdSB <-  filter(cdSB, sampleNumber != 2.5 & sampleNumber != 10.1)

# some formatting fixes

cdSB$section <- as.numeric( cdSB$section )
cdSB$inside <- ifelse( cdSB$section %in% 1:50 | cdSB$survey == "stationaryAntenna", T, F ) #changed from 47 to 50

cdSB$river[cdSB$section < 7] <- "tidal"

cdSB$riverOrdered <- factor(cdSB$river,levels=c('mainstem', 'west', 'east',"tidal"),labels=c("mainstem","west","east","tidal"), ordered=T)

cdSB <- cdSB %>%
  group_by(tag) %>%
  # arrange(tag,sampleNumber) %>%
  mutate( lagSection = lead(section),
          distMoved = section - lagSection,
          minSample = min(sampleNumber),
          maxSample = max(sampleNumber)) %>%
  ungroup()

cdSB$moveDir <- ifelse( cdSB$section == cdSB$lagSection, 0, ifelse( cdSB$section > cdSB$lagSection, 1,-1 ) )

cdSB$drainage <- "stanley"

# add riverM to shocking samples
# will replace this with sites table data from Matt
cdSB$riverMeter <- ifelse( cdSB$survey == "shock" | cdSB$survey == "portableAntenna" | cdSB$survey == "acoustic", cdSB$section * 40 - 20, cdSB$riverMeter )

cdSB$sizeForGraph <- ifelse( is.na(cdSB$observedLength), 60, cdSB$observedLength )

cdSBHold <- cdSB

cdSB <- cdSBHold %>% 
  # filter(tag %in% c(  '257c683e2f', '00088d1c80', '00088d2e1f')) %>% 
  select(tag,riverMeter,detectionDate,survey,sizeForGraph,observedLength)

counts <- cdSB %>% group_by(tag) %>% summarize(n=n()) %>% arrange(desc(n))
cdSB <- left_join(cdSB,counts) %>%
                filter(n > 1)  # remove single observations 

save(cdSB, file='/home/shinyApps/pitAntenna/SB/cdForShiny.RData')

counts <- counts %>% filter(n > 1)
save(counts, file='/home/shinyApps/pitAntenna/SB/counts.RData')

sampDateRange <- cdSBHold %>% group_by(sampleNumber) %>% 
                   summarize(minDate = min(detectionDate), maxDate = max(detectionDate), medDate = median(detectionDate))
save(sampDateRange, file='/home/shinyApps/pitAntenna/SB/sampDateRange.RData')

countsByDay <- cdSB %>%
  filter(survey %in% c("acoustic","stationaryAntenna")) %>%
  group_by(date=as.POSIXct(as.Date(detectionDate)),tag,survey,riverMeter) %>%
  summarize( nForInd = n() ) %>%
  ungroup() %>%
  group_by(date,survey,riverMeter) %>%
  summarize( n = n() )

portableSampleDays <- cdSB %>% filter(survey=="portableAntenna") %>% distinct(date = as.Date(detectionDate))

save(countsByDay, portableSampleDays, file='/home/shinyApps/pitAntenna/SB/countsByDay.RData')

# for testing
#cdShort <- cdSB %>% 
#             filter(tag %in% c(  '257c683e2f', '00088d1c80', '00088d2e1f')) %>% 
#             select(tag,riverMeter,detectionDate,survey,sizeForGraph)

#save(cdShort,file='/home/ben/git/pitAntenna/shinyApp/cdForShinyShort.RData')


########## 3_envdata
library(dplyr)
library(getWBData)

# run this to reestablish connection
# createCoreData()

envData <- tbl(conDplyr, "stanley_environmental") %>% collect(n = Inf) %>%
  filter( section == 11 ) %>%
  #      mutate(date = as.POSIXct(as.Date(datetime))) %>% 
  mutate(date = format(datetime, "%Y-%m-%d")) %>% 
  group_by(date) %>% 
  mutate(tempMean = mean(temperature), depthMean = mean(depth,na.rm=T)) %>%
  rename(temp15Min = temperature, daily_mean_temp = tempMean,
         depth15Min = depth, qPredicted = depthMean) %>%
  select(-datetime,-temp15Min,-depth15Min,-salinity,-logger) %>%
  unique()

######
# temporary fix for negative depths
# 
envData$qPredicted <- ifelse(envData$qPredicted < 0, 0, envData$qPredicted)



# get median sample dates
# sampleName is the original, sampleNumber is consecutive
msd <- tbl(conDplyr,"data_seasonal_sampling") %>%
  filter(drainage=="stanley", seasonal) %>%
  select(sample_number,median_date,season,seasonal) %>%
  distinct() %>%
  collect() %>%
  arrange(median_date) %>%
  mutate(date = format(median_date, "%Y-%m-%d")) %>%
  filter(sample_number !=2.5 & sample_number !=10.1)

envDataSB <- envData %>%
  left_join(.,msd) #, by = c("date" = "median_date"))

envDataSB$date <- as.POSIXct(envDataSB$date) # for row binding in the next step

envDataSB$seasonFill <- NA; envDataSB$sampleNumberFill <- NA
envDataSB$season[2] <- msd$season[1]; envDataSB$sample_number[2] <- msd$sample_number[1]
for (i in 2:(nrow(envDataSB))){
  if ( !is.na(envDataSB$season[i]) ) { envDataSB$seasonFill[i] = envDataSB$season[i]; envDataSB$sampleNumberFill[i] = envDataSB$sample_number[i] }
  else                               { envDataSB$seasonFill[i] = (envDataSB$seasonFill[i-1]); envDataSB$sampleNumberFill[i] = (envDataSB$sampleNumberFill[i-1]) }
}

envDataSB$drainage <- 'stanley'

save(envDataSB,file='/home/shinyApps/pitAntenna/SB/envDataSBForMM.RData')

# tide data
# takes a while to run - only run when need to change
 tidePred = tide_height('Bar Harbor, Frenchman Bay, Maine',from = as.Date(min(cdSB$detectionDate)), to = as.Date(max(cdSB$detectionDate)), minutes = 30, tz ='PST8PDT')
 tidePred$tideAdj <- tidePred$TideHeight * 30 - 30 * 5 
 save(tidePred,file='/home/shinyApps/pitAntenna/SB/tidePred.RData')

