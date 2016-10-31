library(ggplot2)

# to show observedlength on graph
cdSBHold$sizeForGraph <- ifelse( is.na(cdSBHold$observedLength), 3.5, cdSBHold$observedLength/20 )

# dist of counts by fish
counts <- cdSB %>% group_by(tag) %>% summarize(n=n()) %>% arrange(desc(n))
counts %>% ggplot(aes(n)) + geom_histogram()

# get riverMeters for readers
stationaryRM <- cdSBHold %>% filter(survey == "stationaryAntenna") %>% distinct(riverMeter) %>% filter(!is.na(riverMeter))
acousticRM <- cdSBHold %>% filter(survey == "acoustic") %>% distinct(riverMeter) %>% filter(!is.na(riverMeter))

tmpCD <- cdSBHold %>% filter(tag=="00088d2e1f") %>% arrange(detectionDate)

p <-  
  ggplot(tmpCD, aes(detectionDate,riverMeter)) +
    geom_point(aes(color=survey, size=sizeForGraph)) +
    geom_line(color="grey") +
    geom_hline(yintercept = stationaryRM$riverMeter, color="lightblue", size=0.5) +
    geom_hline(yintercept = acousticRM$riverMeter, color="orange", size=0.5) +
    theme_bw()




p <- ggplot(tmpCD, aes(detectionDate,riverMeter)) +
 
  geom_point(aes(color=survey, size=sizeForGraph)) +
  scale_colour_manual( values = c("acoustic" = palette1[1],"portableAntenna" = palette1[4],"shock" = palette1[2],"stationaryAntenna" = palette1[3])) +
  scale_size( "Body size (mm)", guide = guide_legend(override.aes = list(colour = palette1[2]))) + #range = c(1, 8),
  geom_line(color="darkgrey") +
  
  geom_hline(yintercept = stationaryRM$riverMeter, color="lightblue", size=0.5) +
  geom_hline(yintercept = acousticRM$riverMeter, color="orange", size=0.5) +
  
 # coord_cartesian(xlim = ranges$x, ylim = ranges$y) + #xlim(ranges$x) +
  scale_x_datetime("Detection date",date_labels = "%b %d %Y") +
  scale_y_continuous("River meter") +
  theme_bw()


countsByDay <- cdSB %>%
  filter(survey %in% c("acoustic","stationaryAntenna")) %>%
  group_by(date=as.POSIXct(as.Date(detectionDate)),survey,riverMeter) %>%
  summarize( n = n() )

p + geom_ribbon( data=countsByDay %>% filter(riverMeter==420),  
                 aes(date,y=riverMeter, ymin=riverMeter-log(n)*5, ymax=riverMeter+log(n)*5), fill = "grey70")



ggplot(countsByDay, aes(date,riverMeter)) + 
  geom_point(aes(color = log(n)), size=1) +
  scale_colour_gradient2() +
  theme_bw()

p + geom_point( data=countsByDay,  aes(date,riverMeter), alpha=n)


p +       
  geom_vline(data= sampDateRange,aes(xintercept = as.numeric(minDate)), color="red", size=0.5) +
  geom_vline(data= sampDateRange,aes(xintercept = as.numeric(maxDate)), color="darkred", size=0.5) 

p + geom_rect( inherit.aes = FALSE, data=sampDateRange,
               aes(xmin=as.numeric(minDate), 
                   xmax=as.numeric(maxDate),
                   ymin=-Inf, ymax=+Inf))#) )
p + geom_rect( inherit.aes = FALSE, data=sampDateRange,
               aes(xmin=minDate, 
                   xmax=maxDate,
                   ymin=-Inf, ymax=+Inf))
p + geom_rect( aes(xmin=as.POSIXct("2012-05-22 08:01:19", origin = "1970-01-01"), 
                   xmax=as.POSIXct("2012-09-22 08:01:19", origin = "1970-01-01"),
                   ymin = 0, ymax = 1000),alpha=0.2)


xMin = min(tmpCD$detectionDate)
xMax = max(tmpCD$detectionDate)

p <- p + geom_line(aes(DateTime,tideAdj), color="red", data=tidePred) + xlim(xMin, xMax)


