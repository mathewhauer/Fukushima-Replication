###------Hellinger Distance results-----
## @knitr Hellinger

# Making a new database that is basically throw away.
zzz <- migdata5 %>%
  dplyr::select(origin, X1, OD, year, dest_per)

# Creating a function to filter the throwaway database based on the year
filtdistance <- function(YEAR){
  zzz %>%
    filter(year == YEAR) %>%
    group_by(origin) %>%
    na.omit %>%
    dplyr::select(origin, X1, year, dest_per) %>%
    spread(X1, dest_per)
}

# Creating an individual database for each year.
x2004 <- filtdistance("X2004")
x2005 <- filtdistance("X2005")
x2006 <- filtdistance("X2006")
x2007 <- filtdistance("X2007")
x2008 <- filtdistance("X2008")
x2009 <- filtdistance("X2009")
x2010 <- filtdistance("X2010")
x2011 <- filtdistance("X2011")
x2012 <- filtdistance("X2012")
x2013 <- filtdistance("X2013")
x2014 <- filtdistance("X2014")
x2015 <- filtdistance("X2015")
x2016 <- filtdistance("X2016")

# Getting the unique number of years
nums <- unique(zzz$year)
# Setting an output to hold the results of the following for loop.
out<- NULL

# The following for loop goes through each year from 1 through the number of years minus 1.
# It creates a new database and calculates the Hellinger distance.
for(i in 1:(length(nums)-1)){
  KLdata <- rbind(get(paste0("x",as.numeric(substr(unique(zzz$year)[i],2,6)))),
                  get(paste0("x",as.numeric(substr(unique(zzz$year)[i],2,6))+1))
  )
  
   for(id in unique(KLdata$origin)){
    x <- as.matrix(remove_empty(filter(KLdata, origin == id)[,-1:-2], which = "cols"))
    out2 <- NULL
    out2$id[id] <- id
    out2$YEAR[id] <- paste0(unique(KLdata$year)[1], unique(KLdata$year)[2])
    out2$distance[id]<- philentropy::distance(x, method = "hellinger")/2 # for some reason philentropy bounds hellinger from 0 to 2, instead of 0 to 1.
    out2 <- as.data.frame(out2)
    out <- rbind(out, out2)
}}

# Gathering the for loop results as a data frame.
a <- as.data.frame(out) %>%
  mutate(id = as.character(id))

# Preping the data for the Evacuee's Hellinger distance. The comparison is with the Fukushima pre-disaster migration.
fukmiga <- Fukushimadat %>%
  dplyr::select(origin, X1, destper2010) %>%
  spread(X1, destper2010)
# Preping the data the for the Evacuees.
fukevaca <- Fukushimadat %>%
  dplyr::select(origin, X1, evac_per) %>%
  spread(X1, evac_per)

# Putting the Fukushima Migration and Fukushima Evacuation data together.
fuku <- as.matrix(rbind(fukmiga, fukevaca) %>%
  dplyr::select(-origin))
f <- NULL
f$id <- "Evacuees"
f$distance <- philentropy::distance(fuku, method = "hellinger")/2
f$YEAR <- "X2010X2011"
f <- as.data.frame(f)

# Joining All of the Migration distances with the Evacuee distances.
# Calculating the log distance for the log-normal analysis.
KLdat<- rbind(a,f) %>%
  mutate(dstfrommean = log(distance) - mean(log(distance)),
         sdfrommean = dstfrommean/ sd(log(distance)))

# Filtering out the Evacuees to create some summary statistics
sumstats <- filter(KLdat, !id == "Evacuees")
 
# Calculating the significance where the SD is above 1.96.
sigstats <- filter(KLdat, sdfrommean >= 1.96) %>%
  dplyr::select(Prefecture = id, Year = YEAR, Distance = distance, `z-score` = sdfrommean) %>%
  mutate(Year = paste0(substr(as.character(Year),2,5),", ",substr(as.character(Year),7,10)),
         Year = if_else(Prefecture == "Evacuees", "2010, Evacuees", Year),
         `p-val` = 2*pnorm(-abs(`z-score`))) %>%
  arrange(-`z-score`) %>%
  dplyr::select(Prefecture, `P, Q` = Year, `H(P, Q)` = Distance, `z-score`, `p-val`)
