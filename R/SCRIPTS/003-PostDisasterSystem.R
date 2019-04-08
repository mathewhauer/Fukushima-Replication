###------Post-disaster System results-----
## @knitr PostSys

# This script compares 2011, 2012, and 2013 with 2010 out-migration. 
nums <- c("X2011", "X2012", "X2013")
out<- NULL
for(i in 1:length(nums)){
  KLdata <- rbind(get("x2010"),
                  get(paste0("x",as.numeric(substr(unique(nums)[i],2,6))))
  )
  for(id in unique(KLdata$origin)){
    x <- as.matrix(remove_empty(filter(KLdata, origin == id)[,-1:-2], which = "cols"))
    out2 <- NULL
    out2$id[id] <- id
    out2$YEAR[id] <- paste0(unique(KLdata$year)[1], unique(KLdata$year)[2])
    out2$distance[id]<- philentropy::distance(x, method = "hellinger")/2
    out2 <- as.data.frame(out2)
    out <- rbind(out, out2)
  }
}

# The final post-disaster migration system comparisons to the pre-existing system.
b <- as.data.frame(out) %>%
  mutate(id = as.character(id),
         Year = paste0(substr(as.character(YEAR),2,5),", ",substr(as.character(YEAR),7,10))) %>%
  filter(id %in% c("Fukushima", "Iwate", "Miyagi")) %>%
  dplyr::select(Prefecture = id, `P, Q` = Year, `H(P, Q)` = distance) %>%
  arrange(Prefecture, `P, Q`)