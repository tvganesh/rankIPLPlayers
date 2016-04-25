rankIPLBowlers <- function() {
    
    load("Chennai Super Kings-BowlingDetails.RData")
    csk_details <- bowlingDetails
    load("Deccan Chargers-BowlingDetails.RData")
    dc_details <- bowlingDetails
    load("Delhi Daredevils-BowlingDetails.RData")
    dd_details <- bowlingDetails
    load("Kings XI Punjab-BowlingDetails.RData")
    kxip_details <- bowlingDetails
    load("Kochi Tuskers Kerala-BowlingDetails.RData")
    ktk_details <- bowlingDetails
    load("Kolkata Knight Riders-BowlingDetails.RData")
    kkr_details <- bowlingDetails
    load("Mumbai Indians-BowlingDetails.RData")
    mi_details <- bowlingDetails
    load("Pune Warriors-BowlingDetails.RData")
    pw_details <- bowlingDetails
    load("Rajasthan Royals-BowlingDetails.RData")
    rr_details <- bowlingDetails
    load("Royal Challengers Bangalore-BowlingDetails.RData")
    rcb_details <- bowlingDetails
    load("Sunrisers Hyderabad-BowlingDetails.RData")
    sh_details <- bowlingDetails
    
    aa <- list(csk_details,dc_details,dd_details,kxip_details,ktk_details,kkr_details,
               mi_details,pw_details,rr_details,rcb_details,sh_details)
    
    theTeams <-c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
                 "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
                 "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
                 "Royal Challengers Bangalore","Sunrisers Hyderabad")
    
    setwd("C:/software/cricket-package/cricsheet/cleanup/IPL/rank")

    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(x in 1:length(aa)){
        bowlers <- unique(aa[[x]]$bowler)
        for (y in 1:length(bowlers)){
            #cat("x=",x,"team",theTeams[x],"\n")
            tryCatch(l <- getBowlerWicketDetails(team=theTeams[x],name=bowlers[y],dir="."),
                     error = function(e) {
                         #print("Error!")
                         
                     }
                     
            )
            if(exists("l")){
            
               m <- select(l,bowler,wickets,economyRate)
               o <-rbind(o,m)
            }
            
        }
    }
    print(dim(o))
    bowlers <- unique(o$bowler)
    print(getwd())
    u <- NULL
    s <- data.frame(bowler=character(0),matches=numeric(0),meanWickets=numeric(0),meanER=numeric(0))
    for (x in 1:length(bowlers)){
        m <- filter(o,bowler==bowlers[x])
        m <- mutate(m,matches=n(),meanWickets=mean(wickets),meanER=mean(economyRate))
        m <- select(m,bowler,matches,meanWickets,meanER)
        s <- m[1,]
        u <- rbind(u,s)
    }
    
    # Select only players who have played 60 matches or more
    q <- filter(u,matches >= 60)
    
    IPLBowlersRank <- arrange(q,desc(meanWickets),desc(meanER))
    IPLBowlersRank
    
    
    
}