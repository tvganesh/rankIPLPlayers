setwd("C:/software/cricket-package/york-test/yorkrData/IPL/IPL-T20-matches")
csk_details <- getTeamBattingDetails("Chennai Super Kings",dir=".", save=TRUE)
dc_details <- getTeamBattingDetails("Deccan Chargers",dir=".", save=TRUE)
dd_details <- getTeamBattingDetails("Delhi Daredevils",dir=".",save=TRUE)
kxip_details <- getTeamBattingDetails("Kings XI Punjab",dir=".",save=TRUE)
ktk_details <- getTeamBattingDetails("Kochi Tuskers Kerala",dir=".",save=TRUE)
kkr_details <- getTeamBattingDetails("Kolkata Knight Riders",dir=".",save=TRUE)
mi_details <- getTeamBattingDetails("Mumbai Indians",dir=".",save=TRUE)
pw_details <- getTeamBattingDetails("Pune Warriors",dir=".",save=TRUE)
rr_details <- getTeamBattingDetails("Rajasthan Royals",dir=".",save=TRUE)
rcb_details <- getTeamBattingDetails("Royal Challengers Bangalore",dir=".",save=TRUE)
sh_details <- getTeamBattingDetails("Sunrisers Hyderabad",dir=".",save=TRUE)



a <- select(csk_details,batsman,runs,strikeRate)
b <- select(dc_details,batsman,runs,strikeRate)
c <- select(dd_details,batsman,runs,strikeRate)
d <- select(kxip_details,batsman,runs,strikeRate)
e <- select(ktk_details,batsman,runs,strikeRate)
f <- select(kkr_details,batsman,runs,strikeRate)
g <- select(mi_details,batsman,runs,strikeRate)
h <- select(pw_details,batsman,runs,strikeRate)
i <- select(rr_details,batsman,runs,strikeRate)
j <- select(rcb_details,batsman,runs,strikeRate)
k <- select(sh_details,batsman,runs,strikeRate)

df <- rbind(a,b,c,d,e,f,g,h,i,j,k)

batsmen <- unique(df$batsman)

o <- NULL
n <- data.frame(name=character(0),matches=numeric(0),meanRuns=numeric(0),meanSR=numeric(0))
for (x in 1:length(batsmen)){
    m <- filter(df,batsman==batsmen[x])
    m <- mutate(m,matches=n(),meanRuns=mean(runs),meanSR=mean(strikeRate))
    m <- select(m,batsman,matches,meanRuns,meanSR)
    n <- m[1,]
    o <- rbind(o,n)
}

# Select only players who have played 60 matches or more
p <- filter(o,matches >= 60)

rankIPLBatsmen <- arrange(p,desc(meanRuns),desc(meanSR))

# Get team bowling details

setwd("C:/software/cricket-package/york-test/yorkrData/IPL/IPL-T20-matches")
csk_details <- getTeamBowlingDetails("Chennai Super Kings",dir=".", save=TRUE)
dc_details <- getTeamBowlingDetails("Deccan Chargers",dir=".", save=TRUE)
dd_details <- getTeamBowlingDetails("Delhi Daredevils",dir=".",save=TRUE)
kxip_details <- getTeamBowlingDetails("Kings XI Punjab",dir=".",save=TRUE)
ktk_details <- getTeamBowlingDetails("Kochi Tuskers Kerala",dir=".",save=TRUE)
kkr_details <- getTeamBowlingDetails("Kolkata Knight Riders",dir=".",save=TRUE)
mi_details <- getTeamBowlingDetails("Mumbai Indians",dir=".",save=TRUE)
pw_details <- getTeamBowlingDetails("Pune Warriors",dir=".",save=TRUE)
rr_details <- getTeamBowlingDetails("Rajasthan Royals",dir=".",save=TRUE)
rcb_details <- getTeamBowlingDetails("Royal Challengers Bangalore",dir=".",save=TRUE)
sh_details <- getTeamBowlingDetails("Sunrisers Hyderabad",dir=".",save=TRUE)

aa <- list(csk_details,dc_details,dd_details,kxip_details,ktk_details,kkr_details,
           mi_details,pw_details,rr_details,rcb_details,sh_details)

theTeams <-c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
          "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
          "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
          "Royal Challengers Bangalore","Sunrisers Hyderabad")

o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))

for(x in 1:length(aa)){
    bowlers <- unique(aa[[x]]$bowler)
    for (y in 1:length(bowlers)){
        cat("x=",x,"team",theTeams[x],"\n")
        tryCatch(l <- getBowlerWicketDetails(team=theTeams[x],name=bowlers[y],dir="."),
                 error = function(e) {
                     print("Error!")
                 }
        )    
        l <- select(l,bowler,wickets,economyRate)
        o <-rbind(o,l)
    }
}

o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))


bowlers <- unique(o$bowler)
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

rankIPLBowlers <- arrange(q,desc(meanWickets),desc(meanER))


