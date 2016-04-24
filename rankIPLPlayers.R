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

#aa <- list(csk_details,dc_details,dd_details,kxip_details,ktk_details,kkr_details,
           #mi_details,pw_details,rr_details,rcb_details,sh_details)


o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))


#1, CSK
bowlers <- unique(csk_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Chennai Super Kings",name=bowlers[x],dir="."),
        error = function(e) {
        print("Error!")
        }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#2. Deccan Chargers
bowlers <- unique(dc_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Deccan Chargers",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#3, Delhi Daredevils
bowlers <- unique(dd_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Delhi Daredevils",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#4. Kings XI Punjab
bowlers <- unique(kxip_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Kings XI Punjab",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#5. Kochi Tuskers Kerala
bowlers <- unique(ktk_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Kochi Tuskers Kerala",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#6. Kolkata Knight Riders
bowlers <- unique(ktk_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Kolkata Knight Riders",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#7. Mumbai Indians
bowlers <- unique(mi_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Mumbai Indians",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#8.  Pune Warriors
bowlers <- unique(pw_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Pune Warriors",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#9. Rajasthan Royals
bowlers <- unique(rr_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Rajasthan Royals",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#10.  Royal Challengers Bangalore
bowlers <- unique(rcb_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Royal Challengers Bangalore",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

#11. Sunrisers Hyderabad
bowlers <- unique(sh_details$bowler)
for (x in 1:length(bowlers)){
    tryCatch(l <- getBowlerWicketDetails(team="Sunrisers Hyderabad",name=bowlers[x],dir="."),
             error = function(e) {
                 print("Error!")
             }
    )    
    l <- select(l,bowler,wickets,economyRate)
    o <-rbind(o,l)
}

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

m <- mutate(o,matches=n(),meanWickets=mean(wickets),meanER=mean(economyRate))
m <- select(m,bowler,matches,meanWickets,meanER)
n <- m[1,]
o <- rbind(o,n)

