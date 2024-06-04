df <- read_excel("C:\\Users\\raksh\\OneDrive\\Desktop\\Sem 3\\Causal AB\\Data\\Uber Pool.xlsx", sheet = 'Switchbacks')
head(df)
View(df)

# Create the 'total_trips', 'matches' and tproportion columns
df$total_trips <- df$trips_pool + df$trips_express
df$matches <- df$total_matches+df$total_double_matches
df$tproportion <- df$total_matches/df$total_trips

#Getting a visual look at it

# Create a subset with wait_time = "2 mins"
two.min <- df[df$wait_time == "2 mins", ]

# Create a subset with wait_time = "5 mins"
five.min <- df[df$wait_time == "5 mins", ]

# Create a side-by-side box plot for total_trips
boxplot(two.min$total_trips, five.min$total_trips,
        names = c("2 mins", "5 mins"),
        main = "Comparison of Trips Pool",
        col = c("blue", "green"))

# Create a side-by-side box plot for matches
boxplot(two.min$matches, five.min$matches,
        names = c("2 mins", "5 mins"),
        main = "Comparison of Total matches",
        col = c("blue", "green"))

# Create a side-by-side box plot for tproportion
boxplot(two.min$tproportion, five.min$tproportion,
        names = c("2 mins", "5 mins"),
        main = "Comparison of Total Proportion",
        col = c("blue", "green"))

# Summary statistics for total_trips
summary(two.min$total_trips)
summary(five.min$total_trips)

# Summary statistics for matches
summary(two.min$matches)
summary(five.min$matches)

# Summary statistics for tproportion
summary(two.min$tproportion)
summary(five.min$tproportion)

#paired t.test on total number of trips
t.test(two.min$total_trips,five.min$total_trips, paired=T)

#paired t.test on total proportion of trips
t.test(two.min$tproportion,five.min$tproportion, paired=T)

#paired t.test on total proportion of trips
t.test(two.min$total_driver_payout,five.min$total_driver_payout, paired=T)


#q3
p1= 21; p2=9; c=6;
avg.payout.two.min=mean(two.min$total_driver_payout/two.min$total_trips)
avg.payout.five.min=mean(five.min$total_driver_payout/five.min$total_trips)
#Assume that you had the data for the control groupbut not the treatment group.
avg.trips=mean(five.min$total_trips)
#The opportunity cost of keeping wait time
oc =(avg.payout.two.min-avg.payout.five.min)*avg.trips* p1*p2*c;
oc 
#equal to 1660601
