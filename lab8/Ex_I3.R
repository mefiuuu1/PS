tablou =  read.csv("life_expect.csv", header=T, sep=',')

male=tablou[['male']]
min1=min(male)
max1=max(male)
interval = seq(60, 85, 4)
hist(male, breaks=interval, right=T, freq=F)

female=tablou[['female']]
min2=min(female)
max2=max(female)
interval1 = seq (74, 86, 2)
hist(female, breaks=interval1, right=T, freq=F)

