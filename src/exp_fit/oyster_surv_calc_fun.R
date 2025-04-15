# oyster mortality function
# we will calculate mortality by ... 
# (Ot/Dt-1) * (St-1)

# generate some fake data
oyster_fake <- data.frame(event = c(1,1,1,2,2,2,3,3,3), 
                          pop = c("A","B","C","A","B","C","A","B","C"),
                          alive_count = c(110,100,120,75,70,76,50,40,45),
                          dead_count = c(25,35,14,35,30,44,0,10,5),
                          alive_returned = c(110,100,120,50,50,50,50,40,45))

# Now I need to create a function to calculate mortality
# could simplify function and just use grouping in tidyr to determine by pop?
surv_calc_p <- function(dat, e, p) {
  O = sum(dat[dat$event == e & dat$pop == p,]$alive_count)
  D = ifelse(e - 1 == 0, 
             sum(dat[dat$event == 1 & dat$pop == p,]$alive_count,
                 dat[dat$event == 1 & dat$pop == p,]$dead_count),
             sum(dat[dat$event == (e-1) & dat$pop == p,]$alive_returned))
  S = ifelse(e - 1 == 0, 1,
             surv_calc_p(dat, (e-1), p))
  survival = O/D * S
}

# simplified version
surv_calc <- function(dat, e) {
  O = sum(dat[dat$event == e,]$alive_count)
  D = ifelse(e - 1 == 0, 
             sum(dat[dat$event == 1,]$alive_count,
                 dat[dat$event == 1,]$dead_count),
             sum(dat[dat$event == (e-1),]$alive_returned))
  S = ifelse(e - 1 == 0, 1,
             surv_calc(dat, (e-1)))
  survival = O/D * S
}

# could also make a version that calculates survival on a bag level only
surv_calc_bag <- function(dat, e) {
  O = dat[dat$event == e,]$alive_count
  D = ifelse(e - 1 == 0, 
             (dat[dat$event == 1,]$alive_count + dat[dat$event == 1,]$dead_count),
             dat[dat$event == (e-1),]$alive_returned)
  S = ifelse(e - 1 == 0, 1,
             surv_calc_bag(dat, (e-1)))
  survival = O/D * S
  df = data.frame(bag = dat$bags_label, event = e, surv = survival)
  df1 = df[1:length(levels(as.factor(dat$bags_label))),]
  return(df1)
}

print(surv_calc_p(oyster_fake, 1, "A"))
print(surv_calc_p(oyster_fake, 1, "B"))
print(surv_calc_p(oyster_fake, 1, "C"))

print(surv_calc(oyster_fake, 1))

oyster_fake_bags <- data.frame(event = c(1,1,1,1,1,1,
                                    2,2,2,2,2,2,
                                    3,3,3,3,3,3), 
                          pop = c("A","A","B","B","C","C",
                                  "A","A","B","B","C","C",
                                  "A","A","B","B","C","C"),
                          bags_label = c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6),
                          alive_count = c(110,100,120,112,102,118,
                                          75,70,76,75,75,74,
                                          50,40,45,50,48,40),
                          dead_count = c(25,35,14,20,16,24,
                                         35,30,44,37,27,44,
                                         0,10,5,0,2,10),
                          alive_returned = c(110,100,120,112,102,118,
                                             50,50,50,50,50,50,
                                             50,40,45,50,48,40))

surv_calc_b <- function(dat, e) {
  O = dat[dat$event == e,]$alive_count
  D = ifelse(e - 1 == 0, 
             (dat[dat$event == 1,]$alive_count + dat[dat$event == 1,]$dead_count),
             dat[dat$event == (e-1),]$alive_returned)
  S = ifelse(e - 1 == 0, 1,
             surv_calc_b(dat, (e-1)))
  survival = O/D * S
  return(survival)
}

# testing
print(surv_calc_p(oyster_fake, 1, "A"))
print(surv_calc_p(oyster_fake_bags, 1, "A"))

print(surv_calc(oyster_fake, 1))
print(surv_calc(oyster_fake_bags, 1))

print(surv_calc_bag(oyster_fake, 1))
surv_calc_bag(oyster_fake_bags, 1)
surv_calc_bag(oyster_fake_bags, 2)
