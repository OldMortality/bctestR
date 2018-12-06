# create test cases for breast cancer project
#
#
#

# print Odds ratios 
PRINTORS= FALSE

create.eth <- function() {
  i <- round(runif(1,0,3))
  eth = 'eur'
  if (i==0) {
    eth <- 'eur'
  }
  if (i==1) {
    eth <- 'mao'
  }
  if (i==2) {
    eth <- 'oth'
  }
  return(eth)
  
}



create.test <- function() {
  eth <- create.eth()
  age1 <- round(runif(1,1,3))
  age2 <- round(runif(1,1,4))
  par <- round(runif(1,0,4))
  con <- round(runif(1,0,1))
  hbcan <- round(runif(1,0,2))
  hbben <- round(runif(1,0,1))
  htdis <-  round(runif(1,0,1))
  age <- round(runif(1,20,54))
  rr <- calc.risk(eth,age1,age2,par,con,hbcan,hbben,htdis,age)
  return(c(eth,age1,age2,par,con,hbcan,hbben,htdis,age,rr))
}


calc.rr <- function(ethP,age1P,age2P,parP,conP,hbcanP,hbbenP,htdisP,age) {
  result <- 0.1
  # ethnicity
  eth <- c(1,1.6933480)
  if (age < 50) {
    eth <- c(1,1.6945510)
  }
  if (age < 40) {
    eth <- c(1,1.8781130)
  }
  
  # age1
  age1 <- c(1,1.4724590,1.3363460)
  if (age < 50) {
    age1 <- c(1,1.1611370,1.2784060)
  }
  if (age < 40) {
    age1 <- c(1,1.2458240,1.5706720)
  }
  
  # age2
  age2 <- c(1,1.6360440,1.8564560,1.6326780)
  if (age < 50) {
    age2 <- c(-1,2.5666830,1,1.3288150)
  }
  if (age < 40) {
    age2 <- c(-1,-1,1.3860280,1)
  }
  # parity
  par <- c(2.4877230,1.8288870,1.2481120,1.2545910,1)
  if (age < 50) {
    par <- c(2.1628540,2.8637210,1.5404100,1.4023330,1)
  }
  if (age < 40) {
    par <- c(2.4407610,1,2.0563060,3.1174840,2.2136740)
  }
  # oral contraceptives
  con <- c(1,1.3689630)
  if (age < 50) {
    con <- c(1,1.2328330)
  }
  if (age < 40) {
    con <- c(1,1.1665310)
  }
  # fam hist bc hbcan
  hbcan <- c(1,1.1381650,2.0561460)
  if (age < 50) {
    hbcan <- c(1,2.3343510,2.7571180)
  }
  if (age < 40) {
    hbcan <- c(1,1.2265770,3.2352410)
  }  
  # hbben benign breast disease
  hbben <- c(1,1.6373880)
  if (age < 50) {
    hbben <- c(1,1.1701340)
  }
  if (age < 40) {
    hbben <- c(1,2.9657820)
  }
  # history of theroid disease
  htdis <- c(1,2.3022770)
  if (age < 50) {
    htdis <- c(1,1.3971010)
  }
  if (age < 40) {
    htdis <- c(1,1.3393520)
  }
  ethIndex <- 1
  if (ethP == "mao") {
    ethIndex = 2
  } 
  
  or.eth <- eth[ethIndex]
  or.age1 <- age1[age1P]
  or.age2 <- age2[age2P]
  # add 1 to index of those who can be zero,
  #   because R arrays start at 1
  or.par <- par[parP+1]
  or.con <- con[conP+1]
  or.hbcan <- hbcan[hbcanP+1]
  or.hbben <- hbben[hbbenP+1]
  or.htdis <- htdis[htdisP+1]
  if (PRINTORS) {
    print(or.eth)
    print(or.age1)
    print(or.age2)
    print(or.par)
    print(or.con)
    print(or.hbcan)
    print(or.hbben)
    print(or.htdis)
  }
  result <- or.eth * or.age1 * or.age2 * or.par * or.con * or.hbcan * or.hbben * or.htdis
  return(result)
} 

calc.ar <- function(age) {
  ar <- 0.74961775
  if (age < 50) {
    ar <- 0.690972837
  }
  if (age < 40) {
    ar <- 0.761488315
  }
  return(ar)
}

to.agegroup <- function(age) {
  result <- 6
  if (age < 50) { result <- 5}
  if (age < 45) { result <- 4}
  if (age < 40) { result <- 3}
  if (age < 35) { result <- 2}
  if (age < 30) { result <- 1}
  if (age < 25) { result <- 0}
  return(result)
} 

mortality <- function(eth,age.group) {
  eur.mortality <- c(30.79,29.43,41.44,43.83,79.92,118.54,177.30)
  mao.mortality <- c(54.89,62.69,83.81,117.45,155.05,291.82,492.52)
  if (eth == 'eur') {
    # add 1 because R starts arrays at index 1
    result <- eur.mortality[age.group + 1]
  } else {
    result <- mao.mortality[age.group + 1]
  }
  return(1e-5 *result)
}

baseline <- function(age.group) {
  base <- 1e-5 * c(1.65,7.93,29.97,64.97,125.09,237.35,215.08)
  return(base[age.group+1])
}


calc.risk <- function(eth,age1,age2,par,con,hbcan,hbben,htdis,age) {
  result <- -1
  rr <- calc.rr(eth,age1,age2,par,con,hbcan,hbben,htdis,age)
  rr2 <- calc.rr(eth,age1,age2,par,con,hbcan,hbben,htdis,age+5)
  #return(rr)
  age.group <- to.agegroup(age)
  next.age.group <- to.agegroup(age + 5)
  years.in.age.group <- 5 - age %% 5
  years.in.next.age.group <- 5 - years.in.age.group
  numerator <- baseline(age.group) * (1-calc.ar(age)) * rr 
  denominator <- numerator + mortality(eth,age.group)
  first.term <- (numerator / denominator ) * (1 - exp(-years.in.age.group * denominator))
  
  numerator.2 <- baseline(next.age.group) * (1-calc.ar(age+5)) * rr2
  denominator.2 <- numerator.2 + mortality(eth,next.age.group)
  second <- (numerator.2 / denominator.2) * exp(-years.in.age.group * denominator)
  third <- 1 - exp(-years.in.next.age.group * denominator.2)
  second.term <- second * third
  result <- first.term + second.term
  return(result)
  
} 

create.test()

eth = "eur"
age1 = 3
age2 = 3
par = 0
con  = 1
hbcan = 2
hbben = 1
htdis = 1
age = 50

# example 1
calc.rr("eur",3,3,2,1,2,1,1,30)
calc.risk("eur",3,3,2,1,2,1,1,30)
# example 2
calc.rr("eur",3,3,2,1,2,1,1,33)
calc.risk("eur",3,3,2,1,2,1,1,33)
# example 3
calc.rr("eur",3,3,1,1,2,1,1,40)
calc.risk("eur",3,3,1,1,2,1,1,40)
# example 4
eth = "eur"
age1 = 3
age2 = 3
par = 1
con  = 1
hbcan = 2
hbben = 1
htdis = 1
age = 47 
  
calc.rr("eur",3,3,1,1,2,1,1,47)
calc.risk("eur",3,3,1,1,2,1,1,47)

#example 5
eth = "eur"
age1 = 3
age2 = 3
par = 0
con  = 1
hbcan = 2
hbben = 1
htdis = 1
age = 50

calc.rr("eur",3,3,0,1,2,1,1,50)
calc.risk("eur",3,3,0,1,2,1,1,50)




# example 2
calc.rr("eur",3,3,0,1,2,1,1,50)


calc.rr("mao",2,3,1,1,1,0,1,50)

{
result <- data.frame(eth,age1,age2,par,con,hbcan,hbben,htdis,age,absrisk,
  stringsAsFactors=FALSE)
N <- 1000
for (i in 1:N) {
  
  eth <- "eur"
  if (rbinom(1,1,prob=0.5)==1) { eth = "mao"}
  age <- trunc(runif(1,20,51))
  age1 <- sample(c(1,2,3),size=1)
  age2 <- sample(c(1,2,3,4),size=1)
  par <- sample(c(0,1,2,3,4),size=1)
  con <- sample(c(0,1),size=1)
  hbcan <- sample(c(0,1,2),size=1)
  hbben <- sample(c(0,1),size=1)
  htdis <- sample(c(0,1),size=1)
  age <- trunc(runif(1,20,51))
  absrisk <- calc.risk(eth,
                       age1,
                       age2,
                       par,
                       con,
                       hbcan,
                       hbben,
                       htdis,
                       age)
  print(paste(i,absrisk))
    #if (absrisk < 0) {
    # test cases with negative absrisk occur if the test case is invalid
    #   for example, when we have age at menopause > age. The user interface
    #   will prevent such cases, and the algorithm (web service) will produce
    #   a negative number to show something is amiss. However, we can leave them
    #   in here, because the output (the negative number) should still be the same
    #   in both implemtations (this one here and the web service)
    absrisk <- formatC( round( absrisk, 4 ), format='f', digits=4 )
    
    testcase <- c(eth,
                  age1,
                  age2,
                  par,
                  con,
                  hbcan,
                  hbben,
                  htdis,
                  age,
                  absrisk)
    
    result <- rbind(testcase,result)
  #} else {
  #  next;
  #}
  #rr <- calc.risk(testcase)
  #calc.risk("eur",3,3,2,1,2,1,1,30)
  
  
}
}

# drop the last line, because -inexplicably- it can contain negative risk
result <- result[-dim(result)[1],]
tail(result)
write.csv(result,'/home/michel/Documents/breast cancer calculator/testdata/mytest02.csv',row.names=F,quote=F)


x <- 0.00034


formatC( round( x, 4 ), format='f', digits=4 )

calc.rr("mao",2,2,1,1,0,1,1,47)
calc.risk("mao",2,2,1,1,0,1,1,47)

PRINTORS = F
calc.rr("mao",3,3, 0, 1, 2, 1, 1, 20)
calc.risk("mao",3,3, 0, 1, 2, 1, 1, 20)

PRINTORS = T
absrisk = calc.rr("mao",    3,    1,   1,   0,     1,     1,     1,  37)

absrisk = calc.risk("mao",    3,    1,   1,   0,     1,     1,     1,  37)
