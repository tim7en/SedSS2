x <- read.csv (file.choose())
BANK <- x[x[,2] %in% 'Bank',]

drops <- c('4SO4','NSSO2','SO1(CCS01),18p')

BANK_WITHOUT_OUTLIERS <- BANK[which(!BANK$ID %in% drops),]


dat <- BANK_WITHOUT_OUTLIERS
corfor <- 'D50'
drop  <- NULL
dev <- 3
R2 <- 0.5
alpha <- 0.05


myfit <- lm (BANK_WITHOUT_OUTLIERS$Caesium_ppm~BANK_WITHOUT_OUTLIERS$D50)


mytarget <- read.csv (file.choose())

mytarget <- mytarget[4,]

myfitT <- lm(mytarget$Caesium_ppm~mytarget$D50)

myfit$model$`BANK_WITHOUT_OUTLIERS$D50`



MYVAR <- BANK$Caesium_ppm

N <- BANK$N
D50 <- BANK$D50
TOC <- BANK$TOC

D50 <- D50^2
TOC <- TOC^(1/3)

N <- N^(1/2)

myfit <- lm(N~D50+TOC)

summary(myfit)

C13 <- BANK$C13

C13 <- BANK$C13^2
D50 <- BANK$D50^(-1)
TOC <- BANK$TOC^(1/2)

myfit <- lm(C13~D50+TOC)

summary (myfit)

y <- read.csv ('AllenTarget.csv')

AL <- 1/BANK$Aluminum
D50 <- BANK$D50^(1/2)

T_AL <- 1/y$Aluminum[1]
T_D50 <- y$D50[1]^(1/2)


myfit <- lm (AL~D50)
summary (myfit)

result <- AL - (myfit$coefficients[2]*(D50-T_D50))
result <- 1/result

