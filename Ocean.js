library(pwr)
library(pwr2)
library(nlme)   
library(lattice)   
library(plyr)      
library(plm)       
library(AER)        
library(multcomp)
library(car)
library(mvnormtest)  
library(sandwich)
 
rabs <- read.csv("Rabbit2.csv")
rabs
 
summary(rabs$Diet)   
n1 =4
n2 = 4
with(rabs,tapply(weight, Diet, var))   
var1 = 14.66667   
var2 = 22.66667   
sd.pool = sqrt(((n1 - 1) * var1 + (n2 - 1) * var2)/(n1 + n2 - 2))
sd.pool
 
with(rabs,tapply(weight, Diet, sd))
 
pwr.t.test(n =4 , d =10/sd.pool , sig.level =0.05 , power = , type = c("two.sample")) 
 
pwr.t.test(n = , d =10/sd.pool , sig.level =0.05 , power =0.9 , type = c("two.sample")) 
 
pwr.t.test(n =4 , d =5/sd.pool , sig.level =0.05 , power = , type = c("two.sample")) 
 
pwr.t.test(n =4 , d =5/sd.pool , sig.level =0.05 , power = , type = c("paired")) 
 
pwr.t.test(n =20 , d =5/sd.pool , sig.level =0.05 , power = , type = c("two.sample")) 
 
pwr.t2n.test(n1 =10 , n2=30 , d = 0.6, sig.level = 0.05, power =  )
 
pwr.1way(k=5, n=15, alpha=0.05, f=0.1)
 
pwr.1way(k=3, n=25, alpha=0.05, f=0.1)
 
pwr.1way(k=3, n=25, alpha=0.05, f=0.25)
 
pwr.1way(k=3, n=25, alpha=0.05, f=0.4)
 
ss.1way(k=3, alpha=0.05, beta=0.2, f=0.1, B=100)
 
ss.1way(k=3, alpha=0.05, beta=0.1, f=0.25, B=100)
 
ss.1way(k=3, alpha=0.05, beta=0.2, f=0.4, B=100)
 
ss.2way(a=3, b=3, alpha=0.05, beta=0.2, f.A=0.2, f.B=0.2, B=100)
 
ss.1way(k=3, alpha=0.05, beta=0.2, f=0.2, B=100)
 
pwr.2way(a=4, b=2, alpha=0.05, size.A = 10, f.A=0.25, size.B = 40, f.B=0.25)
 
pwr.2way(a=3, b=3, alpha=0.05, size.A = 24, f.A=0.25, size.B = 24, f.B=0.25)
 
Rat <- read.csv("Rat1.csv")
Rat
 
Rat <- stack(Rat)
Rat
 
mem2 <- lme(values ~ 1, random = ~1|ind, data=Rat)
 
VarCorr(mem2)
 
Tot.Var <- 0.15752083 + 0.02566667
across.rats <- 0.15752083  
within.liver <- 0.02566667
 
within.liver/Tot.Var
 
across.rats/Tot.Var
 
mydata <- read.csv("analyte1.csv")
 
head(mydata)
 
mem2.1 <- lme(Conc ~ 1, random = ~1|Lab, data=mydata)
VarCorr(mem2.1)
 
mem2.2 <- lme(Conc ~ 1, random = ~1|Lab/Bat, data=mydata)
 
VarCorr(mem2.2)
 
Tot.Var.A <- 0.060168241 + 0.005368056 + 0.006297222
labs <- 0.060168241 
batch <- 0.02566667
within.batch <- 0.006297222
 
labs/Tot.Var.A
batch/Tot.Var.A
within.batch/Tot.Var.A
 
aggregate( Conc~Lab, mydata, mean )
 
boxplot(Conc~Lab, data = mydata)
 
rab2 <- read.csv('Bunnie1.csv')
str(rab2)
rab2
 
boxplot(weight~Diet, data = rab2)
 
with(rab2, bartlett.test(weight~Diet))  
 
anova(lm(weight~Diet, data = rab2))
 
anova(lm( weight~Diet +Breed,  data = rab2))
 
with(rab2, bartlett.test(weight~interaction(Diet,Breed)))
 
Tuk1 <- aov(weight~Diet +Breed, data = rab2)
 
TukeyHSD(Tuk1, "Diet")
 
mem1 <- lme(weight ~ Diet, random = ~1|Breed, data=rab2)
 
mem2 <- lme(weight ~ Diet, random = ~1|Breed, weights=varIdent(form=~1|Diet),data=rab2)
 
anova(mem1,mem2)
 
Anova(mem1)   
 
TukeyResult <- glht(mem1,  mcp(Diet= "Tukey") )
summary(TukeyResult)
 
kat <- read.csv("cats.csv")
head(kat)
str(kat)
kat$ID <- as.factor(kat$cat)
str(kat)
 
boxplot(time~anesthetic, data =kat)
 
with(kat,bartlett.test(time,anesthetic))
 
anova(lm(time~anesthetic, data=kat))
 
TukeyHSD(aov(time~anesthetic, data = kat))
 
model.1 <- lme(time~anesthetic, random = ~1|ID, data = kat)
 
Anova(model.1)   
 
summary(glht(model.1, linfct=mcp(anesthetic = "Tukey")))
 
confint(glht(model.1, linfct=mcp(anesthetic = "Tukey")))
 
plot(confint(glht(model.1, linfct=mcp(anesthetic = "Tukey"))))
 
model.2 <- lme(time~anesthetic, random = ~1|ID, weights=varIdent(form=~1|anesthetic), data = kat)
 
anova(model.1,model.2)
 
Anova(model.2)   
 
summary(glht(model.2, linfct=mcp(anesthetic = "Tukey")))
 
opar <- par(mfrow = c(2, 1))
 
plot(confint(glht(model.2, linfct=mcp(anesthetic = "Tukey"))))
 
plot(confint(glht(model.1, linfct=mcp(anesthetic = "Tukey"))))
 
opar <- par(mfrow = c(1, 1))
 
Trial <- read.csv("UnbalancedDrug1.csv")
Trial
head(Trial)  
 
Trial <- na.omit(Trial)  
 
str(Trial)
 
Trial$Drug <- as.factor(Trial$Drug) 
Trial$Disease <- as.factor(Trial$Disease) 
 
str(Trial)
 
boxplot(Reps~Drug*Disease, data = Trial)
 
with(Trial,interaction.plot(Drug,Disease,Reps , col=c(1,2,4), lty=c(1,1,1)))
 
with(Trial,tapply(Reps,list(Drug,Disease), mean))
 
with(Trial,tapply(Reps,list(Drug,Disease), var))
 
with(Trial,tapply(Reps,list(Drug,Disease), sd))
 
with(Trial,tapply(Reps,list(Drug,Disease), length)) 
 
with(Trial, bartlett.test(Reps, interaction(Drug,Disease)))
 
lm1 <- lm(Reps~Drug*Disease, data = Trial)
 
anova(lm1)
 
lm2 <- lm(Reps~Disease+ Drug, data = Trial)
 
anova(lm2)
 
lm3 <- lm(Reps~Drug+Disease, data = Trial)
 
anova(lm3)
 
Anova(lm3)
Anova(lm2)
 
summary(glht(lm3, mcp(Drug= "Tukey")))
summary(glht(lm3, mcp(Disease= "Tukey")))
 
V <- read.csv("varietycontrast.csv")
head(V)
 
with(V,tapply(Yield,Variety, mean))  
with(V,tapply(Yield,Variety, sd))   
 
boxplot(Yield~Variety, data = V)  
 
with(V, bartlett.test(Yield~Variety)) 
 
lm1 <- lm(Yield~Variety, data =V)
Anova(lm1, vcov =vcovHC(lm1))
 
lm2 <- lm(Yield~C(Variety, contr.treatment(3, base=2)), data =V)
 
coeftest(lm2, vcovHC(lm2))
 
lm3 <- lm(Yield~C(Variety, contr.sum(3)), data =V)
 
coeftest(lm3, vcovHC(lm3))  
 
V$Variety <- factor(V$Variety, levels=rev(levels(V$Variety)) )
 
lm4 <- lm(Yield~C(Variety, contr.sum(3)), data =V)
coeftest(lm4, vcovHC(lm4))
 
P <- read.csv("pigwt1.csv")
 
head(P, n=10)
tail(P, n=10)
str(P)
 
P
 
xyplot(weight~num.weeks, groups=id.num, type ="b",data = P) 
 
xyplot(weight~num.weeks, groups=id.num, type ="p",data = P) 
 
plot(weight~num.weeks, col=as.factor(id.num), type ="p",data = P) 
 
lm1 <-lm(weight ~ num.weeks, data =P)  
summary(lm1)
 
plot(resid(lm1)~P$id.num, col=as.numeric(P$id.num))
abline(h=0)
 
boxplot(resid(lm1)~P$id.num)
abline(h=0)
 
lm2 <-lm(weight ~ num.weeks+ as.factor(id.num), data =P)  
summary(lm2)
 
anova(lm1,lm2)  
 
plot(resid(lm2)~P$id.num, col=as.numeric(P$id.num))
abline(h=0)
 
boxplot(resid(lm2)~P$id.num, col="grey60")
abline(h=0)
 
summary(lm2)
 
P <- ddply(P, "id.num", transform, g.m.weeks = mean(num.weeks))
P <- ddply(P, "id.num", transform, g.m.weight = mean(weight))
 
str(P)
 
P$Dweeks <- P$num.weeks - P$g.m.weeks
P$Dweight <- P$weight - P$g.m.weight
 
str(P)
 
xyplot(Dweight ~ Dweeks, groups=id.num, type ="b",data = P) 
 
lm3 <- lm(Dweight~Dweeks -1, data = P)
summary(lm3)
 
boxplot(resid(lm3)~P$id.num, col='green')
abline(h=0)
 
fp1 <- plm(weight ~ num.weeks, data = P, index=c("id.num"), model ="within") 
 
summary(fp1)
 
mem1 <-lme(weight ~ num.weeks, random = ~1|id.num, data=P)   
summary(mem1)
 
rp1 <- plm(weight ~ num.weeks , data = P, index=c("id.num"), model ="random")   
summary(rp1)
 
mem1 <-lme(weight ~ num.weeks, random = ~1|id.num, data=P)   
summary(mem1)
 
plot(ACF(mem1))
 
mem2 <-lme(weight ~ num.weeks, random = ~1|id.num, corr = corARMA(p=1, q= 0), data=P)   
summary(mem2)
 
anova(mem2,mem1)
 
mem3 <-lme(weight ~ num.weeks, random = ~1|id.num, corr = corARMA(p=2, q= 0), data=P)   
summary(mem3)
 
anova(mem3,mem2)
 
xyplot(weight~num.weeks, groups=id.num, type ="b",data = P) 
 
ctrl <- lmeControl(opt='optim')   
 
mem4 <-lme(weight ~ num.weeks, random = ~1+ num.weeks|id.num, corr = corARMA(p=1, q= 0), control=ctrl, data=P)  
summary(mem4)
 
anova(mem4,mem2)
 
mem5 <-lme(weight ~ num.weeks, random = ~1+ num.weeks|id.num, corr = corARMA(p=1, q= 0), weights =varPower(),
           control=ctrl, data=P)  
summary(mem5)
 
anova(mem4,mem5)
 
R <- read.csv("rats1.csv")
R
str(R)
head(R)
tail(R)
 
R <- subset(R,Time!=44)
 
str(R)
 
R$rat.id <- as.factor(R$Rat)
 
R$DietF <- as.factor(R$Diet)
 
str(R)
 
xyplot(weight~Time, groups=DietF, type ="b",data = R) 
 
xyplot(weight~Time, groups=DietF, type ="p",data = R) 
 
with(R, interaction.plot(Time, Rat, weight, col=rep(c('blue', 'deeppink'), c(4,4))))
 
Ratlm <-lm(weight ~ Time+ DietF, data=R)
summary(Ratlm)
 
Ratlm2 <-lm(weight ~ Time*DietF, data=R)
summary(Ratlm2)
 
boxplot(resid(Ratlm2)~R$rat.id, col="grey60")
abline(h=0)
 
rat1 <-lme(weight ~ Time*DietF, random = ~1|rat.id , data=R)  
summary(rat1)
 
rat2 <-lme(weight ~ Time*DietF, random = ~0+Time|rat.id , data=R)  
summary(rat2)
 
rat3 <-lme(weight ~ Time*DietF, random = ~1+Time|rat.id , data=R)  
summary(rat3)
 
anova(rat1, Ratlm2)   
 
anova(rat2, rat1)   
 
anova(rat3, rat1)   
 
plot(rat3,resid(.)~fitted(.))
 
ctrl <- lmeControl(opt='optim')   
 
rat4 <-lme(weight ~ Time*DietF, random = ~1+Time|rat.id , 
           weights =varPower(),control=ctrl, data=R)  
summary(rat4)
 
anova(rat4,rat3)  
 
xyplot(weight~Time, groups=DietF, type ="b",data = R) 
 
rat5 <-lme(weight ~ Time*DietF, random = ~1+Time|rat.id , corr = corARMA(p=1, q= 0),control=ctrl, data=R) 
 
summary(rat5)
 
anova(rat5,rat3)   
 
summary(Ratlm2)
 
summary(rat5)
 
oats1 <- read.csv("oats.csv")
 
head(oats1)
str(oats1)
 
with(oats1,interaction.plot(Variety, Nitrogen, Yield))
 
aggregate(Yield~Variety, oats1, mean )
aggregate(Yield~Block, oats1, mean )
aggregate(Yield~Nitrogen, oats1, mean )
 
aggregate(Yield~Nitrogen*Variety, oats1, mean )
boxplot(Yield~Nitrogen*Variety, data =oats1 )
 
mem.oats.1 <- lme(Yield~ Variety*Nitrogen, random =~1|Block/Variety, data =oats1)
 
Anova(mem.oats.1)
 
mem.oats.0 <- lme(Yield~ Variety*Nitrogen, random =~1|Block, data =oats1)
 
anova(mem.oats.0,mem.oats.1)  
 
mem.oats.2 <- lme(Yield~ Variety+Nitrogen, random =~1|Block/Variety, data =oats1)
 
Anova(mem.oats.2)   
 
mem.oats.3 <- lme(Yield~ Variety+Nitrogen, random =~1|Block/Variety, 
                  weights=varIdent(form=~1|Block*Variety),
                  data =oats1)
 
anova(mem.oats.3,mem.oats.2)
 
Anova(mem.oats.2)   
 
summary(glht(mem.oats.2, mcp(Nitrogen= "Tukey")))
confint(glht(mem.oats.2, mcp(Nitrogen= "Tukey")))
 
summary(glht(mem.oats.2, mcp(Variety= "Tukey")))
confint(glht(mem.oats.2, mcp(Variety= "Tukey")))
 
my.data <- read.csv("birds.csv", stringsAsFactors = T)
 
str(my.data)
head(my.data)
 
boxplot(Fat~Month, data = my.data)
boxplot(Dryweight~Month, data = my.data)
 
bartlett.test(Fat~Month, data = my.data)
bartlett.test(Dryweight~Month, data = my.data)
 
with(my.data,cor.test(Fat,Dryweight))
 
out.1 <- lm(Fat~Month, data = my.data)
out.2 <- lm(Dryweight~Month, data = my.data)
 
shapiro.test(out.1$res)  
shapiro.test(out.2$res)  
 
errors <- rbind(out.1$res,out.2$res)
mshapiro.test(errors)
 
man.out <- manova(cbind(Dryweight, Fat) ~  Month, data=my.data )
 
summary(man.out, test = c("Pillai"))
 
summary(man.out, test = c("Wilks"))
 
Anova(out.1, vcov. = vcovHC)
with(my.data,pairwise.t.test(Fat,Month, pool.sd =F ))
 
tuk.out.1 <- glht(out.1, linfct = mcp(Month = "Tukey"), vcov.=vcovHC(out.1))
summary(tuk.out.1)
 
Anova(out.2)
with(my.data,pairwise.t.test(Dryweigh,Month, pool.sd =T ))
 
tuk.out.2 <- glht(out.2, linfct = mcp(Month = "Tukey"))
summary(tuk.out.2)
 
sugar <- read.csv("sugarcaneexperiment.csv") 
 
head(sugar)
tail(sugar)
str(sugar)
 
with(sugar,plot(Yield~Nitrogen))
 
with(sugar,plot(log(Yield)~Nitrogen))
 
lm1 <- lm(log(Yield)~Nitrogen, data = sugar)
 
with(sugar,plot(log(Yield)~Nitrogen, ylim =c(4,5.6), las=1))
abline(lm1)
 
with(sugar,plot(Yield~Nitrogen))
 
fm <- nls(Yield ~ SSasymp(Nitrogen, Asym, R0, lrc), data = sugar)
summary(fm)
 
new.nitrogen <- data.frame(Nitrogen = c(seq(from= 0, to=400, by =1 )))
 
preds1 <- predict(fm, newdata = new.nitrogen)
 
with(sugar,plot(Yield~Nitrogen, ylim = c(0,220), 
                ylab = "Yield T per Ha",
                xlab = "Nitrogen kg per ha",
                pch=4,
                las=1))
 
lines(seq(from= 0, to=400, by =1 ),preds1[1:401])
 
lines(seq(from= 0, to=400, by =1 ), rep(203,401), col ="grey", )
 
legend("bottomright", legend =c("Data", "Asymtote", "Fitted"),
       pch =c(4,NA,NA),
       col = c("black", "grey", "black"),
       lty = c(NA,1,1),
       bty = "n")
 
diff <- preds1[2:400] -preds1[1:399]
 
head(diff) 
 
plot(seq(from= 1, to=399, by =1 ), diff, las=1,
     type="l",
     xlab = "Level of nitogen (kg per ha)",
     ylab = "Marginal effect (T per ha)"  )
 
fox.data <-read.csv("foxpopulation.csv", stringsAsFactors = T)
head(fox.data)
str(fox.data)
 
plot(Foxes~Year, data = fox.data)
 
fm1 <- nls(Foxes ~ SSlogis(Year, Asym, xmid, scal), data =fox.data)
summary(fm1)
 
preds2 <- predict(fm1)
preds2
lines(fox.data$Year,preds2)
 
preds3 <- predict(fm1, newdata = data.frame(Year = c(seq(from= 2000, to=2030, by =1 ))))
 
plot(Foxes~Year, 
     ylim =c(0,3000),
     xlim=c(2000, 2030),
     ylab = "Fox population",
     xlab = "Year",
     pch = 16,
     col = "blue",
     las=1,
     data = fox.data)
 
segments(x0=0, y0=coef(fm1)[1], x1 = 2030, y1 = coef(fm1)[1],col="grey")
 
lines(seq(from= 2020, to=2030, by =1 ),preds3[21:31], lwd=2, lty=2, col="red")
 
lines(fox.data$Year,preds2,lwd=2)
 
text(2020,2600, "Predictions only from here", pos=4, cex=0.8)
 
collist <- c('blue','black','red')
pchlist <- c(16,NA,NA)
lwdlist <- c(NA,2,2)
ltylist <-  c(NA,1,2)
 
legend("bottomright", 
       c("data", "predictions", "forecasts"), 
       lty =ltylist,
       lwd=lwdlist,
       pch=pchlist,
       col = collist,  
       bty='y')
 
loss <- read.csv("YieldLoss.csv")
head(loss)
str(loss)
with(loss,plot(YieldLoss~WeedRate))
 
whale.data <-read.csv("whalepopulation.csv", stringsAsFactors = T)
head(whale.data)
str(whale.data)
plot(Sightings~Year, data = whale.data)
 