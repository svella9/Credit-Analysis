#
#setwd("/home/vijay/dapro")
library(gdata)
library(gmodels)
data = read.csv("german_credit.csv",sep = ',',header = TRUE)
options(max.print = 100000)

colnames(data)

c <- data$Creditability
ab <- data$Account.Balance
dcm <- data$Duration.of.Credit..month.
pspc <- data$Payment.Status.of.Previous.Credit
p<-data$Purpose
ca <- data$Credit.Amount
vss <- data$Value.Savings.Stocks
lce <- data$Length.of.current.employment
ipc <- data$Instalment.per.cent
sms <- data$Sex...Marital.Status
g <- data$Guarantors
dca <- data$Duration.in.Current.address
mva <- data$Most.valuable.available.asset
ay <- data$Age..years.
cc <- data$Concurrent.Credits
toa <- data$Type.of.apartment
ncab <- data$No.of.Credits.at.this.Bank
o <- data$Occupation
nod <- data$No.of.dependents
tp <- data$Telephone
fw <- data$Foreign.Worker


for(i in colnames(data))
{
  #print(i)
  #print(table(data[,i]))
  for(j in (table(data[,i]))){
    #cat(sprintf("\"%0.2f\"\t",(j/1000)*100))
  }
  #cat('\n\n')  
}

print("Percentage observations Having")

print("Bad credits and good credits are")

for(i in (table(c))){
  print((i/1000)*100)
}

print(" Account Balance as \n No Account, None, Below 200DM(<), Above 200DM(>=)")
for(i in (table(ab))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}


print(" Payment status of previous credits- Delayed,Other credits,
      Paid up, No Problem with Currrent credits, Previous Credits Paid")
for(i in (table(pspc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
  
}

print(" Savings/Stock value- None,Below 100DM, (100,500),(500,1000),Above 1000")
for(i in (table(vss))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}


print("Length of current Employment-  Unemployed, < 1 year, [1,4),[4,7),Above 7")
for(i in (table(lce))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Installment %-  Above 35%, [25,35), [20,25),Below 20%")
for(i in (table(ipc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Occupation- Unemployed unskilled, Unskilled Permanent resident,Skilled,Executive")
for(i in (table(o))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Sex and Martial- Status Male Divirced,Male Single, Male married/widowed, Female")
for(i in (table(sms))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Duration in Current Addresss < 1 year, [1,4),[4,7),Above 7")
for(i in (table(dca))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Type of appartment Free, Rented, Owned")
for(i in (table(toa))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Most valuable asset None, Car, Life Insurence, Real Estate")
for(i in (table(mva))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("No of Credits At Bank 1, 2 or 3, 4 or 5, Above")
for(i in (table(ncab))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Gurantator- None, Co-applicat,Gurantator")
for(i in (table(g))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Concurrent Credits- Other banks, Dept Stores, None")
for(i in (table(cc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("No of Dependents-  >= 3, < 3")
for(i in (table(nod))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Telephone-  Yes, No")
for(i in (table(tp))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Foreign Worker- Yes, No")
for(i in (table(fw))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}


print("Modifying the data")

#print(table(c))

print(table(ab))
ab <- replace(ab, ab==4, 3)
print(table(ab))

pspc <- replace(pspc, pspc==0, 1)
pspc <- replace(pspc, pspc==4, 3)


#print(table(vss))
vss <- replace(vss, vss==4, 3)
#print(table(vss))
vss <- replace(vss, vss==5, 4)
#print(table(vss))

lce <- replace(lce, lce==2, 1)
lce <- replace(lce, lce==3, 2)
lce <- replace(lce, lce==4, 3)
lce <- replace(lce, lce==5, 4)
table(lce)

table(sms)
sms <- replace(sms,sms==2,1)
sms <- replace(sms,sms==3,2)
sms <- replace(sms,sms==4,3)
table(sms)

table(ncab)
ncab <- replace(ncab,ncab==3, 2)
ncab <- replace(ncab,ncab==4, 2)
table(ncab)

table(g)
g <- replace(g,g==3,2)
#g <- replace(g,g==2,0)
table(g)


table(cc)
cc <- replace(cc,cc==2,1)
cc <- replace(cc,cc==3,2)
table(cc)


table(o)
o <- replace(o,o==2,1)
o <- replace(o,o==4,2)
o <- replace(o,o==3,2)
#g <- replace(g,g==2,0)
table(o)


table(p)
p<- replace(p, p==4,3)
p<- replace(p, p==5,3)
p<- replace(p, p==6,3)
p<- replace(p, p==0,4)
p<- replace(p, p==8,4)
p<- replace(p, p==9,4)
p<- replace(p, p==10,4)
print("4 others 1 new car 2 old car 3 home")
table(p)

x <- data.frame(Creditability=c,Account.Balance=ab,Duration.of.Credit..month.=dcm,Payment.Status.of.Previous.Credit=pspc,Purpose=p,Credit.Amount=ca,Value.Savings.Stocks=vss,Length.of.current.employment=lce,Instalment.per.cent =ipc,Sex...Marital.Status=sms,Guarantors=g,Duration.in.Current.address=dca,Most.valuable.available.asset=mva,Age..years.=ay,Concurrent.Credits=cc,Type.of.apartment=toa,No.of.Credits.at.this.Bank=ncab,Occupation=o,No.of.dependents=nod,Telephone=tp)

write.csv(x,file="cleaned_data.csv") #cleaned_data.csv contains the modified data

CrossTable(c,ab, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,pspc, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,vss, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,lce, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,sms, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,ncab, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,g, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,cc, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,toa, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,nod, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,p, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(c,ipc, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

summary(dcm)
summary(ca)
summary(ay)

bns <- seq(0, 80, 10) 

hist(dcm, breaks=bns, xlab = "Duration Credit Month", ylab = "Frequency", main = " ", cex=0.4)
hist(ca, breaks=bns, xlab = "Credit Amount in DM", ylab = "Frequency", main = " ", cex=0.4)
hist(ay, xlab = "Age in years", ylab = "Frequency", main = " ", cex=0.4)

boxplot(dcm, bty="n",xlab = "Duration of Credit Month", cex=0.4)
boxplot(ca, bty="n",xlab = "Credit Amount in DM", cex=0.4)
boxplot(ay, bty="n",xlab = "Age in years", cex=0.4)

