#healthcare analysis

healthcare<-read.csv(file.choose(),header=T)

View(healthcare)
str(healthcare)
summary(healthcare)
names(healthcare)

table(healthcare$AGE)
table(healthcare$FEMALE)
table(healthcare$RACE)

#Which age category Has maximum Expenditure

summary(as.factor(healthcare$AGE))
max(table(healthcare$AGE))
max(summary(as.factor(healthcare$AGE)))
which.max(table(healthcare$AGE))
age <- aggregate(TOTCHG ~ AGE , data = healthcare, sum)
age
which.max(age$TOTCHG)
age[which.max(age$TOTCHG),]



# Diagnosis related group which has maximum hospitalized and expenditure
t <- table(healthcare$APRDRG)
d <- as.data.frame(t)
names(d)[1] = 'Diagnosis Group'
d
which.max(table(healthcare$APRDRG))
which.max(t)
which.max(d)          
res <- aggregate(TOTCHG ~ APRDRG, data = healthcare, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]


#Race of patient related to Hospital cost or not


healthcare$RACE <- as.factor(healthcare$RACE)

R_A<- aov(TOTCHG ~ RACE, data=healthcare)
summary(R_A)

R_L<-lm(TOTCHG ~RACE,data= healthcare)
summary(R_L)

healthcare <- na.omit(healthcare)

#Hospital cost can be predicted by Age or Gender

T_A<- aov(TOTCHG ~ AGE+FEMALE, data=healthcare)
summary(T_A)

T_L<-lm(TOTCHG ~AGE+FEMALE,data= healthcare)
summary(T_L)

#length of stay can be predicted by AGE,Female or Race


L<- aov(LOS ~ AGE+FEMALE+RACE, data=healthcare)
summary(L)

fit<-lm(LOS ~AGE+FEMALE+RACE,data= healthcare)
summary(fit)


# Agency want to know which entity effects the hospital cost most

A<- aov(TOTCHG ~., data=healthcare)
summary(A)

B<- lm(TOTCHG ~.,data=healthcare)
summary(B)





