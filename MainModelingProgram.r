
library(fastDummies)
library(dplyr)
library(MASS)


rm(list=ls())

coil<-read.csv('f:/IDS462/Spring2023Redo/Week06/Coil1.csv')
#str(coil)

coil<-subset(coil,select=c(
SeqNum,MOSTYP,MOSHOO,MSKB1,MGEMOM,MSKC,MAANTH,MAUT0,MGODPR,MAUT1,MAUT2,MFALLE,MGEMLE,MGODRK,MSKA,MSKB2,MRELGE,
MOPLHO,MINKGE,MFWEKI,MHHUUR,PAANHA,PWAPAR,PPERSA,APERSA,AWAPAR,AMOTSC,Resp))

str(coil)
			

coil<-coil %>% dplyr::select(SeqNum, 
MOSTYP,
MOSHOO,
MGEMLE,
MGEMOM,
MAANTH,
everything())
			
	
coil<-coil %>% 
  mutate_at(vars(MSKB1:MHHUUR),~ifelse(.x==0,  0,
                                  ifelse(.x==1, 5.5,
								  ifelse(.x==2, 17,
								  ifelse(.x==3, 30,
								  ifelse(.x==4, 43,
								  ifelse(.x==5, 56,
								  ifelse(.x==6, 69,
								  ifelse(.x==7, 82,
								  ifelse(.x==8, 94,
								  ifelse(.x==9,100,
								  -99)))))))))))





converted<-coil %>% 
  mutate(across(starts_with('P'),~ifelse(.x==0,   0,
                                  ifelse(.x==1,   25,
								  ifelse(.x==2,   75,
								  ifelse(.x==3,  150,
								  ifelse(.x==4,  350,
								  ifelse(.x==5,  750,
								  ifelse(.x==6, 3000,
								  ifelse(.x==7, 7500,
								  ifelse(.x==8,15000,
								  ifelse(.x==9,30000,
								  -99))))))))))))

head(converted)
table(converted$PWAPAR, coil$PWAPAR)
	
converted <- dummy_cols(converted, select_columns = c('MOSTYP', 'MOSHOO'),remove_selected_columns = TRUE)

#table(converted$MOSHOO_1)
#table(converted$MOSHOO_9)

#converted<-subset(converted,select=-c(sqrMoshoo,difMoshoo))

set.seed(1)
train <- converted %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(converted, train, by = 'SeqNum')

train<-subset(train,select=-c(SeqNum))
test <-subset(test,select=-c(SeqNum))



fullModel = glm(Resp ~ ., family = 'binomial', data = train) # model with all 9 variables
nullModel = glm(Resp ~ 1, family = 'binomial', data = train) # model with the intercept only

interim<-summary(stepAIC(nullModel, # start with a model containing no variables
                direction = 'forward', # run forward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection

coef<-data.frame(interim[['coefficients']])
final<-coef[coef$Pr...z..<0.05,]
print(final)

varnames<-rownames(final)
varnames<-varnames[2:length(varnames)]
finalmodel<-glm(Resp ~ PPERSA+MOPLHO+PWAPAR+MOSHOO_10+MOSTYP_12+MGODPR+MOSTYP_20, family = 'binomial', data = train)
test$LRpred<-predict(finalmodel,newdata=test,type="response")
test<-test[order(-test$pred),]
test$one<-1
test$cumprospects<-cumsum(test$one)
test$cumresp    <-cumsum(test$Resp)

Perf<-subset(test,select=c(pred,cumprospects,cumresp))
Perf$PctProspect<-Perf$cumprospects/nrow(Perf)
Perf$PctResp    <-Perf$cumresp/max(Perf$cumresp)

cutpoint<-subset(Perf,PctProspect>0.745 & PctProspect<0.755)




ForQuiz<-glm(Resp ~ AWAPAR+PPERSA+MOSHOO_10+MGODPR, family = 'binomial', data = train)
summary(ForQuiz)

ForMemo<-lm(Resp ~ PPERSA+MOPLHO+PWAPAR+MOSHOO_10+MOSTYP_12+MGODPR+MOSTYP_20, data = train)

write.csv(Perf,"F:/IDS462/Spring2023Redo/Week10/Perf.csv")

Marginal<-Perf %>% filter(row_number()==88  | row_number()==88*2 | row_number()==88*3 | row_number()==88*4 | row_number()==88*5
| row_number()==88*6 | row_number()==88*7 | row_number()==88*8 | row_number()==88*9 | row_number()==88*10 
| row_number()==88*11| row_number()==88*12| row_number()==88*13| row_number()==88*14| row_number()==88*15
| row_number()==88*16| row_number()==88*17| row_number()==88*18| row_number()==88*19| row_number()==88*20
)

Perf$Lift<-Perf$PctResp-Perf$PctProspect
MaxLift<-Perf[Perf$Lift==max(Perf$Lift),]
MaxLift


library(earth)
library(rpart)
library(randomForest)
library(neuralnet)

rf<-randomForest(Resp~.,data=train)
tree<-rpart     (Resp~.,data=train)
MARS<-earth     (Resp~.,data=train)
NN  <-neuralnet (Resp~.,data=train)

test$NNpred  <-predict(NN,newdata=test)

test$rfpred  <-predict(rf,newdata=test)
test$treepred<-predict(tree,newdata=test)
test$MARSpred<-predict(MARS,newdata=test)

str(test)


test[]<-lapply(test,function(x) {attributes(x)<-NULL;x})
test$one<-1
cumprospects<-data.frame(cumsum(test$one))
colnames(cumprospects)<-"Contacts"
cumprospects$Contacts<-cumprospects$Contacts/1747

test<-test[order(-test$LRpred),]
cumrespLR<-data.frame(cumsum(test$Resp))
colnames(cumrespLR)<-"LR"
test<-test[order(-test$NNpred),]
cumrespNN<-data.frame(cumsum(test$Resp))
colnames(cumrespNN)<-"NN"
#Repeat for all others

Bound<-do.call(cbind, lapply(ls(pattern = "^cum"), get))
Bound$LR<-Bound$LR/105
Bound$NN<-Bound$NN/105
#Repeat for all others
#Use your total sales for 105


write.csv(Bound,"F:/IDS462/Spring2023Redo/Week15/Bound.csv")



library(ggplot2)

ggplot()+
 geom_line(data=Bound,mapping=aes(x=Contacts,y=LR),color='blue')








