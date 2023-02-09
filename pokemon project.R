library(dplyr)
read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Pokemon\\Pokemon.csv")->pokemon
head(pokemon)
pokemon %>% select(-1) -> pokemon

#Renaming columns
colnames(pokemon)[2] <- "Primary_Type"
colnames(pokemon)[3] <- "Secondary_Type"
colnames(pokemon)[5] <- "Health_Points"
colnames(pokemon)[8] <- "Special_Attack"
colnames(pokemon)[9] <- "Special_Defence"


#Looking at the different primary types 
table(pokemon$Primary_Type)

#Selecting Grass pokemon
pokemon %>% filter(Primary_Type=="Grass" & Secondary_Type=="Poison")-> Grass_Poison_Pokemon
range(Grass_Poison_Pokemon$Speed)
Grass_Poison_Pokemon %>% filter(Speed==90) -> My_Grass_Pokemon

#Selecting Water Pokemon
pokemon %>% filter(Primary_Type=="Water" & Secondary_Type=="Flying") -> Water_Flying_Pokemon
range(Water_Flying_Pokemon$Defense)
Water_Flying_Pokemon %>% filter(Defense==100) -> My_Water_Pokemon

#Selecting Fire Fighting Pokemon
pokemon %>% filter(Primary_Type=="Fire") -> Fire_Pokemon
Fire_Pokemon %>% filter(Secondary_Type=="Psychic") -> Fire_Psychic_Pokemon
range(Fire_Psychic_Pokemon$Attack)
Fire_Psychic_Pokemon %>% filter(Attack==69) -> My_Fire_Pokemon


rbind(My_Grass_Pokemon,My_Water_Pokemon,My_Fire_Pokemon) -> My_pokemons
-----------------------------
library(caTools)  

sample.split(pokemon$Attack,SplitRatio = 0.65) -> split_tag
subset(pokemon,split_tag==T) -> train
subset(pokemon,split_tag==F) -> test

#Building the model
lm(Attack~Defense,data=train) -> mod1

#predicting the values
predict(mod1,newdata = test) -> result1

#-----------
cbind(Acutal=test$Attack,Predicted=result1) -> Final_data
as.data.frame(Final_data) -> Final_data

#root mean square error

(Final_data$Acutal-Final_data$Predicted) -> error
cbind(Final_data,error) -> Final_data

sqrt(mean(Final_data$error^2))
#---------------------------------------------------

#classification
as.factor(pokemon$Legendary) -> pokemon$Legendary
sample.split(pokemon$Legendary,SplitRatio = 0.65) -> split_tag
subset(pokemon,split_tag==T) -> train
subset(pokemon,split_tag==F) -> test

library(tree)

tree(Legendary~.,data = train) -> mod1
predict(mod1,newdata = test,type = "class") -> result1
table(test$Legendary,result1)
accuracy = (246+19)/nrow(test) 
