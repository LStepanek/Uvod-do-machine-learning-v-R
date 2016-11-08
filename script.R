###############################################################################
###############################################################################
###############################################################################

## loaduju potřebné balíčky ---------------------------------------------------

for(my_package in c("openxlsx", "e1071", "tree", "neuralnet", "RCurl")){
    
    if(!my_package %in% rownames(installed.packages()){
        install.packages(my_package, dependencies = TRUE)
    }
    
    library(my_package)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

setwd(choose.dir())


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

my_data <- read.csv(
    paste(
       "https://raw.githubusercontent.com/LStepanek",
       "Uvod-do-machine-learning-v-R/master/my_data.csv",
       sep = "/"
       )
    )


## ----------------------------------------------------------------------------

###############################################################################

## preprocessing --------------------------------------------------------------

for(i in 1:9){
    my_data[, i] <- as.factor(as.character(my_data[, i]))
}

for(i in 10:dim(my_data)[2]){
    my_data[, i] <- as.numeric(as.character(my_data[, i]))
}


## ----------------------------------------------------------------------------

###############################################################################

## helper funkce --------------------------------------------------------------

getMyAccuracy <- function(my_table){
    # '''
    # vrací přesnost pro konfuzní matici "my_table"
    # '''
    return(sum(diag(my_table))/sum(my_table))
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## naivní Bayesův klasifikátor ------------------------------------------------

###############################################################################

## nejdříve rozděluji data na trénovací (28 probandů) a testovací (8 probandů)
## množinu --------------------------------------------------------------------

set.seed(2016)

train_set_indices <- sample(levels(my_data$id), 28, replace = FALSE)

train_set <- my_data[my_data$id %in% train_set_indices, ]
test_set <- my_data[!my_data$id %in% train_set_indices, ]


## ----------------------------------------------------------------------------

## počítám naivní bayesovskou klasifikaci na trénovací množině ----------------

my_bayes <- naiveBayes(smer ~ ., data = train_set[, c(6, 10:21)])


## predikuji směr emoce nad testovací množinou --------------------------------

predict(my_bayes, test_set[, c(6,10:21)])
predict(my_bayes, test_set,type="raw")


## confusion matrix (tabulka přesnosti) ---------------------------------------

table(
    test_set$smer,
    predict(my_bayes, test_set)
)


## přesnost predikce ----------------------------------------------------------

getMyAccuracy(table(test_set$smer, predict(my_bayes, test_set)))


## ----------------------------------------------------------------------------

###############################################################################

## automatizuji předchozí proces ----------------------------------------------

for(i in c(6, 7, 8, 9, 22, 2, 4)){

formula = paste(
    "my_bayes <- naiveBayes(",
    colnames(train_set)[i],
    " ~ ., ",
    "data = train_set[, c(i, 10:21)])",
    sep = ""
)

eval(parse(text = formula))

print(colnames(train_set)[i])

## confusion matrix (tabulka přesnosti) ---------------------------------------

print(
    table(test_set[, i], predict(my_bayes, test_set))
)


## accuracy

print(
    getMyAccuracy(table(test_set[, i], predict(my_bayes, test_set)))
)


for(j in 1:3){print("")}

}


## ----------------------------------------------------------------------------

###############################################################################

## zkouším rozhodovací stromy -------------------------------------------------

my_tree <- tree(smer~., data = train_set[,c(6, 10:21)])

summary(my_tree)
plot(my_tree)
text(my_tree)

## konfuzní matice a přesnost -------------------------------------------------

predict(object = my_tree, newdata = test_set, type = "class")

table(
    test_set$smer,
    predict(object = my_tree, newdata = test_set, type = "class")
)

getMyAccuracy(
              table(test_set$smer, predict(my_tree, test_set, type = "class"))
              )


## prořezávám strom -----------------------------------------------------------

my_pruned_tree <- prune.misclass(my_tree, best = 7)


## přesnost -------------------------------------------------------------------

getMyAccuracy(
    table(
          test_set$smer,
          predict(my_pruned_tree, test_set, type = "class")
         )
    )


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## zkouším neuronovou síť -----------------------------------------------------

###############################################################################

## bootstrapem generuji 95 % konfidenční intervaly pro přesnosti --------------

my_hidden_layers<-c(
"emoce"=3,
"kategorie"=3,
"smer"=3,
"intenzita"=3,
"kvalita"=3,
"instinkt"=1,
"atraktivita"=1
)


list_of_accuracies<-NULL
number_of_repetitions<-10

for(i in c(14,15,17:21)){

my_accuracies<-NULL

for(k in 1:number_of_repetitions){

flush.console()
print(
paste(
colnames(train_set)[i],
": ",
format(round(k/number_of_repetitions*100,1),nsmall=1),
" %",
sep="")
)

set.seed(k)

train_set_indices<-sample(levels(compact_data$probandka),9,replace=FALSE)

train_set<-compact_data[compact_data$probandka%in%train_set_indices,]
test_set<-compact_data[!compact_data$probandka%in%train_set_indices,]

## ----------------------------------------------------------------------------

for(my_level in levels(compact_data[,colnames(compact_data)[i]])){

for(item in c("train_set","test_set")){

data<-get(item)
my_new_variable<-NULL

for(j in 1:length(data[,colnames(compact_data)[i]])){
if(data[j,colnames(compact_data)[i]]==my_level){
my_new_variable<-c(my_new_variable,1)
}else{
my_new_variable<-c(my_new_variable,0)
}
}

data<-cbind(
data,
my_new_variable
)

colnames(data)[dim(data)[2]]<-my_level

assign(item,data)

}

}


my_formula<-paste(
paste(levels(compact_data[,colnames(compact_data)[i]]),collapse="+"),
"~",
paste(colnames(compact_data)[c(1:13)],collapse="+"),
sep=""
)

my_net<-neuralnet(
my_formula,data=train_set,
hidden=my_hidden_layers[colnames(compact_data)[i]],
lifesign="minimal",
linear.output=FALSE,threshold=0.1
)

my_prediction<-compute(my_net,test_set[,c(1:13)])

predicted<-test_set[,colnames(compact_data)[i]]

for(j in 1:length(predicted)){
predicted[j]<-levels(compact_data[,colnames(compact_data)[i]])[
apply(my_prediction$net.result,1,which.max)
][j]
}


table(
test_set[,colnames(compact_data)[i]],
predicted
)

my_accuracies<-c(my_accuracies,
getMyAccuracy(
table(
test_set[,colnames(compact_data)[i]],
predicted
)
)
)

}

list_of_accuracies<-c(list_of_accuracies,
getMyCI(my_accuracies,digits=3)
)

names(list_of_accuracies)[
length(list_of_accuracies)
]<-colnames(train_set)[i]

}


## ----------------------------------------------------------------------------

addWorksheet(
wb=net_accuracies<-createWorkbook(),
sheetName="95 % CI přesnosti"
)

writeData(
wb=net_accuracies,
sheet="95 % CI přesnosti",
rowNames=TRUE,
colNames=FALSE,
x=as.data.frame(list_of_accuracies)
)

setwd(paste(mother_working_directory,"vystupy",sep="/"))

saveWorkbook(
wb=net_accuracies,
file="net_accuracies.xlsx",
overwrite=TRUE
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## ----------------------------------------------------------------------------

my_hidden_layers<-c(
"emoce"=3,
"kategorie"=3,
"smer"=3,
"intenzita"=3,
"kvalita"=3,
"instinkt"=1,
"atraktivita"=1
)


setwd(paste(mother_working_directory,"vystupy",sep="/"))

for(i in c(14,15,17:21)){

set.seed(1)

train_set_indices<-sample(levels(compact_data$probandka),9,replace=FALSE)

train_set<-compact_data[compact_data$probandka%in%train_set_indices,c(1:13,i)]
test_set<-compact_data[!compact_data$probandka%in%train_set_indices,c(1:13,i)]

## ----------------------------------------------------------------------------

for(my_level in levels(compact_data[,colnames(compact_data)[i]])){

for(item in c("train_set","test_set")){

data<-get(item)
my_new_variable<-NULL

for(j in 1:length(data[,colnames(compact_data)[i]])){
if(data[j,colnames(compact_data)[i]]==my_level){
my_new_variable<-c(my_new_variable,1)
}else{
my_new_variable<-c(my_new_variable,0)
}
}

data<-cbind(
data,
my_new_variable
)

colnames(data)[dim(data)[2]]<-my_level

assign(item,data)

}

}


my_formula<-paste(
paste(levels(compact_data[,colnames(compact_data)[i]]),collapse="+"),
"~",
paste(colnames(compact_data)[c(1:13)],collapse="+"),
sep=""
)

my_net<-neuralnet(
my_formula,data=train_set,
hidden=my_hidden_layers[colnames(compact_data)[i]],
lifesign="minimal",
linear.output=FALSE,threshold=0.1
)

my_net$model.list$variables<-as.character(my_net$model.list$variables)

for(j in 1:length(my_net$model.list$response)){
if(my_net$model.list$response[j]%in%prevodnik$jmeno){
my_net$model.list$response[j]<-prevodnik$jmeno_cesky[
which(prevodnik$jmeno==my_net$model.list$response[j])
]
}
}

my_net$model.list$variables<-as.character(my_net$model.list$variables)

for(j in 1:length(my_net$model.list$variables)){
if(my_net$model.list$variable[j]%in%prevodnik$jmeno){
my_net$model.list$variables[j]<-prevodnik$jmeno_cesky[
which(prevodnik$jmeno==my_net$model.list$variables[j])
]
}
}

jpeg(
filename=paste(colnames(compact_data)[i],"_nnet.jpg",sep=""),
width=8,height=if(i==14){10}else{5},units="in",res=600
)

par(mar=c(1,0,1,0))

plot(my_net,rep="best",information=FALSE,fontsize=10)

dev.off()

}


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím konfuzní matice ---------------------------------------------------

my_hidden_layers<-c(
"emoce"=3,
"kategorie"=3,
"smer"=3,
"intenzita"=3,
"kvalita"=3,
"instinkt"=1,
"atraktivita"=1
)

nets<-createWorkbook()

for(i in c(14,15,17:21)){

set.seed(2016)

train_set_indices<-sample(levels(compact_data$probandka),9,replace=FALSE)

train_set<-compact_data[compact_data$probandka%in%train_set_indices,]
test_set<-compact_data[!compact_data$probandka%in%train_set_indices,]

## ----------------------------------------------------------------------------

for(my_level in levels(compact_data[,colnames(compact_data)[i]])){

for(item in c("train_set","test_set")){

data<-get(item)
my_new_variable<-NULL

for(j in 1:length(data[,colnames(compact_data)[i]])){
if(data[j,colnames(compact_data)[i]]==my_level){
my_new_variable<-c(my_new_variable,1)
}else{
my_new_variable<-c(my_new_variable,0)
}
}

data<-cbind(
data,
my_new_variable
)

colnames(data)[dim(data)[2]]<-my_level

assign(item,data)

}

}


my_formula<-paste(
paste(levels(compact_data[,colnames(compact_data)[i]]),collapse="+"),
"~",
paste(colnames(compact_data)[c(1:13)],collapse="+"),
sep=""
)

my_net<-neuralnet(
my_formula,data=train_set,
hidden=3,
#my_hidden_layers[colnames(compact_data)[i]],
lifesign="minimal",
linear.output=FALSE,threshold=0.1
)

my_prediction<-compute(my_net,test_set[,c(1:13)])

predicted<-test_set[,colnames(compact_data)[i]]

for(j in 1:length(predicted)){
predicted[j]<-levels(compact_data[,colnames(compact_data)[i]])[
apply(my_prediction$net.result,1,which.max)
][j]
}


addWorksheet(
wb=nets,
sheetName=colnames(train_set)[i]
)

writeData(
wb=nets,
sheet=colnames(train_set)[i],
rowNames=TRUE,
colNames=TRUE,
x=table(test_set[,colnames(compact_data)[i]],predicted)
)

}


setwd(paste(mother_working_directory,"vystupy",sep="/"))

saveWorkbook(
wb=nets,
file="nets.xlsx",
overwrite=TRUE
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






