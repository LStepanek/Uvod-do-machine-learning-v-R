###############################################################################
###############################################################################
###############################################################################

## loaduju potřebné balíčky ---------------------------------------------------

for(my_package in c("openxlsx", "e1071", "tree", "neuralnet", "RCurl")){
    
    if(!my_package %in% rownames(installed.packages())){
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

my_tree <- tree(smer ~ ., data = train_set[, c(6, 10:21)])

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

my_pruned_tree <- prune.misclass(my_tree, best = 5)


## přesnost -------------------------------------------------------------------

getMyAccuracy(
    table(
          test_set$smer,
          predict(my_pruned_tree, test_set, type = "class")
         )
    )


## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






