# GA

# Why use GA? 

The premise of GA is simple: Allows you to use evolutive theory by apply genetic algorithm's in classification datasets with the 
objetive of select the optimal variables combination accordinly to a fitness function. 


# Installation

To get the current development version from github:

```R
devtools::install_github("anvelezec/GA")
```

# Running

```R
require(GA)
#####################

# Target variable for training
y = dataBalanced$train$target

# Create training feature data frame
trainData = dataBalanced$train[,-1] 

#####################
resulTime = system.time(
  
  resultRunGA <- runGA(Nruns = 1,N = 2,NChrSel = 2,
                       pMutation = 0.05,pCrossover = 0.8,
                       yTrain = y,train = trainData,
                       yTest = y,test = trainData)
)

result = list(resulTime = resulTime,
              resultRunGA = resultRunGA)
              
```
