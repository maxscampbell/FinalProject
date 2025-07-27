#Load in required package(s)
library(plumber)
library(tidyverse)
library(tidymodels)

#Read in data from the diabetes dataset from EDA.qmd
raw_data <- read.csv("diabetes.csv", header = TRUE)

diabetes <- raw_data |>
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("Low", "High")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("Low", "High")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c(0, 1), labels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c(0, 1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c(0, 1), labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0, 1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c(1:5)),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Age = factor(Age, levels = c(1:13), labels = c(
      "18-24", "25-29", "30-34", "35-39", "40-44",
      "45-49", "50-54", "55-59", "60-64", "65-69",
      "70-74", "75-79", "80<="
    )),
    Education = factor(Education, levels = c(1:6), labels = c(
      "Kindergarten", "Grade 8", "Grade 11",
      "GED", "Some College", "College 4+ years"
    )),
    Income = factor(Income, levels = c(1:8), labels = c(
      "< $10k", "< $15k", "< $20k", "< $25k",
      "< 35k", "< $50k", "< $75k", "More than $75k"
    )))

#Fit the best model from Modeling.qmd
#Logistic 3
log_rec3 <- recipe(Diabetes_binary ~ HighBP + HighChol + CholCheck + BMI +
                     Smoker + Stroke + HeartDiseaseorAttack + PhysActivity +
                     Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare +
                     NoDocbcCost + GenHlth + MentHlth + PhysHlth +
                     DiffWalk + Sex + Age + Education + Income,
                   data = diabetes) |>
  step_normalize(BMI, MentHlth, PhysHlth) |>
  step_dummy(HighBP, HighChol, CholCheck, Smoker, Stroke,
             HeartDiseaseorAttack, PhysActivity, Fruits, Veggies,
             HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, GenHlth,
             DiffWalk, Sex, Age, Education, Income)

log_spec <- logistic_reg() |>
  set_engine("glm")

log_work3 <- workflow() |>
  add_recipe(log_rec3) |>
  add_model(log_spec)

model <- log_work3 |>
  fit(diabetes)

#API: Set up two endpoints

#*Output basic information related to this project
#* @get /info
function() {
  "Diabetes Health Indicators Final Project by Max Campbell. Website URL: https://maxscampbell.github.io/FinalProject"
}

#EXAMPLE:
#http://localhost:8000/info

#*Takes in a predictor used in final model and returns the mean (if numeric) or most prevalent (if factor) result.
#* @param var The variable(s) to be analyzed, separated by a semi-colon (;)
#* @param input The input(s) corresponding to the variables, separated by a semi-colon (;)
#* @get /pred
function(var, input) {
  #Split values by individual arguments
  vars <- strsplit(var, split = ";")
  inputs <- strsplit(input, split = ";")
  
  #Create empty df
  values <- diabetes[FALSE,]
  
  #Sanity check: same number of inputs as vars
  if (length(vars[[1]]) == length(inputs[[1]])) {
    inputs <- data.frame(t(inputs))
    colnames(inputs) <- vars
    
    #Load columns
    for (i in seq_along(diabetes)) {
      
      if (i == 1) {
        #Do nothing
      } else {
        
        if (is.factor(diabetes[,i])) {
          values[1,i] <- names(sort(table(diabetes[,i]),decreasing = TRUE)[1])
        } else if (is.numeric(diabetes[,i])) {
          values[1,i] <- mean(diabetes[,i])
        }
        
      }
    }
    
    colnames(values) <- colnames(diabetes)
    
    
    #Overwrite defaults with user inputs
    for (i in seq_along(inputs)) {
      
      for (j in seq_along(values)) {
        
        if (colnames(inputs)[i] == colnames(values)[j]) {
          if (is.factor(values[,j])) {
            values[1,j] <- as.factor(inputs[1,i])
          } else {
            values[1,j] <- as.numeric(inputs[1,i])
          }
          
        }
        
      }
      
    }
    
    #Predict outcome using final model
    pred <- predict(model, new_data = values)
    pred <- names(sort(table(pred), decreasing = TRUE))[1] #Converts most prevalent factor (the prediction) into a string for readability
    
    
    
    #Print output!
    paste("Diabetes Health Indicators Model Prediction:", pred)
    
  } else {
    "Error! Make sure predictors are valid and there is the same number of inputs."
  }
  
}

#EXAMPLES:

#Returns prediction based on one value
#http://localhost:8000/pred?var=BMI?input=25

#Returns prediction based on multiple values
#http://localhost:8000/pred?var=BMI;Age;HighBP?input=25;18-24;Low

#Returns a string error
#http://localhost:8000/pred?var=BMI
