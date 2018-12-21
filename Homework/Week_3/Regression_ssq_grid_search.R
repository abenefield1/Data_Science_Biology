# Least squares algorithms
#General algorithmic idea: vary model parameters until we find parameter values that minimize the distance of the model from the data
# distance (dsubi) = observed -predicted. ssq = sum of distance squared
    #points further away from model are penalized more. Residuals
# Optimization algorithms: 
  #systematicaly try all combos of parameters (grid search algorithms)
  #Narrowing in - keep changing parameters in the direction that leads to lower SSQ - descent algorithms
  #Try random values for parameters: monte carlo algorithms
  #Solve using math: analytical/numerical

#Developing algorithm: pseudocode, 3 phases, top down refinement

#Grid search algorithm: pseudocode
# For each value of betanaught, for each value of betaone, calculate sum of squares

#Three phases:
  #Initialization phase
  #Calculation phase
  #Termination phase

#Top-down refinement:

#In R regressions, uses analytical or numerical algorithms

#Descent algorithms: optim.
  #Not guaranteed to find global optimum (might find a local one)
  #Might not converge (might not get all the way to the optimum
#Biometry, computational biology, population genetics, quantitative something

#obserr
