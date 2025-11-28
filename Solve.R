# the one that solves the problem
Simplex <- function(tableau, isMax = TRUE){
  iterTables <- list()
  iterCount <- 1
  
  while (TRUE) {
    # stores initial tableau
    iterTables[[iterCount]] <- list( # because r doesnt do 0 indexing
      tableau = tableau, 
      basicSol = double(ncol(tableau)), 
      pivotRow = NA, 
      pivotCol = NA
    )
    
    # checks for negative values in the last row
    # we take the last row only, not the column since the last column is the solution
    # if true ang sagot, we loop until false sagot
    lastRow <- tableau[nrow(tableau), -ncol(tableau)]
    
    # this stores the largest negative number
    negativeNum <- 0 
    
    # this stores the pivots column
    pivotCol <- 0
    
    # this stores the pivot row
    pivotRow <- 0
    
    # this stores the smallestRatio to compare with the current Ratio
    smallestRatio <- Inf # starts with infinite so ma change agad (since malaki siya na number)
    
    # STEP 1: FIND PIVOT COLUMN
    # find most negative value and its position
    # loops through the all values in the last row and updates if its smaller
    for (i in 1:length(lastRow)) {
      if (lastRow[i] < negativeNum) {
        negativeNum <- lastRow[i]
        pivotCol <- i
      }
    }
    
    # if no negative number found we stop, meaning we found the optimal value
    if (negativeNum >= 0) {
      break
    }
    
    # STEP 2: FIND PIVOT ROW
    # using pivot column, we'll find pivot row
    # we loop through all values in pivotCol
    for (i in 1:(nrow(tableau) -1)) {
      if (tableau[i, pivotCol] > 0){ # divide only if positive
        currentRatio <- tableau[i, ncol(tableau)] / tableau[i, pivotCol]
        
        # compares current ratio to smallest, updates if their is a ratio much smaller
        if (currentRatio < smallestRatio){
          smallestRatio <- currentRatio
          pivotRow <- i
        }
      }
      
    }
    
    # check if the smallest ratio is Inf → infeasible
    if (is.infinite(smallestRatio)) {
      
      # save the final tableau BEFORE stopping
      iterTables[[length(iterTables) + 1]] <- list(
        tableau = tableau,
        pivotRow = NA,
        pivotCol = pivotCol  # or NA if you prefer
      )
      
      return(list(
        status = "INFEASIBLE",
        message = "Problem is unbounded.",
        iterTables = iterTables
      ))
    }
    
    
    # store pivot information
    iterTables[[iterCount]]$pivotRow <- pivotRow
    iterTables[[iterCount]]$pivotCol <- pivotCol
    
    # STEP 3: PIVOT OPERATION
    # we now make a new tableau!
    # PR/PE on the pivot element then Row - (NormRow * Element in PivotCol in Row u want to elim (basta ung C))
    pivotElement = tableau[pivotRow, pivotCol]
    tableau[pivotRow, ] = tableau[pivotRow, ] / pivotElement
    
    # loop through each row in the matrix EXCEPT THE PIVOT ROW and change values
    for (i in 1:nrow(tableau)) {
      if (i != pivotRow){
        c <- tableau[i, pivotCol]
        tableau[i, ] <- tableau[i, ] - (c * tableau[pivotRow, ])
      }
    }
    # then this will loop again and again until no more yey
    iterCount <- iterCount + 1
  }
  
  # this is for the Basic Solution
  basicSol <- double(ncol(tableau))  # make a vector to store results, no -1 since we save it for the solution
  
  for (j in 1:(ncol(tableau) - 1)) {
    col <- tableau[1:(nrow(tableau)-1), j]  # take the column without Z row
    
    # if column has one 1 and the rest 0s, it is basic and we get the solution part
    if (sum(col == 1) == 1 && sum(col == 0) == length(col) - 1) {
      rowIndex <- which(col == 1)            # find where the 1 is
      basicSol[j] <- tableau[rowIndex, ncol(tableau)]
    } else {
      basicSol[j] <- 0
    }
  }
  
  # adds Z at the reseverd space
  basicSol[ncol(tableau)] <- tableau[nrow(tableau), ncol(tableau)]
  
  iterCount <- iterCount + 1
  iterTables[[iterCount]] <- list(
    tableau = tableau, 
    basicSol = basicSol, 
    pivotRow = pivotRow, 
    pivotCol = pivotCol
  )
  
  return(list(status ="OPTIMAL", finalTableau = tableau, basicSolution = basicSol, Z = tableau[nrow(tableau), ncol(tableau)], iterTables = iterTables))
  
}

# project data
Projects <- data.frame(
  number = 1:30, 
  name = c("Large Solar Park", "Small Solar Installations", "Wind Farm", "Gas-to-renewables conversion", "Boiler Retrofit",
           "Catalytic Converters for Buses", "Diesel Bus Replacement", "Traffic Signal/Flow Upgrade", "Low-Emission Stove Program", "Residential Insulation/Efficiency",
           "Industrial Scrubbers", "Waste Methane Capture System", "Landfill Gas-to-energy", "Reforestation (acre-package)", "Urban Tree Canopy Program (street trees)", 
           "Industrial Energy Efficiency Retrofit", "Natural Gas Leak Repair", "Agricultural Methane Reduction", "Clean Cookstove & Fuel Switching (community scale)", "Rail Electrification",
           "EV Charging Infrastructure", "Biochar for soils (per project unit)", "Industrial VOC", "Heavy-Duty Truck Retrofit", "Port/Harbor Electrification",
           "Black Carbon reduction", "Wetlands restoration", "Household LPG conversion program", "Industrial process change", "Behavioral demand-reduction program"),
  cost = c(4000, 1200, 3800, 3200, 1400, 2600, 5000, 1000, 180, 900, 4200, 3600, 3400, 220, 300, 1600, 1800, 2800, 450, 6000, 2200, 1400, 2600, 4200, 4800, 600, 1800, 700, 5000, 400),
  co2 = c(60, 18, 55, 25, 20, 30, 48, 12, 2, 15, 6, 28, 24, 3.5, 4.2, 22, 10, 8, 3.2, 80, 20, 6, 2, 36, 28, 1.8, 10, 2.5, 3, 9),
  nox = c(0, 0, 0, 1, 0.9, 2.8, 3.2, 0.6, 0.02, 0.1, 0.4, 0.2, 0.15, 0.04, 0.06, 0.5, 0.05, 0.02, 0.04, 2, 0.3, 0.01, 0.01, 2.2, 1.9, 0.02, 0.03, 0.03, 0.02, 0.4),
  so2 = c(0, 0, 0, 0.2, 0.4, 0.6, 0.9, 0.1, 0.01, 0.05, 6, 0.1, 0.05, 0.02, 0.01, 0.3, 0.01, 0.01, 0.02, 0.4, 0.05, 0, 0, 0.6, 0.8, 0.01, 0.02, 0.01, 0.01, 0.05),
  pm2.5 = c(0, 0, 0, 0.1, 0.2, 0.8, 1, 0.4, 0.7, 0.05, 0.4, 0.05, 0.03, 0.01, 0.03, 0.15, 0.01, 0.02, 0.9, 1.2, 0.1, 0.01, 0, 0.6, 0.7, 0.6, 0.02, 0.4, 0, 0.05),
  ch4 = c(0, 0, 0, 1.5, 0.1, 0, 0, 0.05, 0, 0.02, 0, 8, 6.5, 0.8, 0.6, 0.2, 4, 7.2, 0.1, 0, 0, 2.5, 0, 0, 0, 0.05, 3.2, 0.05, 0, 0.01),
  voc = c(0, 0, 0, 0.5, 0.05, 0.5, 0.7, 0.2, 0.01, 0.02, 0.1, 0.2, 0.1, 0.03, 0.02, 0.1, 0.02, 0.05, 0.02, 0.6, 0.05, 0.01, 6.5, 0.3, 0.2, 0.01, 0.01, 0.02, 0, 0.3),
  co = c(0, 0, 0, 2, 1.2, 5, 6, 3, 1.5, 0.5, 0.6, 0.1, 0.05, 0.1, 0.15, 1, 0.02, 0.02, 2, 10, 0.5, 0.01, 0.1, 4.2, 3.6, 1, 0.05, 1.2, 0, 2.5),
  nh3 = c(0, 0, 0, 0.05, 0.02, 0.01, 0.02, 0.02, 0.03, 0, 0.01, 0, 0, 0.01, 0.005, 0.01, 0, 0.1, 0.05, 0.02, 0.01, 0.2, 0, 0.01, 0.01, 0.02, 0.15, 0.03, 0, 0.01),
  bc = c(0, 0, 0, 0.01, 0.01, 0.05, 0.08, 0.02, 0.2, 0, 0.01, 0, 0, 0.005, 0.02, 0.01, 0, 0, 0.25, 0.1, 0.01, 0, 0, 0.04, 0.03, 0.9, 0.02, 0.1, 0, 0.01),
  n2o = c(0, 0, 0, 0.3, 0.05, 0.02, 0.03, 0.01, 0, 0.01, 0, 0.05, 0.03, 0.005, 0.002, 0.03, 0.01, 0.05, 0, 0.05, 0.01, 0.02, 0, 0.02, 0.02, 0, 0.04, 0, 1.5, 0.01)
)

buildTableau <- function(selectedProjects, projectData, target){
  
  # filters the selected projects and store into a var 
  finalProjects = projectData[selectedProjects, ]
  numOfProjects <- nrow(finalProjects) # counts num of chosen projects
  numOfPollutants <- 10 # always set to 10 as per pdf
  
  # build primal constraint matrix
  numConstraints <- numOfPollutants + numOfProjects
  A <- matrix(0, nrow = numConstraints, ncol = numOfProjects)
  b <- double(numConstraints)
  
  pollutantCols <- c("co2", "nox", "so2", "pm2.5", "ch4", 
                     "voc", "co", "nh3", "bc", "n2o")
  
  # constraints
  for (i in 1:numOfPollutants) {
    A[i, ] <- finalProjects[[pollutantCols[i]]] # contains selected projects emissions for that pollutant
    b[i] <- targets[i] # target reduction for the pollutant
  }
  
  # sets RHS to -20 and fills diagonals with -1 (bounds)
  for(i in 11:(numOfProjects+10)){
    A[i,(i-10)] = -1
    b[i] = -20
  }
  
  # adds the RHS to A as last column and print
  A = cbind(A,b)
  print(A)
  
  # gets project costs
  # appends to last row of A which will become the objective func
  c <- finalProjects$cost
  proj_costs = c(c, 0)
  A = rbind(A, proj_costs)
  print(A)
  
  # transpose tableau for dual form
  # negates last row for simplex form
  A_t <- t(A)
  print(A_t)
  A_t[nrow(A_t),] = A_t[nrow(A_t),] * -1
  
  # creates slack var identity mat for dual simplex
  slack = c()
  for(i in 1:nrow(A_t)){
    for(j in 1:nrow(A_t)){
      if(i==j){
        slack = c(slack, 1)
      }
      else {
        slack = c(slack, 0)
      }
    }
  }
  
  slackk = matrix(data=slack, byrow=TRUE, ncol=numOfProjects+1)
  print(slackk)
  
  # add slack to tableau
  A_t = cbind(A_t[,1:(10+numOfProjects)], slackk, A_t[,ncol(A_t)])
  print(A_t)
  
  return(list(tableau = A_t, projectNames = finalProjects$name, projectNumbers = finalProjects$number, numOfProjects = numOfProjects))
}

# constraint value
targets <- c(
  co2 = 1000,
  nox = 35,
  so2 = 25,
  pm2.5 = 20,
  ch4 = 60,
  voc = 45,
  co = 80,
  nh3 = 12,
  bc = 6,
  n2o = 10
)