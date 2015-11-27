# FORTS-ACO ant colony optimisation algorithm

An implementation of a multi-objective ant colony optimisation (ACO) algorithm in FORTRAN as part of a broader framework (FORTS) to allow for the exploration of the impact of shocks on a global trade network. The ant colony optimisation algorithm also has the following custom operators:

•	-DDV (Dynamic Decision Variable)

•	-RSO (Random Shuffling Operator)

•	-TAU (Distribution of Initial Pheromone Operator)

The code was developed at the University of Adelaide by Jonathan Schulz, Peter Golding, Sam Kapadia and Stella Naylor specifically for the application to the global trade problem. 

###General Guidelines

The main FORTRAN algorithm is available in the folder PACO_DYN_CONST. Two other algorithms are also available (Pareto Front Sort and Pareto Point Storage Evaluation) to assist the user in extracting detailed information from the results. 
A number of other files are required to ensure appropriate operation. A short overview of each file and its requirements is presented below:

*ACO_input*	Contains user selected ant colony optimisation parameters as well as parameters that affect the algorithm file sizes

*FORTS_input*	Contains information about the resolution of data to be used and the period length of simulation among others

*importing country codes*	Contains the list and codes of importing countries

*exporting country codes*	Contains the list and codes of importing countries

*trade_links*	Contains the country codes of current trade links and current trade quantities (used to calculate deviation from current trade)

*PDMSR_input*	Contains the deficit/surplus of commodity for each country in the same order as the country_index list (used to calculate global deficit)
