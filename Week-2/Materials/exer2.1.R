# Introduction to R and RStudio. In this script you should type R code 
# to solve the exercises. The first step is getting familiar with operators 
# in R. 
# 

###############################
# Arithmetic with R

# An addition
5 + 5 

# A subtraction
5 - 5 
# A multiplication
3 * 5

# A division
(5 + 5) / 2 

# Exponentiation
2^5

# Modulo
5%%2
28%%6

#############################
# Variable assignment
# A basic concept in (statistical) programming is called a variable.
# A variable allows you to store a value (e.g. 4) or an object (e.g. a function description) 
# in R. You can then later use this variable's name 
# to easily access the value or the object that is stored within this variable

# Assign the value 42 to x
x <- 42

# Print out the value of the variable x
x

# Suppose you have a fruit basket with five apples. 
# As a data analyst in training, you want to store the number of apples 
# in a variable with the name my_apples.

# Assign the value 5 to the variable my_apples


# Print out the value of the variable my_apples


# Assign a value 6 to a variable my_oranges


# Add these two variables together
my_apples+my_oranges

# Create the variable my_fruit variable 
my_fruit<-my_apples+my_oranges




###########################
# Data type

# Declare variables of different types
my_numeric <- 42
my_character <- "universe"
my_logical <- FALSE 

# Check class of my_numeric
class(my_numeric)

# Check class of my_character
class(my_character)

# Check class of my_logical
class(my_logical)



