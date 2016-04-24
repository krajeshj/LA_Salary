library(dplyr)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)

# For fun. Let's do some eda 

# the data is in LA_Salary directory

LA_Salary <- read.csv("~/Desktop/Coursera/SpringBoardGitbub/LA_Salary/LA_Salary.csv", header=TRUE, sep= ",", stringsAsFactors=FALSE)
#LA_Salary <-read.csv("~/Desktop/Coursera/SpringBoardGitbub/LA_Salary/LA_Salary.csv", header=TRUE, sep= ",", as.is = TRUE)
                    

View(LA_Salary)
data_csv <-LA_Salary
data <- data_csv



#names(data) yields 
# [1] "Year"                         "Department.Title"            
# [3] "Job.Class.Title"              "Employment.Type"             
# [5] "Hourly.or.Event.Rate"         "Projected.Annual.Salary"     
# [7] "Payments.Over.Base.Pay"       "Total.Payments"              
# [9] "Base.Pay"                     "Permanent.Bonus.Pay"         
# [11] "Longevity.Bonus.Pay"          "Temporary.Bonus.Pay"         
# [13] "Lump.Sum.Pay"                 "Overtime.Pay"                
# [15] "Other.Pay...Adjustments"      "Other.Pay..Payroll.Explorer."
# [17] "MOU.Title"                    "Job.Class"                   
# [19] "Average.Health.Cost"          "Average.Dental.Cost"         
# [21] "Average.Basic.Life"           "Average.Benefit.Cost"        
# [23] "Benefits.Plan"                "Job.Class.Link"    
 
#sapply(data, is.numeric)
# Year             Department.Title              Job.Class.Title 
# TRUE                        FALSE                        FALSE 
# Employment.Type         Hourly.or.Event.Rate      Projected.Annual.Salary 
# FALSE                        FALSE                        FALSE 
# Payments.Over.Base.Pay               Total.Payments                     Base.Pay 
# FALSE                        FALSE                        FALSE 
# Permanent.Bonus.Pay          Longevity.Bonus.Pay          Temporary.Bonus.Pay 
# FALSE                        FALSE                        FALSE 
# Lump.Sum.Pay                 Overtime.Pay      Other.Pay...Adjustments 
# FALSE                        FALSE                        FALSE 
# Other.Pay..Payroll.Explorer.                    MOU.Title                    Job.Class 
# FALSE                        FALSE                         TRUE 
# Average.Health.Cost          Average.Dental.Cost           Average.Basic.Life 
# FALSE                        FALSE                        FALSE 
# Average.Benefit.Cost                Benefits.Plan               Job.Class.Link 
# FALSE                        FALSE                        FALSE 

# What is the difference between lapply, sapply, apply 
# apply - When you want to apply a function to the rows or columns of a matrix
#         (and higher-dimensional analogues); not generally advisable for data 
#         frames as it will coerce to a matrix first.
# lapply - When you want to apply a function to each element of a list in 
#          turn and get a list back
# sapply - When you want to apply a function to each element of a list in turn,
#          but you want a vector back, rather than a list.
# Mnemonic: l is for 'list'( c-struct), s is for 'simplifying', t is for 'per type' 
#(each level of the grouping is a type) – Lutz Prechelt Sep 16 '14 at 13:20

# I want to know what  non numeric values are there

non_numeric_vars <- names(data)[!sapply(data, is.numeric)]
# [1] "Department.Title"             "Job.Class.Title"              "Employment.Type"              "Hourly.or.Event.Rate"        
# [5] "Projected.Annual.Salary"      "Payments.Over.Base.Pay"       "Total.Payments"               "Base.Pay"                    
# [9] "Permanent.Bonus.Pay"          "Longevity.Bonus.Pay"          "Temporary.Bonus.Pay"          "Lump.Sum.Pay"                
# [13] "Overtime.Pay"                 "Other.Pay...Adjustments"      "Other.Pay..Payroll.Explorer." "MOU.Title"                   
# [17] "Average.Health.Cost"          "Average.Dental.Cost"          "Average.Basic.Life"           "Average.Benefit.Cost"        
# [21] "Benefits.Plan"                "Job.Class.Link"              

# For the non-numeric vars (like `Notes`) I'd like to know:
# 
# * How many unique vablues are there?
# * What's the relative frequency?
data %>% 
  select(one_of(non_numeric_vars)) %>%
  summarise_each(funs(unique_vars = length(unique(.))))

#length(unique(data))
#[1] 24

# 

# Department.Title Job.Class.Title Employment.Type Hourly.or.Event.Rate Projected.Annual.Salary Payments.Over.Base.Pay Total.Payments
# 1                9               8               1                   30                      30                    173            181
# Base.Pay Permanent.Bonus.Pay Longevity.Bonus.Pay Temporary.Bonus.Pay Lump.Sum.Pay Overtime.Pay Other.Pay...Adjustments
# 1       93                  34                   1                  18           27          131                      67
# Other.Pay..Payroll.Explorer. MOU.Title Average.Health.Cost Average.Dental.Cost Average.Basic.Life Average.Benefit.Cost Benefits.Plan
# 1                          121         4                   2                   2                  2                    2             2
# Job.Class.Link
# 1              3
# These numbers seem to be the same as factors

# Total Payments seem to be 188 spread over 197 emplyees : Looks oK
# Payments Over Vase pay  is 173 : that is also OK
# However,  Overtime Pay is 131  : over 131 employees...
# Most People are making money on Overtime then ?
head(data$Overtime.Pay)

#data looks like $NN,NNN.NN
# Scrub the data using gsub applied over the Column
data$Overtime.Pay<-sapply(data$Overtime.Pay,function(x) {gsub("\\$","", as.character(x))})
data$Overtime.Pay<-sapply(data$Overtime.Pay,function(x) {gsub(",","", as.character(x))})

# Coerce the factor into numeric
data$Overtime.Pay<-sapply(data$Overtime.Pay,function(x) as.numeric(x))

min(data$Overtime.Pay)
max(data$Overtime.Pay)
summary(data$Overtime.Pay)
#  How much do most people make in overtime
# Median is 6K
#Max - someone made 88K
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0    6665   15840   23540   88410 

g<-ggplot(LA_Salary)
g + aes(LA_Salary$Department.Title, LA_Salary$Total.Payments, color=LA_Salary$Job.Class.Title, alpha= 0.2) + geom_point()
