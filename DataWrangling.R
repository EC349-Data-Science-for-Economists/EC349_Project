# Import the relavant packages
library(jsonlite)

# Clear the R Workspace
cat("\014")  
rm(list=ls())

# Set Working Directory
setwd("D:/Documents/EC349_Project")

# Import the Datasets
user_data <- stream_in(file("Data/yelp_academic_dataset_user.json"))

# Save the Workspace file
save.image()

# Export Data to R Data File
saveRDS(user_data, file = "user_data.rds")
