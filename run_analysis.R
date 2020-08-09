library(tidyverse)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","assignment_data.zip")

## manually unzip

activity_labels<- read_table("//UCI HAR Dataset//activity_labels.txt")
  