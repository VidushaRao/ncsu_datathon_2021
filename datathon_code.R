## libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(Rtsne)
library(cluster)

## read in data
data = read.csv(file = "C:\\Users\\Vidusha\\Documents\\MSA\\ncsu_datathon\\Raleigh_Police_Incidents.csv", sep = ',')

#######################
## Exploratory Steps ##
#######################

## crime category breakdown
table(data$crime_category)

## crime type breakdown
table(data$crime_type)

## crime type by year, exclude 2021 (not a full year)
crime.type.year = data %>%
  filter(reported_year != 2021) %>%
  group_by(crime_type, reported_year) %>%
  summarize(count = n())


#########################################
## create line graph with the 4 groups ##
#########################################

## label blanks as unspecified
crime.type.year$crime_type = ifelse(crime.type.year$crime_type == '', 'UNSPECIFIED', crime.type.year$crime_type)

ggplot(data = crime.type.year, aes(x = reported_year, y = count, group = crime_type, color = crime_type)) +
  geom_line(size = 2) +
  labs(x = 'Year', y = 'Number of Reported Crimes') +
  ggtitle('Change in Crime Types over 6 Years')
  
###############################################
## Time of day and crime category/crime type ##
###############################################

## create time groups
data$time_group = 0
data$time_group = ifelse(data$reported_hour > 18, 4,
                         ifelse(data$reported_hour > 12, 3,
                                ifelse(data$reported_hour > 6, 2,
                                       ifelse(data$reported_hour >= 0, 1, 0))))

crime.time = data %>%
  group_by(crime_type, time_group) %>%
  summarise(count = n())
crime.time$crime_type = ifelse(crime.time$crime_type == '', 'UNSPECIFIED', crime.time$crime_type)

## create visual
ggplot(data = crime.time, aes(x = time_group, y = count, group = crime_type, color = crime_type)) +
  geom_line(size = 2) +
  xlab('Time of Day (Hours)') +
  ylab('Number of Reported Crimes') +
  ggtitle('Change in Crime Types over Time of Day')

## write updated data to csv for tableau
write.csv(data, file = "C:\\Users\\Vidusha\\Documents\\MSA\\ncsu_datathon\\RPI_Updated.csv")

################
## Clustering ##
################

## create season variable
data$season = 0
data$season = ifelse(data$reported_month %in% c(12,1,2), 'Winter',
                         ifelse(data$reported_month %in% c(3,4,5), 'Spring',
                                ifelse(data$reported_month %in% c(6,7,8), 'Summer',
                                       ifelse(data$reported_month %in% c(9,10,11), 'Fall', 'Unkown'))))


## create dataset for variables that go into clustering
cluster.data = data[,c(8,9,13,18,19,25)]

#impute blank cells with missing
cluster.data$crime_type = ifelse(cluster.data$crime_type == '', 'Missing', cluster.data$crime_type)

## convert data frame so all columns are factors
for (i in 1:length(cluster.data))
{
  cluster.data[,i] = as.factor(cluster.data[,i])
}
str(cluster.data)

## fill in missing values

## take small random sample
set.seed(123)
sample.size = 10000
obs = sample(1:nrow(cluster.data), sample.size, replace = FALSE)
cluster.sample = cluster.data[obs,]

# Compute Gower distance
gower_dist = daisy(cluster.sample, metric = "gower")
gower_mat = as.matrix(gower_dist)

## find ideal # of clusters
sil_width <- c(NA)
for(i in 2:10){ 
  print(i)
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:10, sil_width,
      xlab = "Number of clusters",
      ylab = "Silhouette Width")
lines(1:10, sil_width)

## 3 clusters is good start
## get summary of the 3 clusters
k <- 2
pam_fit = pam(gower_dist, diss = TRUE, k)
pam_results = cluster.sample %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

## visualize clusters in lower dimension
tsne_obj = Rtsne(gower_dist, is_distance = TRUE)
tsne_data = tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle('Cluster Representation using Gower Distances') +
  ylab('Y') +
  xlab('X')

