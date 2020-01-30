library(tidyverse)

# load the data cleaned in python
data = read.csv('C:/Datasets/grailed_lm.csv', row.names=1, na.strings=c("","NA"))
head(data)

# drop color column 
drop = c('color', 'Description_Size', 'LengthOfCaptions')
data = data[,!names(data) %in% drop]
names(data)

# Linear Regression
lm0 = lm(SoldPrice~.-Type-Size+factor(Type)+factor(Size), data=data)
summary(lm0)

# create a dataframe of the regression output for filtering and visualizations
coef = data.frame(summary(lm0)$coefficients)
colnames(coef) = c('estimates','std error','t-value', 'p-value')
# select only those with p-value < 0.05
plot_data = coef[coef[,'p-value']<0.05,]
plot_data$variables = rownames(coef)


# visualization of the first ten variable's coefficients. 
ggplot(data=plot_data[1:10,], aes(x=variables,y=estimates)) + geom_col(fill=('lightskyblue')) + coord_flip() + 
  ggtitle('Coefficients Graph')+theme(plot.title = element_text(hjust = 0.5, size=16))
