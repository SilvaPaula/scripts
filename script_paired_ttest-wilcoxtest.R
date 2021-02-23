# Paired Samples t-Test in R

# Data in two numeric vectors
# ++++++++++++++++++++++++++
# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 3),
  weight = c(before,  after)
)
library("dplyr")
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")

install.packages("PairedData")
# Subset weight data before treatment
before <- subset(my_data,  group == "before", weight,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(my_data,  group == "after", weight,
                drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

# Preleminary test to check paired t-test assumptions
# Assumption 1: Are the two samples paired?
# Yes, since the data have been collected from measuring twice the weight of the same mice.
# Assumption 2: Is this a large sample?
# No, because n < 30. Since the sample size is not large enough (less than 30), we need to check whether the differences of the pairs follow a normal distribution.
# How to check the normality?
# Use Shapiro-Wilk normality test as described at: Normality Test in R.

# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed
# compute the difference
d <- with(my_data, 
          weight[group == "before"] - weight[group == "after"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141
# From the output, the p-value is greater than the significance level 0.05 
# implying that the distribution of the differences (d) are not significantly different from normal distribution. 
# In other words, we can assume the normality.

# Compute paired samples t-test
# Question : Is there any significant changes in the weights of mice after treatment?
# 1) Compute paired t-test - Method 1: The data are saved in two different numeric vectors.

# Compute t-test
res <- t.test(before, after, paired = TRUE)
res

#Compute paired t-test - Method 2: The data are saved in a data frame.

# Compute t-test
res <- t.test(weight ~ group, data = my_data, paired = TRUE)
res

# In the result above :
# t is the t-test statistic value (t = 20.88),
# df is the degrees of freedom (df= 9),
# p-value is the significance level of the t-test (p-value = 6.210^{-9}).
# conf.int is the confidence interval (conf.int) of the mean differences at 95% is also shown (conf.int= [173.42, 215.56])
# sample estimates is the mean differences between pairs (mean = 194.49).

# Interpretation of the result
# The p-value of the test is 6.210^{-9}, which is less than the significance level alpha = 0.05. 
# We can then reject null hypothesis and conclude that the average weight of the mice before treatment 
# is significantly different from the average weight after treatment with a p-value = 6.210^{-9}.

# Access to the values returned by t.test() function
# The result of t.test() function is a list containing the following components:

# statistic: the value of the t test statistics
# parameter: the degrees of freedom for the t test statistics
# p.value: the p-value for the test
# conf.int: a confidence interval for the mean appropriate to the specified alternative hypothesis.
# estimate: the means of the two groups being compared (in the case of independent t test) or difference in means (in the case of paired t test).


# The format of the R code to use for getting these values is as follow:
# printing the p-value
res$p.value

# Paired Samples Wilcoxon Test in R

# The paired samples Wilcoxon test (also known as Wilcoxon signed-rank test) is a non-parametric alternative 
# to paired t-test used to compare paired data. 
# It’s used when your data are not normally distributed. 

wilcox.test(before, after, paired = TRUE, alternative = "two.sided")

