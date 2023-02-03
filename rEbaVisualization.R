install.packages("ggplot2")                    # Install ggplot2 package
library("ggplot2")                             # Load ggplot2
library("dplyr")

print("Hello World")
print(approvals)

approvals <- approvals

print(approvals)

# 1
str(approvals)
approvals$Gender <- as.character(approvals$Gender)
approvals$Approved <- as.character(approvals$Approved)


# 2
summary(approvals)

# 3a
debtHist <- hist(approvals$Debt)
plot(debtHist, main="Histogram of Debt", xlab= "Debt")

# 3b
debtDensity <- density(approvals$Debt)
plot(debtDensity, main="Density of Debt", xlab= "Debt")

# 4a
x<-approvals$Income
options(scipen=999)
hist(x, main="Histogram of Income", xlab = "Income")                                   # Draw histogram
abline(v = mean(x),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = mean(x) * 1.7,                   # Add text for mean
     y = mean(x) * 1.7,
     paste("Mean =", mean(x)),
     col = "red",
     cex = 2)

# 4b
mean_salary <- approvals %>% 
  pull(Income) %>% 
  mean() %>%
  signif(6)

approvals %>% 
  ggplot(aes(x=Income)) +
  ggtitle("Density of Income") +
  geom_density( fill="dodgerblue", alpha=0.5)+
  scale_x_log10()+
  geom_vline(xintercept=mean_salary, size=1.5, color="red")

# 5
ggplot(approvals, aes(x=Income, y=Debt), main="Scatter plot of Years Employed and Debt") +
  geom_point() +
  geom_smooth(se=FALSE, method=lm) +
  ggtitle("Scatter plot of Debt and Income")

# 6
counts <- table(approvals$Ethnicity, approvals$Industry)
barplot(counts, main="Bar Plot of Ethnicities with Industries",
        xlab="Industry", ylab="count", col=c("darkorange","black", "gold", "grey", "white"),
        legend = rownames(counts), beside=TRUE)

#7 industry approval

ggplot(data=approvals) + 
  geom_bar(
    mapping = aes(x=Industry, fill=Approved),
    position = "fill"
  ) + coord_flip() + 
  ggtitle("Bar plot for Industry and Approval")

#8
png(file = "debt_industry.png")
debtIndustryBP <- ggplot(data=approvals, mapping=aes(x = Industry, y = Debt)) +
  geom_boxplot() +
  ggtitle("Box plot of Debt and Industry")
debtIndustryBP + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

#9
png(file = "employed_industry.png")
debtIndustryBP <- ggplot(data=approvals, mapping=aes(x = Industry, y = YearsEmployed)) +
  ggtitle("Box plot of Industry and Years Employed")
  geom_boxplot()
debtIndustryBP + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

#10a
counts <- table(approvals$Approved, approvals$Gender)
approvalGenderBP <- barplot(counts, main="Bar Plot of Genders and Approvals",
        xlab="Gender (0 is Female & 1 is Male)",
        ylab="Count",
        col=c("red","green"),
        legend = c("Not Approved", "Approved"), beside = TRUE)

#10b
ggplot(approvals, aes(x=YearsEmployed, y=Debt)) +
  geom_point()+geom_smooth(se=FALSE, method=lm) +
  ggtitle("Scatter Plot of Debt and Years Employed")















