mariokart <- read.csv("C:\\Users\\brian\\Downloads\\mariokart.csv")
clean_mk <- subset(mariokart, total_pr < 100)
clean_mk$wheels <- ifelse(clean_mk$wheels > 0, "yes", "no")

library(tidyverse)
clean_mk %>%
  group_by(wheels) %>%
  summarize(Mean_Price = mean(total_pr, na.rm = T), SD_Price = sd(total_pr, na.rm = T), Sample_Size = n())

library(tidyverse)
clean_mk %>%
  group_by(stock_photo) %>%
  summarize(Mean_Price = mean(total_pr, na.rm = T), SD_Price = sd(total_pr, na.rm = T), Sample_Size = n())

library(tidyverse)
ggplot(clean_mk, aes(x = wheels, y = total_pr, fill = wheels)) + 
  geom_boxplot(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribution of Sales by Inclusion of Wheel(s)", x = "Wheels", y = "Sales Amount ($)") + 
  theme(axis.title.y = element_text(margin = margin(r = 15)), 
        axis.title.x = element_text(margin = margin(t = 15)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b=10)), 
        legend.position = "none")

shapiro.test(clean_mk$total_pr[clean_mk$wheels == "yes"])
shapiro.test(clean_mk$total_pr[clean_mk$wheels == "no"])
t.test(total_pr ~ wheels, data = clean_mk)

library(tidyverse)
ggplot(clean_mk, aes(x = stock_photo, y = total_pr, fill = stock_photo)) + 
  geom_boxplot(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribution of Sales by Stock Photo Inclusion", x = "Stock Photo Included", y = "Sales Amount ($)") + 
  theme(axis.title.y = element_text(margin = margin(r = 15)), 
        axis.title.x = element_text(margin = margin(t = 15)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b=10)), 
        legend.position = "none")

shapiro.test(clean_mk$total_pr[clean_mk$stock_photo == "yes"])
shapiro.test(clean_mk$total_pr[clean_mk$stock_photo == "no"])
t.test(total_pr ~ stock_photo, data = clean_mk)

anova_result <- aov(total_pr ~ wheels * stock_photo, data = clean_mk)
summary(anova_result)
