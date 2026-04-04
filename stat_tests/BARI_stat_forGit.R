library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)



# import data
load("BARI_stat_forGit_data.RData")











# remove non-list
data_20202023 <- data_20202023 %>% filter(new_name %in% other_list$new_name)

# remove COVID-19
data_20202023 <- data_20202023 %>% filter(new_name != "COVID-19")

# assign NTD
data_20202023 <- data_20202023 %>% mutate(ntd = ifelse(new_name %in% ntd_list$new_name, "1", "0"))

# t.test
x0 <- data_20202023 %>% filter(ntd == "0") %>% pull(X20202023)
x1 <- data_20202023 %>% filter(ntd == "1") %>% pull(X20202023)

mean(x0)
sd(x0)*2
mean(x1)
sd(x1)*2

median(x0)
quantile(x0, probs = seq(0, 1, by = 0.25))
median(x1)
quantile(x1, probs = seq(0, 1, by = 0.25))

# test
t.test(x0, x1)   # Welchs t-test
wilcox.test(x0, x1)  # Mann-Whitney U test



# Chi-sq
# count NTD = 0, X20202023 < -2
a <- data_20202023 %>% filter(ntd == "0" & X20202023 < -2) %>% nrow()
# count NTD = 0, X20202023 >= -2
b <- data_20202023 %>% filter(ntd == "0" & X20202023 >= -2) %>% nrow()

paste0(a, "/", a+b, " (", round(a/(a+b)*100, 1), "%)")



# count NTD = 1, X20202023 < -2
c <- data_20202023 %>% filter(ntd == "1" & X20202023 < -2) %>% nrow()
d <- data_20202023 %>% filter(ntd == "1" & X20202023 >= -2) %>% nrow()

paste0(c, "/", c+d, " (", round(c/(c+d)*100, 1), "%)")



# Chi-sq
table <- matrix(c(a, b-a, c, d-c), nrow = 2, byrow = TRUE)
colnames(table) <- c("sig-decrease", "no")
rownames(table) <- c("NTD", "non-NTD")
chisq.test(table)
fisher.test(table)










# remove non-list
data_20162019 <- data_20162019 %>% filter(new_name %in% other_list$new_name)

# remove COVID-19
data_20162019 <- data_20162019 %>% filter(new_name != "COVID-19")

# assign NTD
data_20162019 <- data_20162019 %>% mutate(ntd = ifelse(new_name %in% ntd_list$new_name, "1", "0"))

# t.test
x0 <- data_20162019 %>% filter(ntd == "0") %>% pull(X20162019)
x1 <- data_20162019 %>% filter(ntd == "1") %>% pull(X20162019)

mean(x0)
sd(x0)*2
mean(x1)
sd(x1)*2

median(x0)
quantile(x0, probs = seq(0, 1, by = 0.25))
median(x1)
quantile(x1, probs = seq(0, 1, by = 0.25))

# test
t.test(x0, x1)   # Welchs t-test
wilcox.test(x0, x1)  # Mann-Whitney U test









# assign vir
data_20202023 <- data_20202023 %>% mutate(vir = ifelse(new_name %in% vir_list$new_name, "1", "0"))
data_20162019 <- data_20162019 %>% mutate(vir = ifelse(new_name %in% vir_list$new_name, "1", "0"))

# assign par
data_20202023 <- data_20202023 %>% mutate(par = ifelse(new_name %in% par_list$new_name, "1", "0"))
data_20162019 <- data_20162019 %>% mutate(par = ifelse(new_name %in% par_list$new_name, "1", "0"))

# assign resp
data_20202023 <- data_20202023 %>% mutate(resp = ifelse(new_name %in% resp_list$new_name, "1", "0"))
data_20162019 <- data_20162019 %>% mutate(resp = ifelse(new_name %in% resp_list$new_name, "1", "0"))










# ntd x vir
pre_vir0_ntd0 <- data_20162019 %>% filter(vir == "0" & ntd == "0") %>% pull(X20162019)
pre_vir1_ntd0 <- data_20162019 %>% filter(vir == "1" & ntd == "0") %>% pull(X20162019)
post_vir0_ntd0 <- data_20202023 %>% filter(vir == "0" & ntd == "0") %>% pull(X20202023)
post_vir1_ntd0 <- data_20202023 %>% filter(vir == "1" & ntd == "0") %>% pull(X20202023)
pre_vir0_ntd1 <- data_20162019 %>% filter(vir == "0" & ntd == "1") %>% pull(X20162019)
pre_vir1_ntd1 <- data_20162019 %>% filter(vir == "1" & ntd == "1") %>% pull(X20162019)
post_vir0_ntd1 <- data_20202023 %>% filter(vir == "0" & ntd == "1") %>% pull(X20202023)
post_vir1_ntd1 <- data_20202023 %>% filter(vir == "1" & ntd == "1") %>% pull(X20202023)

t.test(pre_vir0_ntd0, post_vir0_ntd0)
t.test(pre_vir1_ntd0, post_vir1_ntd0)
t.test(pre_vir0_ntd1, post_vir0_ntd1)
t.test(pre_vir1_ntd1, post_vir1_ntd1)

wilcox.test(pre_vir0_ntd0, post_vir0_ntd0)
wilcox.test(pre_vir1_ntd0, post_vir1_ntd0)
wilcox.test(pre_vir0_ntd1, post_vir0_ntd1)
wilcox.test(pre_vir1_ntd1, post_vir1_ntd1)










# one data
data_20162019$timing <- rep("pre-pandemic", nrow(data_20162019))
data_20202023$timing <- rep("post-pandemic", nrow(data_20202023))

colnames(data_20162019)[which(colnames(data_20162019) == "X20162019")] <- "bari"
colnames(data_20202023)[which(colnames(data_20202023) == "X20202023")] <- "bari"










# ntd x par
pre_par0_ntd0 <- data_20162019 %>% filter(par == "0" & ntd == "0") %>% pull(bari)
pre_par1_ntd0 <- data_20162019 %>% filter(par == "1" & ntd == "0") %>% pull(bari)
post_par0_ntd0 <- data_20202023 %>% filter(par == "0" & ntd == "0") %>% pull(bari)
post_par1_ntd0 <- data_20202023 %>% filter(par == "1" & ntd == "0") %>% pull(bari)
pre_par0_ntd1 <- data_20162019 %>% filter(par == "0" & ntd == "1") %>% pull(bari)
pre_par1_ntd1 <- data_20162019 %>% filter(par == "1" & ntd == "1") %>% pull(bari)
post_par0_ntd1 <- data_20202023 %>% filter(par == "0" & ntd == "1") %>% pull(bari)
post_par1_ntd1 <- data_20202023 %>% filter(par == "1" & ntd == "1") %>% pull(bari)

t.test(pre_par0_ntd0, post_par0_ntd0)
t.test(pre_par1_ntd0, post_par1_ntd0)
t.test(pre_par0_ntd1, post_par0_ntd1)
t.test(pre_par1_ntd1, post_par1_ntd1)

wilcox.test(pre_par0_ntd0, post_par0_ntd0)
wilcox.test(pre_par1_ntd0, post_par1_ntd0)
wilcox.test(pre_par0_ntd1, post_par0_ntd1)
wilcox.test(pre_par1_ntd1, post_par1_ntd1)










# ntd x resp
pre_resp0_ntd0 <- data_20162019 %>% filter(resp == "0" & ntd == "0") %>% pull(bari)
pre_resp1_ntd0 <- data_20162019 %>% filter(resp == "1" & ntd == "0") %>% pull(bari)
post_resp0_ntd0 <- data_20202023 %>% filter(resp == "0" & ntd == "0") %>% pull(bari)
post_resp1_ntd0 <- data_20202023 %>% filter(resp == "1" & ntd == "0") %>% pull(bari)
pre_resp0_ntd1 <- data_20162019 %>% filter(resp == "0" & ntd == "1") %>% pull(bari)
pre_resp1_ntd1 <- data_20162019 %>% filter(resp == "1" & ntd == "1") %>% pull(bari)
post_resp0_ntd1 <- data_20202023 %>% filter(resp == "0" & ntd == "1") %>% pull(bari)
post_resp1_ntd1 <- data_20202023 %>% filter(resp == "1" & ntd == "1") %>% pull(bari)

t.test(pre_resp0_ntd0, post_resp0_ntd0)
t.test(pre_resp1_ntd0, post_resp1_ntd0)
t.test(pre_resp0_ntd1, post_resp0_ntd1)
t.test(pre_resp1_ntd1, post_resp1_ntd1)

wilcox.test(pre_resp0_ntd0, post_resp0_ntd0)
wilcox.test(pre_resp1_ntd0, post_resp1_ntd0)
wilcox.test(pre_resp0_ntd1, post_resp0_ntd1)
wilcox.test(pre_resp1_ntd1, post_resp1_ntd1)










# remove non-list
data <- data %>% filter(new_name %in% other_list$new_name)

# remove COVID-19
data <- data %>% filter(new_name != "COVID-19")

#convert data to long format
data_long <- pivot_longer(data, cols = -c(new_name), names_to = "year", values_to = "bari")

# remove X from year
data_long$year <- gsub("X", "", data_long$year)





# new column to indicate if new_name is in ntd_list
data_long2 <- data_long %>% mutate(ntd = ifelse(new_name %in% ntd_list$new_name, "1", "0"))

# # check by extracting only 1
# data_long2 %>% filter(ntd == "1") %>% group_by(new_name) %>% summarise(count = n())

# # exclude COVID-19
# data_long_noCOIVD <- data_long2 %>% filter(new_name != "COVID-19")
data_long_noCOIVD <- data_long2



# test between groups
data_long_noCOIVD$ntd <- factor(data_long_noCOIVD$ntd)
data_long_noCOIVD$year <- factor(data_long_noCOIVD$year)
data_long_noCOIVD$new_name <- factor(data_long_noCOIVD$new_name)

model <- aov(bari ~ ntd * year + Error(new_name/year), data = data_long_noCOIVD)
summary(model) # ntcxyear indicates difference between NTD vs. non-NTD in yearly changes










# conduct t-test for each new_name between X2016-X2019 vs. X2020-X2023
p_list1 <- c()
p_list2 <- c()
change_list <- c()
for (i in data$new_name) {
  data_sub <- data %>% filter(new_name == i)
  group1 <- c(data_sub$"X2016", data_sub$"X2017", data_sub$"X2018", data_sub$"X2019")
  group2 <- c(data_sub$"X2020", data_sub$"X2021", data_sub$"X2022", data_sub$"X2023")
  
  # test_result <- t.test(group1, group2)
  test_result <- tryCatch(
    t.test(group1, group2),
    error = function(e) {
      message("t.test failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(test_result)) {
    p_value <- NA
  } else {
    p_value <- test_result$p.value
  }
  p_list1 <- c(p_list1, p_value)
  
  test_result <- wilcox.test(group1, group2)
  p_value <- test_result$p.value
  p_list2 <- c(p_list2, p_value)
  change_list <- c(change_list, mean(group2) - mean(group1))
}

data$p_value_t <- p_list1
data$p_value_w <- p_list2
data$change <- change_list

# write to clipboard
# sort by new_name
data_sort <- data %>% arrange(new_name)
# write.table(data_sort, "clipboard", sep = "\t", row.names = FALSE)

data_sort$ntd <- ifelse(data_sort$new_name %in% ntd_list$new_name, "1", "0")

# count
# ntd = 0, change < 0, p_value_t < 0.05
nonntd_sig_decrease <- data_sort %>% filter(ntd == "0" & change < 0 & p_value_w < 0.05) %>% nrow()
nonntd_other <- data_sort %>% filter(ntd == "0") %>% nrow() - nonntd_sig_decrease

print(paste0(nonntd_sig_decrease, "/", nonntd_sig_decrease + nonntd_other, " (", round(nonntd_sig_decrease/(nonntd_sig_decrease + nonntd_other)*100, 1), "%)"))

# ntd = 1, change < 0, p_value_t < 0.05
ntd_sig_decrease <- data_sort %>% filter(ntd == "1" & change < 0 & p_value_w < 0.05) %>% nrow()
ntd_other <- data_sort %>% filter(ntd == "1") %>% nrow() - ntd_sig_decrease

print(paste0(ntd_sig_decrease, "/", ntd_sig_decrease + ntd_other, " (", round(ntd_sig_decrease/(ntd_sig_decrease + ntd_other)*100, 1), "%)"))

# Chi-sq test
table <- matrix(c(ntd_sig_decrease, ntd_other, nonntd_sig_decrease, nonntd_other), nrow = 2, byrow = TRUE)
colnames(table) <- c("sig-decrease", "no")
rownames(table) <- c("NTD", "non-NTD")

chisq.test(table)
fisher.test(table)











# 7/17 vs. 4/17 by chi-square test
# create a contingency table
table <- matrix(c(7, 17-7, 4, 17-4), nrow = 2, byrow = TRUE)
colnames(table) <- c("low", "high")
rownames(table) <- c("NTD", "non-NTD")
# perform chi-square test
chisq.test(table)
# fisher
fisher.test(table)
