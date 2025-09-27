#This section is for the preparatory code (load needed libraries, )
library(data.table)
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(nnet)

# fread without stringsAsFactors
data1 <- data.table::fread("Lawsuit.csv")
str(data1)
head(data1)

### STEP 1: Data Preparation

# Check missing values
colSums(is.na(data1))

# ---- Convert categorical variables ----

# Dept
data1$Dept <- factor(data1$Dept,
                     levels = c(1,2,3,4,5,6),
                     labels = c("Biochemistry/Molecular Biology",
                                "Physiology",
                                "Genetics",
                                "Pediatrics",
                                "Medicine",
                                "Surgery"))

# Gender
data1$Gender <- factor(data1$Gender,
                       levels = c(0, 1),
                       labels = c("Female", "Male"))

# Rank
data1$Rank <- factor(data1$Rank,
                     levels = c(1, 2, 3),
                     labels = c("Assistant", "Associate", "Full professor"))

# Clin
data1$Clin <- factor(data1$Clin,
                     levels = c(0, 1),
                     labels = c("Primarily research emphasis",
                                "Primarily clinical emphasis"))

# Cert
data1$Cert <- factor(data1$Cert,
                     levels = c(0, 1),
                     labels = c("Not certified", "Board certified"))

# ---- Global options to avoid scientific notation ----
options(scipen = 999)

# Convert any remaining character columns to factors
data1 <- data1 %>%
  mutate(across(where(is.character), as.factor))
#Slide 3 Salary Density by Gender (1994)
ggplot(data1, aes(x = Sal94, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Salary Density by Gender (1994)",
    x = "Salary (1994)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme(
    legend.position = c(0.8, 0.65),
    legend.background = element_blank(),
    legend.key = element_blank()
)

#Slide 3 Salary Change Distribution by Gender
data1$sal_change <- data1$Sal95-data1$Sal94
boxplot(sal_change ~ Gender, data = data1,
        main = "Salary Change Distribution by Gender ",
        xlab = "Gender",
        ylab = "Salary (change)",
        col = c("lightpink", "lightblue"),
        axes = FALSE)
axis(1, at = 1:2, labels = c("Female", "Male"))
axis(2, at = pretty(data1$sal_change),
     labels = format(pretty(data1$sal_change), big.mark = ",", scientific = FALSE))
box()

#Slide 4 Salary Distribution by Gender within Each Rank (1994)
ggplot(data1, aes(x = Gender, y = Sal94, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Rank) +
  labs(
    title = "Salary Distribution by Gender within Each Rank (1994)",
    x = "Gender",
    y = "Salary (1994)"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue"))

#Slide 5 Salary Distribution by Gender within Each Department (1994)
ggplot(data1, aes(x = Gender, y = Sal94, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Dept) +
  labs(
    title = "Salary Distribution by Gender within Each Department (1994)",
    x = "Gender",
    y = "Salary (1994)"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue"))

#Slide 6 Salary Distribution by Gender and Certification (1994)
ggplot(data1, aes(x = Gender, y = Sal94, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Cert) +
  labs(
    title = "Salary Distribution by Gender and Certification (1994)",
    x = "Gender",
    y = "Salary (1994)"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue"))

#Slide 7 Average Salary by Department with Gender Proportion
library(dplyr)
library(ggplot2)
library(scales)
avg_salary <- data1 %>%
  group_by(Dept) %>%
  summarise(mean_salary = mean(Sal94, na.rm = TRUE))
gender_prop <- data1 %>%
  group_by(Dept, Gender) %>%
  summarise(n = n(), .groups="drop") %>%
  group_by(Dept) %>%
  mutate(prop = n / sum(n))
plot_data <- gender_prop %>%
  left_join(avg_salary, by="Dept") %>%
  mutate(height = mean_salary * prop,
         label = paste0(round(prop*100,1), "%"))
ggplot(plot_data, aes(x=Dept, y=height, fill=Gender)) +
  geom_col(position="stack") +
  scale_fill_manual(values=c("Female"="lightpink", "Male"="lightblue")) +
  scale_y_continuous(labels=comma) +
  geom_text(aes(label=label),
            position=position_stack(vjust=0.5), size=3) +
  labs(
    x="Department",
    y="Average Salary (1994)",
    fill="Gender",
    title="Average Salary by Department with Gender Proportion"
  ) +
  theme_minimal(base_size = 14)

#Slide 8 Number of People by Gender within Each Rank
ggplot(data1, aes(x = Rank, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of People by Gender within Each Rank",
    x = "Rank",
    y = "Count"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue"))

#Slide 8 Gender Proportion within Each Rank

ggplot(data1, aes(x = Rank, fill = Gender)) +
  geom_bar(position = "fill") +
  labs(
    title = "Gender Proportion with each Rank",
    x = "Rank",
    y = "Proportion",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Slide 9 Gender Proportion by Rank within Each Department
library(ggplot2)
ggplot(data1, aes(x = Rank, fill = Gender)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Dept) +
  labs(
    title = "Gender Proportion by Rank within Each Department",
    x = "Rank",
    y = "Proportion",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Safety: ensure numeric types
data1$Sal94  <- as.numeric(data1$Sal94)
data1$Prate  <- as.numeric(data1$Prate)
data1$Exper  <- as.numeric(data1$Exper)
data1$Clin   <- as.numeric(data1$Clin)
data1$Cert   <- as.numeric(data1$Cert)

### STEP 2: Log transforms
# Add +1 to avoid log(0) for counts/scores
data1 <- data1 %>%
  mutate(
    log_Sal94  = log(Sal94),
    log_Prate  = log(Prate + 1),
    log_Exper  = log(Exper + 1)
  )

### STEP 3: Train/Test split with seed
set.seed(42)
n <- nrow(data1)
test_ratio <- 0.2
test_idx <- sample.int(n, size = ceiling(test_ratio * n))
train <- data1[-test_idx, ]
test  <- data1[ test_idx, ]

### STEP 4: Fit model (on TRAIN)
m1 <- Sal94 ~ Gender + Dept + Clin + Cert + Prate + Exper + Rank
m2 <- log_Sal94 ~ Gender + Dept + Clin + Cert + log_Prate + log_Exper + Rank
lm_log <- lm(m2, data = train)
lm_org <- lm(m1, data = train)

summary(lm_org)
summary(lm_log)

### STEP 5: Evaluate metrics (Train & Test)
# Helper metrics
rmse_vec <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
r2_vec   <- function(actual, pred) {
  sse <- sum((actual - pred)^2, na.rm = TRUE)
  sst <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - sse/sst
}
adj_r2_from_r2 <- function(r2, n, p) 1 - (1 - r2) * (n - 1) / (n - p - 1)

# TRAIN (model summary has train adj R2; we also compute train RMSE on log scale)
train_pred_log <- predict(lm_log, newdata = train)
train_r2  <- r2_vec(train$log_Sal94, train_pred_log)
train_rmse <- rmse_vec(train$log_Sal94, train_pred_log)

# TEST (evaluate generalization on log scale)
test_pred_log <- predict(lm_log, newdata = test)
test_r2  <- r2_vec(test$log_Sal94, test_pred_log)
# p = number of predictors (excluding intercept) actually estimated in the model
p <- length(coef(lm_log)) - 1
test_adj_r2 <- adj_r2_from_r2(test_r2, n = nrow(test), p = p)
test_rmse <- rmse_vec(test$log_Sal94, test_pred_log)

cat("\n=== MODEL METRICS (log scale) ===\n")
cat(sprintf("TRAIN:  R^2 = %.3f | Adj R^2 (from summary) = %.3f | RMSE = %.3f\n",
            train_r2, summary(lm_log)$adj.r.squared, train_rmse))
cat(sprintf("TEST:   R^2 = %.3f | Adj R^2 (computed)     = %.3f | RMSE = %.3f\n\n",
            test_r2, test_adj_r2, test_rmse))

# (Optional) Also show RMSE on the original $ scale by exponentiating predictions.
# Note: exp(pred_log) is the median-unbiased back-transform if residuals are symmetric on log-scale,
# but a smearing correction is often used; below is simple exp() back-transform.
test_pred_dollar <- exp(test_pred_log)
test_rmse_dollar <- rmse_vec(test$Sal94, test_pred_dollar)
cat(sprintf("TEST RMSE (original $ scale, naive back-transform): %s\n\n",
            dollar_format()(test_rmse_dollar)))

### STEP 6: Convert coefficients to $ impacts
# Interpretation for log(Sal94):
# A coefficient beta corresponds to a multiplicative salary change of exp(beta) - 1.
# We'll express that change in dollars relative to a baseline salary.
# Use the TRAIN median salary for robustness; change to mean(train$Sal94) if you prefer.
base_salary <- median(train$Sal94, na.rm = TRUE)

coef_df <- summary(lm_log)$coefficients
coef_table <- data.frame(
  Term = rownames(coef_df),
  Estimate = coef_df[, "Estimate"],
  stringsAsFactors = FALSE
) %>%
  # Drop intercept for interpretation
  filter(Term != "(Intercept)") %>%
  mutate(
    Dollar_Impact = (exp(Estimate) - 1) * base_salary
  ) %>%
  # Order by absolute impact for a nicer plot
  arrange(desc(abs(Dollar_Impact)))

print(coef_table)

### STEP 7: Chart the $ impacts (white background + value labels)
# Nice labels
coef_table$label <- dollar_format()(coef_table$Dollar_Impact)

gg <- ggplot(coef_table, aes(x = reorder(Term, Dollar_Impact), y = Dollar_Impact)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  geom_text(aes(label = label),
            hjust = ifelse(coef_table$Dollar_Impact >= 0, -0.1, 1.1),
            size = 3.6) +
  scale_y_continuous(labels = dollar_format(),
                     expand = expansion(mult = c(0.1, 0.25))) +  # more margin on both ends
  labs(
    title = "Estimated Dollar Impact on Salary (vs. baseline)",
    subtitle = paste0("Baseline = ", dollar_format()(base_salary),
                      " (median salary)"),
    x = "Predictor",
    y = "Estimated Dollar Impact"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

print(gg)

#Slide 12 Observed vs Predicted - m3
set.seed(2020)

strata <- interaction(data1$Rank, data1$Gender, data1$Dept, data1$Clin,
                      data1$Cert, data1$Prate, data1$Exper)
train <- sample.split(strata, SplitRatio = 0.65)
trainset <- data1[train == T]

testset <- data1[train == F]
multi_logit <- multinom(Rank ~ Gender + Dept + Clin + Cert + Prate + Exper,
                        data = trainset)


summary(multi_logit)


pred_class <- predict(multi_logit, newdata = testset, type = "class")
true_class <- testset$Rank

coefs <- coef(multi_logit)                          # matrix: outcomes x terms
ses   <- summary(multi_logit)$standard.errors       # same shape

# Find the Gender coefficient column robustly (e.g., "GenderMale" or "Gender1")
gender_col <- grep("^Gender", colnames(coefs), value = TRUE)
if (length(gender_col) != 1) {
  stop("Couldn't uniquely identify the Gender coefficient column. Check factor coding of Gender.")
}

# Build dataframe of ORs for Gender effect across outcomes (vs Rank baseline)
or_df <- data.frame(
  Outcome = rownames(coefs),
  Beta = coefs[, gender_col],
  SE   = ses[, gender_col]
)
or_df$OR  <- exp(or_df$Beta)
or_df$LCL <- exp(or_df$Beta - 1.96 * or_df$SE)
or_df$UCL <- exp(or_df$Beta + 1.96 * or_df$SE)

# Add baseline outcome explicitly at OR = 1
baseline_lvl <- levels(data1$Rank)[1]  # current reference outcome
baseline <- data.frame(
  Outcome = baseline_lvl, Beta = 0, SE = NA, OR = 1, LCL = 1, UCL = 1
)

plot_df <- rbind(or_df, baseline)

# Color mapping: baseline pink, other outcomes blue
plot_df$Color <- ifelse(plot_df$Outcome == baseline_lvl, "Baseline", "Male vs Female")

# Bar chart (no error bars), log scale for readability
ggplot(plot_df, aes(x = Outcome, y = OR, fill = Color)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = round(OR, 2)), 
            vjust = -0.3, size = 4) +   # numbers above bars
  scale_y_continuous(limits = c(0, max(plot_df$OR) * 1.2)) +  
  scale_fill_manual(values = c("Baseline" = "pink", "Male vs Female" = "lightblue")) +
  labs(
    title = "Odds Ratios (Male vs Female) for Academic Rank",
    subtitle = paste("Baseline:", baseline_lvl, "| Adjusted for other variables"),
    x = "Rank Category",
    y = "Odds Ratio (linear scale)",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
# Confusion matrix
cm <- table(Predicted = pred_class, Actual = true_class)
print(cm)

# Overall accuracy
overall_accuracy <- sum(diag(cm)) / sum(cm)
overall_accuracy
library(tidyr)
library(scales)
# Optional for combining plots later
# install.packages("patchwork")
# library(patchwork)

# --- Build predicted probabilities for impact visuals ---

predictors <- data1 %>% select(-Rank)  # remove outcome

newdata_female <- predictors
newdata_female$Gender <- factor("Female", levels = levels(data1$Gender))

newdata_male <- predictors
newdata_male$Gender <- factor("Male", levels = levels(data1$Gender))


# newdata_female <- data1; newdata_female$Gender <- factor("Female", levels = levels(data1$Gender))
# newdata_male   <- data1; newdata_male$Gender   <- factor("Male",   levels = levels(data1$Gender))

p_female <- colMeans(predict(multi_logit, newdata = newdata_female, type = "probs"))
p_male   <- colMeans(predict(multi_logit, newdata = newdata_male,   type = "probs"))

prob_df <- data.frame(
  Outcome = names(p_female),
  Female  = as.numeric(p_female),
  Male    = as.numeric(p_male),
  check.names = FALSE
) %>%
  tidyr::pivot_longer(cols = c(Female, Male), names_to = "Gender", values_to = "Probability") %>%
  mutate(Gender = factor(Gender, levels = c("Female","Male")))

# --- PLOT 1: Side-by-side probability bars (standalone) ---
p_prob_bars <- ggplot(prob_df, aes(x = Outcome, y = Probability, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_text(aes(label = scales::percent(Probability, accuracy = 0.1)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(
    title = "Males vs Females: Predicted Probability of Each Academic Rank",
    subtitle = "Males retain higher predicted chances of promotion to senior ranks, holding other factors constant",
    x = "Rank Category", y = "Predicted Probability", fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(p_prob_bars)  # <- make sure to print, don't add another ggplot() with +

library(nnet)
library(ggplot2)


set.seed(2020)
data1$Dept <- relevel(data1$Dept, ref = "Medicine")
# ---- Train / test split (stratify on existing important fields) ----
strata <- interaction(data1$Rank, data1$Gender, data1$Dept, data1$Clin,
                      data1$Cert, data1$Prate, data1$Exper)
train <- sample.split(strata, SplitRatio = 0.65)
trainset <- data1[train == TRUE, ]
testset  <- data1[train == FALSE, ]

# ---- Multinomial logit predicting DEPARTMENT (not Rank) ----
multi_logit_1 <- multinom(Dept ~ Gender + Rank + Clin + Cert + Prate + Exper,
                        data = trainset)
summary(multi_logit_1)

# Predictions & accuracy on test set
pred_class <- predict(multi_logit_1, newdata = testset, type = "class")
true_class <- testset$Dept
cm <- table(Predicted = pred_class, Actual = true_class)
print(cm)
overall_accuracy <- sum(diag(cm)) / sum(cm)
overall_accuracy

# ---- Build predicted probabilities by Gender for each Department ----
predictors <- data1 %>% dplyr::select(-Dept)  # remove outcome

newdata_female <- predictors
newdata_female$Gender <- factor("Female", levels = levels(data1$Gender))

newdata_male <- predictors
newdata_male$Gender <- factor("Male",   levels = levels(data1$Gender))

p_female <- colMeans(predict(multi_logit_1, newdata = newdata_female, type = "probs"))
p_male   <- colMeans(predict(multi_logit_1, newdata = newdata_male,   type = "probs"))

prob_df <- data.frame(
  Outcome = names(p_female),
  Female  = as.numeric(p_female),
  Male    = as.numeric(p_male),
  check.names = FALSE
) %>%
  tidyr::pivot_longer(cols = c(Female, Male),
                      names_to = "Gender",
                      values_to = "Probability") %>%
  dplyr::mutate(Gender = factor(Gender, levels = c("Female","Male")))

# ---- Median salary (Sal94) by Dept & Gender ----
median_df <- data1 %>%
  dplyr::group_by(Dept, Gender) %>%
  dplyr::summarise(median_sal = median(Sal94, na.rm = TRUE), .groups = "drop")

plot_df <- prob_df %>%
  dplyr::left_join(median_df, by = c("Outcome" = "Dept", "Gender" = "Gender"))

# Scale for secondary axis (map salary -> [0,1] to overlay on probability bars)
salary_max <- max(plot_df$median_sal, na.rm = TRUE)
salary_to_prob <- function(x) x / salary_max
prob_to_salary <- function(y) y * salary_max

# ---- Median salary (Sal94) by Dept (overall, one value per Dept) ----
median_df <- data1 %>%
  dplyr::group_by(Dept) %>%
  dplyr::summarise(median_sal = median(Sal94, na.rm = TRUE), .groups = "drop")

# keep bars data as-is
plot_df <- prob_df

# use ONLY the unique per-Dept medians for the line
median_line_df <- median_df %>%
  dplyr::rename(Outcome = Dept)

# scale for secondary axis
salary_max <- max(median_line_df$median_sal, na.rm = TRUE)
salary_to_prob <- function(x) x / salary_max
prob_to_salary <- function(y) y * salary_max

p_prob_bars <- ggplot(plot_df, aes(x = Outcome)) +
  # probability bars
  geom_col(aes(y = Probability, fill = Gender),
           position = position_dodge(width = 0.7),
           width = 0.6, color = "black") +
  geom_text(aes(y = Probability,
                label = scales::percent(Probability, accuracy = 0.1),
                group = Gender),
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 4) +
  
  # median salary line
  geom_line(data = median_line_df,
            aes(x = Outcome, y = salary_to_prob(median_sal), group = 1),
            inherit.aes = FALSE, linewidth = 1, color = "black") +
  geom_point(data = median_line_df,
             aes(x = Outcome, y = salary_to_prob(median_sal)),
             inherit.aes = FALSE, size = 2, color = "black") +
  geom_text(data = median_line_df,
            aes(x = Outcome, y = salary_to_prob(median_sal),
                label = scales::comma(median_sal, accuracy = 1)),
            inherit.aes = FALSE, vjust = -1.2, size = 3.6, color = "black") +
  
  # y axes
  scale_y_continuous(
    name = "Predicted Probability",
    labels = scales::percent_format(),
    limits = c(0, 1),
    sec.axis = sec_axis(~ prob_to_salary(.),
                        name = "Median Salary (Sal94)",
                        labels = scales::comma_format(accuracy = 1))
  ) +
  
  # 🔑 force all departments to show
  scale_x_discrete(drop = FALSE,
                   limits = levels(data1$Dept)) +
  
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(
    title = "Males vs Females: Predicted Probability by Department",
    subtitle = "Males are more likely to enter more well-paid departments, holding other factors constant",
    x = "Department", fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 15, hjust = 1))

print(p_prob_bars)

library(stringr)

# Optional: wrap the long label to 2 lines so it needs less horizontal space
x_lab_wrapped <- levels(data1$Dept)
x_lab_wrapped[which(x_lab_wrapped == "Biochemistry/Molecular Biology")] <-
  str_replace(x_lab_wrapped[which(x_lab_wrapped == "Biochemistry/Molecular Biology")],
              "/", "/\n")

p_prob_bars <- p_prob_bars +
  # keep ALL departments and add padding on both sides
  scale_x_discrete(
    drop = FALSE,
    limits = levels(data1$Dept),
    labels = x_lab_wrapped,
    expand = expansion(mult = c(0.06, 0.06))
  ) +
  # let ticks/labels draw outside the panel and give extra margins
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(t = 20, r = 30, b = 50, l = 60),
    axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1)
  )

print(p_prob_bars)

coefs <- coef(multi_logit_1)                          # matrix: outcomes x terms
ses   <- summary(multi_logit_1)$standard.errors       # same shape

# Find the Gender coefficient column robustly (e.g., "GenderMale" or "Gender1")
gender_col <- grep("^Gender", colnames(coefs), value = TRUE)
if (length(gender_col) != 1) {
  stop("Couldn't uniquely identify the Gender coefficient column. Check factor coding of Gender.")
}

# Build dataframe of ORs for Gender effect across outcomes (vs Rank baseline)
or_df <- data.frame(
  Outcome = rownames(coefs),
  Beta = coefs[, gender_col],
  SE   = ses[, gender_col]
)
or_df$OR  <- exp(or_df$Beta)
or_df$LCL <- exp(or_df$Beta - 1.96 * or_df$SE)
or_df$UCL <- exp(or_df$Beta + 1.96 * or_df$SE)

# Add baseline outcome explicitly at OR = 1
baseline_lvl <- levels(data1$Dept)[1]  # current reference outcome
baseline <- data.frame(
  Outcome = baseline_lvl, Beta = 0, SE = NA, OR = 1, LCL = 1, UCL = 1
)

plot_df <- rbind(or_df, baseline)

# Color mapping: baseline pink, other outcomes blue
plot_df$Color <- ifelse(plot_df$Outcome == baseline_lvl, "Baseline", "Male vs Female")

# Bar chart (no error bars), log scale for readability
ggplot(plot_df, aes(x = Outcome, y = OR, fill = Color)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = round(OR, 2)), 
            vjust = -0.3, size = 4) +   # numbers above bars
  scale_y_continuous(limits = c(0, max(plot_df$OR) * 1.2)) +  
  scale_fill_manual(values = c("Baseline" = "pink", "Male vs Female" = "lightblue")) +
  labs(
    title = "Odds Ratios (Male vs Female) for Departments",
    subtitle = paste("Baseline:", baseline_lvl, "| Adjusted for other variables"),
    x = "Rank Category",
    y = "Odds Ratio (linear scale)",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

