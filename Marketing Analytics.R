### MARKETING ANALYTICS CODE #####

### DATA LOADING ######

# -------------------------------
# 1. Load required packages
# -------------------------------

library(dplyr)
library(readr)

# -------------------------------
# 2. Load all CSV files
# -------------------------------

retail <- read_csv("Retail transaction data.csv")
prospects <- read_csv("Prospect customers info.csv")
conjoint <- read_csv("Conjoint survey results-1.csv")
profiles <- read_csv("Product profiles.csv")
products <- read_csv("Product attributes information.csv")

# -------------------------------
# 3. Quick check
# -------------------------------

head(retail)
head(prospects)
head(conjoint)
head(profiles)
head(products)

### DATA CLEANING AND PRE PROCESSING ####

# -------------------------------
# 1. Check structure and missing values
# -------------------------------

str(retail)
str(prospects)
str(conjoint)
str(profiles)
str(products)

colSums(is.na(retail))
colSums(is.na(prospects))
colSums(is.na(conjoint))
colSums(is.na(profiles))
colSums(is.na(products))


# -------------------------------
# 2. Rename typo in products dataset
# -------------------------------

products <- products %>%
  rename(sustainability_claim = sustaintability_claim)

# -------------------------------
# 3. Clean retail transaction data
# -------------------------------

retail_clean <- retail %>%
  mutate(
    InvoiceNo = as.character(InvoiceNo),
    ProductID = as.character(ProductID),
    CustomerID = as.character(CustomerID),
    ProductCategory = as.factor(ProductCategory),
    Married = as.factor(Married),
    Work = as.factor(Work),
    Education = as.factor(Education),
    Sales = UnitPrice * Quantity
  ) %>%
  filter(
    !is.na(CustomerID),
    !is.na(InvoiceDate),
    !is.na(UnitPrice),
    !is.na(Quantity),
    UnitPrice > 0,
    Quantity > 0,
    Sales > 0
  ) %>%
  distinct()

# -------------------------------
# 4. Clean prospect customer data
# -------------------------------

prospects_clean <- prospects %>%
  mutate(
    CustomerID = as.character(CustomerID),
    Married = as.factor(Married),
    Work = as.factor(Work),
    Education = as.factor(Education)
  ) %>%
  filter(
    !is.na(CustomerID),
    !is.na(Income),
    !is.na(Age),
    !is.na(HouseholdSize)
  ) %>%
  distinct()

# -------------------------------
# 5. Clean conjoint survey data
# -------------------------------

conjoint_clean <- conjoint %>%
  mutate(
    respondent_id = as.factor(respondent_id),
    productNo = as.factor(productNo),
    price = as.factor(price),
    format = as.factor(format),
    strength = as.factor(strength),
    origin = as.factor(origin),
    sustainability = as.factor(sustainability),
    rating = as.numeric(rating)
  ) %>%
  filter(
    !is.na(rating),
    rating >= 1,
    rating <= 7
  ) %>%
  distinct()

# -------------------------------
# 6. Clean product profiles data
# -------------------------------

profiles_clean <- profiles %>%
  mutate(
    productNo = as.factor(productNo),
    price = as.factor(price),
    format = as.factor(format),
    strength = as.factor(strength),
    origin = as.factor(origin),
    sustainability = as.factor(sustainability)
  ) %>%
  distinct()

# -------------------------------
# 7. Clean product attributes data
# -------------------------------

products_clean <- products %>%
  mutate(
    product_name = as.character(product_name),
    is_decaf = as.factor(is_decaf),
    sustainability_claim = as.factor(sustainability_claim)
  ) %>%
  filter(
    !is.na(product_name),
    !is.na(price),
    !is.na(price_100g),
    price > 0,
    price_100g > 0
  ) %>%
  distinct()

# -------------------------------
# 8. Check cleaned datasets
# -------------------------------

summary(retail_clean)
summary(prospects_clean)
summary(conjoint_clean)
summary(profiles_clean)
summary(products_clean)

colSums(is.na(retail_clean))
colSums(is.na(prospects_clean))
colSums(is.na(conjoint_clean))
colSums(is.na(profiles_clean))
colSums(is.na(products_clean))

### DEALING WITH MISSING VALUES ###

retail_clean <- retail_clean %>%
  mutate(ProductCategory = as.character(ProductCategory),
         ProductCategory = ifelse(is.na(ProductCategory), "Unknown", ProductCategory),
         ProductCategory = as.factor(ProductCategory))

sum(is.na(retail_clean))

### MORE CLEANING STEPS ###

# -------------------------------
# Check date range
# -------------------------------

range(retail_clean$InvoiceDate)

# -------------------------------
# Check extreme values / outliers
# -------------------------------

summary(retail_clean$UnitPrice)
summary(retail_clean$Quantity)
summary(retail_clean$Sales)

# Top 10 highest sales transactions
retail_clean %>%
  arrange(desc(Sales)) %>%
  select(InvoiceNo, InvoiceDate, ProductID, ProductCategory, UnitPrice, Quantity, Sales, CustomerID) %>%
  head(10)

# Top 10 highest unit prices
retail_clean %>%
  arrange(desc(UnitPrice)) %>%
  select(InvoiceNo, InvoiceDate, ProductID, ProductCategory, UnitPrice, Quantity, Sales, CustomerID) %>%
  head(10)

# Top 10 highest quantities
retail_clean %>%
  arrange(desc(Quantity)) %>%
  select(InvoiceNo, InvoiceDate, ProductID, ProductCategory, UnitPrice, Quantity, Sales, CustomerID) %>%
  head(10)
# -------------------------------
# Final cleaning for retail data
# -------------------------------

# 1. Replace missing ProductCategory values with "Unknown"
retail_clean <- retail_clean %>%
  mutate(
    ProductCategory = as.character(ProductCategory),
    ProductCategory = ifelse(is.na(ProductCategory), "Unknown", ProductCategory),
    ProductCategory = as.factor(ProductCategory)
  )

# 2. Check rows with unknown product information
retail_clean %>%
  filter(ProductID == "PNA" | ProductCategory == "Unknown") %>%
  select(InvoiceNo, InvoiceDate, ProductID, ProductCategory, UnitPrice, Quantity, Sales, CustomerID)

# 3. Remove obvious unknown/non-product rows
retail_clean2 <- retail_clean %>%
  filter(ProductID != "PNA")

# 4. Check before and after row numbers
nrow(retail_clean)
nrow(retail_clean2)

# 5. Check numeric summaries after removing PNA rows
summary(retail_clean2$UnitPrice)
summary(retail_clean2$Quantity)
summary(retail_clean2$Sales)

# 6. Use retail_clean2 for the main analysis from now on
retail_final <- retail_clean2

retail_clean <- retail_final

########################## DATA CLEANED ########################################


#### SUMMARY PLOTS AND DESCRIPTIVE STATS ######

# -------------------------------
# EDA: simple and focused
# -------------------------------

library(dplyr)
library(ggplot2)
library(scales)

theme_clean <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# -------------------------------
# 1. Summary statistics table
# -------------------------------

summary_table <- retail_final %>%
  summarise(
    transactions = n(),
    customers = n_distinct(CustomerID),
    invoices = n_distinct(InvoiceNo),
    total_sales = sum(Sales),
    average_sale = mean(Sales),
    median_sale = median(Sales),
    average_quantity = mean(Quantity)
  )

summary_table

# -------------------------------
# 2. Sales by product category - simple fixed version
# -------------------------------

category_sales <- retail_final %>%
  group_by(ProductCategory) %>%
  summarise(total_sales = sum(Sales), .groups = "drop") %>%
  arrange(desc(total_sales))

ggplot(category_sales, aes(x = reorder(ProductCategory, total_sales), y = total_sales)) +
  geom_col(fill = "#2F5597") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0("£", format(x, big.mark = ",", scientific = FALSE))) +
  labs(
    title = "Total Sales by Product Category",
    subtitle = "Shows which categories contribute most to revenue",
    x = "Product category",
    y = "Total sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, colour = "#1F1F1F"),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    axis.title = element_text(colour = "#333333"),
    axis.text = element_text(colour = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# -------------------------------
# 3. Customer spend distribution
# -------------------------------
coffee_customers <- retail_final %>%
  filter(ProductCategory == "Coffee") %>%
  distinct(CustomerID) %>%
  mutate(coffee_buyer = "Coffee buyer")

customer_summary <- retail_final %>%
  group_by(CustomerID) %>%
  summarise(
    total_spend = sum(Sales),
    frequency = n_distinct(InvoiceNo),
    Income = first(Income),
    Age = first(Age),
    .groups = "drop"
  ) %>%
  left_join(coffee_customers, by = "CustomerID") %>%
  mutate(coffee_buyer = ifelse(is.na(coffee_buyer), "Non-coffee buyer", coffee_buyer))

ggplot(customer_summary, aes(x = coffee_buyer, y = total_spend, fill = coffee_buyer)) +
  geom_boxplot(alpha = 0.85, colour = "#333333") +
  scale_fill_manual(values = c("Coffee buyer" = "#2F5597", "Non-coffee buyer" = "#A6A6A6")) +
  labs(
    title = "Customer Spend by Coffee Buyer Status",
    subtitle = "Compares total spending between coffee and non-coffee customers",
    x = "",
    y = "Total customer spend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, colour = "#1F1F1F"),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    axis.title = element_text(colour = "#333333"),
    axis.text = element_text(colour = "#333333"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# -------------------------------
# 4. Coffee buyers vs non-coffee buyers
# -------------------------------

# -------------------------------
# 4. Coffee buyers vs non-coffee buyers
# -------------------------------

coffee_buyer_table <- customer_summary %>%
  group_by(coffee_buyer) %>%
  summarise(
    customers = n(),
    average_spend = mean(total_spend),
    median_spend = median(total_spend),
    average_frequency = mean(frequency),
    average_income = mean(Income),
    average_age = mean(Age),
    .groups = "drop"
  )

coffee_buyer_table

ggplot(coffee_buyer_table, aes(x = coffee_buyer, y = average_spend, fill = coffee_buyer)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Coffee buyer" = "#2F5597", "Non-coffee buyer" = "#A6A6A6")) +
  labs(
    title = "Average Spend by Coffee Buyer Status",
    subtitle = "Coffee buyers and non-coffee buyers differ in customer value",
    x = "",
    y = "Average customer spend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, colour = "#1F1F1F"),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    axis.title = element_text(colour = "#333333"),
    axis.text = element_text(colour = "#333333"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


###################################################################################################
###################################################################################################

# -------------------------------
# RFM analysis: base customer table
# -------------------------------

analysis_date <- max(retail_final$InvoiceDate) + 1

rfm_base <- retail_final %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(analysis_date - max(InvoiceDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales),
    Income = first(Income),
    Age = first(Age),
    HouseholdSize = first(HouseholdSize),
    Married = first(Married),
    Education = first(Education),
    Work = first(Work),
    .groups = "drop"
  )

summary(rfm_base)

# -------------------------------
# Independent RFM scoring
# -------------------------------

rfm_independent <- rfm_base %>%
  mutate(
    R_score = ntile(desc(Recency), 5),   # lower recency = better score
    F_score = ntile(Frequency, 5),       # higher frequency = better score
    M_score = ntile(Monetary, 5),        # higher spend = better score
    RFM_score = paste0(R_score, F_score, M_score),
    RFM_total = R_score + F_score + M_score
  )

# Check scores
head(rfm_independent)

summary(rfm_independent[, c("R_score", "F_score", "M_score", "RFM_total")])

# -------------------------------
# Create simple RFM segments
# -------------------------------

rfm_independent <- rfm_independent %>%
  mutate(
    RFM_segment = case_when(
      R_score >= 4 & F_score >= 4 & M_score >= 4 ~ "Champions",
      R_score >= 4 & F_score >= 3 & M_score >= 3 ~ "Loyal high-value",
      R_score <= 2 & F_score >= 3 & M_score >= 3 ~ "At risk valuable",
      R_score >= 3 & F_score <= 2 ~ "Recent low-frequency",
      R_score <= 2 & F_score <= 2 & M_score <= 2 ~ "Low-value inactive",
      TRUE ~ "Mid-value customers"
    )
  )

# Segment summary
rfm_segment_summary <- rfm_independent %>%
  group_by(RFM_segment) %>%
  summarise(
    customers = n(),
    average_recency = mean(Recency),
    average_frequency = mean(Frequency),
    average_monetary = mean(Monetary),
    average_income = mean(Income),
    average_age = mean(Age),
    .groups = "drop"
  ) %>%
  arrange(desc(average_monetary))

rfm_segment_summary

# -------------------------------
# RFM segment visualisation
# -------------------------------
library(ggplot2)

ggplot(rfm_segment_summary, aes(x = reorder(RFM_segment, average_monetary), y = average_monetary)) +
  geom_col(fill = "#2F5597", width = 0.7) +
  coord_flip() +
  labs(
    title = "Average Monetary Value by RFM Segment",
    x = "RFM segment",
    y = "Average monetary value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, colour = "#1F1F1F"),
    axis.title = element_text(colour = "#333333"),
    axis.text = element_text(colour = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

rfm_independent %>%
  group_by(R_score) %>%
  summarise(
    min_recency = min(Recency),
    max_recency = max(Recency),
    average_recency = mean(Recency),
    customers = n(),
    .groups = "drop"
  ) %>%
  arrange(R_score)

# -------------------------------
# Sequential RFM scoring
# -------------------------------

rfm_sequential <- rfm_base %>%
  mutate(
    R_score = ntile(desc(Recency), 5)
  ) %>%
  group_by(R_score) %>%
  mutate(
    F_score = ntile(Frequency, 5)
  ) %>%
  group_by(R_score, F_score) %>%
  mutate(
    M_score = ntile(Monetary, 5)
  ) %>%
  ungroup() %>%
  mutate(
    RFM_score = paste0(R_score, F_score, M_score),
    RFM_total = R_score + F_score + M_score,
    RFM_segment = case_when(
      R_score >= 4 & F_score >= 4 & M_score >= 4 ~ "Champions",
      R_score >= 4 & F_score >= 3 & M_score >= 3 ~ "Loyal high-value",
      R_score <= 2 & F_score >= 3 & M_score >= 3 ~ "At risk valuable",
      R_score >= 3 & F_score <= 2 ~ "Recent low-frequency",
      R_score <= 2 & F_score <= 2 & M_score <= 2 ~ "Low-value inactive",
      TRUE ~ "Mid-value customers"
    )
  )

rfm_seq_summary <- rfm_sequential %>%
  group_by(RFM_segment) %>%
  summarise(
    customers = n(),
    avg_recency = mean(Recency),
    avg_frequency = mean(Frequency),
    avg_monetary = mean(Monetary),
    avg_income = mean(Income),
    avg_age = mean(Age),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_monetary))

rfm_seq_summary

### RFM COMPARISON TABLE #########
rfm_comparison <- rfm_independent %>%
  count(RFM_segment, name = "Independent") %>%
  full_join(
    rfm_sequential %>%
      count(RFM_segment, name = "Sequential"),
    by = "RFM_segment"
  ) %>%
  arrange(desc(Independent))

rfm_comparison

### SEQUENTIAL RFM #######################

ggplot(rfm_seq_summary, aes(x = reorder(RFM_segment, avg_monetary), y = avg_monetary)) +
  geom_col(fill = "#2F5597", width = 0.7) +
  coord_flip() +
  labs(
    title = "Average Monetary Value by Sequential RFM Segment",
    x = "RFM segment",
    y = "Average monetary value"
  ) +
  theme_minimal()

# -------------------------------
# Cluster analysis preparation
# -------------------------------

cluster_data <- rfm_base %>%
  select(Recency, Frequency, Monetary, Income, Age, HouseholdSize)

cluster_scaled <- scale(cluster_data)

# -------------------------------
# Hierarchical clustering
# -------------------------------

set.seed(123)

dist_matrix <- dist(cluster_scaled, method = "euclidean")

hc_model <- hclust(dist_matrix, method = "ward.D2")

plot(
  hc_model,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Customers",
  ylab = "Distance"
)

rect.hclust(hc_model, k = 4, border = "grey40")

# -------------------------------
# Elbow method for K-means
# -------------------------------

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  kmeans_model <- kmeans(cluster_scaled, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}

elbow_df <- data.frame(
  k = 1:10,
  wss = wss
)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Method for K-means Clustering",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal()

# -------------------------------
# K-means clustering
# -------------------------------

set.seed(123)

kmeans_4 <- kmeans(cluster_scaled, centers = 4, nstart = 25)

rfm_clustered <- rfm_base %>%
  mutate(cluster = as.factor(kmeans_4$cluster))

# Cluster size
table(rfm_clustered$cluster)

# Cluster profile
cluster_profile <- rfm_clustered %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    avg_recency = mean(Recency),
    avg_frequency = mean(Frequency),
    avg_monetary = mean(Monetary),
    avg_income = mean(Income),
    avg_age = mean(Age),
    avg_household_size = mean(HouseholdSize),
    married_rate = mean(as.numeric(as.character(Married))),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_monetary))

cluster_profile

# -------------------------------
# Simple cluster visualisation
# -------------------------------

ggplot(cluster_profile, aes(x = reorder(cluster, avg_monetary), y = avg_monetary)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Average Monetary Value by Cluster",
    x = "Cluster",
    y = "Average monetary value"
  ) +
  theme_minimal()

# -------------------------------
# ANOVA: test whether clusters differ
# -------------------------------

anova_recency <- aov(Recency ~ cluster, data = rfm_clustered)
anova_frequency <- aov(Frequency ~ cluster, data = rfm_clustered)
anova_monetary <- aov(Monetary ~ cluster, data = rfm_clustered)
anova_income <- aov(Income ~ cluster, data = rfm_clustered)
anova_age <- aov(Age ~ cluster, data = rfm_clustered)
anova_household <- aov(HouseholdSize ~ cluster, data = rfm_clustered)

summary(anova_recency)
summary(anova_frequency)
summary(anova_monetary)
summary(anova_income)
summary(anova_age)
summary(anova_household)

# -------------------------------
# LDA model for cluster prediction
# -------------------------------

install.packages("MASS")
install.packages("caret")
library(MASS)
library(caret)

set.seed(123)

lda_data <- rfm_clustered %>%
  dplyr::select(cluster, Recency, Frequency, Monetary, Income, Age, HouseholdSize)
train_index <- createDataPartition(lda_data$cluster, p = 0.7, list = FALSE)

train_data <- lda_data[train_index, ]
test_data <- lda_data[-train_index, ]

lda_model <- lda(cluster ~ Recency + Frequency + Monetary + Income + Age + HouseholdSize,
                 data = train_data)

lda_pred <- predict(lda_model, test_data)

confusionMatrix(
  data = lda_pred$class,
  reference = test_data$cluster
)

# -------------------------------
# Predict prospect customer clusters
# -------------------------------
prospect_for_lda <- prospects_clean%>%
  dplyr::select(Income, Age, HouseholdSize) %>%
  mutate(
    Recency = mean(rfm_clustered$Recency),
    Frequency = mean(rfm_clustered$Frequency),
    Monetary = mean(rfm_clustered$Monetary)
  ) %>%
  dplyr::select(Recency, Frequency, Monetary, Income, Age, HouseholdSize)

prospect_pred <- predict(lda_model, prospect_for_lda)

prospect_customers_segmented <- prospects_clean %>%
  mutate(predicted_cluster = prospect_pred$class)

table(prospect_customers_segmented$predicted_cluster)

# -------------------------------
# Target segment bubble chart
# -------------------------------

target_bubble <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  Segment = c(
    "Affluent moderate",
    "Mainstream active",
    "High-value loyal",
    "Low-engagement"
  ),
  Customers = c(165, 399, 84, 274),
  Avg_Recency = c(64.7, 41.1, 25.6, 118.0),
  Avg_Monetary = c(80.7, 84.1, 452.0, 44.7),
  Role = c(
    "Premium upsell",
    "Secondary growth",
    "Primary target",
    "Low priority"
  )
)

ggplot(target_bubble, aes(
  x = Avg_Recency,
  y = Avg_Monetary,
  size = Customers,
  label = paste0(Cluster, "\n", Segment, "\n", Role)
)) +
  geom_point(alpha = 0.75) +
  geom_text(vjust = -1, size = 3.5) +
  scale_size(range = c(6, 18)) +
  labs(
    title = "Target Segment Prioritisation Map",
    subtitle = "Lower recency and higher monetary value indicate stronger commercial attractiveness",
    x = "Average recency in days (lower is better)",
    y = "Average monetary value (£)",
    size = "Segment size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# -------------------------------
# Conjoint analysis: part-worth model
# -------------------------------

conjoint_clean <- conjoint_clean %>%
  mutate(
    price = as.factor(price),
    format = as.factor(format),
    strength = as.factor(strength),
    origin = as.factor(origin),
    sustainability = as.factor(sustainability)
  )

conjoint_model <- lm(
  rating ~ price + format + strength + origin + sustainability,
  data = conjoint_clean
)

summary(conjoint_model)
anova(conjoint_model)

# -------------------------------
# Conjoint: part-worth utilities
# -------------------------------

library(dplyr)
library(ggplot2)
library(broom)

partworths <- tidy(conjoint_model) %>%
  filter(term != "(Intercept)") %>%
  arrange(term)

partworths

ggplot(partworths, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Conjoint Part-Worth Utilities",
    x = "Attribute level",
    y = "Utility estimate"
  ) +
  theme_minimal()

# -------------------------------
# Conjoint: attribute importance
# -------------------------------

conjoint_importance <- anova(conjoint_model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("attribute") %>%
  filter(attribute != "Residuals") %>%
  mutate(importance = `Sum Sq` / sum(`Sum Sq`) * 100) %>%
  dplyr::select(attribute, importance) %>%
  arrange(desc(importance))
conjoint_importance

ggplot(conjoint_importance, aes(x = reorder(attribute, importance), y = importance)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Attribute Importance from Conjoint Analysis",
    x = "Attribute",
    y = "Importance (%)"
  ) +
  theme_minimal()
# -------------------------------
# Conjoint: Willingness to Pay
# -------------------------------

price_coef <- abs(coef(conjoint_model)["price6"] - coef(conjoint_model)["price5"])

wtp <- partworths %>%
  mutate(
    WTP = estimate / price_coef
  ) %>%
  arrange(desc(WTP))

wtp

# -------------------------------
# Market share prediction
# -------------------------------

choice_set <- data.frame(
  product = c("Proposed PB coffee", "Competitor A", "Competitor B", "Competitor C"),
  price = factor(c(5, 4, 6, 5), levels = levels(conjoint_clean$price)),
  format = factor(c("Capsule", "Capsule", "Capsule", "Capsule"), levels = levels(conjoint_clean$format)),
  strength = factor(c("Dark", "Medium", "Dark", "Mild"), levels = levels(conjoint_clean$strength)),
  origin = factor(c("100% Arabica blend", "House blend", "100% Arabica blend", "Single-origin"), levels = levels(conjoint_clean$origin)),
  sustainability = factor(c("Yes", "No", "No", "Yes"), levels = levels(conjoint_clean$sustainability))
)

choice_set$predicted_rating <- predict(conjoint_model, newdata = choice_set)

choice_set <- choice_set %>%
  mutate(
    exp_utility = exp(predicted_rating),
    market_share = exp_utility / sum(exp_utility) * 100
  )

choice_set

# -------------------------------
# PCA preparation
# -------------------------------

str(products_clean)

pca_vars <- products_clean %>%
  dplyr::select(
    num_servings, price, price_100g, strength_level,
    is_decaf, sustainability_claim, convenience,
    authenticity, premium, perceived_sustainability,
    taste_quality
  ) %>%
  mutate(
    is_decaf = as.numeric(as.character(is_decaf)),
    sustainability_claim = as.numeric(as.character(sustainability_claim))
  )

pca_model <- prcomp(pca_vars, scale. = TRUE)

summary(pca_model)
pca_model$rotation[, 1:2]

# -------------------------------
# PCA scores and loadings
# -------------------------------

pca_scores <- as.data.frame(pca_model$x[, 1:2]) %>%
  mutate(product_name = products_clean$product_name)

pca_loadings <- as.data.frame(pca_model$rotation[, 1:2]) %>%
  mutate(attribute = rownames(.))

# -------------------------------
# PCA perceptual map
# -------------------------------

ggplot(pca_scores, aes(x = PC1, y = PC2, label = product_name)) +
  geom_point(size = 2.5) +
  geom_text(vjust = -0.7, size = 3) +
  geom_segment(
    data = pca_loadings,
    aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
    arrow = arrow(length = unit(0.2, "cm")),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = pca_loadings,
    aes(x = PC1 * 3.3, y = PC2 * 3.3, label = attribute),
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "PCA Perceptual Map of Coffee Products",
    subtitle = "PC1 and PC2 explain 52.95% of total product variation",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal()
