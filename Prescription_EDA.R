rm(list=ls())
drug_sum = read.csv("Prescription_Clean_v2.csv")

drug_sum_df = drug_sum %>%
  select(NPROPNAME_2012,NUMBER_SCRIPTS_2012,THER_CLASS_2012,PAYER_2012, UNIQUE_MEMBERS_2012,TOTAL_COST_2012,
         INSURER_PAID_SUM_2012,MEMBER_PAID_SUM_2012,PCT_SCRIPTS_MALE_2012,PCT_SCRIPTS_FEMALE_2012,PCT_SCRIPTS_0_18_2012,
         PCT_SCRIPTS_19_44_2012,PCT_SCRIPTS_45_64_2012,PCT_SCRIPTS_65_PLUS_2012,PCT_URBAN_CORE_2012,PCT_SUBURBAN_2012,
         PCT_MICROPOLITAN_2012,PCT_RURAL_SMALLTOWN_2012,
         NPROPNAME_2016,NUMBER_SCRIPTS_2016,THER_CLASS_2016,PAYER_2016, UNIQUE_MEMBERS_2016,TOTAL_COST_2016,
         INSURER_PAID_SUM_2016,MEMBER_PAID_SUM_2016,PCT_SCRIPTS_MALE_2016,PCT_SCRIPTS_FEMALE_2016,PCT_SCRIPTS_0_18_2016,
         PCT_SCRIPTS_19_44_2016,PCT_SCRIPTS_45_64_2016,PCT_SCRIPTS_65_PLUS_2016,PCT_URBAN_CORE_2016,PCT_SUBURBAN_2016,
         PCT_MICROPOLITAN_2016,PCT_RURAL_SMALLTOWN_2016)

drug_sum_2012 = drug_sum_df %>%
  select(NPROPNAME_2012,NUMBER_SCRIPTS_2012,THER_CLASS_2012,PAYER_2012, UNIQUE_MEMBERS_2012,TOTAL_COST_2012,
         INSURER_PAID_SUM_2012,MEMBER_PAID_SUM_2012,PCT_SCRIPTS_MALE_2012,PCT_SCRIPTS_FEMALE_2012,PCT_SCRIPTS_0_18_2012,
         PCT_SCRIPTS_19_44_2012,PCT_SCRIPTS_45_64_2012,PCT_SCRIPTS_65_PLUS_2012,PCT_URBAN_CORE_2012,PCT_SUBURBAN_2012,
         PCT_MICROPOLITAN_2012,PCT_RURAL_SMALLTOWN_2012)

drug_sum_2016 = drug_sum_df %>%
  select(NPROPNAME_2016,NUMBER_SCRIPTS_2016,THER_CLASS_2016,PAYER_2016, UNIQUE_MEMBERS_2016,TOTAL_COST_2016,
         INSURER_PAID_SUM_2016,MEMBER_PAID_SUM_2016,PCT_SCRIPTS_MALE_2016,PCT_SCRIPTS_FEMALE_2016,PCT_SCRIPTS_0_18_2016,
         PCT_SCRIPTS_19_44_2016,PCT_SCRIPTS_45_64_2016,PCT_SCRIPTS_65_PLUS_2016,PCT_URBAN_CORE_2016,PCT_SUBURBAN_2016,
         PCT_MICROPOLITAN_2016,PCT_RURAL_SMALLTOWN_2016)

drug_sum_2012 = drug_sum_2012 %>%
  mutate(num_0_18 = NUMBER_SCRIPTS_2012 * PCT_SCRIPTS_0_18_2012 /100,
         num_19_44 = NUMBER_SCRIPTS_2012  * PCT_SCRIPTS_19_44_2012 /100,
         num_45_64 = NUMBER_SCRIPTS_2012  * PCT_SCRIPTS_45_64_2012/100,
         num_65_plus = NUMBER_SCRIPTS_2012  * PCT_SCRIPTS_65_PLUS_2012/100,
         num_65_less = num_0_18 + num_19_44 + num_45_64)

drug_sum_2012 %>%
  group_by(PAYER_2012) %>%
  summarise(total_num = sum(num_65_less,na.rm = TRUE))


a = drug_sum_2012 %>%
  group_by(PAYER_2012) %>%
  summarise(total_num = sum(NUMBER_SCRIPTS_2012,na.rm = TRUE))



# Therapeutic class by gender

drug_theu_2012 = drug_sum_2012 %>%
  group_by(THER_CLASS_2012) %>%
  summarise(pct_male_2012 = mean(PCT_SCRIPTS_MALE_2012,na.rm= TRUE),
            pct_female_2012 = mean(PCT_SCRIPTS_FEMALE_2012,na.rm = TRUE))



drug_theu_2016 = drug_sum_2016 %>%
  group_by(THER_CLASS_2016) %>%
  summarise(pct_male_2016 = mean(PCT_SCRIPTS_MALE_2016,na.rm= TRUE),
            pct_female_2016 = mean(PCT_SCRIPTS_FEMALE_2016,na.rm = TRUE))

drug_theu = merge(drug_theu_2012,drug_theu_2016,by.x = "THER_CLASS_2012", by.y = "THER_CLASS_2016")

# Therapeutic class by age groups 

drug_age_2012 = drug_sum_2012 %>%
  group_by(THER_CLASS_2012) %>%
  summarise(age_0_18_2012 = mean(PCT_SCRIPTS_0_18_2012,na.rm=TRUE),
            age_19_44_2012 = mean(PCT_SCRIPTS_19_44_2012,na.rm=TRUE),
            age_45_64_2012 = mean(PCT_SCRIPTS_45_64_2012,na.rm=TRUE),
            age_65_plus_2012 = mean(PCT_SCRIPTS_65_PLUS_2012,na.rm=TRUE))

drug_age_2016 = drug_sum_2016 %>%
  group_by(THER_CLASS_2016) %>%
  summarise(age_0_18_2016 = mean(PCT_SCRIPTS_0_18_2016,na.rm=TRUE),
            age_19_44_2016 = mean(PCT_SCRIPTS_19_44_2016,na.rm=TRUE),
            age_45_64_2016 = mean(PCT_SCRIPTS_45_64_2016,na.rm=TRUE),
            age_65_plus_2016 = mean(PCT_SCRIPTS_65_PLUS_2016,na.rm=TRUE))

drug_age = merge(drug_age_2012,drug_age_2016,by.x = "THER_CLASS_2012",by.y = "THER_CLASS_2016")

drug_age = drug_age[c("THER_CLASS_2012", "age_0_18_2012","age_0_18_2016",
                       "age_19_44_2012","age_19_44_2016","age_45_64_2012",
                       "age_45_64_2016","age_65_plus_2012","age_65_plus_2016")]

drug_age[drug_age]

# Therapeutic class by area

summary(drug_sum_2012)

summary(drug_sum_2012)

drug_area_2012 = drug_sum_2012 %>%
  group_by(THER_CLASS_2012)  %>%
  summarise(urban_core_2012 = mean(PCT_URBAN_CORE_2012,na.rm = TRUE),
            suburban_2012 = mean(PCT_SUBURBAN_2012,na.rm = TRUE),
            micropolitan_2012 = mean(PCT_MICROPOLITAN_2012,na.rm = TRUE),
            rural_smalltown_2012 = mean(PCT_RURAL_SMALLTOWN_2012,na.rm = TRUE))


drug_area_2016 = drug_sum_2016 %>%
  group_by(THER_CLASS_2016)  %>%
  summarise(urban_core_2016 = mean(PCT_URBAN_CORE_2016,na.rm = TRUE),
            suburban_2016 = mean(PCT_SUBURBAN_2016,na.rm = TRUE),
            micropolitan_2016 = mean(PCT_MICROPOLITAN_2016,na.rm = TRUE),
            rural_smalltown_2016 = mean(PCT_RURAL_SMALLTOWN_2016,na.rm = TRUE))

drug_area = merge(drug_area_2012,drug_area_2016,by.x = "THER_CLASS_2012",by.y = "THER_CLASS_2016")


drug_area = drug_area[c("THER_CLASS_2012", "urban_core_2012","urban_core_2016",
                        "suburban_2012","suburban_2016","micropolitan_2012",
                        "micropolitan_2016","rural_smalltown_2012","rural_smalltown_2016")]

summary(drug_area)

# visualizations about drug area
drug_area_2016 = drug_area_2016[drug_area_2016$THER_CLASS_2016 == "Cardiovascular Agents",]

df = data.frame(area_type = c("urban core","rural smalltown", "suburban", "micropolitan"),
                mean_percentage = c(55.02, 21.40, 10.17, 13.42 ))

df$area_type =factor(df$area_type, levels = c("micropolitan", "suburban", "rural smalltown", "urban core" ))

ggplot(data = df, aes(x = area_type, y = mean_percentage)) + 
  geom_bar(stat = "identity",fill = "steelblue") + 
  ggtitle("Cardiovascular Issues by Area Type")  + 
  coord_flip()


### Trying clustering 

library(purrr)



drug_cluster = drug_sum_df %>%
  filter(THER_CLASS_2016 == "Cardiovascular Agents") %>%
  select(NPROPNAME_2012,PAYER_2016,PCT_SCRIPTS_MALE_2016, PCT_SCRIPTS_FEMALE_2016,PCT_SCRIPTS_0_18_2016,PCT_SCRIPTS_19_44_2016,
         PCT_SCRIPTS_45_64_2016,
         PCT_SCRIPTS_65_PLUS_2016,PCT_URBAN_CORE_2016,PCT_SUBURBAN_2016,PCT_MICROPOLITAN_2016,PCT_RURAL_SMALLTOWN_2016)


drug_cluster = drug_cluster[complete.cases(drug_cluster),]

drug_cluster_sub = drug_cluster %>%
  select(-c(NPROPNAME_2012))

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = drug_cluster_sub, centers = k)
  model$tot.withinss
})

elbow_drug <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_drug, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

### Set K to 2 

set.seed(20)

cluster_drug = kmeans(drug_cluster_sub,2)
drug_cluster$segments = cluster_drug$cluster
drug_cluster_1 = drug_cluster[drug_cluster$segments ==1, ]
drug_cluster_2 = drug_cluster[drug_cluster$segments ==2, ]

summary(drug_cluster_1)

summary(drug_cluster_2)

write.csv(drug_cluster_1,"cluster1.csv")

write.csv(drug_cluster_2, "cluster2.csv")

unique(drug_cluster_1$NPROPNAME_2012)

unique(drug_cluster_2$NPROPNAME_2012)


drug_cluster_sub$PCT_SCRIPTS_65_PLUS_2016


### Trying clustering with mixed types (numeric and categorical data)

# Using gower distance

install.packages("cluster")
library(cluster)

gower_dist <- daisy(drug_cluster_sub,
                    metric = "gower",
                    type = list(logratio = c("PCT_SCRIPTS_0_18_2016",
                                             "PCT_SCRIPTS_19_44_2016",
                                             "PCT_SCRIPTS_45_64_2016",
                                             "PCT_SCRIPTS_65_PLUS_2016"
                                             )))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

sil_curve <- c()
for (k in 2:10) {
  pam_fit <- pam(gower_dist, diss = TRUE,k = k)
  #PAM interally computes the silhouette measure
  sil_curve[k] <- pam_fit$silinfo$avg.width
}
sil_curve = sil_curve[2:10]
plot(2:10, sil_curve, type="b", xlab="Number of Clusters", ylab="Silhouette")

### From the silhouette point the number of cluster should be 3 
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- drug_cluster_sub %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary




