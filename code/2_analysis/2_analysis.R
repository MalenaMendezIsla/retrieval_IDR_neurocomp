#| message: false
#| warning: false

library(tidyverse)
library(ggpubr)
library(funModeling)
library(readr)
library(gridExtra)
library(wesanderson)
library(here)
library(openxlsx)
library(svglite)
library(UpSetR)
library(writexl)
library(boot)
library(car)
library(rsample)
library(ggbump)
library(ggrepel)
library(plotly)
library(patchwork)
library(readxl)
library(reshape2)

# Import datasets ---------------------------------------------------------

# Import the dataset of titles and abstracts to create samples and evaluate precision and pseudo-recall
titles_abstracts_24_08_28 <- read_delim(here("data", "2_strategies_outputs", "metadata", "titles_abstracts_24_08_28.csv"))

# Import data sets for each strategy from Web of Science
S1 <- read_delim(here("data", "2_strategies_outputs", "metadata", "e1_metadata_24_08_20.csv"))
S2 <- read_delim(here("data", "2_strategies_outputs", "metadata", "e2_metadata_24_08_16.csv"), delim = ";")

# The versions New_E3&E4 haven't cites/ref threshold and have duplicates. These steps are done below.
New_S3 <- read_delim(here("data", "2_strategies_outputs", "metadata", "e3_metadata_24_08_19.csv"), delim = ";")
New_S4 <- read_delim(here("data", "2_strategies_outputs", "metadata", "e4_metadata_24_08_19.csv"), delim = ";")

# Import quality samples
sample_columns <- c("OST_BK", "Included", "Title", "Abstract")
sample_s2_120 <- read_delim(here("data", "2_strategies_outputs", "sample", "as2_120.csv"), delim = ",") %>% select(all_of(sample_columns))
sample_s3_120 <- read_delim(here("data", "2_strategies_outputs", "sample", "as3_120.csv"), delim = ",") %>% select(all_of(sample_columns))
sample_s4_120 <- read_delim(here("data", "2_strategies_outputs", "sample", "as4_120.csv"), delim = ",") %>% select(all_of(sample_columns))
sample_s1_221_24_09_22 <- read_delim(here("data", "2_strategies_outputs", "sample", "sample_s1_221_24_09_22.csv"), delim = ",") %>% select(all_of(sample_columns))
sample_s2_101 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s2_101_25_02_25.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
sample_s3_101 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s3_101_25_02_25.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
sample_s4_101 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s4_101_25_02_25.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
categorizedsample_s1_1000 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s1_1000_25_03_05.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
categorizedsample_s2_1000 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s2_1000_25_03_05.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
categorizedsample_s3_1000 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s3_1000_25_03_05.csv"), delim = ",") %>% 
  select(all_of(sample_columns))
categorizedsample_s4_1000 <- read_delim(here("data", "2_strategies_outputs", "sample", "categorizedsample_s4_1000_25_03_05.csv"), delim = ",") %>% 
  select(all_of(sample_columns))



# Import topic modeling results
strategy_topic_500 <- read_csv("~/neuro_comp_sci/results/topic_modeling/strategy_topic_500.csv")
articles_and_topics <- read_excel("~/neuro_comp_sci/results/topic_modeling/articles_and_topics.xlsx")
topic_info_500 <- read_csv("~/neuro_comp_sci/results/topic_modeling/topic_info_500.csv")
newname_topic_info_500 <- read_csv("~/neuro_comp_sci/data/2_strategies_outputs/metadata/newname_topic_info_500.csv")

# Data Preparation --------------------------------------------------------

# Unify abstract data: one paper per row
titles_abstracts_unify <- titles_abstracts_24_08_28 %>%
  group_by(OST_BK) %>%
  summarise(
    Title = first(Title),                    
    Abstract = paste(Abstract, collapse = " ")
  )

# Drop topic -1
strategy_topic_500 <- strategy_topic_500 %>%
  filter(topic_500 > -1)

articles_and_topics <- articles_and_topics %>%
  filter(topic_500 > -1)

newname_topic_info_500 <- newname_topic_info_500 %>%
  filter(Topic > -1) %>%
  mutate(New_Name = ifelse(New_Name == "Dementia Alzheimer", "Alzheimer", New_Name))


#####Strategies results preparation #####

# Delete data prior to 1991
datasets <- list(S1, S2, New_S3, New_S4)
datasets <- lapply(datasets, function(df) df %>% filter(Year > 1990))

# Mutate strategy column for each dataset

S1 <- S1 %>%
  mutate(strategy = "S1")

S2 <- S2 %>%
  mutate(strategy = "S2")

##### Defining Threshold S3 and S4 #####

# Create the tables E3_ft and E4_ft with the frequency of each Proportion value
S3_ft <- New_S3 %>%
  mutate(Proportion = CitRef_other / Cit_ALL_iac, strategy = "S3") %>% 
  select(OST_BK, Discipline, Proportion, strategy) %>% 
  group_by(Proportion, strategy) %>% 
  summarise(n_articles=n(), .groups = 'drop') %>% 
  arrange(-Proportion) %>% 
  mutate(cumulative_proportion = cumsum(n_articles) / sum(n_articles))


S4_ft <- New_S4 %>%
  mutate(Proportion = CitRef_other / Nb_Reference, strategy = "S4") %>% 
  select(OST_BK, Discipline, Proportion, strategy) %>% 
  group_by(Proportion, strategy) %>% 
  summarise(n_articles=n(), .groups = 'drop') %>%
  arrange(-Proportion) %>% 
  mutate(cumulative_proportion = cumsum(n_articles) / sum(n_articles))

# Combine S3 and S4 data frames
S3_S4_ft <- bind_rows(S3_ft,S4_ft)


# Custom plot theme
custom_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.y = element_text(size = 30, color = "black"),
      axis.text.x = element_text(size = 30, color = "black"),
      axis.title.y = element_text(size = 30, color = "black"),
      axis.title.x = element_text(size = 30, color = "black"),
      legend.text = element_text(size = 30, color = "black"),
      legend.title = element_text(size = 30, color = "black"),
      text = element_text(size = 30, color = "black") 
    )
}


###### Figure_1 : Threshold ######
Figure_1 <- ggplot(S3_S4_ft, aes(x = Proportion, y = cumulative_proportion, color = strategy, linetype = strategy)) +
  geom_line(size=3) +  
  labs(x = "Threshold percentage",
       y = "Cumulative percentage of papers") +
  custom_theme () +
  scale_color_manual(values = c("#9C964A", "#CDC08C")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_vline(xintercept = c(0.15, 0.25), linetype = "dotted") + 
  scale_x_continuous(
    breaks = c(0.15, 0.25),  
    labels = scales::percent
  ) + 
  scale_y_continuous(labels = scales::percent) + 
  guides(color = guide_legend(title = "Strategy"), linetype = guide_legend(title = "Strategy")) 

# Save the plot
ggsave(plot = Figure_1, filename = here('results', 'figures', 'Figure_1.png'), width = 20, height = 10, dpi = 300)

##### Apply Threshold E3 & E4 #####
# Calculate proportion and filter

S3_wd <- New_S3 %>%
  mutate(Proportion = CitRef_other / Cit_ALL_iac,
         strategy = "E3") %>% 
  arrange(desc(Proportion)) %>% 
  filter(Proportion > 0.25)

S4_wd <- New_S4 %>%
  mutate (Proportion = CitRef_other / Nb_Reference,
          strategy = "E4") %>% 
  arrange(desc(Proportion)) %>% 
  filter(Proportion > 0.15)

# Disciplines (duplicated papers in S3 and S4) ----------------

##### Discipline frequency #####

###### Preparing Figure 3: Distribution of disciplines across strategies & time evolution ######

# Function to prepare data for S3 and S4 with fractional weighting
prepare_fractional_data <- function(data, strategy_label) 
  data %>%
  group_by(OST_BK, Discipline) %>%
  mutate(duplicated_count = n()) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarise(n = sum(1 / duplicated_count)) %>%
  mutate(Strategy = strategy_label)

# Prepare data for S1 and S2 (simple count) and S3, S4 (fractional weighting)
S1_dist_d <- S1 %>% count(Discipline) %>% mutate(Strategy = "S1")
S2_dist_d <- S2 %>% count(Discipline) %>% mutate(Strategy = "S2")
S3_dist_d <- prepare_fractional_data(S3_wd, "S3")
S4_dist_d <- prepare_fractional_data(S4_wd, "S4")

# Combine all datasets
combined_df <- bind_rows(S1_dist_d, S2_dist_d, S3_dist_d, S4_dist_d)

# Compute the total count and proportion for each strategy
combined_df <- combined_df %>%
  group_by(Strategy) %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Proportion = n / total)

# Compute global total and global percentage
global_total <- sum(combined_df$n, na.rm = TRUE)
combined_df <- combined_df %>%
  mutate(Global_Percentage = (n / global_total) * 100)

# Palette definitions
custom_palette_wes <- c(wes_palette("Moonrise3", 5), "#F8DDE0")

# Calculate the top 5 disciplines
top_5_disciplines <- combined_df %>%
  group_by(Discipline) %>%
  summarise(total_proportion = sum(Proportion)) %>%
  arrange(desc(total_proportion)) %>%
  slice(1:5) %>%
  pull(Discipline)

# Group the remaining disciplines as "Others"
combined_df <- combined_df %>%
  mutate(Discipline = ifelse(Discipline %in% top_5_disciplines, Discipline, "Others"))

####### Figure_3: Disciplines #######

#Prepare data
S1_dist_d_prop <- S1_dist_d %>% mutate(Proportion=n/sum(n))

# Figure_3
Figure_3 <- ggplot(combined_df, aes(x = Strategy, y = Proportion, fill = Discipline)) +
  geom_bar(stat = "identity", position = "fill") +
  custom_theme () +
  labs(x = "Strategy", y = "Proportion of Papers", fill = "Discipline") +
  theme(
    text = element_text(size = 30),
    theme(legend.position = "bottom")
  ) +
  scale_fill_manual(values = custom_palette_wes) 
#  annotate("text", x = 1, y = Inf, label = "A", size = 8, fontface = "bold", vjust = 2)  # Alineado arriba

ggsave(plot = Figure_3, filename = here('results', 'figures', 'Figure_3.png'),
       width = 20, height = 10, dpi = 300)

#### --- Data Preparation ##### 

sum(duplicated(S1$OST_BK))
sum(duplicated(S2$OST_BK))
sum(duplicated(S3_wd$OST_BK))
sum(duplicated(S4_wd$OST_BK))

###### Remove duplicates in E3 and E4 before joining all strategies#####

# This is where S3 and S4 take their final form. We decided to calculate disciplines
# based in a fractional counting of duplicated in S3 and S4
# (papers that are duplicated because they for under the definition of compu and neuro)
# in this section we eliminate duplicated entries so we can use S3 and S4


S3 <- S3_wd %>%
  distinct(OST_BK, .keep_all = TRUE)

S4 <- S4_wd %>%
  distinct(OST_BK, .keep_all = TRUE)

sum(duplicated(S3$OST_BK))
sum(duplicated(S4$OST_BK))


S3 <- S3 %>% mutate (strategy="S3")

S4 <- S4 %>% mutate (strategy="S4")


# Unify strategies

# Make sure that the variables have the same class and change if it's necessary


S3$Cit_ALL_iac<- as.character(S3$Cit_ALL_iac)


###### Unify strategies #####

full_dataset <- bind_rows(S1, S2, S3, S4) %>% filter(Year>1990)

OST_BK_strategy <- full_dataset %>% select (OST_BK, strategy) 

names(full_dataset)

###### Quality samples ######

# Drop evaluated samples

remaining_s1 <- S1 %>% anti_join(sample_s1_221, by = "OST_BK") %>% inner_join (titles_abstracts_unify, by= "OST_BK")
remaining_s2 <- S2 %>% anti_join(sample_s2_221, by = "OST_BK") %>% inner_join (titles_abstracts_unify, by= "OST_BK")
remaining_s3 <- S3 %>% anti_join(sample_s3_221, by = "OST_BK") %>% inner_join (titles_abstracts_unify, by= "OST_BK")
remaining_s4 <- S4 %>% anti_join(sample_s4_221, by = "OST_BK") %>% inner_join (titles_abstracts_unify, by= "OST_BK")

# Sample 1000 for each strategy
sample_s1_1000 <- remaining_s1 %>% sample_n(779)
sample_s2_1000 <- remaining_s2 %>% sample_n(779)
sample_s3_1000 <- remaining_s3 %>% sample_n(779)
sample_s4_1000 <- remaining_s4 %>% sample_n(779)


# Download datasets 
#write_xlsx(sample_s1_1000, here("data", "2_strategies_outputs", "sample", "sample_s1_1000_25_03_05.xlsx"))
#write_xlsx(sample_s2_1000, here("data", "2_strategies_outputs", "sample", "sample_s2_1000_25_03_05.xlsx"))
#write_xlsx(sample_s3_1000, here("data", "2_strategies_outputs", "sample", "sample_s3_1000_25_03_05.xlsx"))
#write_xlsx(sample_s4_1000, here("data", "2_strategies_outputs", "sample", "sample_s4_1000_25_03_05.xlsx"))


##### Sizes #####

#Distribution of Papers by Strategy (Absolute)

# Calculate absolute data without fractional counting
absolute_data <- full_dataset %>%
  group_by(strategy) %>%
  summarise(total_count = n())

# Calculate proportions
absolute_data <- absolute_data %>%
  mutate(proportion = total_count / sum(total_count))

#Distribution of Papers by Strategy (Frac count)


# Calculate data with fractional counting
fraction_data <- full_dataset %>%
  group_by(OST_BK) %>%
  mutate(total_strategies = n(),  w=1/total_strategies) %>% 
  ungroup() %>% 
  group_by(strategy) %>% 
  summarise(frac_count = sum(w)) %>% 
  ungroup() %>%  
  mutate(proportion = frac_count /sum(frac_count), 
         proportion = round(proportion, 3), frac_count = round(frac_count, 1))


###### Figure 2 A & B: Distribution of papers across strategies ######

custom_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.y = element_text(size = 30, color = "black"),
      axis.text.x = element_text(size = 30, color = "black"),
      axis.title.y = element_text(size = 30, color = "black"),
      axis.title.x = element_text(size = 30, color = "black"),
      legend.text = element_text(size = 30, color = "black"),
      legend.title = element_text(size = 30, color = "black"),
      text = element_text(size = 30, color = "black") 
    )
}

# Absolute count Panel "A"
bar_plot_absolute <- ggplot(absolute_data, aes(x = strategy, y = proportion, fill = strategy)) +
  geom_col() +
  geom_text(aes(label = total_count), vjust = -0.5, size = 10) +
  labs(x = "Strategy", y = "Percentage of papers") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = wes_palette("Moonrise3")) +
  custom_theme() +
  theme(legend.position = "none") + 
  annotate("text", x = 1, y = max(absolute_data$proportion) + 0.05, label = "A", size = 8, fontface = "bold") +
  coord_cartesian(ylim = c(0, 0.33))

# Fractional count Panel "B"
bar_plot_relative <- ggplot(fraction_data, aes(x = strategy, y = proportion, fill = strategy)) +
  geom_col() +
  geom_text(aes(label = as.integer(frac_count)), vjust = -0.5, size = 10) +
  labs(x = "Strategy") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = wes_palette("Moonrise3")) +
  custom_theme() +
  theme(legend.position = "none",
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) +
  annotate("text", x = 1, y = max(fraction_data$proportion) + 0.05, label = "B", size = 8, fontface = "bold") +
  coord_cartesian(ylim = c(0, 0.33))

##### Figure 2 C Overlap####

overlap_data <- strategy_pairs %>% 
  rowwise() %>% 
  mutate(
    ids1 = list(full_dataset %>% filter(strategy == strategy1) %>% pull(OST_BK)),
    ids2 = list(full_dataset %>% filter(strategy == strategy2) %>% pull(OST_BK)),
    overlap_count = length(intersect(ids1, ids2)),
    union_count = length(union(ids1, ids2))
  ) %>% 
  ungroup() %>% 
  mutate(
    jaccard_index = ifelse(strategy1 == strategy2, 1, round(overlap_count / union_count, 3)),
    jaccard_percent = round(jaccard_index * 100, 1)
  )

extended_palette <- colorRampPalette(custom_palette_wes)(21)

Figure_2c <- ggplot(overlap_data, aes(x = strategy2, y = strategy1, fill = jaccard_percent)) +
  geom_tile(data = subset(overlap_data, strategy1 != strategy2), color = "white") +
  geom_text(aes(label = ifelse(strategy1 == strategy2, "", paste0(jaccard_percent, "%"))), 
            color = "black", size = 10) +  
  scale_fill_gradient(low = "#FFFFFF", high = "#B9A070", limits = c(0, max(overlap_data$jaccard_percent))) +  
  labs(x = "Strategies",
       y = "Strategies",
       fill = "Overlap (%)") +  
  custom_theme() + 
  annotate("text", x = 1, y = length(unique(overlap_data$strategy1)) + 0.45, label = "C", size = 8, fontface = "bold") +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1,  
                               barwidth = 15, barheight = 1.5)) +  
  theme(
    legend.position = 'bottom',
    legend.margin = margin(t = 10, b = 10, l = 100, r = 100)
  )


##### Figure 2 D Temporal evolution####

year <- full_dataset %>% select(OST_BK, Year, strategy)

year_freq <- year %>% count (Year, strategy)

# Create lines plot 

Figure_2d <- ggplot(year_freq, aes(x = Year, y = n, color = strategy)) +
  geom_smooth(method = "loess", se = FALSE, size=3) + 
  scale_color_manual(values = wes_palette("Moonrise3"))+
  labs(x = "Year",
       y = "Frequency") +
  custom_theme() +
  annotate("text", x = 1990, y = Inf, label = "D", size = 8, fontface = "bold", vjust = 2)+
  guides(color = guide_legend(title = "Strategy"), linetype = guide_legend(title = "Strategy", size=40)) +
  theme(
    legend.position = 'bottom',
    legend.margin = margin(t = 10, b = 10, l = 100, r = 100)
  )


Figure_2 <- (bar_plot_absolute | bar_plot_relative) / (Figure_2c + Figure_2d) + 
  plot_layout(heights = c(2, 2),
              widths = c(2, 2)   
  )

ggsave(plot = Figure_2, filename = here('results', 'figures', 'Figure_2.png'),
       width = 20, height = 20, dpi = 300)


##### Preparing Figure_4 Journals #####

# General

total_freq <- full_dataset %>%
  group_by(Revue) %>%
  summarise(AbsolutFreq = n()) %>%
  arrange(desc(AbsolutFreq)) %>% mutate(Proportion= AbsolutFreq/sum(AbsolutFreq))


tube1 <- S1 %>% 
  count(Revue) %>% 
  mutate(Strategy = "S1") %>% 
  arrange(-n) %>% 
  mutate(ranking = dense_rank(desc(n))) %>% 
  filter (ranking<11)


tube2 <- S2 %>% 
  count(Revue) %>% 
  mutate(Strategy = "S2") %>% 
  arrange(-n) %>% 
  mutate(ranking = dense_rank(desc(n))) %>% 
  filter (ranking<11)

tube3 <- S3 %>% 
  count(Revue) %>% 
  mutate(Strategy = "S3") %>% 
  arrange(-n) %>% 
  mutate(ranking = dense_rank(desc(n))) %>% 
  filter (ranking<11)

tube4 <- S4 %>% 
  count(Revue) %>% 
  mutate(Strategy = "S4") %>% 
  arrange(-n) %>% 
  mutate(ranking = dense_rank(desc(n))) %>% 
  filter (ranking<11)

# Combine the tubes into one dataset
all_tubes <- bind_rows(tube1, tube2, tube3, tube4)


#For the journals in S1 & S4 & in more than 1 strategy n> 2 set name 

filtered_tubes <- 
  all_tubes %>%  
  mutate(Revue = str_squish(Revue)) %>% 
  group_by(Revue) %>% 
  mutate(n_strategy=n(),
         journal=case_match(Revue,.default = Revue,
                            "IEEE JOURNAL OF BIOMEDICAL AND HEALTH INFORMATICS"~"IEEE JOURNAL OF BIOMEDICAL\nAND HEALTH INFORMATICS", 	
                            "COMPUTATIONAL INTELLIGENCE AND NEUROSCIENCE"~"COMPUTATIONAL INTELLIGENCE\nAND NEUROSCIENCE",
                            "COMPUTER METHODS AND PROGRAMS IN BIOMEDICINE"~"COMPUTER METHODS\nAND PROGRAMS IN BIOMEDICINE",
                            "LECTURE NOTES IN COMPUTER SCIENCE"~"LECTURE NOTES\nIN COMPUTER SCIENCE",
                            "FRONTIERS IN NEUROINFORMATICS"~"FRONTIERS IN\nNEUROINFORMATICS",
                            "FRONTIERS IN NEUROROBOTICS"~"FRONTIERS IN\nNEUROROBOTICS"
         )) %>% 
  filter(n_strategy >=2)



color_map <- setNames(extended_palette, unique(all_tubes$Revue))

##### Figure_4: Journals #####
Figure_4 <- ggplot(all_tubes, aes(x = Strategy, y = ranking, group = Revue, color = Revue)) +
  geom_line(size = 2) +                          
  geom_point(size = 3) +                         
  scale_y_reverse(
    breaks = 1:10,
    labels = 1:10
  ) +
  scale_color_manual(values = color_map) + 
  labs(x = "Strategy", y = "Ranking") +
  theme_minimal() +                                
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 30, face = "bold",  color="black"),
    axis.title.x = element_text(size = 25, color="black"),
    axis.text.y = element_text(size = 20, color="black"),
    axis.title.y = element_text(size = 25,  color="black"),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 100)
  ) +
  geom_text_repel(
    data = filtered_tubes %>% filter(Strategy == "S1"),
    aes(label = journal),
    hjust = 0,
    nudge_x = -3,
    size = 8,
    fontface = "bold",
    color="black"  
  ) +
  geom_text_repel(
    data = filtered_tubes %>% filter(Strategy == "S4"),
    aes(label = journal),
    hjust = 1,
    nudge_x = 3,
    size = 8,
    fontface = "bold",
    color="black"
  )

# Save the plot
ggsave(plot = Figure_4, filename = here('results', 'figures', 'Figure_4.png'), width = 20, height = 12, dpi = 300)

###### Figure 5: Distribution of topics across strategies ######

#Data preparation 
strategy_totals <- strategy_topic_500 %>% 
  group_by (strategy) %>% 
  summarise (total_count=sum(count), .groups= "drop")

strategy_tcount <- strategy_topic_500 %>%
  left_join (strategy_totals, by = "strategy") %>% 
  mutate(percentage= count / total_count * 100)

# Names of the topics
strategy_tcount <- strategy_tcount %>%
  left_join(newname_topic_info_500, by = c("topic_500" = "Topic")) %>% 
  mutate(New_Name = factor(New_Name, levels = sort(unique(New_Name)))) 

# Create interactive scatterplot
plot <- strategy_tcount %>%
  ggplot(aes(x = topic_500, y = percentage, color = strategy, text = New_Name)) +
  geom_point() +
  facet_wrap(~ strategy, scales = "free") +
  custom_theme() +
  labs(
    x = "Topics",
    y = "% of Papers",
    color = "Strategy"
  )

interactive_plot <- ggplotly(plot, tooltip = "text")


Figure_5 <- ggplot(strategy_tcount, aes(x = strategy, y = reorder(New_Name, percentage), fill = percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#B9A070", guide = guide_colorbar(position = "bottom")) +  
  labs(x = "Strategy",
       y = "Topic",
       fill = "Percentage (%)") +
  custom_theme() + 
  guides(fill = guide_colorbar(
    title.position = "top", 
    title.vjust = 1,  
    barwidth = 15, 
    barheight = 1.5,
    label.theme = element_text(size = 20),
    title.theme = element_text(size = 20, hjust = 0.5)
  )) +  
  theme(
    legend.position = 'bottom',
    legend.margin = margin(t = 10, b = 10, l = 100, r = 100),
    axis.text.y = element_text(size = 15, margin = margin(t = 10, b = 10, l = 10, r = 10)),  # Ajuste de tamaño de letra
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20)
  )

# Guardar la figura
ggsave(
  plot = Figure_5, 
  filename = here('results', 'figures', 'Figure_5.png'), 
  width = 12,  
  height = 8,  
  dpi = 300
)



##### Precision #####

# List of dataframes to process
samples <- list(
  sample_s1_221_24_09_22, sample_s2_101, sample_s2_120,
  sample_s3_101, sample_s4_101, sample_s3_120, sample_s4_120, categorizedsample_s1_1000, categorizedsample_s2_1000, 
  categorizedsample_s3_1000, categorizedsample_s4_1000
)

# Function to process each dataframe
process_sample <- function(df) {
  if (all(df$Included %in% c(1, 2, NA))) {
    # If Included has 1, 2, and NA, convert 2 to 0 and leave NA as is
    df <- df %>%
      mutate(
        Included = ifelse(Included == 1, 1, ifelse(Included == 2, 0, NA)),  # Convert 2 to 0, keep NA
        binary_included = Included,  # binary_included is the same as Included
        Included = as.factor(Included)  # Convert Included to a factor
      )
  } else if (all(df$Included %in% c(TRUE, FALSE, NA))) {
    # If Included has TRUE, FALSE, and NA, convert to 1, 0, and NA
    df <- df %>%
      mutate(
        Included = as.numeric(Included),  # TRUE -> 1, FALSE -> 0, NA -> NA
        binary_included = Included,  # binary_included is the same as Included
        Included = as.factor(Included)  # Convert Included to a factor
      )
  }
  return(df)
}

# Apply the function to all dataframes
samples <- lapply(samples, process_sample)

# Extract the processed dataframes
sample_s1_221_24_09_22 <- samples[[1]]
sample_s2_101 <- samples[[2]]
sample_s2_120 <- samples[[3]]
sample_s3_101 <- samples[[4]]
sample_s4_101 <- samples[[5]]
sample_s3_120 <- samples[[6]]
sample_s4_120 <- samples[[7]]
categorizedsample_s1_1000 <- samples[[8]]
categorizedsample_s2_1000 <- samples[[9]]
categorizedsample_s3_1000 <- samples[[10]]
categorizedsample_s4_1000 <- samples[[11]]

# Unify samples
sample_s2_221 <- bind_rows(sample_s2_101, sample_s2_120)
sample_s3_221 <- bind_rows(sample_s3_101, sample_s3_120)
sample_s4_221 <- bind_rows(sample_s4_101, sample_s4_120)

# Combine final samples (ignore NA in later calculations)

sample_s1_combined <- bind_rows(sample_s1_221, categorizedsample_s1_1000)
sample_s2_combined <- bind_rows(sample_s2_221, categorizedsample_s2_1000)
sample_s3_combined <- bind_rows(sample_s3_221, categorizedsample_s3_1000)
sample_s4_combined <- bind_rows(sample_s4_221, categorizedsample_s4_1000)

# The samples are:

sample_s1_combined 
sample_s2_combined 
sample_s3_combined 
sample_s4_combined

# drop NA rows in binary_included

sum(!is.na(sample_s1_combined$binary_included))
sum(!is.na(sample_s2_combined$binary_included))
sum(!is.na(sample_s3_combined$binary_included))
sum(!is.na(sample_s4_combined$binary_included))


sample_s1_combined <- sample_s1_combined %>% filter(!is.na(binary_included))
sample_s2_combined <- sample_s2_combined %>% filter(!is.na(binary_included))
sample_s3_combined <- sample_s3_combined %>% filter(!is.na(binary_included))
sample_s4_combined <- sample_s4_combined %>% filter(!is.na(binary_included))

# Precision table 

precision_table <- tibble(
  Strategy = c("S1", "S2", "S3", "S4"),
  Data = list(sample_s1_combined, sample_s2_combined, sample_s3_combined, sample_s4_combined)
) %>%
  rowwise() %>%
  mutate(
    Positive_OST_BK = sum(Data$binary_included == 1),
    Total_OST_BK = nrow(Data),
    Precision= Positive_OST_BK / Total_OST_BK * 100
  ) %>%
  ungroup() %>%
  select(Strategy, Positive_OST_BK, Precision)

# Confidence intervals 

mean.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function 
  return(mean(d))  
}

BootDist_s1 <- boot(data = sample_s1_combined$binary_included, statistic = mean.function, R = 10000)
BootDist_s2 <- boot(data = sample_s2_combined$binary_included, statistic = mean.function, R = 10000)
BootDist_s3 <- boot(data = sample_s3_combined$binary_included, statistic = mean.function, R = 10000)
BootDist_s4 <- boot(data = sample_s4_combined$binary_included, statistic = mean.function, R = 10000)

# Obtain confidence intervals for each strategy
ci_s1 <- boot.ci(BootDist_s1, type = "perc")
ci_s2 <- boot.ci(BootDist_s2, type = "perc")
ci_s3 <- boot.ci(BootDist_s3, type = "perc")
ci_s4 <- boot.ci(BootDist_s4, type = "perc")

ci_s1
ci_s2
ci_s3
ci_s4


##### Pseudo-Recall#####

#Considering that recall= relevant papers retrieved / total relevant papers

unified_sample <-  bind_rows(sample_s1_combined, sample_s2_combined, sample_s3_combined, sample_s4_combined)

unified_sample$binary_included <- ifelse(unified_sample$Included == 1, 1, 0)

#check and drop duplicated OST_BK

sum(duplicated(unified_sample$OST_BK))

unified_sample <- unified_sample %>%
  distinct(OST_BK, .keep_all = TRUE)

sum(duplicated(unified_sample$OST_BK))

tot_relevant <- unified_sample %>% filter(binary_included == 1)
total_relevant_count <- nrow(tot_relevant)

relevant_S1 <- S1%>%
  inner_join(tot_relevant, by = "OST_BK") %>% nrow ()

recall_S1 <-  (relevant_S1/total_relevant_count) 

relevant_S2 <- S2%>%
  inner_join(tot_relevant, by = "OST_BK") %>% nrow ()

recall_S2 <-  (relevant_S2/total_relevant_count) 

relevant_S3 <- S3%>%
  inner_join(tot_relevant, by = "OST_BK") %>% nrow ()

recall_S3 <-  (relevant_S3/total_relevant_count) 

relevant_S4 <- S4%>%
  inner_join(tot_relevant, by = "OST_BK") %>% nrow ()

recall_S4 <-  (relevant_S4/total_relevant_count) 

recall_S1
recall_S2
recall_S3
recall_S4


recall_data <- data.frame(
  Strategy = c("S1", "S2", "S3", "S4"),
  Recall = c(recall_S1*100, recall_S2*100, recall_S3*100, recall_S4*100)
)

# CI

relevantS1_totrelevant <- tot_relevant %>%
  left_join(S1, by = "OST_BK") %>%
  mutate(relevantS1 = ifelse(is.na(strategy.y), 0, ifelse(strategy.y == "S1", 1, 0))) %>%
  select(OST_BK, relevantS1)  

relevantS2_totrelevant <- tot_relevant %>%
  left_join(S2, by = "OST_BK") %>%
  mutate(relevantS2 = ifelse(is.na(strategy.y), 0, ifelse(strategy.y == "S2", 1, 0))) %>%
  select(OST_BK, relevantS2)  

relevantS3_totrelevant <- tot_relevant %>%
  left_join(S3, by = "OST_BK") %>%
  mutate(relevantS3 = ifelse(is.na(strategy.y), 0, ifelse(strategy.y == "S3", 1, 0))) %>%
  select(OST_BK, relevantS3)  

relevantS4_totrelevant <- tot_relevant %>%
  left_join(S4, by = "OST_BK") %>%
  mutate(relevantS4 = ifelse(is.na(strategy.y), 0, ifelse(strategy.y == "S4", 1, 0))) %>%
  select(OST_BK, relevantS4)  

# Confidence intervals 

mean.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function 
  return(mean(d))  
}

BootDist_s1_recall <- boot(data = relevantS1_totrelevant$relevantS1, statistic = mean.function, R = 10000)
BootDist_s2_recall <- boot(data = relevantS2_totrelevant$relevantS2, statistic = mean.function, R = 10000)
BootDist_s3_recall <- boot(data = relevantS3_totrelevant$relevantS3, statistic = mean.function, R = 10000)
BootDist_s4_recall <- boot(data = relevantS4_totrelevant$relevantS4, statistic = mean.function, R = 10000)

# Obtain confidence intervals for each strategy
ci_s1_recall <- boot.ci(BootDist_s1_recall, type = "perc")
ci_s2_recall <- boot.ci(BootDist_s2_recall, type = "perc")
ci_s3_recall <- boot.ci(BootDist_s3_recall, type = "perc")
ci_s4_recall <- boot.ci(BootDist_s4_recall, type = "perc")

ci_s1_recall
ci_s2_recall
ci_s3_recall
ci_s4_recall


