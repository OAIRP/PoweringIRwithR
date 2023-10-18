# install packages

install.packages("tidyverse")
install.packages("readr")
install.packages("curl")
install.packages("kableExtra")
install.packages("gridExtra")


# load libraries
library("tidyverse")
library("readr")
library("curl")
library("dplyr")
library("knitr")
library("kableExtra")



# download and process data
IPEDS_url <- "https://nces.ed.gov/ipeds/datacenter/data/"
IPEDS_year <- 2022

curl_download(
  paste0(IPEDS_url, "HD", IPEDS_year, ".zip"),
  destfile = paste0("HD", IPEDS_year, ".zip")
)

curl_download(
  paste0(IPEDS_url, "C", IPEDS_year, "_A", ".zip"),
  destfile = paste0("C", IPEDS_year, "_A", ".zip")
)

# read in IPEDS IC Header data
institutions <- read_csv(
  paste0("HD", IPEDS_year, ".zip"),
)  %>%
  
  # select variables of interest
  select(UNITID, INSTNM, OPEID, STABBR, SECTOR, HLOFFER) %>%
  
  # create new variables
  mutate(
    OPEID_branch = substr(OPEID, 7, 8),
  )

# read in IPEDS Completions data
completions <- read_csv(
  paste0("C", IPEDS_year, "_A.zip"),
) %>%
  
  # select variables of interest
  select(UNITID, CIPCODE, MAJORNUM, AWLEVEL, CTOTALT) %>%
  
  # filter first majors only
  filter(MAJORNUM == 1)

# combine data sets
combined <- full_join(institutions, completions, by = "UNITID", keep = TRUE) %>%
  
  # filter to Ohio institutions
  filter(STABBR == "OH")

#code to explore the combined data set

#displaying all of the codes that are used for the different sectors in the data
distinct_sectors <- unique(combined$SECTOR)
distinct_sectors

#recode the sectors column

combined <- combined %>%
  mutate(SECTOR_TEXT = case_when(
    SECTOR == 0 ~ 'Admin Unit',
    SECTOR == 1 ~ 'Pub 4-year+',
    SECTOR == 2 ~ 'Pri NP 4-year+',
    SECTOR == 3 ~ 'Pri FP 4-year+',
    SECTOR == 4 ~ 'Pub 2-Year',
    SECTOR == 5 ~ 'Pri NP 2-year',
    SECTOR == 6 ~ 'Pri FP 2-year',
    SECTOR == 7 ~ 'Pub <2-year',
    SECTOR == 8 ~ 'Pri NP <2-year',
    SECTOR == 9 ~ 'Pri FP <2-year',
    TRUE ~ as.character(SECTOR)  # If none of the above conditions are met, keep the original value
  ))

# Create a data frame with unique sector descriptions
sector_descriptions <- unique(combined[, c("SECTOR", "SECTOR_TEXT")])

# Rename the columns for clarity
colnames(sector_descriptions) <- c("Sector Code", "Sector Description")

# Arrange the data frame numerically by the "Sector Code" column
sector_descriptions <- sector_descriptions %>% arrange(`Sector Code`)

# Create a table using kable
sector_table <- kable(sector_descriptions, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Print the table
sector_table


#create a bar chart to display the number of OH institutions by sector

# Group the data by SECTOR and count unique institutions (via the ID)
sector_counts <- combined %>%
  group_by(SECTOR) %>%
  summarize(Unique_Institutions = n_distinct(UNITID.x))


# Most basic Bar chart to show the distribution of institutions by sector
ggplot(sector_counts, aes(x = factor(SECTOR), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count")

# Reorder the bars by the count of institutions from highest to lowest
sector_counts <- sector_counts %>%
  arrange(desc(Unique_Institutions))

# Create the bar chart after reordering
ggplot(sector_counts, aes(x = reorder(factor(SECTOR), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count")


# Add in data labels
ggplot(sector_counts, aes(x = reorder(factor(SECTOR), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Unique_Institutions), vjust = -0.5, size = 3) +  # Add data labels
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count")

# Use the sector categories that we created earlier
# Adding the sector descriptions using a "case when" into our sector counts
sector_counts <- sector_counts %>%
  mutate(SECTOR_TEXT = case_when(
    SECTOR == 0 ~ 'Admin Unit',
    SECTOR == 1 ~ 'Pub 4-year+',
    SECTOR == 2 ~ 'Pri NP 4-year+',
    SECTOR == 3 ~ 'Pri FP 4-year+',
    SECTOR == 4 ~ 'Pub 2-Year',
    SECTOR == 5 ~ 'Pri NP 2-year',
    SECTOR == 6 ~ 'Pri FP 2-year',
    SECTOR == 7 ~ 'Pub <2-year',
    SECTOR == 8 ~ 'Pri NP <2-year',
    SECTOR == 9 ~ 'Pri FP <2-year',
    TRUE ~ as.character(SECTOR)
  ))



# Adding the sector descriptions to our bar chart
ggplot(sector_counts, aes(x = reorder(factor(SECTOR_TEXT), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Unique_Institutions), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8)) # Rotate x-axis labels by 40 degrees



# Edit the bar color and remove grey grid background
ggplot(sector_counts, aes(x = reorder(factor(SECTOR_TEXT), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Unique_Institutions), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count") +
  # Rotate x-axis labels by 45 degrees
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
    panel.background = element_blank()
  )

# Define a custom bar color 
# I searched for CSCC's blue and found this on our marketing site
#(https://www.cscc.edu/employee/communications/marketing/branding.shtml)
custom_blue_color <- "#007298"  # Replace with any color code

# Bar chart with CSCC blue :)
ggplot(sector_counts, aes(x = reorder(factor(SECTOR_TEXT), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = custom_blue_color) +
  geom_text(aes(label = Unique_Institutions), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count") +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
    panel.background = element_blank()
  )


# add in a caption, a box around the chart, and adjust axis titles
#This is the final version of our bar chart
ggplot(sector_counts, aes(x = reorder(factor(SECTOR_TEXT), -Unique_Institutions), y = Unique_Institutions)) +
  geom_bar(stat = "identity", fill = custom_blue_color, color = "black") +  # Add a border color
  geom_text(aes(label = Unique_Institutions), vjust = -0.25, size = 3) +
  labs(title = "Distribution of Ohio Institutions by Sector", x = "Sector", y = "Count of Unique Institutions", caption = "Data Source: IPEDS Data Center") +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
    panel.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10), color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black"),
    axis.text = element_text(color = "black")  
  ) + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "black", fill = NA, alpha = 0)


# create a new chart that displays awards conferred by CIP Code
# for simplicity's sake, we'll just look at the public sector institutions

# Create sector labels
sector_labels <- c(
  '0' = 'Administrative Unit',
  '1' = 'Public 4-year or above',
  '2' = 'Private nonprofit 4-year or above',
  '3' = 'Private for-profit 4-year or above',
  '4' = 'Public 2-Year',
  '5' = 'Private nonprofit 2-year',
  '6' = 'Private for-profit 2-year',
  '7' = 'Public less-than-2-year',
  '8' = 'Private nonprofit less-than-2-year',
  '9' = 'Private for-profit less-than-2-year',
  '99' = 'Sector unknown'
)

# Sum the awards (CTOTALT) by CIPCODE within each sector
sector_awards <- combined %>%
  group_by(SECTOR, CIPCODE) %>%
  summarize(TotalAwards = sum(CTOTALT)) %>%
  arrange(desc(TotalAwards))

# Get the top 5 CIP Codes by awards conferred for the public sector
# I decided to exclude CIP Code 99 - Other
# That CIP Code isn't very descriptive, but we could easily leave it in
top5_awards <- sector_awards %>%
  filter(SECTOR %in% c(1, 4, 7), CIPCODE != '99') %>%
  group_by(SECTOR) %>%
  slice_head(n = 5)

# Create horizontal bar charts for the public sector institutions
ggplot(data = top5_awards, aes(x = TotalAwards, y = reorder(CIPCODE, -TotalAwards))) +
  geom_bar(stat = "identity", aes(fill = as.factor(SECTOR))) +
  geom_text(aes(label = TotalAwards), hjust = -0.25, size = 3, color = "black") + 
  facet_wrap(~SECTOR, scales = "free_y", labeller = labeller(SECTOR = sector_labels)) +
  labs(title = "Top 5 CIP Codes by Number of Awards Conferred in the Public Sector",
       x = "Total Awards",
       y = "CIPCODE") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#003E52", "#99DAEA", "#646569"))
