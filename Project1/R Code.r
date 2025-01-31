#Step 1: Importing Our Libraries----

library(readr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(dplyr)

#Reading The Data----
data <- read_csv("pineapple.csv")

#Setting The Theme----

theme_set(theme_ipsum(base_family = "Helvetica",
                      plot_title_size = 17.5,
                      base_size = 12.5, axis_title_size = 15))

#Understanding The Data----

##Key Features Of The Data

data

##Number of Columns (Variables)

col_n <- length(data)

print(col_n)

##Number Of Rows (Observations)

row_n <- n_distinct(data)

print(row_n)

##Number of unique cases (clusters of rows referring to the same entity)

cases <- n_distinct(data$User_ID)

print(cases)

##Identifying Missing Values

summary(data)

any(is.na(data))

## Counting occurrences by variable (Columns)

options(dplyr.print_max = Inf)

for (col_name in colnames(data)[-1]) {
  observation <- data |>
    group_by(.data[[col_name]]) |>
    count()
  print(paste("Observation for", col_name, ":"))
  print(observation)
}

#Plotting The Data----

##Plot 1: User Behaviour Boxplot - to showcase the impact ofoutlier values

g1 <- ggplot() +
  geom_boxplot(aes(x = "1.Screen Time", y = data$Screen_On_Time),
               fill = "#B3E5FC", color = "#B3E5FC") +
  geom_boxplot(aes(x = "2.Apps Installed", y = data$Apps_Installed),
               fill = "#81D4FA", color = "#4D4D4D") +
  geom_boxplot(aes(x = "3.App Usage", y = data$App_Usage_Time),
               fill = "#4FC3F7", color = "#4D4D4D") +
  geom_boxplot(aes(x = "4.Data Usage", y = data$Data_Usage),
               fill = "#0288D1", color = "#4D4D4D") +
  geom_boxplot(aes(x = "5.Battery Drain", y = data$Battery_Drain),
               fill = "#01579B", color = "#4D4D4D") +
  labs(title = "Not All Variables Are Created Equal ", x = NULL,
       y = "Value Range") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.75)) +
  theme(axis.title.y = element_text(hjust = 0.5))

##Plot 2: GOAT chart

datac2 <- data %>%
  mutate(age_group = case_when(
                               Age >= 18 & Age <= 27 ~ "Gen Z",
                               Age >= 28 & Age <= 43 ~ "Gen Y",
                               Age >= 44 & Age <= 59 ~ "Gen X"))

datac2mean <- datac2 %>%
  group_by(Device_Model, age_group, Gender) %>%
  summarize(avg_screen_time = mean(Screen_On_Time))

datac2mean <- datac2mean %>%
  mutate(age_group = factor(age_group, levels = c("Gen Z", "Gen Y", "Gen X")))

g2 <- ggplot(datac2mean, aes(x = age_group, y = avg_screen_time,
                             group = Gender, color = Gender)) +
  geom_line(aes(), linewidth = 1) +
  scale_color_manual(values = c("Male" = "#0066CC", "Female" = "#FF69B4")) +
  facet_grid(~ Device_Model, switch = "x") +
  labs(title = "Tapping Trends", x = NULL, y = "Screen Time",
       color = NULL, subtitle = "Smartphone usage differs dramatically across people of different Age, Gender or Device Model") +
  theme(
        strip.placement = "outside",
        strip.text.x = element_text(size = 12.5, hjust = 0.5),
        axis.title.y = element_text(size = 15, hjust = 0.5),
        legend.position = "top",
        panel.spacing = unit(1, "lines"))

#Small GOAT plot

data_long <- data %>%
  filter(Gender == "Female") %>%
  select(Operating_System, Apps_Installed, App_Usage_Time) %>%
  pivot_longer(cols = c(Apps_Installed, App_Usage_Time),
               names_to = "Variable", values_to = "Value")

# Calculate overall mean values for each variable
overall_means <- data_long %>%
  group_by(Variable) %>%
  summarize(overall_mean = mean(Value, na.rm = TRUE))

variable_names <- c("App_Usage_Time" = "App Usage Time", "Apps_Installed" = "Apps Installed")


# Plot with density, custom colors, and overall mean lines
g3 <- ggplot(data_long, aes(x = Value, fill = Operating_System)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free", labeller = labeller(Variable = variable_names)) +
  scale_fill_manual(values = c("iOS" = "#67a9cf", "Android" = "#ef8a62")) +
  labs(title = "An Apple a day keeps your phone from being away",
       x = "Variable Value", y = "Density", fill = "Operating System") +
  theme(legend.position = "top",
        axis.title.y = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(size = 15, hjust = 0.5)) +
  geom_vline(data = overall_means, aes(xintercept = overall_mean),
             linetype = "dotted", color = "black", linewidth = 0.75)
