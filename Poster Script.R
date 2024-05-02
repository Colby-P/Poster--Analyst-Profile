#+ Poster Script
#+ 1 May 2024

# Load Packages
library(tidyverse)
library(readxl)
library(knitr)
library(patchwork)
library(modelsummary)
library(lfe)

# Load Data
df = read_excel('STATcompiler_Cleaned.xlsx')

# Format Variable Types
df1 = as.numeric(df$Survey) # Converts character to numeric format
df2 = as.numeric(df$Child_Mort) 

df =
  df |>
  select(-Survey, -Child_Mort) |> 
  mutate(
    Year = df1,
    Child_Mort = df2
  )

df = df[, c(1, 23, 24, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, # Reorder variables
             13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]


# Data Analysis
summary(df$Child_Mort)
summary(df$Year)
summary(df$Water_Total)
summary(df$Water_Urban)
summary(df$Water_Rural)

# Child Mortality vs. Water Access Plots
p1 = 
  df |>
    ggplot(aes(x = Water_Total, y = Child_Mort)) +
    geom_point(shape = 21, color = 'red') +
    geom_smooth(color = 'black', method = 'lm', se = F, linewidth = 1, linetype = 5) +
    labs(
      x = 'Coverage',
      y = 'Child Mortality',
      title = 'Combined'
    ) +
  scale_y_continuous(
    limits = c(0, 350), 
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.margin = margin(10, 10, 10, 10)
  )

p1

p2 = 
  df |>
  ggplot(aes(x = Water_Urban, y = Child_Mort)) +
  geom_point(shape = 21, color = 'blue') +
  geom_smooth(color = 'black', method = 'lm', se = F, linewidth = 1, linetype = 5) +
  labs(
    x = 'Coverage',
    y = 'Child Mortality',
    title = 'Urban'
  ) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 12)

p3 = 
  df |>
  ggplot(aes(x = Water_Rural, y = Child_Mort)) +
  geom_point(shape = 21, color = 'chartreuse3') +
  geom_smooth(color = 'black', method = 'lm', se = F, linewidth = 1, linetype = 5) +
  labs(
    x = 'Coverage',
    y = 'Child Mortality',
    title = 'Rural'
  ) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 12)

patch = 
  p1 + p2 + p3 +
  plot_annotation(
    title = 'Child Mortality Compared to Improved Drinking Water Source Coverage (%)'
  ) &
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )

patch

# Child Mortality vs. Time Plot

p5 =
  df |>
  ggplot(aes(x = Year, y = Child_Mort)) +
  geom_point(color = 'firebrick3', shape = 21) +
  geom_smooth(color = 'black', method = 'lm', se = F, linewidth = 1) +
  labs(
    x = 'Survey Year',
    y = 'Child Mortality',
    title = 'Child Mortality Over Time'
  ) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0)
  ) + 
  scale_x_continuous(
    limits = c(1980, 2025),
    breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
    labels = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 12),
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.major.y = element_line(color = 'gray', linewidth = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks = element_blank()
  )

p5

# Linear Regression 
mods = list(
  '(1)' = felm(Child_Mort ~ Water_Total | Country, df),
  '(2)' = felm(Child_Mort ~ Water_Total + Sanitation_Total | Country, df),
  '(3)' = felm(Child_Mort ~ Water_Total + Sanitation_Total + Lowest_Wealth_Urban + Lowest_Wealth_Rural + Household_Size_Total | Country, df)
  )

modelsummary(mods, fmt = 3, gof_map = 'nobs', statistic = NULL,
             stars = c('*' = 0.05),
             title = 'Water and Sanitation Access Affect Child Mortality Outcomes',
             coef_map = c(
               'Water_Total' = 'Water Access',
               'Sanitation_Total' = 'Sanitation Access',
               'Lowest_Wealth_Urban' = 'Lowest Wealth Qu. (Urban)',
               'Lowest_Wealth_Rural' = 'Lowest Wealth Qu. (Rural)',
               'Household_Size_Total' = 'Household Size'
             )
)
