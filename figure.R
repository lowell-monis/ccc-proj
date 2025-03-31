library(ggplot2)
library(patchwork)
library(ggthemes)
library(tidyverse)
library(patchwork)
library(tidytext)

ccc1 <- read.csv("data/ccc_compiled_20172020.csv")
ccc1 <- ccc1 %>% 
  select(-starts_with("source"))

ccc2 <- read.csv("data/ccc_compiled_20212024.csv")
ccc2$issues <- ccc2$issue_tags
ccc2 <- ccc2 %>% 
  select(-starts_with("source"))
ccc <- bind_rows(ccc1, ccc2)


ccc <- ccc %>%
  separate_wider_delim(date,
                       delim="-",
                       names=c('yyyy', 'mm', 'dd')) %>%
  separate_longer_delim(issues, delim=";") %>%
  separate_longer_delim(type, delim=";")

issue_counts <- ccc %>%
  filter(yyyy %in% c("2018", "2022"), !is.na(valence), issues != "") %>%
  count(yyyy, issues, valence) %>%
  group_by(yyyy, issues) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

top_issues_2018 <- issue_counts %>%
  filter(yyyy == "2018") %>%
  slice_max(order_by = n, n = 10) %>%
  pull(issues)

top_issues_2022 <- issue_counts %>%
  filter(yyyy == "2022") %>%
  slice_max(order_by = n, n = 10) %>%
  pull(issues)

issue_counts_filtered <- ccc %>%
  filter((yyyy == "2018" & issues %in% top_issues_2018) | 
           (yyyy == "2022" & issues %in% top_issues_2022), 
         !is.na(valence)) %>%
  count(yyyy, issues, valence)

midterm_data1 <- ccc %>%
  filter(yyyy == "2018", !is.na(valence)) %>%
  mutate(date = as.Date(paste(yyyy, mm, dd, sep = "-")))

midterm_data2 <- ccc %>%
  filter(yyyy == "2022", !is.na(valence)) %>%
  mutate(date = as.Date(paste(yyyy, mm, dd, sep = "-")))

custom_theme <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "#666666", linewidth = 0.5),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    axis.title = element_text(size = 15, color = "#444444"),
    axis.text = element_text(size = 10, color = "#555555"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(color = "#333333", face = "bold")
  )

valence_colors <- c("0" = "#E69F00", 
                    "1" = "#0072B2",  
                    "2" = "#D55E00")  

issue_plot <- ggplot(issue_counts_filtered, 
                     aes(x = reorder_within(issues, n, yyyy), 
                         y = n, 
                         fill = factor(valence))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_wrap(~ yyyy, scales = "free_y") +
  scale_fill_manual(values = valence_colors,
                    labels = c("0" = "Neither/Other", 
                               "1" = "Left-wing", 
                               "2" = "Right-wing")) +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "There has been a significant shift in issues alongside changes \nin partisan involvement reflective of the national mood in protests.",
       x = "Issue",
       y = "Number of Events",
       fill = "Political Alignment") +
  custom_theme +
  theme(panel.grid.major.y = element_blank()) 

max_count1 <- max(table(midterm_data1$date, midterm_data1$valence))
max_count2 <- max(table(midterm_data2$date, midterm_data2$valence))
y_max <- max(max_count1, max_count2) * 1.1

midterm2018 <- ggplot(midterm_data1, aes(x = date, color = factor(valence))) +
  geom_line(stat = "count", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2018-11-06"), linetype = "dashed", 
             color = "#333333", linewidth = 0.5) +
  annotate("text", x = as.Date("2018-11-02"), 
           y = 4000, 
           label = "Election Day", 
           color = "#222222", 
           angle = 90,
           hjust = -0.2, 
           vjust = 0.2, 
           size = 3.8,
           family='serif') +
  annotate("rect", 
           xmin = as.Date("2018-02-14"), 
           xmax = as.Date("2018-04-22"),
           ymin = 0, 
           ymax = Inf,
           fill = "#F39C12", 
           alpha = 0.15) +
  annotate("text", 
           x = as.Date("2018-04-28"), 
           y = 9400,
           label = "Parkland Shooting Fallout", 
           color = "#222222", 
           size = 3.5, 
           angle = 0,  
           hjust = 1.1,
           vjust = 0.5,
           family = "serif") +
  scale_color_manual(values = valence_colors) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(title = "2018 saw a lopsided number of left-leaning political events.",
       x = "Date",
       y = "Number of Protests") +
  custom_theme +
  theme(legend.position = "none", 
        axis.title.x = element_blank())


midterm2022 <- ggplot(midterm_data2, aes(x = date, color = factor(valence))) +
  geom_line(stat = "count", size = 0.7) +
  geom_vline(xintercept = as.Date("2022-06-24"), linetype = "dashed", color = "black", size=0.5) +
  annotate("text", x = as.Date("2022-06-20"), y = 4000, label = "Dobbs Decision", 
           color = "#222222", 
           angle = 90,
           hjust = -0.2,
           vjust = 0.2,
           size = 3.8,
           family='serif') +
  geom_vline(xintercept = as.Date("2022-11-08"), linetype = "dashed", color = "black", size=0.5) +
  annotate("text", x = as.Date("2022-11-04"), y = 4000, label = "Election Day", color = "#222222", 
           angle = 90,
           hjust = -0.2, 
           vjust = 0.2, 
           size = 3.8,
           family='serif')  +
  scale_color_manual(values = valence_colors) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(title = "There were a smaller number of events in 2022, with disparities between \nleft- and right-wing events reduced but still favoring the left.",
       x = "Date",
       y = "Number of Protests") +
  custom_theme +
  theme(legend.position = "none", 
        axis.title.x = element_blank())

combined_plot <- (midterm2018 / midterm2022 | issue_plot) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    caption = "Data source: Crowd Counting Consortium, crowdcounting.org, accessed March 2025",
    theme = theme(
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5, 
                                color = "#111111", margin = margin(b = 15)),
      plot.caption = element_text(size = 15, hjust = 0, color = "#666666"),
      plot.background = element_rect(fill = "#FAFAFA"))) &
  theme(legend.text = element_text(size = 10))



ggsave(
  "figure.png",
  plot = combined_plot,
  width=20, height=13)
