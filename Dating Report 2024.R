
# create plot of dates by month
path <- "https://docs.google.com/spreadsheets/d/1C4uEC3RFWRc8mEjg-gC7vDNeqQ9o9HNbBx3oX2JIkwI/edit?gid=92338529#gid=92338529"
month_totals <- googlesheets4::read_sheet(path, sheet = "Totals")
marrieds <- googlesheets4::read_sheet(path, sheet = "Married/Not Married") %>%
  janitor::clean_names() %>%
  select(year, status)

funnel <- googlesheets4::read_sheet(path, sheet = "All Dates") %>%
  janitor::clean_names()

cluster <- googlesheets4::read_sheet(path, sheet = "2024 Clustering") %>%
  janitor::clean_names() %>%
  select(-hometown_distance_from_american_fork) %>%
  tibble::column_to_rownames(var = "name")


# may need to run gs4_deauth() if it throws an auth error

month_totals <- month_totals %>%
  janitor::clean_names() %>%
  mutate(month = factor(month, levels = month.name),
         month_num = match(month, month.name),
         year = year %>% factor)

month_totals %>% str

month_totals %>%
  # filter(year == "2024") %>%
  ggplot() +
  geom_line(aes(x = month_num, y = count, color = year)) +
  ylim(0, 8) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  # xlim(1, 12) +
  labs(x = "Month",
       y = "Count",
       title = "Dates by Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## oooooffff chaos

# find mean for each month
month_avg <- month_totals %>%
  group_by(month) %>%
  summarise(mean = count %>% mean)

month_totals %>%
  filter(year == 2024) %>%
  left_join(month_avg) %>%
  ggplot() +
  geom_line(aes(x = month_num, y = count), color = "navy") +
  geom_line(aes(x = month_num, y = mean), color = "red3", alpha = 0.2) +
  geom_text(
    aes(x = max(month_num), y = count[month_num == max(month_num)], label = "2024"),
    color = "navy", hjust = -0.1, size = 2.5
  ) +
  geom_text(
    aes(x = max(month_num), y = mean[month_num == max(month_num)], label = "5 year average"),
    color = "red3", hjust = -0.1, size = 2.5
  ) +
  ylim(0, 8) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    limits = c(1, 12),
    expand = expand_scale(mult = c(0, 0.08),
                          add = c(.5, 1))
  ) +
  labs(x = "Month",
       y = "Count",
       title = "Dates by Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())




# dating and other activities
month_totals %>%
  filter(year == 2024) %>%
  ggplot() +
  geom_line(aes(x = month_num, y = count), color = "navy") +
  geom_line(aes(x = month_num, y = fishing_trips), color = "indianred1") +
  geom_line(aes(x = month_num, y = fish_caught), color = "steelblue") +
  geom_line(aes(x = month_num, y = miles_ran), color = "indianred3") +
  geom_text(
    aes(x = max(month_num), y = count[month_num == max(month_num)], label = "Dates"),
    color = "navy", hjust = -0.1, size = 3
  ) +
  geom_text(
    aes(x = max(month_num), y = fishing_trips[month_num == max(month_num)], label = "Fishing Trips"),
    color = "indianred1", hjust = -0.1, size = 3, vjust = 1
  ) +
  geom_text(
    aes(x = max(month_num), y = fish_caught[month_num == max(month_num)], label = "Fish Caught"),
    color = "steelblue", hjust = -0.1, size = 3
  ) +
  geom_text(
    aes(x = max(month_num), y = miles_ran[month_num == max(month_num)], label = "Miles Ran"),
    color = "indianred3", hjust = -0.1, size = 3
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    limits = c(1, 12),
    expand = expand_scale(mult = c(0, 0.08),
                          add = c(.5, 1))
  ) +
  labs(x = "Month",
       y = "Count",
       title = "Dates by Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())



# activities correlation plot

month_totals %>%
  filter(year == 2024) %>%
  select(count, fishing_trips, fish_caught, miles_ran) %>%
  cor %>%
  corrplot::corrplot.mixed()


M <- month_totals %>%
  filter(year == 2024) %>%
  select(count, fishing_trips, fish_caught, miles_ran) %>%
  cor

colnames(M) <- c("Dates", "Fishing Trips", "Fish Caught", "Miles Ran")
corrplot::corrplot.mixed(M)



# year average
month_totals %>%
  group_by(year) %>%
  summarise(mean = count %>% mean)


# marrieds plot
marrieds %>%
  group_by(year, status) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  View()

marrieds %>%
  group_by(status) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


# marrieds plot
tibble(
  year = seq(from = 2018, to = 2024),
  prop = c(0.667, 0.667, 0.8, 0.48, 0.272, 0.2, 0)
) %>%
  mutate(percent = scales::percent(prop)) %>%
  ggplot() +
  geom_line(aes(x = year, y = prop), color = "navy") +
  geom_label(aes(x = year, y = prop, label = percent),
            color = "navy", size = 3.5) +
  scale_x_continuous(
    breaks = 2018:2024) +
  labs(x = "Year", y = "Proportion", title = "Proportion of Dates Now Married or Engaged") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# conversion funnel
p1 <- funnel %>%
  group_by(number) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(number, n), y = n, fill = number)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  geom_text(aes(label = n),
            hjust = -0.5,  # Adjust label position to the right of the bar
            size = 3,      # Adjust label size
            color = "black") +
  labs(y = "Count", x = "Date Number", title = "Revisits as Technicalities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())

# adjusted
p2 <- funnel %>%
  mutate(number = ifelse(!is.na(revisit_adj), revisit_adj, number)) %>%
  group_by(number) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(number, n), y = n, fill = number)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  geom_text(aes(label = n),
            hjust = -0.5,  # Adjust label position to the right of the bar
            size = 3,      # Adjust label size
            color = "black") +
  labs(y = "Count", x = "Date Number", title = "Revisits as First Dates") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


gridExtra::grid.arrange(p1, p2, ncol = 2, bottom = "Count",
                        top = grid::textGrob("Dating Conversion Funnel",
                                             gp = grid::gpar(fontsize = 18)))



# cluster analysis

cluster_scaled <- scale(cluster) %>% data.frame
kclusts <- kmeans(cluster_scaled, centers = 4)

pca <- prcomp(cluster_scaled, scale = TRUE,
              center = TRUE, retx = T)

pca %>% summary
# pc 1 and 2 haad .3535 and .2104

pca$rotation # print out the components

# get components for each girl
pca$x

# update df
cluster_scaled <- cluster_scaled %>%
  mutate(pc1 = pca$x[,1],
         pc2 = pca$x[,2],
         cluster = kclusts$cluster %>% factor,
         initials = c("AP", "JM", "AA", "AL", "RR", "TL", "CW", "LW", "ET", "AC"))

# plot it
cluster_scaled %>%
  ggplot() +
  geom_text(aes(x = pc1,
                y = pc2,
                color = cluster,
                label = initials),
            size = 4) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title = "Selection of 2024 Dating Candidates",
       color = "Cluster") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))








