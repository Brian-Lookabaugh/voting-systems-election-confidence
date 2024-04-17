########################################################################
################## SPAE Confidence Over Time Graphic ###################
########################################################################

pacman::p_load(
  "dplyr", # Data Management
  "ggplot2", # Visualization
  "ggrepel", # Labels
  "extrafont", # Franklin Gothic Bold Font
  install = FALSE
)

# Develop a Custom Theme - Custom Fors Marsh Theme (Load Custom Fors Marsh Font)
loadfonts(quiet = T)

theme.fm <- function() {
  theme_minimal(base_family = "Franklin Gothic Demi Cond") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

# Make Labels Use Franklin Gothic by Default
update_geom_defaults("label_repel", 
                     list(family = "Franklin Gothic Demi Cond",
                          fontface = "bold"))
update_geom_defaults("label", 
                     list(family = "Franklin Gothic Demi Cond",
                          fontface = "bold"))

# Data Cleaning
spae <- spae %>% 
  # Invert Confidence Measures (1 = Less Confident, 4 = More Confident)
  mutate(
    conf.personal = as.integer(conf.personal),
    conf.county = as.integer(conf.county),
    conf.state = as.integer(conf.state),
    conf.nation = as.integer(conf.nation)
  ) %>%
  mutate(conf.personal = recode(conf.personal, '1' = 4, '2' = 3, '3' = 2, '4' = 1, .default = as.double(conf.personal)),
         conf.county = recode(conf.county, '1' = 4, '2' = 3, '3' = 2, '4' = 1, .default = as.double(conf.county)),
         conf.state = recode(conf.state, '1' = 4, '2' = 3, '3' = 2, '4' = 1, .default = as.double(conf.state)),
         conf.nation = recode(conf.nation, '1' = 4, '2' = 3, '3' = 2, '4' = 1, .default = as.double(conf.nation)
  )) %>%
  # Convert "Don't Know" Values to NA
  mutate(
    conf.personal = ifelse(conf.personal > 4, NA, conf.personal),
    conf.county = ifelse(conf.county > 4, NA, conf.county),
    conf.state = ifelse(conf.state > 4, NA, conf.state),
    conf.nation = ifelse(conf.nation > 4, NA, conf.nation)
  ) %>%
  # Generate Averages
  group_by(year) %>%
  mutate(
    avg.person = mean(conf.personal, na.rm = TRUE),
    avg.county = mean(conf.county, na.rm = TRUE),
    avg.state = mean(conf.state, na.rm = TRUE),
    avg.nation = mean(conf.nation, na.rm = TRUE)
  )

# Create the Election Confidence Over Time Graphic
ec.labels <- c("2012", "2014", "2016", "2020", "2022")
ec.colors <- c("#002f80", "#007bc9", "#00bfff", "#a50021") 
ec.legend <- c("Personal Vote", "Votes in Jurisdiction", "Votes in State", "Votes in the Nation")

ec.over.time <- ggplot(spae, aes(x = as.factor(year))) +
  geom_line(aes(y = avg.person, color = ec.colors[1], group = 1), linewidth = 1) +
  geom_line(aes(y = avg.county, color = ec.colors[2], group = 1), linewidth = 1) +
  geom_line(aes(y = avg.state, color = ec.colors[3], group = 1), linewidth = 1) +
  geom_line(aes(y = avg.nation, color = ec.colors[4], group = 1), linewidth = 1) +
  geom_point(aes(y = avg.person, color = ec.colors[1]), size = 3) +
  geom_point(aes(y = avg.county, color = ec.colors[2]), size = 3) +
  geom_point(aes(y = avg.state, color = ec.colors[3]), size = 3) +
  geom_point(aes(y = avg.nation, color = ec.colors[4]), size = 3) +
  labs(title = "Election Confidence Over Time",
       subtitle = "Note: Higher Values (Scale of 1 - 4) Indicate Increased Election Confidence",
       x = "Year",
       y = "Average Confidence Level") +
  scale_x_discrete(breaks = ec.labels) +
  scale_color_manual(name = "Type of Vote", values = ec.colors, labels = ec.legend) +
  theme.fm()

ggsave("ec.over.time.png", plot = ec.over.time, width = 8, height = 6, dpi = 300)