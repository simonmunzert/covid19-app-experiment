### -----------------------------
### global app descriptives
### -----------------------------

## Public Corona-Warn-App download figures ------------------

# load data and format date
app_downloads <- readr::read_tsv("../data/app_downloads.tsv") %>% 
  dplyr::mutate(date = lubridate::as_date(date, format = "%d.%m.%Y"))

# create date breaks for plots
date_vector <- seq(lubridate::as_date("2020-06-16"), by = "2 weeks", length.out = 20)

# plot
p <- app_downloads %>%
  ggplot(., aes(x=date, y=downloads)) +
  scale_x_date(breaks = date_vector,
               date_labels = "%b %d",
               limits = c(as_date("2020-06-16"), as_date("2020-09-22"))) +
  ylim(0, 20) + 
  geom_line(color = "black", size = 1.5) + 
  theme(axis.title.x=element_blank(),
        plot.caption = element_text(size = 8)) +
  labs(y = "Number of downloads (in Millions)\n") 

# export
pdf(file="../figures/app-downloads-official.pdf", height = 3, width = 6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(2.5,4,.8,.8))
p
dev.off()






## World map of tracing apps -------------

##load data
tracing_df <- readr::read_csv("../data/mit_ct_covid_tracing_tracker.csv",
                              col_types = cols(Users = "d")) %>% 
  dplyr::mutate(app_status = case_when(.$Status == "Launched" ~ 2,
                                       .$Status == "In Dev" ~ 1))
#list
live_list <- tracing_df %>% dplyr::filter(app_status == 2) %>% dplyr::select(Location)
dev_list <- tracing_df %>% dplyr::filter(app_status == 1) %>% dplyr::select(Location)

shape_df <- ggplot2::map_data("world") %>%
  dplyr::filter(region != "Antarctica") %>%
  dplyr::mutate(app_in_place = factor(case_when(.$region %in% dev_list$Location ~ "In Dev",
                                                .$region %in% live_list$Location ~ "Launched",
                                                T ~ "None"
  )))

#plot
p <- ggplot(shape_df, aes(long, lat, group=group, fill = app_in_place)) +
  geom_polygon(color = "#636363", size = 0.05) +
  theme_void() +
  theme(legend.position = c(0.13, 0.2)) + 
  labs(fill = "Application\nStatus") +
  scale_fill_manual(breaks = c("Launched", "In Dev", "None"),
                    labels = c("Live", "Dev", "None"),
                    values = c("#00a19a", "#7a0177", "#d2d2d2"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top",
                      label.position = "bottom",
                      label.hjust = 0.5,
                      label.vjust = 0.5,
                      title.vjust = 1,
                      title.hjust = 0.5
                    )) +
  theme(text = element_text(family = "Roboto Condensed"),
        panel.background = element_rect(fill = "#F5F9FC", colour = "#F5F9FC"))

ggsave(p, file = "../figures/tracing-apps-map.png", width = 4.7, height = 2.5, units = "cm", dpi = 300, scale = 5)

