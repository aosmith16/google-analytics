
# Packages ----
library(googleAnalyticsR)
library(dplyr) # data manip
library(tidyr) # joins
library(ggplot2) # plotting
library(sf) # plotting maps
library(forcats) # factors

# Authorization
ga_auth(email = "ariel.muldoon@gmail.com")

# Find very statisticious View ID
# ga_accounts = ga_account_list()
# View(ga_accounts)

vs_id = 220230987

# Play around with first week then first 30 days tracking
start = "2020-06-07"
end = "2020-07-06"
# page & page views
pageviews = google_analytics(vs_id,
                             date_range = c("2020-06-05", "2020-06-11"),
                             metrics = "uniquePageViews",
                             dimensions = "pagePath")

# Country
# Remove those without location info
country = google_analytics(vs_id,
                           date_range = c(start, end),
                           metrics = c("users", "sessions"),
                           dimensions = "country") %>%
     filter(country != "(not set)")

# US States
# Remove those without location info
us_states = google_analytics(vs_id,
                             date_range = c("2020-06-05", "2020-06-11"),
                             metrics = c("users", "sessions"),
                             dimensions = c("country", "region"),
                             dim_filters = filter_clause_ga4(list(dim_filter("country", "EXACT", "United States", not = FALSE) ) ) ) %>%
     filter(region != "(not set)")


# US cities
# Remove those without location info
us_cities = google_analytics(vs_id,
                             date_range = c("2020-06-05", "2020-06-11"),
                             metrics = c("users", "sessions"),
                             dimensions = c("country", "region", "city"),
                             dim_filters = filter_clause_ga4(list(dim_filter("country", "EXACT", "United States", not = FALSE) ) ),
                             max = -1) %>%
     filter(country != "(not set)")


# Data manipulation ----

# Need only blog post pages 
     # remove click info from addresses then collapse dupes
     # filter out home page, second page, tag pages
cleanpage = pageviews %>%
     mutate(pagePath = gsub("\\?.*", "", pagePath) ) %>%
     group_by(pagePath) %>%
     summarise(uniquePageViews = sum(uniquePageViews) ) %>%
     filter(pagePath != "/", !grepl("tags|page", pagePath) )


# Create look-up table with page and tag 
     # (ggplot2, simulation, emmeans only, any more "other")
tag = tribble(~pagePath, ~tag,
              "/2018/01/09/simulate-simulate-part1/", "simulation",
              "/2018/01/09/simulate-simulate-part2/", "simulation",
              "/2018/01/19/reversing-the-order-of-a-ggplot2-legend/", "ggplot2",
              "/2018/04/23/simulate-simulate-part-2/", "simulation",
              "/2018/06/05/a-closer-look-at-replicate-and-purrr/", "simulation",
              "/2018/07/18/simulate-poisson-edition/", "simulation",
              "/2018/07/19/manual-legends-ggplot2/", "ggplot2",
              "/2018/08/20/automating-exploratory-plots/", "ggplot2",
              "/2018/08/29/getting-started-simulating-data/", "simulation",
              "/2018/11/16/plot-fitted-lines/", "ggplot2",
              "/2019/03/25/getting-started-with-emmeans/", "emmeans",
              "/2019/04/15/custom-contrasts-emmeans/", "emmeans",
              "/2019/04/22/embedding-subplots/", "ggplot2",
              "/2019/05/13/small-multiples-plot/", "ggplot2",
              "/2019/10/14/background-color_gradient/", "ggplot2"
)

# Add tag info to pagepath
     # Make any without tag "other"
pagetag = left_join(cleanpage, tag, by = "pagePath") %>%
     mutate(tag = fct_explicit_na(tag, na_level = "other") ) %>%
     rename(views = uniquePageViews)

# Plotting ----

# US map
# Using urbnmapr, from GitHub, https://urbaninstitute.github.io/urbnmapr/
states_sf = urbnmapr::get_urbn_map("states", sf = TRUE)

# Add in user info to us map data
state_users = left_join(states_sf, us_states, by = c("state_name" = "region"))

# Not adjusting for population
state_plot = ggplot(data = state_users) + 
     geom_sf(aes(fill = users), color = "black", size = 0.5/.pt) +
     cowplot::theme_minimal_grid() + 
     colorspace::scale_fill_continuous_sequential(
          palette = "Blues", rev = TRUE,
          na.value = "grey60",
          breaks = c(10, 50, 100),
          name = NULL, trans = "sqrt",
          guide = guide_colorbar(frame.colour = "black",
                                 barwidth = 7.5,
                                 label.vjust = 2.5) # weirdly, moves labels closer to bar
     ) +
     theme(legend.direction = "horizontal",
           legend.position = c(.6, .05),
           plot.title = element_text(hjust = 0.5),
           plot.caption = element_text(face = "italic") ) +
     labs(title = "Number user visits 2020-06-05 thru 2020-06-11",
          caption = "States with no visits in gray")

ggsave(filename = here::here("Plots", "2020-06-11_state_visits.png"),
       plot = state_plot,
       width = 10,
       height = 6)

# Country plot ----
# World map
world_sf = st_as_sf(rworldmap::getMap(resolution = "low"))

# This world map uses some diff country names
     # than what I have
# anti_join(country, world_sf, by = c("country" = "ADMIN"))
# filter(world_sf, grepl("Macau", ADMIN) )

# First change all "&" to "and" in country dataset
# Then:
# I believe Svalbard and Jan Mayen is a part of Norway
# I believe Réunion is part of French Southern and Antarctic Lands
# Other than that world_sf = country 
     # United States of America = United States,
     # Republic of Serbia = Serbia,
     # Czech Republic = Czechia
     # Myanmar = Myanmar (Burma)
     # United Republic of Tanzania = Tanzania
     # Republic of the Congo = Congo - Brazzaville
     # Ivory Coast = Côte d’Ivoire
     # Curacao = Curaçao
     # Hong Kong S.A.R. = Hong Kong
     # Macau S.A.R = Macao
     # Saint Kitts and Nevis = St. Kitts and Nevis

country = country %>%
     mutate(country = gsub("&", "and", country),
            ADMIN = case_when(country == "United States" ~ "United States of America",
                           country == "Serbia" ~ "Republic of Serbia",
                           country == "Myanmar (Burma)" ~ "Myanmar",
                           country == "Czechia" ~ "Czech Republic",
                           country == "Tanzania" ~ "United Republic of Tanzania",
                           country == "Congo - Brazzaville" ~ "Republic of the Congo",
                           country == "Côte d’Ivoire" ~ "Ivory Coast",
                           country == "Curaçao" ~ "Curacao",
                           country == "Hong Kong" ~ "Hong Kong S.A.R.",
                           country == "Macao" ~ "Macau S.A.R",
                           country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                           country == "Svalbard and Jan Mayen" ~ "Norway",
                           country == "Réunion" ~ "French Southern and Antarctic Lands",
                           TRUE ~ country) ) %>%
     group_by(ADMIN) %>%
     summarise(across(.cols = users:sessions, sum) )

# Add in user info to world map data
world_users = left_join(world_sf, country)

# Try Robinson projection based on Wilke gist
     # https://gist.github.com/clauswilke/783e1a8ee3233775c9c3b8bfe531e28a
crs_robin = "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"
# projection outline in long-lat coordinates
lats = c(90:-90, -90:90, 90)
longs = c(rep(c(180, -180), each = 181), 180)
robin_outline = 
     list(cbind(longs, lats)) %>%
     st_polygon() %>%
     st_sfc(
          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
     ) %>% 
     st_transform(crs = crs_robin)
# bounding box in transformed coordinates
xlim = c(-18494733, 18613795)
ylim = c(-9473396, 9188587)
robin_bbox = 
     list(
          cbind(
               c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
               c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
          )
     ) %>%
     st_polygon() %>%
     st_sfc(crs = crs_robin)
# area outside the earth outline
robin_without = st_difference(robin_bbox, robin_outline)

# Different breaks if 7 days or 30 days
if(diff(c(as.Date(start), as.Date(end))) > 7 ) {
     leg_breaks = c(10, 100, 1000, 10000) 
} else {
     leg_breaks = c(10, 100, 500)
}

# Sequential color scheme example: https://wilkelab.org/practicalgg/articles/Texas_income.html
world_plot = ggplot() + 
     geom_sf(data = robin_without, fill = "white", color = NA) +
     geom_sf(data = robin_outline, fill = NA, color = "black", size = 0.5/.pt) +
     geom_sf(data = world_users,
             aes(fill = users), color = NA) +
     geom_sf(data = world_users, fill = NA, color = "black", size = 0.5/.pt) +
     scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
     scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
     coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = crs_robin, ndiscr = 1000) +
     cowplot::theme_minimal_grid() + 
     colorspace::scale_fill_continuous_sequential(
          palette = "Blues", rev = TRUE,
          na.value = "grey60", trans = "log10",
          breaks = leg_breaks,
          name = NULL,
          guide = guide_colorbar(frame.colour = "black",
                                 barwidth = 10,
                                 label.vjust = 2.5) # weirdly, moves labels closer to bar
     ) +
     theme(legend.direction = "horizontal",
           legend.position = c(.4, .2),
           plot.title = element_text(hjust = 0.5),
           plot.caption = element_text(face = "italic") ) +
     labs(title = paste("Number user visits", start, "thru", end),
          caption = "Countries with no visits in gray")

ggsave(filename = here::here("Plots", paste(end, "country_visits.png", sep = "_") ),
       plot = world_plot,
       width = 10,
       height = 6)

# Proportion page views ----

# Each page weekly proportion
pagetag %>%
     arrange( desc(views) ) %>%
     mutate(prop = views/sum(views) )

# Each tag weekly proportion
pagetag %>%
     group_by(tag) %>%
     summarise(total = sum(views) ) %>%
     arrange( desc(total) ) %>%
     mutate(prop = total/sum(total) )

# Weighted proportion tag
pagetag %>%
     group_by(tag) %>%
     summarise(total = sum(views),
               n = n() ) %>%
     arrange( desc(total) ) %>%
     mutate(prop = (total/n)/(sum(total/n) ) )



# For world plotting, can likely
     # take an easier route than the code from
     # the gist above since robinson proj is 'ESRI:54030'
     # However, not sure on how to put in the black outline
     # so not pursuing further for now (2020-07-07)
ggplot() + 
     # geom_sf(data = robin_without, fill = "white", color = NA) +
     # geom_sf(data = robin_outline, fill = NA, color = "black", size = 0.5/.pt) +
     geom_sf(data = world_users,
             aes(fill = users), color = NA) +
     geom_sf(data = world_users, fill = NA, color = "black", size = 0.5/.pt) +
     # coord_sf(crs = st_crs('ESRI:54030') ) +
     scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
     scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
     coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = st_crs('ESRI:54030'), ndiscr = 1000) +
     cowplot::theme_minimal_grid() + 
     colorspace::scale_fill_continuous_sequential(
          palette = "Blues", rev = TRUE,
          na.value = "grey60", trans = "log10",
          breaks = leg_breaks,
          name = NULL,
          guide = guide_colorbar(frame.colour = "black",
                                 barwidth = 10,
                                 label.vjust = 2.5) # weirdly, moves labels closer to bar
     ) +
     theme(legend.direction = "horizontal",
           legend.position = c(.4, .2),
           plot.title = element_text(hjust = 0.5),
           plot.caption = element_text(face = "italic") ) +
     labs(title = paste("Number user visits", start, "thru", end),
          caption = "Countries with no visits in gray")
