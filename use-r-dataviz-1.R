
# SESSION 1 ---------------------------------------------

library(palmerpenguins)

# inspect data
penguins
View(penguins)
dplyr::glimpse(penguins) # dplyr package
summary(penguins)
penguins = na.omit(penguins)

# BASE R - a flyby ----------------------------------
# also see https://r-graph-gallery.com/base-R.html

# Univatiate data (single variable)

plot(penguins$bill_length_mm)
plot(penguins$bill_length_mm, cex = 2)  # symbol size
plot(penguins$bill_length_mm, pch = 16) # symbol choice
plot(penguins$bill_length_mm, pch = 16, col = penguins$species)  # colours
title('Bill lengths of 344 penguins, by species') # base R often builds plots iteratively

# understanding data distributions
penguins$bill_length_mm |> sort() |> plot(type='l')  # rank plot
hist(penguins$bill_length_mm, breaks = 30)           # histogram
density(penguins$bill_length_mm) |> plot()           # density plot
boxplot(penguins$bill_length_mm)                     # boxplot

# Bivariate data (two variables)

# scatterplot
plot(x = penguins$flipper_length_mm, y = penguins$bill_length_mm, col = penguins$species, pch=16)

par(mfrow = c(1, 2)) # configure for 2 panels (side-by-side)

# bar plot
library(dplyr)
d = penguins |> group_by(species) |> 
  summarise(bill_length_mm = mean(bill_length_mm), .groups = 'drop') |> 
  print()

barplot(bill_length_mm ~ species, data = d)

boxplot(bill_length_mm ~ species,  data = penguins)

par(mfrow = c(1, 1)) # reset paneling to single-plot

# heatmaps - visualising matrices
View(volcano)
browseURL('https://earth.google.com/web/search/Maunga+Whau+(Mt+Eden)/') # if interested..
image(volcano)

# heatmaps can also visualise 'n' dimensions (1 column for each)
head(mtcars)
heatmap(as.matrix(mtcars), scale="column")

# geospatial
filled.contour(volcano, color.palette = terrain.colors)
contour(volcano) # not a heatmap but same data


# GGPLOT2 - The grammar of graphics ----------------------------------
# Predecessor to dplyr and tidyverse

library(ggplot2)

ggplot(penguins, aes(bill_length_mm, bill_depth_mm, col = island)) + geom_point()

# Key concepts:
# 1. data
# 2. geometry (geoms or symbology)
# 3. aesthetics (geom attributes)

browseURL('https://en.wikipedia.org/wiki/Data_and_information_visualization#/media/File:Minard.png')

# Principles of "tidy data":
# 1. Each variable forms a column.
# 2. Each observation forms a row.
# 3. Each type of observational unit forms a table.

# Aesthetics (eg. for point geom)

p = tibble(x = sample(10), y = sample(10)) |> ggplot(aes(x, y))

p + geom_point()
p + geom_point(size = 5)
p + geom_point(size = 5, col = 'red')
p + geom_point(size = 5, col = 'red', shape = '#')
p + geom_point(size = 5, col = 'red', shape = 15)  # for native symbols see plot(1:25, pch = 1:25)
p + geom_point(size = 5, col = 'red', shape = 15, alpha = 0.3)  # transparency
p + geom_point(size = 5, fill = 'red', col ='yellow', shape = 22, stroke=5)  # stroke/border is outline

# what aesthetics have we missed..?

# Geom functions

# univariate
p = ggplot(penguins, aes(bill_length_mm))  # bill length is mapped to x-axis aesthetic
p + geom_histogram()
p + geom_density()
p + geom_boxplot()

# Bivariate
p = ggplot(penguins)  # plot initialised with data object but no aesthetic mappings
p + geom_point(aes(bill_length_mm, bill_depth_mm))
p + geom_point(aes(species, bill_length_mm))  # results in overplotting
p + geom_jitter(aes(species, bill_length_mm), width = .2, height = 0) # adding noise to x-axis positions
p + geom_boxplot(aes(species, bill_length_mm))


# BREAKOUT SESSION 1 (10 mins) -----------------------------------------------

#' Using the inbuilt 'iris' and 'diamonds' datasets, create a basic ggplot scatterplot that plots
#' one numeric field along one the x-axis and another numeric field along y. Don't worry about 
#' looking nice - we're only interested in ensuring everyone understands the basics of mapping data
#' fields to symbol attributes and applying the appropriate geom function to produce a basic plot.

# to help kick off..
as_tibble(iris) # Group 1
diamonds        # Group A


# BACK TO MAIN GROUP -----------------------------------------------


# Grouping with colour
p + geom_histogram(aes(bill_length_mm, fill = species))  # bars are 'stacked'
p + geom_histogram(aes(bill_length_mm, fill = species), position = 'dodge')
p + geom_density(aes(bill_length_mm, fill = species))
p + geom_density(aes(bill_length_mm, y = after_stat(count), fill = species), alpha = .3)
p + geom_density_2d(aes(bill_length_mm, bill_depth_mm, col = species))
p + geom_density_2d_filled(aes(bill_length_mm, bill_depth_mm), alpha = .7)

# Data binning
p + geom_bin_2d(aes(bill_length_mm, bill_depth_mm))
p + geom_hex(aes(bill_length_mm, bill_depth_mm))


# Coord functions - defining the coordinate system
p + geom_bar(aes(species))  # default cartesian
p + geom_bar(aes(species)) + coord_flip()
p + geom_bar(aes(species)) + coord_flip() + scale_y_reverse() # so maybe scale_x before coord_x..
p + geom_bar(aes(species)) + coord_polar()
p + geom_bar(aes(species)) + coord_polar(theta = 'y')

# pie chart is common example of this cooordinate system
count(penguins, species) |> 
  ggplot(aes(x = "", y = n, fill = species, label = n)) +
  geom_bar(stat = "identity", width = 1, col = 'white') +
  coord_polar("y", start = 10) +
  theme_void() +
  labs(title = 'Penguin populations by species')

# hive plot - a more advanced example
browseURL('https://datavizproject.com/wp-content/uploads/examples/www.geotheory.co_.uk_.png')

# SCALE functions - customising the symbol attributes that data variables map to
p + geom_hex(aes(bill_length_mm, bill_depth_mm)) + scale_fill_viridis_c()

# colour scales
library(tidyr)
library(stringr)

p = as.data.frame(volcano) |> mutate(x = 1:n()) |> 
  pivot_longer(-x, names_to = 'y', values_to = 'height') |> 
  mutate(y = str_remove(y, 'V') |> as.integer()) |> print() |> 
  ggplot() + geom_tile(aes(x, y, fill = height))

p
p + scale_fill_distiller()
p + scale_fill_distiller(palette = 3)
p + scale_fill_fermenter(type = 'seq')
p + scale_fill_viridis_c()
# manual scales
p + scale_fill_gradient(low = 'green4', high = 'white')
p + scale_fill_gradient2(low = 'magenta', mid = 'yellow', high = 'red', midpoint = 160)
p + scale_fill_gradientn(colours = c('lightgreen','white','pink','purple','black'))

# Identity colours - optimally distinct colours
browseURL('http://medialab.github.io/iwanthue/') # project origin
browseURL('https://geotheory.github.io/assets/images/refugee.destinations.png') # example usage

# R implementation
# install.packages('hues') # if not installed
library(hues)
cols = iwanthue(12, random = TRUE) |> print()
plot(1:12, col = cols, pch = 15, cex=10)
p + scale_fill_gradientn(colours = cols) + labs(title = 'OK maybe not..')

# Axis scales
rivers
ggplot(aes(rivers), data=NULL) + geom_density() + labs(title = 'x-axis with linear scale (default)')
ggplot(aes(rivers), data=NULL) + geom_density() + scale_x_sqrt() + labs(title = 'x-axis with square-root scale')
ggplot(aes(rivers), data=NULL) + geom_density() + scale_x_log10() + labs(title = 'x-axis with log scale')
ggplot(aes(rivers), data=NULL) + geom_density() + scale_x_reverse()

# Often a solution to manage data overplotting
p = ggplot(diamonds, aes(carat, price))
p + geom_point(size = .2)
p + geom_point(size = .2, alpha = .1)
p + stat_bin_2d(bins = 50) + scale_fill_continuous(low = 'white', high = 'red')
p + geom_point(alpha = .1) + scale_x_log10() + scale_y_log10() # x,y both on log scales
p + stat_bin_2d(bins = 50) + scale_fill_continuous(low = 'white', high = 'red') + scale_x_log10() + scale_y_log10()

# scaling size (area) e.g. for cartograms
p = tibble::enframe(state.x77[,1], name = 'state', value = 'pop') |> 
  bind_cols(state.center, region = state.region) |> print() |> 
  ggplot(aes(x, y, size = pop, label = state)) + 
  coord_quickmap()

p + geom_point(shape=15)
p + geom_point(shape=15) + scale_size_area()

p + geom_point(aes(col = region), shape = 15, alpha = .5) + geom_text(size = 4) +
  scale_size_area(max_size = 30)

p + geom_point(aes(col = region), shape = 15, alpha = .5) + geom_text(size = 3) +
  scale_size_area(max_size = 30) +
  guides(size = 'none', colour = guide_legend(override.aes = list(size=8))) +
  theme_void() + 
  theme(legend.position = 'top', plot.title = element_text(size = 16, face = 'bold')) +
  labs(title = ' Relative population sizes of US states\n')

# faceting
p = ggplot(penguins)
p + geom_point(aes(bill_length_mm, bill_depth_mm)) + facet_wrap(~ species)
p + geom_point(aes(bill_length_mm, bill_depth_mm)) + facet_wrap(~ species, scales = 'free')
p + geom_point(aes(bill_length_mm, bill_depth_mm)) + facet_wrap(species ~ island)
p + geom_point(aes(bill_length_mm, bill_depth_mm)) + facet_grid(species ~ island)

# layering geoms mapped to different datasets
p + 
  geom_point(aes(bill_length_mm, bill_depth_mm), data = penguins |> na.omit() |> select(-species), col = 'grey') +
  geom_point(aes(bill_length_mm, bill_depth_mm)) +
  facet_wrap(~ species)

p + 
  geom_density_2d_filled(aes(bill_length_mm, bill_depth_mm), alpha = .5) +
  geom_point(aes(bill_length_mm, bill_depth_mm), data = penguins |> na.omit() |> select(-species), col = 'grey', size=.5) +
  geom_point(aes(bill_length_mm, bill_depth_mm), size=.5) +
  facet_wrap(~ species) +
  guides(fill = 'none')


# BREAKOUT SESSION 2 (15 mins) -----------------------------------------------

#' Using the same inbuilt 'iris' and 'diamonds' datasets, create a ggplot visualisation that 
#' uses colour and/or facetting to explore one or more of the numeric variables by breaking it
#' down by one or of the categorical fields, using any appropriate geom functions.  Please 
#' customise one of the symbolic scales used (such as x/y axis, or symbol colour or size).


# BACK TO MAIN GROUP -----------------------------------------------

# Timeseries visualisation

#' let's visualise the historic trends in life expectancy for African countries 
#' ranked lowest in modern times (ie. 2007)

require(gapminder)

# plot data as well as an ordered listing of countries
region_sel = gapminder |> 
  filter(continent == 'Africa', year == 2007) |> 
  arrange(lifeExp) |> head(12) 

p = gapminder |> filter(country %in% region_sel$country) |> 
  mutate(country = factor(country, levels = region_sel$country)) |> 
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(group = country)) +
  facet_wrap(~ country)

# Themes - customised sets of aesthetic settings
p  # default ggplot grey
p + theme_minimal()
p + theme_classic()
p + theme_dark()
p + theme_void()

# install.packages('ggthemes') # if not installed
library(ggthemes)
p + theme_tufte()
p + theme_economist()
p + theme_wsj()
p + theme_fivethirtyeight()

#' The panels are ranked by life expectancy in 2007 (ascending) but we can do more to
#' facilitate comparative analysis if we add data for other African countries back in

africa = gapminder |> filter(continent == 'Africa') |> 
  rename(country2 = country)  # this prevents facet_wrap splitting this dataset

gapminder |> filter(country %in% region_sel$country) |> 
  mutate(country = factor(country, levels = region_sel$country)) |> 
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(group = country2), data = africa, col = 'grey') +
  geom_line(aes(group = country), col = 'red', linewidth = 1) +
  facet_wrap(~ country) +
  labs(title = 'Trends in life expectancy for African countries ranked lowest in 2007', 
       subtitle = 'Source: Gapminder. Grey lines show all other African countries') +
  ggthemes::theme_clean()
