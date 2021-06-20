#=========================================================================#
#                             ON_herb Figures                             #
#=========================================================================#

# Figures for: Urbanization influences communities of milkweed-specialist herbivorous insects

# Author: David Murray-Stoker (dstoker92@gmail.com)
# Last Updated: 20 June 2021

## Table of Contents-UPDATE
# Line   21: Load Packages & Data
# Line   62: Site Map 
# Line   94: Herbivore Diversity, Abundance, & Leaf Herbivory (Toronto)
# Line  272: Herbivore Diversity, Abundance, & Leaf Herbivory (5 Cities)
# Line  489: Herbivore-Specific Models
# Line 1036: End of Script


#=========================================================================#
#                          Load Packages & Data                           #
#==========================================================================
## Map
library(maps)
library(mapdata)
library(maptools)
library(ggmap)

## Figures
library(calecopal)
library(ggpubr)
library(grid)
library(gridExtra)
library(scales)

## Load the tidyverse
library(tidyverse)


## Read in workspace
load("R/1-Primary_Analyses/ON_herb-analyses-final_workspace.RData")

## Set colour palette (caleco | superbloom3)
superbloom.continuous <- cal_palette(name = "superbloom3", n = 20, type = "continuous")
show_col(superbloom.continuous)

## Standard palette for the site map
superbloom.map <- superbloom.continuous[c(1, 5, 7, 9, 15, 20)]

## Standard palette for the 5 Cities
superbloom.5 <- superbloom.continuous[c(1, 5, 7, 9, 15)]

# Palette for connecting lines in the 5 cities
superbloom.10.lines <- c("#E69512", "#E69512", "#CB135E", "#CB135E", "#7B3478", "#7B3478",
												 "#3A5085", "#3A5085", "#464F63", "#464F63")


#=========================================================================#
#                                Figures                                  #
#==========================================================================

#-------------------------------------------------------------------------#
#                               Site Map                                  #
#--------------------------------------------------------------------------

## Compute bounding box
ON_bbox <- make_bbox(lat = Lat, lon = Long, data = site.locations)

## Get map from google
ON_big <- get_map(location = ON_bbox, source = "google", maptype = "satellite")

## Plot points and colour by city
site.map <- ggmap(ON_big) + 
	geom_point(data = site.locations, 
						 mapping = aes(x = Long, y = Lat, 
						 							color = City, pch = Type)) +
	scale_colour_manual(values = superbloom.map, name = "City") +
	scale_shape_manual(values = c(17, 19),  name = "Habitat") +
	labs(x = "Longitude", y = "Latitude") +
	theme_pubr()

## Export the figure
ggsave("fig_1-site_map.jpg", 
			 plot = site.map,
			 device = "jpg",
			 path = "figures/",
			 width = 8,
			 height = 6,
			 units = "in",
			 dpi = 900)


#-------------------------------------------------------------------------#
#        Herbivore Diversity, Abundance, & Leaf Herbivory (Toronto)       #
#--------------------------------------------------------------------------

## Early season diversity, Toronto
early.toronto.diversity.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Diversity)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Diversity") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 1, 2, 3), limits = c(0, 3.75)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 3.75, 
					 label = "chi^{2}==1.766", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 3.35, 
					 label = "P==0.184", parse = TRUE, size = 5) +
	theme_pubr() +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.diversity.plot

## Late season diversity, Toronto
late.toronto.diversity.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Diversity)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Diversity") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 1, 2, 3), limits = c(0, 3.75)) +
	ggtitle("b", subtitle = "Late Season") +
	annotate('text', x = 40, y = 3.75, 
					 label = "chi^{2}==5.360", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 3.35, 
					 label = "P==0.021", parse = TRUE, size = 5) +
	theme_pubr() +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab") +
	rremove("y.text") +
	rremove("xlab") +
	rremove("x.text")
late.toronto.diversity.plot


## Early season abundance, Toronto
early.toronto.abundance.plot <- ggplot(
	data = early.toronto.n.aphid,
	aes(x = Distance, y = Abundance)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
	ggtitle("c", subtitle = "Early Season") +
	annotate('text', x = 40, y = 22.5, 
					 label = "chi^{2}==0.005", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 19.5, 
					 label = "P==0.945", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 22.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.abundance.plot

## Late season abundance, Toronto
late.toronto.abundance.plot <- ggplot(
	data = late.toronto.n.aphid,
	aes(x = Distance, y = Abundance)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
	ggtitle("d", subtitle = "Late Season") +
	annotate('text', x = 40, y = 22.5, 
					 label = "chi^{2}==12.445", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 19.5, 
					 label = "P<0.001", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 22.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab") +
	rremove("y.text") +
	rremove("xlab") +
	rremove("x.text")
late.toronto.abundance.plot


## Early season leaf herbivory, Toronto
early.toronto.herbivory.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Average_Leaf_Herbivory)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Average Leaf Herbivory") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 0.15, 0.30, 0.45)) +
	ggtitle("e", subtitle = "Early Season") +
	annotate('text', x = 40, y = 0.55, 
					 label = "chi^{2}==3.186", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 0.48, 
					 label = "P==0.074", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 0.565)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
early.toronto.herbivory.plot

## Late season leaf herbivory, Toronto
late.toronto.herbivory.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Average_Leaf_Herbivory)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Average Leaf Herbivory") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 0.15, 0.30, 0.45)) +
	ggtitle("f", subtitle = "Late Season") +
	annotate('text', x = 40, y = 0.55, 
					 label = "chi^{2}==27.573", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 0.48, 
					 label = "P<0.001", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 0.565)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab") +
	rremove("y.text")
late.toronto.herbivory.plot


## Arrange the figure
herbivore.toronto.figure <- ggarrange(
	early.toronto.diversity.plot, late.toronto.diversity.plot,
	early.toronto.abundance.plot, late.toronto.abundance.plot,
	early.toronto.herbivory.plot, late.toronto.herbivory.plot,
	nrow = 3, ncol = 2,
	align = "v",
	legend = "none", common.legend = FALSE
)
herbivore.toronto.figure


## Export the figure
ggsave("fig_2-toronto_herbivores.jpg", 
			 plot = herbivore.toronto.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


#-------------------------------------------------------------------------#
#        Herbivore Diversity, Abundance, & Leaf Herbivory (5 Cities)      #
#--------------------------------------------------------------------------

## Early season diversity, 5 Cities
early.5.cities.diversity.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Diversity",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Diversity",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 1, 2)) +
	coord_cartesian(ylim = c(0, 2.15)) +
	ggtitle("a", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.5.cities.diversity.plot

## Late season diversity, 5 Cities
late.5.cities.diversity.plot <- ggerrorplot(
	data = late.5.cities,
	x = "Habitat", y = "Diversity",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Diversity",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 1, 2)) +
	coord_cartesian(ylim = c(0, 2.15)) +
	ggtitle("b", subtitle = "Late Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab") +
	rremove("y.text") +
	rremove("xlab") +
	rremove("x.text")
late.5.cities.diversity.plot


## Early season abundance, 5 Cities
early.5.cities.abundance.plot <- ggerrorplot(
	data = early.5.cities.n.aphid,
	x = "Habitat", y = "Abundance",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 2, 4, 6)) +
	coord_cartesian(ylim = c(0, 6.75)) +
	ggtitle("c", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("xlab") +
	rremove("x.text")
early.5.cities.abundance.plot

## Late season abundance, 5 Cities
late.5.cities.abundance.plot <- ggerrorplot(
	data = late.5.cities.n.aphid,
	x = "Habitat", y = "Abundance",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 2, 4, 6)) +
	coord_cartesian(ylim = c(0, 6.75)) +
	ggtitle("d", subtitle = "Late Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab") +
	rremove("y.text") +
	rremove("xlab") +
	rremove("x.text")
late.5.cities.abundance.plot

## Early season leaf herbivory, 5 Cities
early.5.cities.herbivory.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Average_Leaf_Herbivory",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Average Leaf Herbivory",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12)) +
	coord_cartesian(ylim = c(0, 0.135)) +
	ggtitle("e", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
early.5.cities.herbivory.plot

## Late season leaf herbivory, 5 Cities
late.5.cities.herbivory.plot <- ggerrorplot(
	data = late.5.cities,
	x = "Habitat", y = "Average_Leaf_Herbivory",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Average Leaf Herbivory",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12)) +
	coord_cartesian(ylim = c(0, 0.135)) +
	ggtitle("f", subtitle = "Late Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab") +
	rremove("y.text")
late.5.cities.herbivory.plot


## Arrange the figure
herbivore.5.cities.figure <- ggarrange(
	early.5.cities.diversity.plot, late.5.cities.diversity.plot,
	early.5.cities.abundance.plot, late.5.cities.abundance.plot,
	early.5.cities.herbivory.plot, late.5.cities.herbivory.plot,
	nrow = 3, ncol = 2,
	align = "v",
	legend = "top", common.legend = TRUE
)
herbivore.5.cities.figure


## Export the figure
ggsave("fig_3-5_cities_herbivores.jpg", 
			 plot = herbivore.5.cities.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


#-------------------------------------------------------------------------#
#                      Herbivore-Specific Abundance                       #
#--------------------------------------------------------------------------

## Danaus-----------------------------------------------------------------#

# Early season Toronto
early.toronto.danaus.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Monarchs)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 16.5, 
					 label = "chi^{2}==1.129", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 13.5, 
					 label = "P==0.288", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 16.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.danaus.plot

# Early season 5 Cities
early.5.cites.danaus.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Monarchs",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5)) +
	coord_cartesian(ylim = c(0, 1.65)) +
	ggtitle("b", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab") +
	rremove("xlab") +
	rremove("x.text")
early.5.cites.danaus.plot

## Late season Toronto
late.toronto.danaus.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Monarchs)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15)) +
	ggtitle("c", subtitle = "Late Season") +
	annotate('text', x = 40, y = 15.5, 
					 label = "chi^{2}==0.008", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 13.5, 
					 label = "P==0.930", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 16.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
late.toronto.danaus.plot

# Late season 5 Cities
late.5.cites.danaus.plot <- ggerrorplot(
	data = late.5.cities,
	x = "Habitat", y = "Monarchs",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5)) +
	coord_cartesian(ylim = c(0, 1.65)) +
	ggtitle("d", subtitle = "Late Season") + 
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab")
late.5.cites.danaus.plot


## Arrange the figure
danaus.abundance.figure <- ggarrange(
	early.toronto.danaus.plot, early.5.cites.danaus.plot,
	late.toronto.danaus.plot, late.5.cites.danaus.plot,
	nrow = 2, ncol = 2,
	legend = "top", common.legend = TRUE
)
danaus.abundance.figure


## Export the figure
ggsave("fig_S3-danaus_abundance.jpg", 
			 plot = danaus.abundance.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


## Rhysomatus-------------------------------------------------------------#

# Early season Toronto
early.toronto.rhysomatus.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Rhysomatus)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 2.5, 
					 label = "chi^{2}==5.178", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 2.2, 
					 label = "P==0.023", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 2.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.rhysomatus.plot

# Early season 5 Cities
early.5.cites.rhysomatus.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Rhysomatus",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
	coord_cartesian(ylim = c(0, 0.9)) +
	ggtitle("b", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab")
early.5.cites.rhysomatus.plot

## Late season Toronto
late.toronto.rhysomatus.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Rhysomatus)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0)) +
	ggtitle("c", subtitle = "Late Season") +
	annotate('text', x = 40, y = 2.5, 
					 label = "chi^{2}==0.008", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 2.2, 
					 label = "P==0.929", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 2.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
late.toronto.rhysomatus.plot


## Arrange the figure
rhysomatus.abundance.figure <- ggarrange(
	early.toronto.rhysomatus.plot, early.5.cites.rhysomatus.plot,
	late.toronto.rhysomatus.plot,
	nrow = 2, ncol = 2,
	legend = "top", common.legend = TRUE
)
rhysomatus.abundance.figure


## Export the figure
ggsave("fig_S1-rhysomatus_abundance.jpg", 
			 plot = rhysomatus.abundance.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


## Aphis------------------------------------------------------------------#

# Early season Toronto
early.toronto.aphis.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Green_aphid)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 10, 20, 30)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 30.5, 
					 label = "chi^{2}==0.118", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 27.5, 
					 label = "P==0.731", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 32.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.aphis.plot

## Late season Toronto
late.toronto.aphis.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Green_aphid)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 10, 20, 30)) +
	ggtitle("b", subtitle = "Late Season") +
	annotate('text', x = 40, y = 30.5, 
					 label = "chi^{2}==0.028", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 27.5, 
					 label = "P==0.867", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 32.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
late.toronto.aphis.plot


## Arrange the figure
aphis.abundance.figure <- ggarrange(
	early.toronto.aphis.plot,
	late.toronto.aphis.plot,
	nrow = 2, ncol = 1,
	legend = "none", common.legend = FALSE
)
aphis.abundance.figure


## Export the figure
ggsave("fig_S4-aphis_abundance.jpg", 
			 plot = aphis.abundance.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


## Tetraopes--------------------------------------------------------------#

# Early season Toronto
early.toronto.tetraopes.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Tetraopes)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4.25)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 4.25, 
					 label = "chi^{2}==0.056", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 3.85, 
					 label = "P==0.814", parse = TRUE, size = 5) +
	theme_pubr() +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.tetraopes.plot

# Early season 5 Cities
early.5.cites.tetraopes.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Tetraopes",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
	coord_cartesian(ylim = c(0, 0.9)) +
	ggtitle("b", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab")
early.5.cites.tetraopes.plot

## Late season Toronto
late.toronto.tetraopes.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Tetraopes)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4.25)) +
	ggtitle("c", subtitle = "Late Season") +
	annotate('text', x = 40, y = 4.25, 
					 label = "chi^{2}==0.076", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 3.85, 
					 label = "P==0.783", parse = TRUE, size = 5) +
	theme_pubr() +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
late.toronto.tetraopes.plot


## Arrange the figure
tetraopes.abundance.figure <- ggarrange(
	early.toronto.tetraopes.plot, early.5.cites.tetraopes.plot,
	late.toronto.tetraopes.plot,
	nrow = 2, ncol = 2,
	legend = "top", common.legend = TRUE
)
tetraopes.abundance.figure


## Export the figure
ggsave("fig_S5-tetraopes_abundance.jpg", 
			 plot = tetraopes.abundance.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


## Liriomyza---------------------------------------------------------------#

# Early season Toronto
early.toronto.liriomyza.plot <- ggplot(
	data = early.toronto,
	aes(x = Distance, y = Fly)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 21.5, 
					 label = "chi^{2}==0.612", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 18.5, 
					 label = "P==0.434", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 21.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("xlab") +
	rremove("x.text")
early.toronto.liriomyza.plot

# Early season 5 Cities
early.5.cites.liriomyza.plot <- ggerrorplot(
	data = early.5.cities,
	x = "Habitat", y = "Fly",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
	coord_cartesian(ylim = c(0, 8.5)) +
	ggtitle("b", subtitle = "Early Season") +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") + 
	rremove("ylab") +
	rremove("xlab") +
	rremove("x.text")
early.5.cites.liriomyza.plot

## Late season Toronto
late.toronto.liriomyza.plot <- ggplot(
	data = late.toronto,
	aes(x = Distance, y = Fly)) + 
	geom_point(colour = "#7B3478") + 
	geom_smooth(method = "glm", formula = y ~ x, se = TRUE,
							method.args = list(family = "quasipoisson"),
							colour = "#3A5085", fill = "#3A5470") +
	labs(x = "Distance from City Center", y = "Abundance") +
	scale_x_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 45)) +
	scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
	ggtitle("a", subtitle = "Early Season") +
	annotate('text', x = 40, y = 21.5, 
					 label = "chi^{2}==19.921", parse = TRUE, size = 5) +
	annotate('text', x = 40, y = 18.5, 
					 label = "P < 0.001", parse = TRUE, size = 5) +
	theme_pubr() +
	coord_cartesian(ylim = c(0, 21.5)) +
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20")
late.toronto.liriomyza.plot

# Late season 5 Cities
late.5.cites.liriomyza.plot <- ggerrorplot(
	data = late.5.cities,
	x = "Habitat", y = "Fly",
	size = 2, width = 2,
	desc_stat = "mean_se",
	color = "City",
	palette = superbloom.5,
	xlab = "Habitat",
	ylab = "Abundance",
	position = position_dodge(0.65),
	ggtheme = theme_pubr()) %>%
	facet(
		facet.by = "City",
		nrow = 1,
		ncol = 5
	) +
	stat_summary(aes(group = City), fun = mean, geom = "path", 
							 color = superbloom.10.lines) +
	scale_x_discrete(labels = c("2" = "Urban", "1" = "Rural"),
									 limits = c("2", "1")) + 
	scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
	coord_cartesian(ylim = c(0, 8.5)) +ggtitle("d", subtitle = "Late Season") + 
	font("xlab", size = 16, color = "gray0") +
	font("ylab", size = 16, color = "gray0") +
	font("xy.text", size = 14, color = "gray20") +
	rremove("ylab")
late.5.cites.liriomyza.plot


## Arrange the figure
liriomyza.abundance.figure <- ggarrange(
	early.toronto.liriomyza.plot, early.5.cites.liriomyza.plot,
	late.toronto.liriomyza.plot, late.5.cites.liriomyza.plot,
	nrow = 2, ncol = 2,
	legend = "top", common.legend = TRUE
)
liriomyza.abundance.figure


## Export the figure
ggsave("fig_S2-liriomyza_abundance.jpg", 
			 plot = liriomyza.abundance.figure,
			 device = "jpg",
			 path = "figures/",
			 width = 14,
			 height = 10,
			 units = "in",
			 dpi = 900)


#=========================================================================#
#                             End of Script                               #
#=========================================================================#

