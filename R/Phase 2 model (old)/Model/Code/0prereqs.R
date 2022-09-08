#  ------------------------------------------------------------------------#
# run prerequisites----
#  ------------------------------------------------------------------------#


# remove objects from environment
rm(list=ls())

# load packages (install if necessary)
invisible(sapply(c("svglite", "magrittr", "raster", "sf", "segmented", "quantreg", "showtext", "grid", "scales", "ggplot2", "plyr", "stringr", "readxl", "readr", "purrr", "broom", "tidyr", "tibble", "dplyr"),
                 function(x) {
                   if (!require(x, character.only=T)) {
                     install.packages(x)
                     library(x, character.only=T)
                   }
                 }))

# set seed for reproducibility
.seed = 42

# add fonts to operating system
font_add("u57", "Univers-Condensed.otf")
font_add("u57o", "Univers-CondensedOblique.otf")
font_add("u67b", "Univers-CondensedBold.otf")
font_add("u67bo", "Univers-CondensedBoldOblique.otf")
font_add("u47l", "Univers-CondensedLight.otf")
font_add("u47lo", "Univers-CondensedLightOblique.otf")

# show text in graphics devices
showtext_auto()

# set theme to SPN standards
.themeSPN = theme_minimal(base_size=8, base_family="u57", base_line_size=0.1, base_rect_size=0.1) +
  theme(axis.title=element_text(color="black", size=8),
        axis.title.x.bottom=element_text(margin=margin(0.05, 0, 0, 0, "in")),
        axis.title.y.left=element_text(margin=margin(0, 0.05, 0, 0, "in")),
        axis.text=element_text(color="black", size=7),
        axis.text.x.bottom=element_text(margin=margin(0.1, 0, 0, 0, "in")),
        axis.text.y.left=element_text(margin=margin(0, 0.1, 0, 0, "in")),
        axis.ticks=element_line(color="black", size=0.1),
        axis.ticks.length=unit(-0.05, "in"),
        axis.line=element_line(color="black", size=0.1),
        legend.background=element_blank(),
        legend.margin=margin(0.01, 0.01, 0.01, 0.01, "in"),
        legend.spacing=unit(0.05, "in"),
        legend.key.height=unit(0.05, "in"),
        legend.key.width=unit(0.25, "in"),
        legend.text=element_text(color="black", size=7, margin=margin(0.01, 0.01, 0.01, 0.01, "in")),
        legend.title=element_text(family="u67b", color="black", size=8, margin=margin(0, 0, 0.05, 0, "in")),
        legend.title.align=0.5,
        legend.position="bottom",
        legend.direction="vertical",
        legend.justification="left",
        legend.box="horizontal",
        legend.box.margin=margin(0.01, 0.01, 0.01, 0.01, "in"),
        legend.box.background=element_blank(),
        legend.box.spacing=unit(0.1, "in"),
        legend.box.just="left",
        panel.background=element_blank(),
        panel.border=element_rect(fill=NA, color="black", size=0.1),
        panel.spacing=unit(0.1, "in"),
        panel.grid=element_blank(),
        plot.background=element_blank(),
        plot.title=element_text(family="u67b", size=8, margin=margin(0, 0, 0.1, 0, "in")),
        plot.margin=unit(c(0.1, 0.2, 0.05, 0.05), "in"),
        strip.background=element_blank(),
        strip.placement="outside",
        strip.text=element_text(family="u67b", color="black", size=8, hjust=0, margin=margin(0, 0, 0.05, 0, "in")))
theme_set(.themeSPN)
