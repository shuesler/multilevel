
# 1
###############################################################################
# geom_bar: Bars charts
geom_bar(mapping = NULL, data = NULL, stat = "count",
         position = "stack", ..., width = NULL, binwidth = NULL, na.rm = FALSE,
         show.legend = NA, inherit.aes = TRUE)

geom_col(mapping = NULL, data = NULL, position = "stack", ...,
         width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_count(mapping = NULL, data = NULL, geom = "bar",
           position = "stack", ..., width = NULL, na.rm = FALSE,
           show.legend = NA, inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# count
# number of points in bin

# prop
# groupwise proportion


# 2
###############################################################################
# geom_boxplot: A box and whiskers plot (in the style of Tukey)
geom_boxplot(mapping = NULL, data = NULL, stat = "boxplot",
             position = "dodge", ..., outlier.colour = NULL, outlier.color = NULL,
             outlier.fill = NULL, outlier.shape = 19, outlier.size = 1.5,
             outlier.stroke = 0.5, outlier.alpha = NULL, notch = FALSE,
             notchwidth = 0.5, varwidth = FALSE, na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

stat_boxplot(mapping = NULL, data = NULL, geom = "boxplot",
             position = "dodge", ..., coef = 1.5, na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# width 
# width of boxplot

# ymin 
# lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR

# lower 
# lower hinge, 25% quantile

# notchlower 
# lower edge of notch = median - 1.58 * IQR / sqrt(n)

# middle 
# median, 50% quantile

# notchupper 
# upper edge of notch = median + 1.58 * IQR / sqrt(n)

# upper 
# upper hinge, 75% quantile

# ymax 
# upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR


# 3
###############################################################################
# geom_contour: 2d contours of a 3d surface

geom_contour(mapping = NULL, data = NULL, stat = "contour",
             position = "identity", ..., lineend = "butt", linejoin = "round",
             linemitre = 1, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_contour(mapping = NULL, data = NULL, geom = "contour",
             position = "identity", ..., na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# level 
# height of contour


# 4
###############################################################################
# geom_count: Count overlapping points
geom_count(mapping = NULL, data = NULL, stat = "sum",
           position = "identity", ..., na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE)

stat_sum(mapping = NULL, data = NULL, geom = "point",
         position = "identity", ..., na.rm = FALSE, show.legend = NA,
         inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# n
# number of observations at position

# prop
# percent of points in that panel at that position


# 5
###############################################################################
# geom_density: Smoothed density estimates
geom_density(mapping = NULL, data = NULL, stat = "density",
             position = "identity", ..., na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

stat_density(mapping = NULL, data = NULL, geom = "area",
             position = "stack", ..., bw = "nrd0", adjust = 1, kernel = "gaussian",
             n = 512, trim = FALSE, na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# density 
# density estimate

# count 
# density * number of points - useful for stacked density plots

# scaled 
# density estimate, scaled to maximum of 1


# 5
###############################################################################
# geom_density_2d: Contours of a 2d density estimate

geom_density_2d(mapping = NULL, data = NULL, stat = "density2d",
                position = "identity", ..., lineend = "butt", linejoin = "round",
                linemitre = 1, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_density_2d(mapping = NULL, data = NULL, geom = "density_2d",
                position = "identity", ..., contour = TRUE, n = 100, h = NULL,
                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# level 
# height of contour


# 6
###############################################################################
# geom_dotplot: Dot plot

geom_dotplot(mapping = NULL, data = NULL, position = "identity", ...,
             binwidth = NULL, binaxis = "x", method = "dotdensity",
             binpositions = "bygroup", stackdir = "up", stackratio = 1,
             dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE,
             width = 0.9, drop = FALSE, na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# x 
# center of each bin, if binaxis is "x"

# y 
# center of each bin, if binaxis is "x"

# binwidth 
# max width of each bin if method is "dotdensity"; width of each bin if method is "histodot"

# count 
# number of points in bin

# ncount 
# count, scaled to maximum of 1

# density 
# density of points in bin, scaled to integrate to 1, if method is "histodot"
 
# ndensity 
# density, scaled to maximum of 1, if method is "histodot"


# 7
###############################################################################
# geom_freqpoly: Histograms and frequency polygons
geom_freqpoly(mapping = NULL, data = NULL, stat = "bin",
              position = "identity", ..., na.rm = FALSE, show.legend = NA,
              inherit.aes = TRUE)

geom_histogram(mapping = NULL, data = NULL, stat = "bin",
               position = "stack", ..., binwidth = NULL, bins = NULL, na.rm = FALSE,
               show.legend = NA, inherit.aes = TRUE)

stat_bin(mapping = NULL, data = NULL, geom = "bar", position = "stack",
         ..., binwidth = NULL, bins = NULL, center = NULL, boundary = NULL,
         breaks = NULL, closed = c("right", "left"), pad = FALSE,
         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# count 
# number of points in bin

# density 
# density of points in bin, scaled to integrate to 1

# ncount 
# count, scaled to maximum of 1

# ndensity 
# density, scaled to maximum of 1


# 8
###############################################################################
# geom_qq: A quantile-quantile plot
geom_qq(mapping = NULL, data = NULL, geom = "point",
        position = "identity", ..., distribution = stats::qnorm,
        dparams = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_qq(mapping = NULL, data = NULL, geom = "point",
        position = "identity", ..., distribution = stats::qnorm,
        dparams = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)


# Computed variables
# -----------------------------------
# sample 
# sample quantiles

# theoretical 
# theoretical quantiles


# 8
###############################################################################
# geom_quantile: Quantile regression
geom_quantile(mapping = NULL, data = NULL, stat = "quantile",
              position = "identity", ..., lineend = "butt", linejoin = "round",
              linemitre = 1, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_quantile(mapping = NULL, data = NULL, geom = "quantile",
              position = "identity", ..., quantiles = c(0.25, 0.5, 0.75),
              formula = NULL, method = "rq", method.args = list(), na.rm = FALSE,
              show.legend = NA, inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# quantile 
# quantile of distribution


# 8
###############################################################################
# geom_smooth: Smoothed conditional means
geom_smooth(mapping = NULL, data = NULL, stat = "smooth",
            position = "identity", ..., method = "auto", formula = y ~ x,
            se = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_smooth(mapping = NULL, data = NULL, geom = "smooth",
            position = "identity", ..., method = "auto", formula = y ~ x,
            se = TRUE, n = 80, span = 0.75, fullrange = FALSE, level = 0.95,
            method.args = list(), na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# y 
# predicted value

# ymin 
# lower pointwise confidence interval around the mean

# ymax 
# upper pointwise confidence interval around the mean

# se 
# standard error


# 9
###############################################################################
# geom_violin: Violin plot
geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
            position = "dodge", ..., draw_quantiles = NULL, trim = TRUE,
            scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

stat_ydensity(mapping = NULL, data = NULL, geom = "violin",
              position = "dodge", ..., bw = "nrd0", adjust = 1, kernel = "gaussian",
              trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA,
              inherit.aes = TRUE)

# Computed variables
# -----------------------------------
# density 
# density estimate

# scaled 
# density estimate, scaled to maximum of 1

# count 
# density * number of points - probably useless for violin plots

# violinwidth 
# density scaled for the violin plot, according to area, counts or to a constant maximum width
# n 
# number of points

# width 
# width of violin bounding box


# 10
###############################################################################
# stat_ecdf: Compute empirical cumulative distribution

stat_ecdf(mapping = NULL, data = NULL, geom = "step",
          position = "identity", ..., n = NULL, pad = TRUE, na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE)


# Computed variables
# -----------------------------------
# x 
# x in data

# y 
# cumulative density corresponding x


# 11
###############################################################################
# stat_function: Compute function for each x value
stat_function(mapping = NULL, data = NULL, geom = "path",
              position = "identity", ..., fun, xlim = NULL, n = 101, args = list(),
              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)


# Computed variables
# -----------------------------------
# x 
# x’s along a grid

# y 
# value of function evaluated at corresponding x


# 12
###############################################################################
# stat_summary_2d: Bin and summarise in 2d (rectangle & hexagons)
stat_summary_2d(mapping = NULL, data = NULL, geom = "tile",
                position = "identity", ..., bins = 30, binwidth = NULL, drop = TRUE,
                fun = "mean", fun.args = list(), na.rm = FALSE, show.legend = NA,
                inherit.aes = TRUE)

stat_summary_hex(mapping = NULL, data = NULL, geom = "hex",
                 position = "identity", ..., bins = 30, binwidth = NULL, drop = TRUE,
                 fun = "mean", fun.args = list(), na.rm = FALSE, show.legend = NA,
                 inherit.aes = TRUE)


# Computed variables
# -----------------------------------
# x,y 
# Location

# value 
# Value of summary statistic.





