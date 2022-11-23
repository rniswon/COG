library(targets)

tar_outdated()

tar_make(reporter = "verbose_positives")
# tar_make_clustermq(reporter = "verbose_positives", workers = 6)
# tar_make_future(reporter = "verbose_positives", workers = 6)

tar_visnetwork(targets_only = TRUE)
