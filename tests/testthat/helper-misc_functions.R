# load semantic data file that is publicly available on the neps webpage
semantic_neps_gap <- nepstools::read_neps(test_path("test_data", "SC6_spGap_S_15-0-0.dta"))

# reduce to two rows ID_t and starting month. We dont need more for testing the packages funcs
semantic_neps_gap <- semantic_neps_gap |>
  dplyr::select(ID_t, ts2911m)

# add some random data
semantic_neps_gap <- rbind(semantic_neps_gap, data.frame(ID_t = c(1,2,3,4,5), ts2911m = c(32,-54,5,NA,1)))

