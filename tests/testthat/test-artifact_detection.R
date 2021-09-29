test_that("non-wavelet features computed over right chunk length", {
  input <- data.frame(DateTime = c(as.POSIXct(0, origin = "1970-01-01"), 
                                   rep(NA, 40)),
             EDA = as.numeric(c(rep(0, 40), 41)),
             filtered_eda = as.numeric(rep(NA, 41)))
  
    result <- compute_features2(input)$raw_mean
    
    expect_equal(result, 
                 c(1, 41))
})

test_that("wavelet features computed over right chunk length", {
  input <- data.frame(DateTime = c(as.POSIXct(0, origin = "1970-01-01"), 
                                   rep(NA, 40)),
                      EDA = as.numeric(c(rep(0, 40), 41)),
                      filtered_eda = as.numeric(rep(NA, 41)))
  
  result <- compute_features2(input)$one_second_level_1_mean
  
  expect_equal(result, 
               c(-Inf, -Inf))
})

test_that("chunks have first row of next chunk as last row", {

  result <- data.frame(x = 1:5) %>% 
    add_chunk_group(2) %>% 
    .$x
  
  expect_equal(result,
               as.integer(c(1, 2, 3, 
                            3, 4, 5, 
                            5)))
})


