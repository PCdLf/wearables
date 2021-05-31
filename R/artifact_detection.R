max <- function(...) suppressWarnings(base::max(... , na.rm = TRUE))
min <- function(...) suppressWarnings(base::min(... , na.rm = TRUE))
mean <- function(...) base::mean(... , na.rm = TRUE)
sum <- function(...) base::sum(... , na.rm = TRUE)
median <- function(...) stats::median(... , na.rm = TRUE)
sd <- function(...) stats::sd(... , na.rm = TRUE)

get_derivative <- function(values){
  end <- length(values)
  if(end < 3){
    NaN
  } else {
    (values[2:(end-1)] + values[3:end]) / 2 - (values[2:(end-1)] + values[1:(end-2)]) / 2
  }
}

get_second_derivative <- function(values){
  end <- length(values)
  if(end < 3){
    NaN
  } else {
    values[3:end] - 2 * values[2:(end-1)] + values[1:(end-2)]
  }
}

compute_derivative_features <- function(derivative, feature_name){
  features <- list()

  features[paste0(feature_name, "_max")] <- max(derivative)
  features[paste0(feature_name, "_min")] <- min(derivative)
  features[paste0(feature_name, "_abs_max")] <- max(abs(derivative))
  features[paste0(feature_name, "_abs_avg")] <- mean(abs(derivative))

  as.data.frame(features)
}


#' Compute amplitude features
compute_amplitude_features <- function(data){
  general_features <- data.frame(raw_mean = mean(data$EDA),
                                 filtered_mean = mean(data$filtered_eda))

  derivatives <- list(raw_derivative = get_derivative(data$EDA),
                      raw_second_derivative = get_second_derivative(data$EDA),
                      filtered_derivative = get_derivative(data$filtered_eda),
                      filtered_second_derivative = get_second_derivative(data$filtered_eda))

  derivative_features <- lapply(seq_along(derivatives),
                                function(index){
                                  compute_derivative_features(derivatives[[index]],
                                                              names(derivatives)[[index]])
                                })

  do.call("cbind", c(general_features, derivative_features))
}

max_per_n <- function(values, n, output_length){

  if (n == 1) {
    abs(values[1:output_length])
  } else {
    matrix <- matrix(values[1:(n * output_length)],
                     nrow = n,
                     byrow =  FALSE,
                     dimnames = list(1:n, 1:output_length))

    as.double(apply(abs(matrix), 2, max))
  }
}

#' @importFrom waveslim dwt
compute_wavelet_decomposition <- function(data){
  output_length <- (length(data) %/% 8) * 8

  decompostion <- waveslim::dwt(data[1:output_length], "haar", 3, "periodic")

  list(level1 = decompostion$d1,
       level2 = decompostion$d2,
       level3 = decompostion$d3)
}


#' Compute wavelet coefficients
compute_wavelet_coefficients <- function(data){

  wavelets <- compute_wavelet_decomposition(data$EDA) 

  one_second_feature_length <- ceiling(nrow(data) / 8)
  one_second_level_1_features <- max_per_n(wavelets$level1, 4, one_second_feature_length)
  one_second_level_2_features <- max_per_n(wavelets$level2, 2, one_second_feature_length)
  one_second_level_3_features <- max_per_n(wavelets$level3, 1, one_second_feature_length)

  half_second_feature_length <- ceiling(nrow(data) / 4)
  half_second_level_1_features <- max_per_n(wavelets$level1, 2, half_second_feature_length)
  half_second_level_2_features <- max_per_n(wavelets$level2, 1, half_second_feature_length)

  list(one_second_features = data.frame(one_second_level_1 = one_second_level_1_features,
                                        one_second_level_2 = one_second_level_2_features,
                                        one_second_level_3 = one_second_level_3_features),
       half_second_features = data.frame(half_second_level_1 = half_second_level_1_features,
                                         half_second_level_2 = half_second_level_2_features))
}


#' Compute wavelet features
compute_wavelet_features <- function(wavelet_coeffcients){
  functions <- c(max, mean, sd, median, function(values) sum(values > 0))
  function_names <- c("max", "mean", "std", "median", "positive")

  features <-lapply(seq_along(functions),
                    function(index){
                      features <- as.data.frame(lapply(wavelet_coeffcients,
                                                       functions[[index]]))
                      names(features) <- paste(names(features),
                                               function_names[[index]],
                                               sep = "_")
                      features
                    })

  do.call("cbind", features)
}



split_in_chunks <- function(data, rows_per_chunk){
  n_rows <- nrow(data)

  lapply(seq(1, n_rows, by = rows_per_chunk),
         function(start_index){
           end_index <- min(start_index + rows_per_chunk, n_rows)
           data[start_index:end_index, , drop = FALSE]
         })
}



#' Compute features
#' @export
compute_features <- function(data){
  sec_per_chunk <- 5

  coefficients <- compute_wavelet_coefficients(data)
  wavelet_one_second_chunks <- split_in_chunks(coefficients$one_second_features,
                                                 sec_per_chunk)
  wavelet_half_second_chunks <- split_in_chunks(coefficients$half_second_features,
                                                  sec_per_chunk * 2)

  amplitude_chunks <- split_in_chunks(data, 8 * sec_per_chunk)

  timestamps <- data$DateTime[1] + sec_per_chunk * (seq_along(amplitude_chunks) - 1)

  as.data.frame(cbind(id = timestamps,
                      do.call("rbind", lapply(wavelet_one_second_chunks, compute_wavelet_features)),
                      do.call("rbind", lapply(wavelet_half_second_chunks, compute_wavelet_features)),
                      do.call("rbind", lapply(amplitude_chunks, compute_amplitude_features))))
}

compute_features2 <- function(data){
  sec_per_chunk <- 5
  
  coefficients <- compute_wavelet_coefficients(data)
  
  fun_lis <- list(
    max = max,
    mean = mean,
    std = sd,
    median = median,
    positive = ~sum(.x > 0)
  )

  out_1sec <- coefficients$one_second_features %>%
    mutate(group = rep(seq(1, by=5, length.out = nrow(.)/5), each=5, length.out = nrow(.))) %>%
    group_by(group) %>%
    summarize(across(.fns = fun_lis), .groups = "drop") %>%
    select(-group)
  
  out_05sec <- coefficients$half_second_features %>%
    mutate(group = rep(seq(1, by=10, length.out = nrow(.)/10), each=10, length.out = nrow(.))) %>%
    group_by(group) %>%
    summarize(across(.fns = fun_lis), .groups = "drop") %>%
    select(-group)
  
  amplitude_features <- compute_amplitude_features(data)
  
  amplitude_chunks <- split_in_chunks(data, 8 * sec_per_chunk)
  timestamps <- data$DateTime[1] + sec_per_chunk * (seq_along(amplitude_chunks) - 1)
  
  as.data.frame(cbind(id = timestamps,
                      out_1sec,
                      out_05sec,
                      amplitude_features))
                      
                      # do.call("rbind", lapply(wavelet_one_second_chunks, compute_wavelet_features)),
                      # do.call("rbind", lapply(wavelet_half_second_chunks, compute_wavelet_features))
                      #do.call("rbind", lapply(amplitude_chunks, compute_amplitude_features))))
}




get_kernel <- function(kernel_transformation, sigma, columns){
  kernlab::kernelMatrix(kernlab::rbfdot(sigma = sigma),
                        kernel_transformation,
                        as.matrix(columns))
}


#' Predict binary classifier
#' @export
predict_binary_classifier <- function(input){
  relevant_columns <-
    input[c("raw_mean",
            "raw_derivative_abs_max",
            "raw_second_derivative_max",
            "raw_second_derivative_abs_avg",
            "filtered_mean",
            "filtered_second_derivative_min",
            "filtered_second_derivative_abs_max",
            "one_second_level_1_max",
            "one_second_level_1_mean",
            "one_second_level_1_std",
            "one_second_level_2_std",
            "one_second_level_3_std",
            "one_second_level_3_median")]

  config <- binary_classifier_config

  kernel <- unname(as.data.frame(get_kernel(config$kernel_tranformation,
                                            config$sigma,
                                            relevant_columns)))

  labels <- sapply(kernel,
                   function(value){
                     as.integer(sign(sum(config$coefficients * value) + config$intercept))
                   })

  data.frame(id = input$id,
             label = labels)
}

choose_between_classes <- function(class_a, class_b, kernels){
  config <- multiclass_classifier_config

  coef_a <- config$coeffcients[[paste0(class_a, "_constrasted_with_", class_b)]]
  coef_b <- config$coeffcients[[paste0(class_b, "_constrasted_with_", class_a)]]
  intercept_a_b <- config$intercept[paste0(class_a, "_and_", class_b)]
  kernel_a <- kernels[[paste0("class_", class_a)]]
  kernel_b <- kernels[[paste0("class_", class_b)]]

  sapply(seq_len(ncol(kernel_a)),
         function(index) {
           prediction_value <-
             sum(coef_a * kernel_a[,index]) +
             sum(coef_b * kernel_b[,index]) +
             intercept_a_b

           if (prediction_value > 0) {
             as.integer(class_a)
           } else {
             as.integer(class_b)
           }
         })
}


#' Predict multiclass classifier
#' @export
predict_multiclass_classifier <- function(input){

  relevant_columns <-
    input[c("filtered_second_derivative_abs_max",
            "filtered_second_derivative_min",
            "one_second_level_1_std",
            "raw_second_derivative_max",
            "raw_mean",
            "one_second_level_1_max",
            "raw_second_derivative_abs_max",
            "raw_second_derivative_abs_avg",
            "filtered_second_derivative_max",
            "filtered_mean")]

  config <- multiclass_classifier_config

  kernels <- lapply(config$kernel_tranformation,
                    get_kernel,
                    config$sigma,
                    relevant_columns)

  label_predictions <- cbind(`class -1 and 0` = choose_between_classes(-1, 0, kernels),
                             `class -1 and 1` = choose_between_classes(-1, 1, kernels),
                             `class 0 and 1` = choose_between_classes(0, 1, kernels))

  label_majority_votes <- apply(label_predictions,
                                1,
                                function(values) {
                                  values[duplicated(values)]
                                })

  data.frame(id = input$id,
             label = label_majority_votes)
}


#' Plot artifacts
#' @export
plot_artifacts <- function(labels, eda_data){
  binaries <-
    labels %>%
    dplyr::filter(label == -1) %>%
    mutate(min = as.numeric(force_tz(id, "CEST") - first(eda_data$DateTime),
                            units = "mins")) %>%
    pull(min)

  eda_data %>%
    mutate(min = as.numeric(DateTime - first(DateTime), units = "mins")) %>%

    ggplot(aes(min, EDA)) +
    geom_vline(xintercept = binaries, colour = "red", size = 4) +
    geom_line(size = 1)
}
