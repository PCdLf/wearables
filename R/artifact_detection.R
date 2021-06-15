max <- function(...) suppressWarnings(base::max(... , na.rm = TRUE))
min <- function(...) suppressWarnings(base::min(... , na.rm = TRUE))
mean <- function(...) base::mean(... , na.rm = TRUE)
sum <- function(...) base::sum(... , na.rm = TRUE)
median <- function(...) stats::median(... , na.rm = TRUE)
sd <- function(...) stats::sd(... , na.rm = TRUE)

#' Configuration of the SVM algorithm for binary classification
#'
#' @author Sara Taylor \email{sataylor@@mit.edu}
#' @references \url{data_blah.com}
"binary_classifier_config"

#' Configuration of the SVM algorithm for ternary classification
#'
#' @author Sara Taylor \email{sataylor@@mit.edu}
#' @references \url{data_blah.com}
"multiclass_classifier_config"

#' First derivative
#' 
#' Get the first derivative.
#' 
#' @param values vector of numbers
get_derivative <- function(values){
  end <- length(values)
  if(end < 3){
    list(NaN)
  } else {
    list((values[2:(end-1)] + values[3:end]) / 2 - 
           (values[2:(end-1)] + values[1:(end-2)]) / 2)
  }
}

#' Second derivative
#' 
#' Get the second derivative.
#' 
#' @param values vector of numbers
get_second_derivative <- function(values){
  end <- length(values)
  if(end < 3){
    list(NaN)
  } else {
    list(values[3:end] - 2 * values[2:(end-1)] + values[1:(end-2)])
  }
}


#' Derivative features
#' 
#' Compute derivative features.
#' 
#' @param derivative vector of derivatives
#' @param feature_name name of feature 
compute_derivative_features <- function(derivative, feature_name){
  features <- list()
  
  features[paste0(feature_name, "_max")] <- max(derivative)
  features[paste0(feature_name, "_min")] <- min(derivative)
  features[paste0(feature_name, "_abs_max")] <- max(abs(derivative))
  features[paste0(feature_name, "_abs_avg")] <- mean(abs(derivative))
  
  as.data.frame(features)
}


#' Amplitude features
#' 
#' Compute amplitude features.
#' 
#' @param data vector of amplitude values
#' @importFrom dplyr across .data
compute_amplitude_features <- function(data){
  
  data %>% 
    group_by(.data$group) %>% 
    mutate(raw_derivative = get_derivative(.data$EDA),
           raw_second_derivative = get_second_derivative(.data$EDA),
           filtered_derivative = get_derivative(.data$filtered_eda),
           filtered_second_derivative = get_second_derivative(.data$filtered_eda)) %>% 
    summarize(raw_mean = mean(.data$EDA),
              filtered_mean = mean(.data$filtered_eda),
              across(c(.data$raw_derivative, .data$raw_second_derivative,
                       .data$filtered_derivative, .data$filtered_second_derivative),
                     list(max = ~max(unlist(.x)),
                          min = ~min(unlist(.x)),
                          abs_max = ~max(abs(unlist(.x))),
                          abs_avg = ~mean(abs(unlist(.x))))),
              .groups = "drop") %>%
    select(-.data$group)
  
}

#' Max value per segment of length n
#' 
#' Give the maximum value of a vector of values per segment of length n.
#' 
#' @param values array of numbers
#' @param n length of each segment
#' @param output_length argument to adjust for final segment not being full
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

#' Wavelet decomposition
#' 
#' Compute wavelet decomposition.
#' 
#' @param data vector of values
#' @importFrom waveslim dwt
compute_wavelet_decomposition <- function(data){
  output_length <- (length(data) %/% 8) * 8
  
  decompostion <- waveslim::dwt(data[1:output_length], "haar", 3, "periodic")
  
  list(level1 = decompostion$d1,
       level2 = decompostion$d2,
       level3 = decompostion$d3)
}


#' Wavelet coefficients
#' 
#' Compute wavelet coefficients.
#' 
#' @param data data with an EDA element
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

#' Addition of chunk groups
#' 
#' partition data into chunks of a fixed number of rows in order to calculate
#'   aggregated features per chunk
#' 
#' @param data df to partition into chunks
#' @param rows_per_chunk size of a chunk
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange bind_rows mutate .data
add_chunk_group <- function(data, rows_per_chunk){
  old_part <- 
    data %>% 
    dplyr::mutate(group = rep(seq(1, 
                                  by=rows_per_chunk, 
                                  length.out = nrow(data)/rows_per_chunk), 
                              each=rows_per_chunk, 
                              length.out = nrow(data)))
  
  new_part <- 
    old_part[tail(unique(old_part$group), -1), ] %>% 
    dplyr::mutate(group = .data$group - rows_per_chunk)
    
  dplyr::bind_rows(old_part, new_part) %>% 
    dplyr::arrange(.data$group)
}

#' Features computation
#' 
#' Compute features for SVM 
#' 
#' @param data df with eda, filtered eda and timestamp columns
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarize select mutate across .data
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
    add_chunk_group(sec_per_chunk) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(across(.fns = fun_lis), .groups = "drop") %>%
    dplyr::select(-.data$group)
  
  out_05sec <- coefficients$half_second_features %>%
    add_chunk_group(2 * sec_per_chunk) %>% 
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(across(.fns = fun_lis), .groups = "drop") %>%
    dplyr::select(-.data$group)
  
  amplitude_features <- 
    data %>% 
    add_chunk_group(8 * sec_per_chunk) %>% 
    compute_amplitude_features()

  timestamps <- 
    data$DateTime[1] + 
    sec_per_chunk * (1:nrow(amplitude_features) - 1)
  
  as.data.frame(cbind(id = timestamps,
                      out_1sec,
                      out_05sec,
                      amplitude_features))
  
}

#' SVM kernel
#' 
#' Generate kernel needed for SVM
#' 
#' @param kernel_transformation Data matrix used to transform EDA features
#'   into kernel values
#' @param sigma The inverse kernel width used by the kernel
#' @param columns Features computed from EDA signal 
get_kernel <- function(kernel_transformation, sigma, columns){
  kernlab::kernelMatrix(kernlab::rbfdot(sigma = sigma),
                        kernel_transformation,
                        as.matrix(columns))
}


#' Binary classifiers
#' 
#' Generate classifiers (artifact, no artifact)
#' 
#' @param data features from EDA signal
#' @export
predict_binary_classifier <- function(data){
  
  relevant_columns <-
    data[c("raw_mean",
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
  
  config <- wearables::binary_classifier_config
  
  kernel <- unname(as.data.frame(get_kernel(config$kernel_tranformation,
                                            config$sigma,
                                            relevant_columns)))
  
  labels <- sapply(kernel,
                   function(value){
                     as.integer(sign(sum(config$coefficients * value) + config$intercept))
                   })
  
  data.frame(id = data$id,
             label = labels)
}


#' Choice between two classes
#' 
#' Make choice between two classes based on kernel values
#' 
#' @param class_a Number by which class a is indicated
#' @param class_b Number by which class b is indicated
#' @param kernels Kernel values from SVM
#' @export
choose_between_classes <- function(class_a, class_b, kernels){
  config <- wearables::multiclass_classifier_config
  
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


#' Ternary classifiers
#' 
#' Generate classifiers (artifact, unclear, no artifact)
#' 
#' @param input features from EDA signal
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
  
  config <- wearables::multiclass_classifier_config
  
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


#' Artifact plots
#' 
#' Plot artifacts after eda_data is classified
#' 
#' @param labels labels with artifact classification
#' @param eda_data data upon which the labels are plotted
#' @export
#' @importFrom ggplot2 ggplot aes geom_vline geom_line
#' @importFrom dplyr .data
plot_artifacts <- function(labels, eda_data){
  binaries <-
    labels %>%
    dplyr::filter(.data$label == -1) %>%
    mutate(min = as.numeric(lubridate::force_tz(.data$id, "CEST") - eda_data$DateTime[1],
                            units = "mins")) %>%
    dplyr::pull(.data$min)
  
  eda_data %>%
    mutate(min = as.numeric(.data$DateTime - .data$DateTime[1], units = "mins")) %>%
    
    ggplot(aes(.data$min, .data$EDA)) +
    geom_vline(xintercept = binaries, colour = "red", size = 4) +
    geom_line(size = 1)
}
