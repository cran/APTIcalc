#' Air Pollution Tolerance Index Calculator
#'
#' @description This function calculates the Air Pollution Tolerance Index (APTI) using plant biochemical parameters: ascorbic acid (A), total chlorophyll (T),leaf extract pH (P), and relative water content (R) and classifies plants into tolerance categories based on biochemical parameters.APTI helps assess plant tolerance to air pollution.
#'
#' @param A Numeric vector of Ascorbic acid content, unit is mg per g dry weight
#' @param TC Numeric vector of Total chlorophyll content, unit is mg per g dry weight
#' @param P Numeric vector of Leaf extract pH.
#' @param R Numeric vector of Relative water content, in percentage
#'
#' @return A data frame containing APTI values and tolerance category:
#' \describe{
#'   \item{APTI}{Calculated index value.}
#'   \item{Category}{Plant tolerance class: \strong{Sensitive} (<12),
#'   \strong{Intermediate} (12–20), or \strong{Tolerant} (>20).}
#' }
#'
#' @details This function returns original parameters, computed APTI values, and  tolerance categories: "Sensitive", "Intermediate", or "Tolerant".
#'
#' @references
#' 1. Singh, S.K. and Rao, D.N. (1983) Evaluation of Plants for Their Tolerance to Air Pollution. Symposium on Air Pollution Control, 1983, 218-224.
#'
#' 2. Thakar, B. K., & Mishra, P. C. (2010). Dust collection potential and air pollution tolerance index of tree vegetation around Vedanta Aluminium Limited, Jharsuguda. The Bioscan, 3, 603-612.
#'
#' 3. Sahu, C., Basti, S., & Sahu, S. K. (2020). Air pollution tolerance index (APTI) and expected performance index (EPI) of trees in Sambalpur town of India. SN Applied Sciences, 2(8), 1327.
#'
#' @examples
#' A <- c(4.91, 5.00, 4.90)
#' TC <- c(9.987, 8.881, 8.202)
#' P <- c(3.927, 3.872, 4.168)
#' R <- c(46.71, 55.11, 52.01)
#' comp_apti(A, TC, P, R)
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_minimal theme element_text

comp_apti <- function(A, TC, P, R) {
  # Input validation
  if (length(A) != length(TC) || length(A) != length(P) || length(A) != length(R)) {
    stop("All input vectors must be of the same length.")
  }
  if (!all(sapply(list(A, TC, P, R), is.numeric))) {
    stop("All inputs must be numeric.")
  }
  if (any(c(A, TC, P, R) <= 0)) {
    stop("All input values must be positive.")
  }

  # Calculate APTI
  APTI <- (A * (TC + P) + R) / 10

  # Categorize APTI values
  Category <- ifelse(APTI < 12, "Sensitive",
                     ifelse(APTI <= 20, "Intermediate", "Tolerant"))

  result <- data.frame(
    Sample = paste0("S", seq_along(A)),
    A = round(A, 3),
    TC = round(TC, 3),
    P = round(P, 3),
    R = round(R, 3),
    APTI = round(APTI, 3),
    Category = Category,
    stringsAsFactors = FALSE
  )


  # Plot (only if in interactive session)
  if (interactive()) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Please install the 'ggplot2' package to generate the plot.")
    }
    p <- ggplot2::ggplot(result, ggplot2::aes(x = Sample, y = APTI, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", color = "black") +
      ggplot2::scale_fill_manual(values = c(
        "Tolerant" = "darkgreen",
        "Intermediate" = "orange",
        "Sensitive" = "red"
      )) +
      ggplot2::labs(title = "Air Pollution Tolerance Index (APTI)",
                    y = "APTI Value", x = "Sample") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
    }

  return(result)
}

# To avoid R CMD check NOTE about 'Sample' in ggplot2
utils::globalVariables("Sample")

