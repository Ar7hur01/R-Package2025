#' Function 0 --> Extracting the optimal sample size of a study site generated through cLHS and KL Divergence analysis
#'
#' @param df Bands (Covariate dataframe:used by cLHS or sampling; used by KL divergence to calculate distribution)
#' @param nb number of bins for created quantiles
#' @param cseq sequence of sampled points by cLHS
#' @param its iterations of sampled data
#' @param output_file file directory
#' @import clhs
#' @import entropy
#'
#' @return csv file with optimal sample size with XYZ parameters
#' @export
#'
#' @examples

optimal_sample_size <- function(df=Bands, nb = 10, cseq = seq(10, 500, 10), its = 10, output_folder) {

  # Ensure the required libraries are loaded
#  library(clhs)
#  library(entropy)

  # Define number of covariate columns (assuming columns 3 and beyond are covariates)
  n_c <- ncol(df) - 2

  # Quantile matrix initialization for covariates
  q.mat <- matrix(NA, nrow = (nb + 1), ncol = n_c)

  # Generate quantiles for each covariate (assuming the covariates start from column 3)
  for (i in 3:ncol(df)) {
    ran1 <- max(df[, i]) - min(df[, i])
    step1 <- ran1 / nb
    q.mat[, i - 2] <- seq(min(df[, i]), to = max(df[, i]), by = step1)
  }

  # Print the quantile matrix to check if it’s correct
  print(q.mat)
  created_matrix_table <- file.path(output_folder, "quantile_matrix.csv")
  write.table(created_matrix_table, output_folder, sep = "\t")

  # Covariate data hypercube initialization
  cov.mat <- matrix(1, nrow = nb, ncol = n_c)
  for (i in 1:nrow(df)) {
    cntj <- 1
    for (j in 3:ncol(df)) {
      dd <- df[i, j]
      for (k in 1:nb) {
        kl <- q.mat[k, cntj]
        ku <- q.mat[k + 1, cntj]
        if (dd >= kl & dd <= ku) {
          cov.mat[k, cntj] <- cov.mat[k, cntj] + 1
        }
      }
      cntj <- cntj + 1
    }
  }
print(cov.mat)

  # Setup for storing results
#  mat.seq <- matrix(NA, ncol = 2, nrow = length(cseq))
#  samples_list <- vector("list", length(cseq))

  # Main loop over different sample sizes
#  for (w in 1:length(cseq)) {
#    s.size <- cseq[w]  # sample size
#    mat.f <- matrix(NA, ncol = 10, nrow = its)  # Placeholder for iteration outputs

    # Internal loop for iterations
#    for (j in 1:its) {
#      repeat {
#        ss <- clhs(df[, 3:n_c], size = s.size, progress = T, iter = 1000)  # Perform cLHS sampling
#        s.df <- df[ss, ]

#        if (sum(duplicated(s.df) | duplicated(s.df[nrow(s.df):1, ])[nrow(s.df):1]) < 2) {
#          break
#        }
#      }

      # Save samples in the list
#      if (is.null(samples_list[[w]])) {
#        samples_list[[w]] <- list()
#      }
#      samples_list[[w]][[j]] <- s.df

      # KL Divergence calculation
#      h.mat <- matrix(1, nrow = nb, ncol = n_c)
#      for (ii in 1:nrow(s.df)) {
#        cntj <- 1
#        for (jj in 3:ncol(s.df)) {
#          dd <- s.df[ii, jj]
#          for (kk in 1:nb) {
#            kl <- q.mat[kk, cntj]
#            ku <- q.mat[kk + 1, cntj]
#            if (dd >= kl & dd <= ku) {
#              h.mat[kk, cntj] <- h.mat[kk, cntj] + 1
#            }
#          }
#          cntj <- cntj + 1
#        }
#      }

      # Calculate KL divergence for each covariate and compute the average
#      klo_values <- sapply(1:n_c, function(i) KL.empirical(c(cov.mat[, i]), c(h.mat[, i])))
#      mat.f[j, 10] <- mean(klo_values)  # Store average KL divergence
#    }

    # Calculate mean and sd for the KL divergence values
#    mat.seq[w, 1] <- mean(mat.f[, 10])
#    mat.seq[w, 2] <- sd(mat.f[, 10])
#  }

  # Create a data frame with the results
#  dat.seq <- as.data.frame(cbind(cseq, mat.seq))
#  names(dat.seq) <- c("samp_nos", "mean_KL", "sd_KL")

  # Normalize the KL values
#  min_KL <- min(dat.seq$mean_KL)
#  max_KL <- max(dat.seq$mean_KL)
#  dat.seq$normalized_KL <- 1 - (dat.seq$mean_KL - min_KL) / (max_KL - min_KL)

  # Print results first
#  print(dat.seq)

  # Find the sample size corresponding to a normalized KL value of 0.95 or greater
#  optimal_sample_size <- dat.seq$samp_nos[dat.seq$normalized_KL >= 0.95][1]  # Find first value >= 0.95

#  if (length(optimal_sample_size) == 0) {
#    print("No sample size meets the 0.95 normalized KL threshold.")
#    return(NULL)  # Return NULL if no sample size meets the threshold
#  }

  # Retrieve the optimal sample corresponding to the best normalized KL value
#  optimal_sample <- samples_list[[which(cseq == optimal_sample_size)]]

  # Optionally: Save the optimal sample to a file
#  if (!is.null(output_file)) {
#    write.csv(optimal_sample[[1]], file = output_file, col.names = TRUE, row.names = FALSE, sep = ",")
#  }

# Return the actual sample set (coordinates and values) for the optimal sample size
#  return(optimal_sample[[1]])  # Return the X and Y coordinates of the optimal sample
}
