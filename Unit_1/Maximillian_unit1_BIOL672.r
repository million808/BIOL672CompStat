# Maximillian_unit1_BIOL672.r
# Author: Maximillian
# Operating System: macOS
# Libraries/Packages used: ggplot2
# Data files: desc.txt, histo.pdf

# Load the libraries
if (!require(ggplot2)) {
  install.packages('ggplot2')
  library(ggplot2)
}

# Step 1: Generate 5000 random numbers from a normal distribution
data <- rnorm(5000, mean = 0, sd = 1)

# Step 2: I am calculating the mean and standard deviation
mean_val <- mean(data)
sd_val <- sd(data)

# Step 3: I am plotting the histogram with a density line 
pdf('histo.pdf')
ggplot(data.frame(x = data), aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = 'skyblue', color = 'black') +
  geom_density(color = 'red', linewidth = 1) +
  labs(title = 'Histogram with Density Line', x = 'Value', y = 'Density')
dev.off()

# Here I am saving the  mean and SD to desc.txt
sink('desc.txt')
cat('Sample Mean:', mean_val, '\n')
cat('Sample SD:', sd_val, '\n')
sink()

# I am also saving the numbers here in a text document
write.table(data, file = "random_numbers.txt", row.names = FALSE, col.names = FALSE)
