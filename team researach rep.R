
# Subset the data for "Fatal(Y/N)" as "Y" or "N"
df = attacks
names(df)[13] = "Fatal(Y/N)"  # Renaming the 13th column to "Fatal(Y/N)"

# Filter the data to include only rows where Fatal(Y/N) is either "Y" or "N"
df2 <- subset(df, `Fatal(Y/N)` == "Y" | `Fatal(Y/N)` == "N")

# List of countries to include (in capital letters)
countries_of_interest <- c("USA", "AUSTRALIA", "SOUTH AFRICA", "NEW ZEALAND", 
                           "BRAZIL", "BAHAMAS", "ITALY", "FIJI", "MEXICO", 
                           "NEW CALEDONIA", "CUBA", "SPAIN", "EGYPT", 
                           "INDIA", "PAPUA NEW GUINEA", "REUNION", "MOZAMBIQUE", "CROATIA", "JAPAN", 
                           "IRAN", "PANAMA", "GREECE", "JAMAICA", "ENGLAND", "SRI LANKA", "TONGA", 
                           "BERMUDA", "FRANCE", "IRAQ")

# Subset the data to include only the countries you're interested in
df2_subset1 <- df2[df2$Country %in% countries_of_interest, ]

# Create the table
TB <- table(df2_subset1$`Fatal(Y/N)`, df2_subset1$Country)
sper <- prop.table(TB, margin=2) * 100

# Sort the table by the percentage of fatalities ("Y") in descending order
sorted_indices <- order(sper[1, ], decreasing=TRUE)
sper_sorted <- sper[, sorted_indices]

# Perform chi-squared test
chisq.test(sper_sorted)

# Plotting
par(mar = c(5, 4, 4, 2) + 0.1)  # Adjusts the plot margins

barplot(sper_sorted, 
        col = c("blue", "lavender"), 
        xlab = "Country", 
        ylab = "Percentage", 
        main = "Stacked Bar Of Fatality between countries", 
        ylim = c(0, 100), 
        legend.text = c("Y", "N"), 
        args.legend = list(x = "topright"),
        cex.names = 0.5, 
        las = 2)  # Rotate labels vertically
sper
