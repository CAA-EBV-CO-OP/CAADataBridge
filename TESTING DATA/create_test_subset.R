set.seed(42)
d <- read.csv("C:/Users/pryac/PR-Repo/CAACollaborative/TESTING DATA/MLS Inputdata/2603 26 ave New SFR Data Basic -FINAL.csv", check.names = FALSE)

sold <- d[d[["Status"]] == "Sold", ]
canc <- d[d[["Status"]] == "Cancelled", ]
expr <- d[d[["Status"]] == "Expired", ]
actv <- d[d[["Status"]] == "Active", ]

subset_data <- rbind(
  sold[sample(nrow(sold), 170), ],
  canc[sample(nrow(canc), 15), ],
  expr[sample(nrow(expr), 10), ],
  actv[sample(nrow(actv), 5), ]
)

subset_data <- subset_data[sample(nrow(subset_data)), ]

cat("Final rows:", nrow(subset_data), "\n")
cat("Status breakdown:\n")
print(table(subset_data[["Status"]]))

write.csv(subset_data, "C:/Users/pryac/PR-Repo/CAACollaborative/TESTING DATA/MLS Inputdata/test_200_mixed_status.csv", row.names = FALSE)
cat("Written successfully.\n")
