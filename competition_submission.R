# Competition Submission 2 (2 Oct) - Group 60
# Model D, refitted on all 2,119 training cars, predictions for the 300 test cars.
# Changes from Model A (submission 1):
#   + engine_size            (significant, partial F-test)
#   + vehicle_age^2          (polynomial term: price does not drop in a straight line with age)
#   - vehicle_age:make       (not significant, p = 0.46)
#   province -> 3 groups     (partial F-test p = 0.62, so merging the 9 provinces is justified)

project_traindata <- read.csv("project_traindata.csv", stringsAsFactors = TRUE)
project_testdata  <- read.csv("project_testdata.csv",  stringsAsFactors = TRUE)

# Province groups: Metro / Middle / Low
province_group <- function(province) {
  factor(ifelse(province %in% c("Gauteng", "Western Cape"), "Metro",
         ifelse(province %in% c("Free State", "Limpopo", "Northern Cape"), "Low", "Middle")),
         levels = c("Metro", "Middle", "Low"))
}
project_traindata$province_group <- province_group(project_traindata$province)
project_testdata$province_group  <- province_group(project_testdata$province)

m <- lm(log(price) ~ vehicle_age + I(vehicle_age^2) + mileage + engine_size + make + segment +
          transmission + fuel_type + service_history + accident_history + seller_type +
          previous_owners + province_group + vehicle_age:segment,
        data = project_traindata)

# exp() turns the log(price) predictions back into rand (the required back-transformation).
# The test set's row order is kept exactly as it is.
preds <- data.frame(Group_60 = exp(predict(m, project_testdata)))

write.table(preds, "Group_60.txt", row.names = FALSE, quote = FALSE)
