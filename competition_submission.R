# Competition Submission 3 (9 Oct) - Group 60
# Model E, refitted on all 2,119 training cars, predictions for the 300 test cars.
# Changes from Model D (submission 2):
#   + warranty_months        (significant, partial F-test p = 0.0002)
#   province: all 9 provinces again instead of 3 groups
#   vehicle_age centred (age_c = vehicle_age - mean training age); predictions unchanged,
#   but age and age^2 are less correlated and the age coefficients are easier to read

project_traindata <- read.csv("project_traindata.csv", stringsAsFactors = TRUE)
project_testdata  <- read.csv("project_testdata.csv",  stringsAsFactors = TRUE)

# Centre age with the TRAINING mean, and use that same number for the test cars
mean_age <- mean(project_traindata$vehicle_age)
project_traindata$age_c <- project_traindata$vehicle_age - mean_age
project_testdata$age_c  <- project_testdata$vehicle_age  - mean_age

m <- lm(log(price) ~ age_c + I(age_c^2) + segment + segment:age_c + make + mileage +
          transmission + fuel_type + engine_size + province + service_history +
          previous_owners + accident_history + warranty_months + seller_type,
        data = project_traindata)

# exp() turns the log(price) predictions back into rand (the required back-transformation).
# The test set's row order is kept exactly as it is.
preds <- data.frame(Group_60 = exp(predict(m, project_testdata)))

write.table(preds, "Group_60.txt", row.names = FALSE, quote = FALSE)
