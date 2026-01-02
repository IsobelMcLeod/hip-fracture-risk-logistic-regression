bmd <- read.csv("~/Desktop/CL7730/bmd-1.csv")

#checking outcome type 
str(bmd)
table(bmd$fracture)

#boxplot of bmd vs hip fracture 
boxplot(bmd ~ fracture,
        data = bmd,
        xlab = "Hip Fracture (0 = No, 1 = Yes)",
        ylab = "Bone Mineral Density",
        main = "Bone Mineral Density by Hip Fracture Status")
#logistics regression 
bmd_model <- glm(fracture ~ bmd,
                 data = bmd,
                 family = binomial)

summary(bmd_model)

# Q2. pt.2 
full_logit <- glm(
  fracture ~ age + sex + weight_kg + height_cm +
    waiting_time + medication + bmd,
  data = bmd,
  family = binomial
)

summary(full_logit)

#finding best model 
step_logit <- step(full_logit, direction = "both", trace = 0)
summary(step_logit)

#prediction of bmd vs hip fracture 

str(bmd$bmd)

range(bmd$bmd)
range(bmd$predicted_prob)
length(bmd$bmd)
length(bmd$predicted_prob)
head(bmd$predicted_prob)


bmd$predicted_prob <- predict(step_logit, newdata = bmd, type = "response")

length(bmd$predicted_prob)
range(bmd$predicted_prob)
head(bmd$predicted_prob)

plot(bmd$bmd, bmd$predicted_prob,
     xlab = "Bone Mineral Density (BMD)",
     ylab = "Predicted Probability of Hip Fracture",
     main = "Predicted Hip Fracture Risk by BMD",
     xlim = range(bmd$bmd),
     ylim = c(0, 1))