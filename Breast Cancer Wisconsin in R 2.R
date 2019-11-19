## Credit:  Eno Chen & Sherlin Whaley, Bethel School of Techonology 2019

library(IDPmisc)
library(dplyr)
library(rcompanion)


BCW = Breast_Cancer_Wisconsin_Diagnostic_Data_Set


BCW2 = BCW[1:569, 2:12]

BCW2$diagnosis[BCW2$diagnosis == "B"] <- 0
BCW2$diagnosis[BCW2$diagnosis == "M"] <- 1


head(BCW2)

## Normal Histograms for "mean" data


## Data for radius_mean looks slightly positively skewed
plotNormalHistogram(BCW2$radius_mean, xlab = "Radius_Mean")

BCW2$radius_meanLOG <- log(BCW2$radius_mean)
plotNormalHistogram(BCW2$radius_meanLOG, xlab = "Radius_Mean Transformed")


## Data for texture_mean looks slightly positively skewed
plotNormalHistogram(BCW2$texture_mean, xlab = "Texture_Mean")

BCW2$texture_meanLOG <- log(BCW2$texture_mean)
plotNormalHistogram(BCW2$texture_meanLOG, xlab = "Texture_Mean Transformed")


## Data for perimeter_mean looks slightly positively skewed
plotNormalHistogram(BCW2$perimeter_mean, xlab = "Perimeter_Mean")

BCW2$perimeter_meanLOG <- log(BCW2$perimeter_mean)
plotNormalHistogram(BCW2$perimeter_meanLOG, xlab = "Perimeter_Mean Transformed")


## Data for area_mean is positively skewed
plotNormalHistogram(BCW2$area_mean, xlab = "Area_Mean")

BCW2$area_meanLOG <- log(BCW2$area_mean)
plotNormalHistogram(BCW2$area_meanLOG, xlab = "Area_Mean Transformed")


##  Data for smoothness_mean looks slightly positively skewed
plotNormalHistogram(BCW2$smoothness_mean, xlab = "Smoothness_Mean")

BCW2$smoothness_meanSQRT <- sqrt(BCW2$smoothness_mean)
plotNormalHistogram(BCW2$smoothness_meanSQRT, xlab = "Smoothness_Mean Transformed")


## Data for compactness_mean is slightly positvely skewed
plotNormalHistogram(BCW2$compactness_mean, xlab = "Compactness_Mean")

BCW2$compactness_meanLOG <- log(BCW2$compactness_mean)
plotNormalHistogram(BCW2$compactness_meanLOG, xlab = "Compactness_Mean Transformed")


## Data for concavity_mean is positvely skewed
plotNormalHistogram(BCW2$concavity_mean, xlab = "Concavity_Mean")

BCW2$concavity_meanSQRT <- sqrt(BCW2$concavity_mean)
plotNormalHistogram(BCW2$concavity_meanSQRT, xlab = "Concavity_Mean Transformed")


## Data for concave_points_mean is positively skewed
plotNormalHistogram(BCW2$concave_points_mean, xlab = "Concave_Points_Mean")

BCW2$concave_points_meanSQRT <- sqrt(BCW2$concave_points_mean)
plotNormalHistogram(BCW2$concave_points_meanSQRT, xlab = "Concave_Points_Mean Transformed")


## Data for symmetry_mean is slightly positvely skewed
plotNormalHistogram(BCW2$symmetry_mean, xlab = "Symmetry_Mean")

BCW2$symmetry_meanSQRT <- sqrt(BCW2$symmetry_mean)
plotNormalHistogram(BCW2$symmetry_meanSQRT, xlab = "Symmetry_Mean Transformed")



## Data for fractal_dimension_mean is positvely skewed
plotNormalHistogram(BCW2$fractal_dimension_mean, xlab = "Fractal_Dimension_Mean")

BCW2$fractal_dimension_meanLOG <- log(BCW2$fractal_dimension_mean)
plotNormalHistogram(BCW2$fractal_dimension_meanLOG, xlab = "Fractal_Dimension_Mean Transformed")


## Drop skewed variables

BCW3 = subset(BCW2, select = -c(radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean, concavity_mean, concave_points_mean, symmetry_mean, fractal_dimension_mean))


## Backwards Elimination with the transformed variables

FitAll1 = lm(diagnosis ~ ., data = BCW3, na.action=na.exclude)

summary(FitAll1)

step(FitAll1, direction = 'backward')


## Forwards Selection with the transformed variables

fitstart = lm(diagnosis ~ 1, data = BCW3)

summary(fitstart)

step(fitstart, direction = 'forward' ,scope=formula(FitAll1))


class(BCW3$diagnosis)


# Change datatype to numeric


BCW3$diagnosis = as.numeric(BCW3$diagnosis)


# Running Logistic Model to selected variables

fitsome2 = glm(formula = diagnosis ~ concave_points_meanSQRT + texture_meanLOG + 
                area_meanLOG + radius_meanLOG + symmetry_meanSQRT + smoothness_meanSQRT + 
                perimeter_meanLOG + fractal_dimension_meanLOG, data = BCW3, family="binomial")


fitsome3 = glm(formula = diagnosis ~ concave_points_meanSQRT + texture_meanLOG + 
                 radius_meanLOG + symmetry_meanSQRT + smoothness_meanSQRT + 
                 fractal_dimension_meanLOG, data = BCW3, family="binomial")

summary(fitsome3)



step(fitstart, direction = 'both' ,scope=formula(FitAll1))

fitsome4 = glm(formula = diagnosis ~ concave_points_meanSQRT + texture_meanLOG + 
     area_meanLOG + radius_meanLOG + symmetry_meanSQRT + smoothness_meanSQRT + 
     perimeter_meanLOG + fractal_dimension_meanLOG, data = BCW3, family="binomial")

summary(fitsome4)

