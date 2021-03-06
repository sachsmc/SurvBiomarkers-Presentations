  - From associations to accuracy
  - Common measures of accuracy
  - Binary biomarkers
  - Continuous biomarkers
  - Time-to-event outcomes
  - Practical information


### Accuracy for binary outcome, binary biomarker

~ | Disease | Healthy | Total
--- | -----------| -------- | -----
  High Risk | True Positive | False Positive | Positive Predictive Value
Low Risk | False Negative | True Negative | Negative Predictive Value
Total | True Positive Rate | True Negative Rate | Accuracy

## Continuous biomarker
<iframe src="http://127.0.0.1:3838"></iframe>



## Time-to-event outcome

  ```{r, fig.align ='center', echo = FALSE, message = FALSE}
library(survival)

Rmod <- sample(c("High", "Low"), 400, replace = TRUE)
survben <- rexp(400, ifelse(Rmod== "High", 1, 2))
plot(survfit(Surv(survben, rep(1, 400)) ~ Rmod), col = c("maroon", "darkblue"), ylab = "Survival", xlab = "Years")
legend("topright", fill = c("maroon", "darkblue"), legend = c("Low risk", "High risk"))
```

## Goals

- Review methods for assessing accuracy of continuous biomarkers for prognosis of time-to-event outcomes
- Incorporate all available information

### Outline

1. Estimands, time dependent measures of accuracy
2. Modeling approaches and estimation
3. Resources for applied use
4. Extensions and active areas of research


library(data.table)

dt <- data.table(iris)
