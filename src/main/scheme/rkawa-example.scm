(require 'list-lib)
(require rkawa)

(define (listing-1-1)
  (r &{age <- c(1,3,5,2,11,9,3,9,12,3)
       weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
       mean_weight <- mean(weight)
       sd_weight <- sd(weight)
       cor_age_weight <- cor(age, weight)}))
