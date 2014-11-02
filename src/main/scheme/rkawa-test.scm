(require 'testing)
(require rkawa)

(test-begin "rkawa")

(define max-error 0.0001)

;;----------------------------------------------------------------------
(r &{a <- c(1:10)})

(test-equal (r &{a[3]}) 3)
(test-equal (r &{a[c(1, 2, 3)]}) #(1 2 3))
(test-equal (r &{a[2:6]}) #(2 3 4 5 6))

;;----------------------------------------------------------------------
(r &{y <- matrix(1:20, nrow=5, ncol=4)
     cells <- c(1, 26, 24, 68)
     rnames <- c("R1", "R2")
     cnames <- c("C1", "C2")
     row_matrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                                 dimnames=list(rnames, cnames))
     column_matrix <- matrix(cells, nrow=2, ncol=2, byrow=FLASE,
                                    dimnames=list(rnames, cnames))})
;;      [,1] [,2] [,3] [,4]
;; [1,]    1    6   11   16
;; [2,]    2    7   12   17
;; [3,]    3    8   13   18
;; [4,]    4    9   14   19
;; [5,]    5   10   15   20
(test-equal (r &{y[1, 2]}) 6)
(test-equal (r &{y[2, 1]}) 2)
(test-equal (r &{y[1, ]}) #(1 6 11 16))
(test-equal (r &{y[, 1]}) #(1 2 3 4 5))
(test-equal (r &{y[1, c(2, 3)]}) #(6 11))

;;----------------------------------------------------------------------
(r &{patientID <- c(1, 2, 3, 4)
     age <- c(25, 34, 28, 52)
     diabetes <- c("Type1", "Type2", "Type1", "Type1")
     status <- c("Poor", "Improved", "Excellent", "Poor")
     patientdata <- data.frame(patientID, age, diabetes, status)})

;;   patientID age diabetes    status
;; 1         1  25    Type1      Poor
;; 2         2  34    Type2  Improved
;; 3         3  28    Type1 Excellent
;; 4         4  52    Type1      Poor
;; (test-equal (r &{patientdata$age}) #(25.0 34.0 28.0 52.0))

(test-end)
