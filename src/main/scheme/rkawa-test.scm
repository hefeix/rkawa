(require gnu.kawa.slib.testing)
(require rkawa)

(test-begin "rkawa")

(r &{a <- c(1:10)})

(test-equal (r &{a[3]}) 3)
(test-equal (r &{a[c(1, 2, 3)]}) #(1 2 3))
(test-equal (r &{a[2:6]}) #(2 3 4 5 6))

(test-end)
