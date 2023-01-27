


#Sodagartojgi_HW5
#MATH 327

#5-1

genderbelief<-matrix(c(1230,357,859,413),ncol = 2,byrow = TRUE)

gb.chiq<-chisq.test(genderbelief)
gb.gsq<-with(gb.chiq,2*sum(observed*log(observed/expected)))


#X-squared = 35.183, df = 1, p-value = 3.001e-09


#5-2

#a)

polparty<-matrix(c(871,821,336,347,42,83),ncol = 3,byrow = TRUE)

p.chiq<-chisq.test(polparty)
p.gsq<-with(p.chiq,2*sum(observed*log(observed/expected)))
p.gsq

#b)producing expexted freq table

p.chiq$expected

#c) residuals

p.chiq$stdres

#5-3


#b)

fisher.test(matrix(c(21,2,15,3),ncol=2,byrow=TRUE),alternative="greater")
#p-value = 0.3808
#According to p-value 0.3808 it is plausible that control of cancer is independent of treatment used.

#c)
fisher.test(matrix(c(21,2,15,3),ncol=2,byrow=TRUE),alternative = "two.sided")
#p-value = 0.6384
#According to p-value 0.3808 it is plausible that control of cancer is independent of treatment used.

