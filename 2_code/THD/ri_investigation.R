n<-dim(THD_comp$variables)[1]
ntr<-table(THD_comp$variables$HDTreatment)
lm1<-lm(as.numeric(emergency_expense_bin)~as.factor(HDTreatment), data = THD_comp$variables)  
mm.ee<-lm1$model
names(mm.ee)[1]<-"emergency_expense_bin"

ri_dat<-THD_comp$variables
ri_dat$emergency_expense_bin<-as.numeric(ri_dat$emergency_expense_bin)
ri_dat$HDTreatment<-as.factor(ri_dat$HDTreatment)

thd_dec<-declare_ra(N=n, m_each=ntr, conditions = c("cntrl", "txt", "vid"))
ri_out <- conduct_ri(emergency_expense_bin ~ HDTreatment,
                     declaration = thd_dec,
                     sharp_hypothesis = 0,
                     data = ri_dat,
                     assignment = "HDTreatment",
                     sims=3000)
