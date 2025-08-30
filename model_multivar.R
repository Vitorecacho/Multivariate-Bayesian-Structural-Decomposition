rm(list=ls())

library(INLA)

load("multvar_likelihood_inputs.RData")

num_cores <- parallel::detectCores() - 4

control.family_b <-  list(list(initial=-0.298430  , fixed=F),
                          list(initial=-1.012226  , fixed=F),
                          list(initial= 0.053347  , fixed=F),
                          list(initial=1.3914  , fixed=F),
                          list(initial=1.836734  , fixed=F),
                          list(initial=-0.253362  , fixed=F),
                          list(initial=2.399239  , fixed=F),
                          list(initial=-0.390835  , fixed=F),
                          list(initial=1.905937  , fixed=F),
                          list(initial=1.977210  , fixed=F),
                          list(initial=0.589521  , fixed=F),
                          list(initial=1.534603  , fixed=F),
                          list(initial=0.586090  , fixed=F),
                          list(initial=2.173562  , fixed=F),
                          list(initial=-0.360973  , fixed=F),
                          list(initial=1.363154  , fixed=F),
                          list(initial=0.107122  , fixed=F),
                          list(initial=0.653498  , fixed=F),
                          list(initial=0.106402  , fixed=F),
                          list(initial=0.119450  , fixed=F),
                          list(initial=-0.261162  , fixed=F),
                          list(initial=0.475481  , fixed=F),
                          list(initial=0.046899  , fixed=F),
                          list(initial=2.097752  , fixed=F),
                          list(initial=0.715397  , fixed=F),
                          list(initial=-0.567073  , fixed=F),
                          list(initial=-0.264683  , fixed=F))






formulamultvarl_basin  <- y ~ -1 +mu1+mu2+mu3+mu4+mu5+mu6+mu7+mu8+mu9+mu10+mu11+mu12+mu13+mu14+mu15+
  mu16+mu17+mu18+mu19+mu20+mu21+mu22+mu23+mu24+mu25+mu26+mu27+
  f(rwtimebasin,model="rw2",constr=T,hyper = list(theta1 = list(initial =10.350684, fixed = F)))+
  f(seastimetbasin,model="seasonal",season.length=12,constr=T,hyper = list(theta1 = list(initial = 10.886952,fixed = F)))+
  f(fgntimebasin, constr = T, model = "fgn",hyper = list(theta1 = list(initial = 5.3138, fixed = F),theta2 = list(initial =  -1.355779, fixed = F)))
  

resmvlgamma_b <- inla(formulamultvarl_basin,
                     family = rep('gamma',27),
                     data = inla.stack.data(stack_mult),
                     control.predictor = list(A = inla.stack.A(stack_mult),
                                              compute = TRUE),
                     control.family = control.family_b,
                     control.compute = list(dic = TRUE, waic = TRUE,mlik = TRUE),
                     verbose = FALSE,
                     num.threads = num_cores,
                     control.inla = list(stupid.search=F,strategy = "adaptive", int.strategy = "eb"))
  
save(resmvlgamma_b,formulamultvarl_basin,data_filled,stack_mult,file = "multvarl_results_b.Rdata")

control.family_tsf <-  list(list(initial=3.106158  , fixed=F),
                            list(initial=0.543178  , fixed=F),
                            list(initial=1.582355  , fixed=F),
                            list(initial=2.601257  , fixed=F),
                            list(initial=2.565079  , fixed=F),
                            list(initial=2.647359  , fixed=F),
                            list(initial=2.424503  , fixed=F),
                            list(initial=2.581459  , fixed=F),
                            list(initial=2.889359  , fixed=F),
                            list(initial=2.288269  , fixed=F),
                            list(initial=1.607563  , fixed=F),
                            list(initial=2.211266  , fixed=F),
                            list(initial=2.187558  , fixed=F),
                            list(initial=2.467602  , fixed=F),
                            list(initial=2.591076  , fixed=F),
                            list(initial=2.499767  , fixed=F),
                            list(initial=1.243613  , fixed=F),
                            list(initial=1.679316  , fixed=F),
                            list(initial=0.699596  , fixed=F),
                            list(initial=1.601124  , fixed=F),
                            list(initial=0.366669  , fixed=F),
                            list(initial=0.444324  , fixed=F),
                            list(initial=0.346840  , fixed=F),
                            list(initial=2.764623  , fixed=F),
                            list(initial=0.897021  , fixed=F),
                            list(initial=-0.258043 , fixed=F),
                            list(initial=-0.266895 , fixed=F)
)


formulamultvarl_tsf  <- y ~ -1 +mu1+mu2+mu3+mu4+mu5+mu6+mu7+mu8+mu9+mu10+mu11+mu12+mu13+mu14+mu15+
  mu16+mu17+mu18+mu19+mu20+mu21+mu22+mu23+mu24+mu25+mu26+mu27+
  f(rwtimebasin,model="rw2",constr=T,hyper = list(theta1 = list(initial =11.130082, fixed = F)))+
  f(seastimetbasin,model="seasonal",season.length=12,constr=T,hyper = list(theta1 = list(initial =  7.211452,fixed = F)))+
  f(fgntimebasin, constr = T, model = "fgn",hyper = list(theta1 = list(initial = 1.573840, fixed = F),theta2 = list(initial =  4.844555, fixed = F))) +
  f(rwtime1,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 0.8211))) +
  f(rwtime3,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 1.340661))) +
  f(rwtime4,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 1.563326))) +
  f(rwtime5,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 1.558994))) +
  f(rwtime6,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 1.611237))) +
  f(rwtime7,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,inital = 0.416306))) +
  f(rwtime8,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.403880))) +
  f(rwtime9,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.387018))) +
  f(rwtime10,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.162689))) +
  f(rwtime11,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.014236))) +
  f(rwtime12,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.399558))) +
  f(rwtime13,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.899922))) +
  f(rwtime14,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.169198))) +
  f(rwtime15,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.431544))) +
  f(rwtime16,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.538804))) +
  f(rwtime17,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.192704))) +
  f(rwtime18,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.5511))) +
  f(rwtime19,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.917108))) +
  f(rwtime20,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.020526))) +
  f(rwtime21,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.781181))) +
  f(rwtime22,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.320744))) +
  f(rwtime23,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.040214))) +
  f(rwtime24,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.787614))) +
  f(rwtime25,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.2026))) +
  f(rwtime26,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.685639))) +
  f(rwtime27,copy="rwtimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.097554))) +
  f(fgntime1,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.054424))) +
  f(fgntime3,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.856432))) +
  f(fgntime4,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.922030))) +
  f(fgntime5,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.080402))) +
  f(fgntime6,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.963615))) +
  f(fgntime7,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.174012))) +
  f(fgntime8,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.100189))) +
  f(fgntime9,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.503625))) +
  f(fgntime10,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.596018))) +
  f(fgntime11,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.875174))) +
  f(fgntime12,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.3853))) +
  f(fgntime13,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.727240))) +
  f(fgntime14,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.825933))) +
  f(fgntime15,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.598337))) +
  f(fgntime16,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.423745))) +
  f(fgntime17,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.408652))) +
  f(fgntime18,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.146323))) +
  f(fgntime19,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.555706))) +
  f(fgntime20,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.670936))) +
  f(fgntime21,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.321357))) +
  f(fgntime22,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.480524))) +
  f(fgntime23,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.236437))) +
  f(fgntime24,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.953189))) +
  f(fgntime25,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.074389))) +
  f(fgntime26,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.842486))) +
  f(fgntime27,copy="fgntimebasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.611394))) +
  f(seastime1,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.107331))) +
  f(seastime3,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.135294))) +
  f(seastime4,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.529120))) +
  f(seastime5,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.629772))) +
  f(seastime6,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.554111))) +
  f(seastime7,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.211917))) +
  f(seastime8,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.814805))) +
  f(seastime9,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.8023))) +
  f(seastime10,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.4086))) +
  f(seastime11,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.905805))) +
  f(seastime12,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.975502))) +
  f(seastime13,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.830476))) +
  f(seastime14,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.725184))) +
  f(seastime15,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.826173))) +
  f(seastime16,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.645564))) +
  f(seastime17,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.629013))) +
  f(seastime18,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.858079))) +
  f(seastime19,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.618936))) +
  f(seastime20,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.400217))) +
  f(seastime21,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.611274))) +
  f(seastime22,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.228157))) +
  f(seastime23,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.631333))) +
  f(seastime24,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 0.385930))) +
  f(seastime25,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.221935))) +
  f(seastime26,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = -0.082901))) +
  f(seastime27,copy="seastimetbasin" ,hyper=list(beta=list(fixed=FALSE,initial = 1.0149)))


control.family_tsfa <- list(list(initial=0.089966  , fixed=F),
                            list(initial=1.469403  , fixed=F),
                            list(initial=2.4524  , fixed=F),
                            list(initial=3.929681  , fixed=F),
                            list(initial=2.876340  , fixed=F),
                            list(initial=5.933387  , fixed=F),
                            list(initial=2.832159  , fixed=F),
                            list(initial=5.237256  , fixed=F),
                            list(initial=3.949970  , fixed=F),
                            list(initial=2.615290  , fixed=F),
                            list(initial=2.045916  , fixed=F),
                            list(initial=2.576209  , fixed=F),
                            list(initial=2.638161  , fixed=F),
                            list(initial=4.243768  , fixed=F),
                            list(initial=3.148664  , fixed=F),
                            list(initial=5.268730  , fixed=F),
                            list(initial=2.351421  , fixed=F),
                            list(initial=2.765138  , fixed=F),
                            list(initial=2.023484  , fixed=F),
                            list(initial=2.356931  , fixed=F),
                            list(initial=1.076149  , fixed=F),
                            list(initial=1.080867  , fixed=F),
                            list(initial=1.958056  , fixed=F),
                            list(initial=3.049864  , fixed=F),
                            list(initial=1.993740  , fixed=F),
                            list(initial=2.361617  , fixed=F),
                            list(initial=2.520127  , fixed=F)
)
  
formulamultvarl_tsfa  <- y ~ -1 +mu1+mu2+mu3+mu4+mu5+mu6+mu7+mu8+mu9+mu10+mu11+mu12+mu13+mu14+mu15+
mu16+mu17+mu18+mu19+mu20+mu21+mu22+mu23+mu24+mu25+mu26+mu27+
f(rwtimebasin,model="rw2",constr=T,hyper = list(theta1 = list(initial =8.8989116, fixed = F)))+
f(seastimetbasin,model="seasonal",season.length=12,constr=T,hyper = list(theta1 = list(initial = 4.076342478,fixed = F)))+
f(fgntimebasin, constr = T, model = "fgn",hyper = list(theta1 = list(initial = 3.355255902, fixed = F),theta2 = list(initial =  0.396844925, fixed = F))) +
f(rwtime1, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.033301))) +
f(rwtime3, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.368695))) +
f(rwtime4, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.521021))) +
f(rwtime5, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.593607))) +
f(rwtime6, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.542182))) +
f(rwtime7, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.451886))) +
f(rwtime8, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.553076))) +
f(rwtime9, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.569996))) +
f(rwtime10, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.294546))) +
f(rwtime11, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.162662))) +
f(rwtime12, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.563919))) +
f(rwtime13, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.931876))) +
f(rwtime14, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.188071))) +
f(rwtime15, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.254127))) +
f(rwtime16, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.511216))) +
f(rwtime17, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.156571))) +
f(rwtime18, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.518870))) +
f(rwtime19, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.883505))) +
f(rwtime20, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.081245))) +
f(rwtime21, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.750539))) +
f(rwtime22, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.3211))) +
f(rwtime23, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.018702))) +
f(rwtime24, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.783453))) +
f(rwtime25, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.228798))) +
f(rwtime26, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.687321))) +
f(rwtime27, copy="rwtimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.078929))) +
f(fgntime1, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.414579))) +
f(fgntime3, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.400462))) +
f(fgntime4, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=3.519777))) +
f(fgntime5, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.158906))) +
f(fgntime6, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=3.528312))) +
f(fgntime7, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.068157))) +
f(fgntime8, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.228609))) +
f(fgntime9, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.137121))) +
f(fgntime10, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.682236))) +
f(fgntime11, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.873565))) +
f(fgntime12, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.045147))) +
f(fgntime13, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.453283))) +
f(fgntime14, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.228752))) +
f(fgntime15, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.576240))) +
f(fgntime16, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.467873))) +
f(fgntime17, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.037558))) +
f(fgntime18, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.353894))) +
f(fgntime19, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.001339))) +
f(fgntime20, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.781958))) +
f(fgntime21, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.149428))) +
f(fgntime22, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.502844))) +
f(fgntime23, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.141929))) +
f(fgntime24, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.710499))) +
f(fgntime25, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=0.209498))) +
f(fgntime26, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=-0.614424))) +
f(fgntime27, copy="fgntimebasin", hyper=list(beta=list(fixed=FALSE, initial=1.770192))) +
f(seastime1, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.274456))) +
f(seastime3, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.230707))) +
f(seastime4, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.316850))) +
f(seastime5, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=1.572740))) +
f(seastime6, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.293475))) +
f(seastime7, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=1.098930))) +
f(seastime8, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.480289))) +
f(seastime9, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.288771))) +
f(seastime10, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.578716))) +
f(seastime11, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.826016))) +
f(seastime12, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.787653))) +
f(seastime13, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.630253))) +
f(seastime14, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=2.568120))) +
f(seastime15, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.789084))) +
f(seastime16, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=3.178998))) +
f(seastime17, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.335288))) +
f(seastime18, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.755045))) +
f(seastime19, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.385499))) +
f(seastime20, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.836435))) +
f(seastime21, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.477796))) +
f(seastime22, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.113621))) +
f(seastime23, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=2.324747))) +
f(seastime24, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.137777))) +
f(seastime25, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=1.288259))) +
f(seastime26, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=-0.454737))) +
f(seastime27, copy="seastimetbasin", hyper=list(beta=list(fixed=FALSE, initial=0.1525))) +
f(artime1, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.07546, fixed = FALSE), theta2 = list(initial = 4.09090, fixed = FALSE))) +
f(artime2, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.24682, fixed = FALSE), theta2 = list(initial = 2.28883, fixed = FALSE))) +
f(artime3, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.881372, fixed = FALSE), theta2 = list(initial = 3.078577 , fixed = FALSE))) +
f(artime4, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.29856, fixed = FALSE), theta2 = list(initial = 5.442177 , fixed = FALSE))) +
f(artime5, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.68181, fixed = FALSE), theta2 = list(initial = 4.73006, fixed = FALSE))) +
f(artime6, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.02946, fixed = FALSE), theta2 = list(initial =  5.52147, fixed = FALSE))) +
f(artime7, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.86894, fixed = FALSE), theta2 = list(initial =  2.41423, fixed = FALSE))) +
f(artime8, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.24173, fixed = FALSE), theta2 = list(initial = 3.930859, fixed = FALSE))) +
f(artime9, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 0.256036, fixed = FALSE), theta2 = list(initial = 4.421303, fixed = FALSE))) +
f(artime10, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 0.334390, fixed = FALSE), theta2 = list(initial = 3.909899, fixed = FALSE))) +
f(artime11, model = "ar1",constr = T, hyper = list(theta1 = list(initial = -0.832816, fixed = FALSE), theta2 = list(initial = 1.754740, fixed = FALSE))) +
f(artime12, model = "ar1",constr = T, hyper = list(theta1 = list(initial = -0.800942, fixed = FALSE), theta2 = list(initial = 1.802022, fixed = FALSE))) +
f(artime13, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.001043, fixed = FALSE), theta2 = list(initial = 4.671318, fixed = FALSE))) +
f(artime14, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.745124, fixed = FALSE), theta2 = list(initial = 1.928256, fixed = FALSE))) +
f(artime15, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 0.106550, fixed = FALSE), theta2 = list(initial = 4.032081, fixed = FALSE))) +
f(artime16, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 0.823796, fixed = FALSE), theta2 = list(initial = 3.455044, fixed = FALSE))) +
f(artime17, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.165731, fixed = FALSE), theta2 = list(initial = 1.986199, fixed = FALSE))) +
f(artime18, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.1619, fixed = FALSE), theta2 = list(initial = 2.637541, fixed = FALSE))) +
f(artime19, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.574424, fixed = FALSE), theta2 = list(initial = 3.066343, fixed = FALSE))) +
f(artime20, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.604485, fixed = FALSE), theta2 = list(initial = 4.018922, fixed = FALSE))) +
f(artime21, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.638159, fixed = FALSE), theta2 = list(initial = 2.810648, fixed = FALSE))) +
f(artime22, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.437889, fixed = FALSE), theta2 = list(initial = 3.632292, fixed = FALSE))) +
f(artime23, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.923270, fixed = FALSE), theta2 = list(initial = 1.566385, fixed = FALSE))) +
f(artime24, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.432666, fixed = FALSE), theta2 = list(initial = 3.501951, fixed = FALSE))) +
f(artime25, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 1.541586, fixed = FALSE), theta2 = list(initial = 3.132258, fixed = FALSE))) +
f(artime26, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 2.959060, fixed = FALSE), theta2 = list(initial = 3.256017, fixed = FALSE))) +
f(artime27, model = "ar1",constr = T, hyper = list(theta1 = list(initial = 3.335170, fixed = FALSE), theta2 = list(initial = 3.645544, fixed = FALSE)))
  

resmvlgamma_tsf <- inla(formulamultvarl_tsf,
                     family = rep('gamma',27),
                     data = inla.stack.data(stack_mult),
                     control.predictor = list(A = inla.stack.A(stack_mult),
                                              compute = TRUE),
                     control.family = control.family_tsf,
                     control.compute = list(dic = TRUE, waic = TRUE,mlik = TRUE),
                     verbose = FALSE,
                     num.threads = num_cores,
                     control.inla = list(stupid.search=F,strategy = "adaptive", int.strategy = "eb"))

save(resmvlgamma_tsf,formulamultvarl_tsf,data_filled,stack_mult,file = "multvarl_results_tsf.Rdata")

resmvlgamma_tsfa <- inla(formulamultvarl_tsfa,
                     family = rep('gamma',27),
                     data = inla.stack.data(stack_mult),
                     control.predictor = list(A = inla.stack.A(stack_mult),
                                              compute = TRUE),
                     control.family = control.family_tsfa,
                     control.compute = list(dic = TRUE, waic = TRUE,mlik = TRUE),
                     verbose = FALSE,
                     num.threads = num_cores,
                     control.inla = list(stupid.search=F,strategy = "adaptive", int.strategy = "eb"))
  
save(resmvlgamma_tsfa,formulamultvarl_tsfa,data_filled,stack_mult,file = "multvarl_results_tsfa.Rdata")
