glmm:ään R:ssä lme4 tai bglmer hyvä jos iso harva data,
muuten mcmcglmm. 

lme4 on tosi nopea jos geneerisistä glmm:istä puhutaan mut väitetään
		 et sen estimointimenetelmä ei toimi binomiaalidatalle
		 kovin hyvin ja sit tietysti koska se tekee
		 map-estimaatin varianssiparametreille, ne menee
		 helposti nollaan
		 

gam-mallit onnistuvat mgcv:llä. näitä kaikki olen
oikeasti käyttänytkin


####################

sisaan <- rbinom(25, size = 1000, prob = 0.5)
ulos <- rbinom(25, size = 1000, prob = 0.6)
maa <- paste("M", as.character(seq(25)), sep = "")
manner <- rep(paste("R", as.character(seq(5)), sep = ""), 5)
mat <- cbind(maa, manner, sisaan, ulos)

library(lme4) #glmer
library(blme) #bglmer
bglmer(mat ~ (1|maa) + (1|manner), family=binomial)

23:15 < scellus> mä pähkin jossain vaiheessa sitä et koska logit-glm
                 on tuollai cbindilla käytettynä validi ratioitten
                 testaukseen, kun ne countit on kuitenkin
                 poisson-jakautuneita.  jotenkin tuntu että
                 logit-linkki on keinotekoinen ja sotkee
                 kaiken. muttei se sotkekaan, homma menee ihan
                 oikein. en tiedä oliks tää ymmärrettävä, enkä ehkä
                 ymmärrä enää itekään 23:16 < scellus> nopeat
                 mixed/multilevel -mallit on hauskoja apuja
                 visualisointiinkin kun usein siellä datassa on just
                 countteja ja jotain rakennetta, eli epävarmuuksia ja
                 sit jotain riippuvuuksia alla kuitenkin

23:21 < scellus> joo, siis jos ne countit laittaa cbindilla noin ku
                 yllä, joka on sama kuin et jokainen unitti olis
                 erikseen binäärisenä joko 0 tai 1 ja sit replikoitu
                 datana yhtä monta kertaa kuin countti sanoo
############

# R version 2.14.0 (2011-10-31)
library(arm)
# arm (Version 1.4-13, built: 2011-6-19)
data(iris)

m <- bayesglm((Species=="setosa") ~ Sepal.Length*Sepal.Width + Petal.Length*Petal.Width,   
              family=binomial, data=iris)
mean(predict(m))
# [1] -2.805841
mean(predict(m, newdata=iris))
# [1] -8.24831        WTF?

m <- bayesglm((Species=="setosa") ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width 
                                    + Sepal.Length*Sepal.Width + Petal.Length*Petal.Width,   
              family=binomial, data=iris)

> mean(predict(m))
[1] -2.805841
> mean(predict(m, newdata=iris))          
[1] -2.805841