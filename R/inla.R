# https://gist.github.com/euxoa/268426379d4cf2d03297e23f4257db9a
# euxoa

#Malli on siis logit(p_presence) = 1 + s(n, e) + log(nspec), jossa
#s(n, e) on spatiaalinen termi koordinaattien suhteen (north, east) ja
#nspec on spatiaalisesti diversiteettikorjattu paikalta havaittujen
#lajien määrä (kontrolloi havaintointensiteettiä). p_presence on
#todennäköisyys havainnolle ko. ruudusta, ja data eli tuplet (presence
#0/1, n, e, nspec) on yli kaikkien ruutujen joista on ylipäätään
#havaittu mitään, eli joissa nspec>0.

# Nämä ovat olevinaan levinneisyyskarttoja. Kuten voit eliöryhmästä
#  kuvitella, ongelmana on havaintojen harvuus ja toisaalta
#  havaintointensiteetin vaihtelu ruuduttain. Kai prediktiivinen on
#  parempi, mutta karttoja yllä ei ole evaluoitu prediktiivisesti (LOO
#  tms.) INLA näyttää minusta paremmalta (arvaus). :) mgcv on
#  olemukseltaan enemmän kuin konvoluutio tai smootheri, INLA tekee
#  jotain monimutkaisempaa, sillä on tuollainen epävarmuuden
#  tasanko. Varmaan ero tulee pitkälti hyperparametrien
#  piste-estimoinnista vs. integroinnista.

# Levinneisyyskartoissa on kyllä visuaalinen puoli myös tärkeä. Ja
#  joidenkin lajien levinneisyysalue on aika patologinen, mgcv
#  suoriutui niistä hyvin, INLAa en ehtinyt harvinaisilla lajeilla
#  testata.

#Yritin myös GP:ja Stanilla, mutta olivat tuolloin liian
#hitaita. Pitäisi implementoida joku edistyksellisempi GP-inferenssi,
#spektraalinen tai muu joka ottaisi kaksidimensionaalisuuden huomioon.

library(INLA)

crd.d <- cbind(d.arr2$e, d.arr2$n)
mesh1 <- inla.mesh.2d(crd.d, max.edge=c(7, 7), cutoff=1.5)

spec <- "Micrasema_gelidum"
plot(mesh1)
points(d.arr2$e, d.arr2$n, pch=19, col="#ff000030")
points(d.arr2$e[d.arr2[,spec]==1], d.arr2$n[d.arr2[,spec]==1], pch=19, col="#00ff0050")

spde1 <- inla.spde2.matern(mesh1, alpha=2.0)
Aobs <- inla.spde.make.A(mesh1, loc=crd.d) # mesh -> loc
Apred <- inla.spde.make.A(mesh1, loc=cbind(d.pred$e, d.pred$n))
# nloc: 1347, nmesh: 824

stack.obs<- 
  inla.stack(data = list(presence = d.arr2[,spec]), 
           A = list(Aobs, 1),
           effects = list(c(list(intercept = rep(1, mesh1$n)),
                            inla.spde.make.index("spatial", spde1$n.spde)),
                          lnspec = log(d.arr2$nspec)), 
           tag="obs")

stack.pred <- 
  inla.stack(data = list(presence = NA), 
             A = list(Apred, 1),
             effects = list(c(list(intercept = rep(1, mesh1$n)),
                              inla.spde.make.index("spatial", spde1$n.spde)),
                            lnspec = log(d.pred$nspec)), 
             tag="pred")

stack.all <- inla.stack(stack.obs, stack.pred)

m <- inla(presence ~ 0 + intercept + lnspec + f(spatial, model=spde1),
          data=inla.stack.data(stack.all, spde=spde1), 
          family="binomial",
          control.predictor = list(A = inla.stack.A(stack.all), compute=TRUE),
          verbose=T)

pred.data <- data.frame(n = d.pred$n, 
                        e = d.pred$e, 
                        m = m$summary.linear.predictor[inla.stack.index(stack.all, 'pred')$data,]$mean,
                        s = m$summary.linear.predictor[inla.stack.index(stack.all, 'pred')$data,]$sd)
pred.data$p <- with(pred.data, 
                    sapply(seq(.01, .99, .01), function (p) qnorm(p, m, s) %>% { 1/(1+exp(-.)) }) %>% rowMeans)

# TODO
# - http://www.math.ntnu.no/inla/r-inla.org/tutorials/spde/spde-tutorial.pdf
#   and the faster prediction method therein.
# - field hyperprior
# - mesh fine enough?
