
library(bayesTPC)
require(nimble)
require(HDInterval)
library(MCMCvis)
require(coda) # makes diagnostic plots
require(IDPmisc) # makes nice colored pairs plots to look at joint posteriors
require("matrixStats")
require("truncnorm")
require(tidyverse)



set.seed(1234)
Aaeg.data <- read.csv("AeaegyptiTraitData.csv")



                                                       
                                                     
mu.data <- subset(Aaeg.data, trait.name == "mu")
lf.data <- subset(Aaeg.data, trait.name == "1/mu")
par(mfrow=c(1,2), bty="l") 
plot(trait ~ Temp, data = mu.data, ylab="mu")
plot(trait ~ Temp, data = lf.data, ylab="1/mu")
                                                     
                                                     

mu.data.inv <- mu.data # make a copy of the mu data
mu.data.inv$trait <- 1/mu.data$trait # take the inverse of the trait values to convert mu to lifespan
lf.data.comb <- rbind(mu.data.inv, lf.data) # combine both lifespan data sets together 
par(mfrow=c(1,1), bty="l") 
plot(trait ~ Temp, data = lf.data.comb, ylab="1/mu",
     ylim=c(0,40))



lf.data.bTPC<-list(Trait = lf.data.comb$trait, Temp=lf.data.comb$Temp)



AedMyBriFit <- b_TPC(data = lf.data.bTPC, ## data
                    model = 'my_briere', ## model to fit
                    niter = 11000, ## total iterations
                    burn = 1000, ## number of burn in samples
                    samplerType = 'AF_slice', ## slice sampler
                    priors = list(sigma.sq = 'dexp(1)'))   ## priors
                  


library(tidyverse)
briere_fit <- as_tibble(bayesTPC_summary(AedMyBriFit, plot = F))

head(briere_fit)


ggplot(briere_fit)+
  geom_line(aes(Tpk, z_at_Tpk), size=0.4)+
  theme_bw()+ 
  geom_ribbon(aes(Tpk, ymin=z_lwr, ymax=z_upr), fill="#30694B",alpha=0.5,
              inherit.aes = T)+
  geom_point(aes(Temp, trait), lf.data.comb, shape=21, fill='#C0C0C0',
             col='#000000', alpha=0.8, stroke=0.5, size=2)+
  theme(text = element_text(size=12))+
  scale_y_continuous(expression(plain(paste("lifespan (days)"))))+
  labs(x=expression(plain(paste(" Temperature, ",degree,"C"))))+
  theme(legend.position = 'none',
        axis.title.y = element_text(size=12), axis.title.x = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


lifespan_Tpk <- briere_fit %>% slice(which.max(Medians)) 

lifespan_Tpk[1,5] <-
  briere_fit %>% filter(Upper_bounds > 27.29, Upper_bounds < 27.31) %>%
  mutate(diff = abs(28.9-27.3)) %>%
  filter(Temp_interval < 25.8) %>%
  select(Temp_interval) %>% rename(Tpklwr = Temp_interval)


lifespan_Tpk[1,6] <-
  briere_fit %>% filter(Upper_bounds > 27.29, Upper_bounds < 27.31) %>%
  mutate(diff = abs(28.9-27.3)) %>%
  filter(Temp_interval > 25.8) %>%
  select(Temp_interval) %>% rename(Tpkupr = Temp_interval)


ggplot(briere_fit)+
  geom_line(aes(Temp_interval, Medians), size=0.4)+
  theme_bw()+ 
  geom_ribbon(aes(Temp_interval, ymin=Lower_bounds, ymax=Upper_bounds), fill="#30694B",alpha=0.5,
              inherit.aes = T)+
  geom_point(aes(Temp, trait), lf.data.comb, shape=21, fill='#C0C0C0',
             col='#000000', alpha=0.8, stroke=0.5, size=2)+
  geom_linerange(aes(x=Temp_interval, ymin=Lower_bounds, ymax=Upper_bounds),lifespan_Tpk, size=0.4, col="red")+
  geom_linerange(aes(y=Medians, xmin=Tpklwr, xmax=Tpkupr),lifespan_Tpk, size=0.4, col="red")+
  geom_point(aes(Temp_interval, Medians), lifespan_Tpk, shape=23, fill='yellow',
            col='#000000', alpha=0.8, stroke=0.5, size=3)+
  theme(text = element_text(size=12))+
  scale_y_continuous(expression(plain(paste("lifespan (days)"))))+
  labs(x=expression(plain(paste(" Temperature, ",degree,"C"))))+
  theme(legend.position = 'none',
        axis.title.y = element_text(size=12), axis.title.x = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

