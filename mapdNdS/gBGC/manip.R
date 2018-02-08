library(ape)

DATA="TENT"

## birds data

birds=read.table("code_name_log10bodymass.txt",row.names=1,col.names=c("acro", "name","lMass"))

## dN & dS trees


dNtree=read.tree(paste(DATA,".counts_dN.dnd",sep=""))

dStree=read.tree(paste(DATA,".counts_dS.dnd",sep=""))

## retrieve on terminal branches

gettip=function(count)
{
    return(unlist(sapply(rownames(birds),function(tip){count$edge.length[which(count$edge[,2]==which(tip == count$tip))]})))
}

dNtip=gettip(dNtree)
dStip=gettip(dStree)

birds=cbind(birds, dN=dNtip, dS=dStip)

## dN S->S & dS S->S counts

dNsstree=read.tree(paste(DATA,".counts_dN_X_S->S.dnd",sep=""))
dSsstree=read.tree(paste(DATA,".counts_dS_X_S->S.dnd",sep=""))
dNsstip=gettip(dNsstree)
dSsstip=gettip(dSsstree)

birds=cbind(birds, dNss=dNsstip, dSss=dSsstip)

## dN W->W & dS W->W counts

dNwwtree=read.tree(paste(DATA,".counts_dN_X_W->W.dnd",sep=""))
dSwwtree=read.tree(paste(DATA,".counts_dS_X_W->W.dnd",sep=""))
dNwwtip=gettip(dNwwtree)
dSwwtip=gettip(dSwwtree)

birds=cbind(birds, dNww=dNwwtip, dSww=dSwwtip)


##########################
### A short study

attach(birds)

#### global dN/dS

plot(lMass,log((dN)/(dS)))
## There are two sets of leaves, low dN/dS and "high" dN/dS.

## We split the data accordingly
lowdNdS=which(log(dN/dS)< -8)

##For each subset, we observe a negative correlation between dN/dS and
## logMass (not significant because not enough data)

summary(lm(lMass[-lowdNdS] ~ log((dN)/(dS))[-lowdNdS]))

summary(lm(lMass[lowdNdS] ~ log((dN)/(dS))[lowdNdS]))


#### conservative dN/dS
##For each subset, we observe a positive correlation between
## conservative dN/dS and logMass (not significant because not enough
## data)

summary(lm(lMass[-lowdNdS] ~ log((dNss+dNww)/(dSss+dSss))[-lowdNdS]))

summary(lm(lMass[lowdNdS] ~ log((dNss+dNww)/(dSss+dSss))[lowdNdS]))





