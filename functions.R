
makeTable2D <- function(flatTab,numTabVars)
{
	# MUST BE SORTED BY CHOICE VAR, IS IT???????? yes, xtabs does this
	
	if(numTabVars==1){
		flatTab=flatTab[order(flatTab[,2],flatTab[,1]),]
	}
	
	#if there are 2 or more variables 
	mergeVars=colnames(flatTab)[1:numTabVars]
	start=1
	for(i in 2:nrow(flatTab)){
		# if you reach the next choice
		if(as.integer(flatTab[i,numTabVars+1])!=as.integer(flatTab[start,numTabVars+1])){
			if(start==1){
				tabOut=cbind(flatTab[start:(i-1),c(1:numTabVars,numTabVars+2)])
				colnames(tabOut)[dim(tabOut)[2]]=paste(colnames(flatTab)[numTabVars+1],flatTab[1,numTabVars+1])
				#colnames(tabOut$Freq)
				start=i
			} else {
				tabOut=merge(tabOut,flatTab[start:(i-1),c(1:numTabVars,ncol(flatTab))],by.x=mergeVars,by.y=mergeVars, all.x = TRUE)
				colnames(tabOut)[dim(tabOut)[2]]=paste(colnames(flatTab)[numTabVars+1],flatTab[start,numTabVars+1])
				start=i
			}
		}
	}
	tabOut=merge(tabOut,flatTab[start:(i),c(1:numTabVars,ncol(flatTab))],by.x=mergeVars,by.y=mergeVars, all.x = TRUE)
	colnames(tabOut)[dim(tabOut)[2]]=paste(colnames(flatTab)[numTabVars+1],flatTab[start,numTabVars+1])
	
	#resort to fix merge's changes
	if(numTabVars==2){
		tabOut=tabOut[order(tabOut[,1],tabOut[,2]),]
	} else if(numTabVars==3) {
		tabOut=tabOut[order(tabOut[,1],tabOut[,2],tabOut[,3]),]
	} else if(numTabVars==4) {
		tabOut=tabOut[order(tabOut[,1],tabOut[,2],tabOut[,3],tabOut[,4]),]
	} else if(numTabVars==1) {
		tabOut=tabOut[order(tabOut[,1]),]
	}
	
	
	return(tabOut)
}


mergeXTables <- function(fulltabsTourcast,fulltabsEst,numTabVars){
	if(numTabVars==1){
		fulltabsTourcast=fulltabsTourcast[order(fulltabsTourcast[,1]),]
	}
	if(numTabVars==2){
		fulltabsTourcast=fulltabsTourcast[order(fulltabsTourcast[,1],fulltabsTourcast[,2]),]
	}
	if(numTabVars==3){
		fulltabsTourcast=fulltabsTourcast[order(fulltabsTourcast[,1],fulltabsTourcast[,2],fulltabsTourcast[,3]),]
	}
	
	return(fulltabs)
}


tabMargins <- function(fulltabs,i,numTabVars){
	#create results table, initially same size as full tabs
	fulltabs=fulltabs[order(fulltabs[,i]),]
	results=fulltabs
	choices=(ncol(results)-numTabVars)/2
	
	#find 
	cats=1
	for(j in 2:nrow(fulltabs)){
		#check if new
		if(fulltabs[j,i]==fulltabs[j-1,i]){ #if same category as previous, add to sum
			results[cats,numTabVars+1:(choices*2)]=results[cats,numTabVars+1:(choices*2)]+fulltabs[j,numTabVars+1:(choices*2)]
		} else { #new category, go to next line in results
			cats=1+cats
			results[cats,i]=fulltabs[j,i] #row label
			results[cats,numTabVars+1:(choices*2)]=fulltabs[j,numTabVars+1:(choices*2)]
		}
	}
	results=results[1:cats,]
	
	for(j in 1:numTabVars){
		if(j!=i){ results[,j]=NA }
	}
	
	results=rbind(results,results)
	results[cats+1:cats,numTabVars+1:(choices*2)]=0
	for(j in 1:cats){
		results[j+cats,numTabVars+1:choices]=round(results[j,numTabVars+1:choices]/sum(results[j,numTabVars+1:choices]),3)
		results[j+cats,numTabVars+choices+1:choices]=round(results[j,numTabVars+choices+1:choices]/sum(results[j,numTabVars+choices+1:choices]),3)
		if(sum(results[j,numTabVars+1:choices])==0){
			results[j+cats,numTabVars+1:choices]=0
		}
		if(sum(results[j,numTabVars+choices+1:choices])==0){
			results[j+cats,numTabVars+choices+1:choices]=0
		}
	}
	rownames(results)=c(paste(rep("Total",cats),1:cats),paste(rep("Total Shares",cats),1:cats))
	return(results)
}

dropNoAge <- function(inDF){
	if(sum(colnames(inDF)=="age3")==1){
		exclude = rep(0,nrow(inDF))
		if(class(inDF$age3)=="factor"){
			for(i in 1:nrow(inDF)){
				if(inDF$age3[i]==98 || inDF$age3[i]==99){exclude[i]=1}
		}} else {
			for(i in 1:nrow(inDF)){
				if(inDF$age3[i]>12){exclude[i]=1}
		}}
		dropExclude(inDF,exclude)
	}
	return(inDF)
}

dropExclude <- function(inDF,exclude){
	# sort by exclude
	inDF=inDF[order(exclude),]

	#eliminate those excluded
	inDF = inDF[1:(nrow(inDF)-sum(exclude)),]
	return(inDF)
}

getWbMode <- function(inDF){
	inDF$WBmode = inDF$wrkmode*inDF$iwbased+(1-inDF$iwbased)*inDF$remode
	return(inDF)
}

scaleByPtype <- function(inDF){
	expfac = rep(0,nrow(inDF))
	adjfac = c(1,1.01060864217383,1.03046907073785,3.65088900240452,0.83139535313615,0.818939325954718,3.03349440139217,1.30731934573675)
	for(i in 1:nrow(inDF)){
		expfac[i]=inDF$expfac[i]*adjfac[inDF$ptype[i]]
	}
	inDF$expfac=expfac
	return(inDF)
}

age3ToAgegroup <- function(age3){
	ageg=c(1,2,2,3,4,5,5,5,5,6,6,6,13:99)
	for(i in 1:length(age3)){
		age3[i]=ageg[age3[i]]
	}
	return(age3)
}

ovttCat <- function(inDF){
	wXitOvtt10 = rep(0,nrow(inDF))
	dXitOvtt10 = rep(0,nrow(inDF))
	
	for(i in 1:nrow(inDF)){
		wXitOvtt10[i] = 10*ceiling((inDF$otwwait1[i] + inDF$itwwait1[i] + inDF$otwwait2[i] + inDF$itwwait2[i] + inDF$otwwalkt[i] + inDF$itwwalkt[i])/10)
		dXitOvtt10[i] = 10*ceiling((inDF$otdwait1[i] + inDF$itdwait1[i] + inDF$otdwait2[i] + inDF$itdwait2[i] + inDF$otdwalkt[i] + inDF$itdwalkt[i] + inDF$otddrv_t[i] + inDF$itddrv_t[i])/10)
		if(wXitOvtt10[i]>140){
			wXitOvtt10[i]=990
		}
		if(dXitOvtt10[i]>140){
			dXitOvtt10[i]=990
		}
	}

	inDF$wXitOvtt10 = wXitOvtt10
	inDF$dXitOvtt10 = dXitOvtt10
}


TTCat <- function(inDF){
	wXitIvtt10 = rep(0,nrow(inDF))
	dXitIvtt10 = rep(0,nrow(inDF))
	dxitWalk5 = rep(0,nrow(inDF))
	
	for(i in 1:nrow(inDF)){
		wXitIvtt10[i] = 10*ceiling((inDF$otwivt_b[i] + inDF$itwivt_b[i] + inDF$otwivt_lb[i] + inDF$itwivt_lb[i] + inDF$otwivt_exp[i] + inDF$itwivt_exp[i] + inDF$otwivt_lrt[i] + inDF$itwivt_lrt[i] + inDF$otwivt_crt[i] + inDF$itwivt_crt[i])/10)
		dXitIvtt10[i] = 10*ceiling((inDF$otdivt_b[i] + inDF$itdivt_b[i] + inDF$otdivt_lb[i] + inDF$itdivt_lb[i] + inDF$otdivt_exp[i] + inDF$itdivt_exp[i] + inDF$otdivt_lrt[i] + inDF$itdivt_lrt[i] + inDF$otdivt_crt[i] + inDF$itdivt_crt[i])/10)
		dxitWalk5[i] = 5*ceiling((inDF$owalk_t[i] + inDF$iwalk_t[i])/5)
		if(wXitIvtt10[i]>140){ wXitIvtt10[i]=990 }
		if(dXitIvtt10[i]>140){ dXitIvtt10[i]=990 }
		if(dxitWalk5[i]>20){ dxitWalk5[i]=990 }
	}

	inDF$wXitIvtt10 = wXitIvtt10
	inDF$dXitIvtt10 = dXitIvtt10
	inDF$dxitWalk5 = dxitWalk5

	return(inDF)
}


getModeDist <- function(inDF){
	return(inDF$srdist)
}

getNStops <- function(inDF){
	stops=inDF$ht1wrk+inDF$ht1sch+inDF$ht1uni+inDF$ht1mnt+inDF$ht1shp
		+inDF$ht1eat+inDF$ht1dsc+inDF$ht1chf+inDF$ht1oth+inDF$ht2wrk
		+inDF$ht2sch+inDF$ht2uni+inDF$ht2mnt+inDF$ht2shp+inDF$ht2eat
		+inDF$ht2dsc+inDF$ht2chf+inDF$ht2oth
	stops=stops-(stops-3)*(stops>3)
	return(stops)
}

getTourDur <- function(inDF){
	time = rep(0,nrow(inDF))
	if(inDF$tourpurp[i]==1){			time[i]=inDF$datimeo[i]+inDF$datimei[i]}	if(inDF$tourpurp[i]==2|| inDF$tourpurp[i]==3){			time[i]=inDF$sr_timeo[i]+inDF$sr_timei[i]}	if(inDF$tourpurp[i]==4){			time[i]=inDF$otwivt_b[i]+inDF$itwivt_b[i]+inDF$otwivt_lb[i]+inDF$itwivt_lb[i]+inDF$otwivt_exp[i]+inDF$itwivt_exp[i]+inDF$otwivt_lrt[i]+inDF$itwivt_lrt[i]+inDF$otwivt_crt[i]+inDF$itwivt_crt[i]+inDF$otwwait1[i]+inDF$itwwait1[i]+inDF$otwwait2[i]+inDF$itwwait2[i]+inDF$otwwalkt[i]+inDF$itwwalkt[i]}}		inDF$otwivt_lb[i]+inDF$itwivt_lb[i]+		inDF$otwivt_exp[i]+inDF$itwivt_exp[i]+		inDF$otwivt_lrt[i]+inDF$itwivt_lrt[i]+		inDF$otwivt_crt[i]+inDF$itwivt_crt[i]+		inDF$otwwait1[i]+inDF$itwwait1[i]+		inDF$otwwait2[i]+inDF$itwwait2[i]+		inDF$otwwalkt[i]+inDF$itwwalkt[i]}	if(inDF$tourpurp[i]==5){			time[i]=inDF$otdivt_b[i]+inDF$itdivt_b[i]+inDF$otdivt_lb[i]+inDF$itdivt_lb[i]+inDF$otdivt_exp[i]+inDF$itdivt_exp[i]+inDF$otdivt_lrt[i]+inDF$itdivt_lrt[i]+inDF$otdivt_crt[i]+inDF$itdivt_crt[i]+inDF$otdwait1[i]+inDF$itdwait1[i]+inDF$otdwait2[i]+inDF$itdwait2[i]+inDF$otdwalkt[i]+inDF$itdwalkt[i]+}		inDF$otdivt_lb[i]+inDF$itdivt_lb[i]+		inDF$otdivt_exp[i]+inDF$itdivt_exp[i]+		inDF$otdivt_lrt[i]+inDF$itdivt_lrt[i]+		inDF$otdivt_crt[i]+inDF$itdivt_crt[i]+		inDF$otdwait1[i]+inDF$itdwait1[i]+		inDF$otdwait2[i]+inDF$itdwait2[i]+		inDF$otdwalkt[i]+inDF$itdwalkt[i]+		inDF$otddrv_t[i]+inDF$itddrv_t[i]}	if(inDF$tourpurp[i]==7){			time[i]=inDF$obiketime[i]+inDF$ibiketime[i]}	if(inDF$tourpurp[i]==8){			time[i]=inDF$owalk_t[i]+inDF$iwalk_t[i]}
		
	return(time)
}

getTourDurWgt <- function(inDF){
	time = rep(0,nrow(inDF))
	if(inDF$tourpurp[i]==1){	
		time[i]=inDF$datimeo[i]+inDF$datimei[i]}
	if(inDF$tourpurp[i]==2|| inDF$tourpurp[i]==3){	
		time[i]=inDF$sr_timeo[i]+inDF$sr_timei[i]}
	if(inDF$tourpurp[i]==4){	
		time[i]=inDF$otwivt_b[i]+inDF$itwivt_b[i]+inDF$otwivt_lb[i]+inDF$itwivt_lb[i]+inDF$otwivt_exp[i]+inDF$itwivt_exp[i]+inDF$otwivt_lrt[i]+inDF$itwivt_lrt[i]+inDF$otwivt_crt[i]+inDF$itwivt_crt[i]+inDF$otwwait1[i]+inDF$itwwait1[i]+inDF$otwwait2[i]+inDF$itwwait2[i]+inDF$otwwalkt[i]+inDF$itwwalkt[i]}}
		inDF$otwivt_lb[i]+inDF$itwivt_lb[i]+
		inDF$otwivt_exp[i]+inDF$itwivt_exp[i]+
		inDF$otwivt_lrt[i]+inDF$itwivt_lrt[i]+
		inDF$otwivt_crt[i]+inDF$itwivt_crt[i]+
		inDF$otwwait1[i]+inDF$itwwait1[i]+
		inDF$otwwait2[i]+inDF$itwwait2[i]+
		inDF$otwwalkt[i]+inDF$itwwalkt[i]}
	if(inDF$tourpurp[i]==5){	
		time[i]=inDF$otdivt_b[i]+inDF$itdivt_b[i]+inDF$otdivt_lb[i]+inDF$itdivt_lb[i]+inDF$otdivt_exp[i]+inDF$itdivt_exp[i]+inDF$otdivt_lrt[i]+inDF$itdivt_lrt[i]+inDF$otdivt_crt[i]+inDF$itdivt_crt[i]+inDF$otdwait1[i]+inDF$itdwait1[i]+inDF$otdwait2[i]+inDF$itdwait2[i]+inDF$otdwalkt[i]+inDF$itdwalkt[i]+}
		inDF$otdivt_lb[i]+inDF$itdivt_lb[i]+
		inDF$otdivt_exp[i]+inDF$itdivt_exp[i]+
		inDF$otdivt_lrt[i]+inDF$itdivt_lrt[i]+
		inDF$otdivt_crt[i]+inDF$itdivt_crt[i]+
		inDF$otdwait1[i]+inDF$itdwait1[i]+
		inDF$otdwait2[i]+inDF$itdwait2[i]+
		inDF$otdwalkt[i]+inDF$itdwalkt[i]+
		inDF$otddrv_t[i]+inDF$itddrv_t[i]}
	if(inDF$tourpurp[i]==7){	
		time[i]=inDF$obiketime[i]+inDF$ibiketime[i]}
	if(inDF$tourpurp[i]==8){	
		time[i]=inDF$owalk_t[i]+inDF$iwalk_t[i]}

	return(time*inDF$expfac)
}



getVehPerWrk <- function(inDF){
	autoPerWrk = rep(0,nrow(inDF))
	for(i in 1:nrow(inDF)){
		if(inDF$hhveh[i]>0){
			if(inDF$hhveh[i]/inDF$hworkers[i]>=1){autoPerWrk[i]=1}
			else if(inDF$hhveh[i]/inDF$hworkers[i]>=2){autoPerWrk[i]=2}
		}
	}
	return(autoPerWrk)
}

getVehPerLic <- function(inDF){
	autoPerDrv = rep(0,nrow(inDF))
	for(i in 1:nrow(inDF)){
		if(inDF$hhveh[i]>0){
			if(inDF$hhveh[i]/inDF$hdrivers[i]>=1){autoPerDrv[i]=1}
			else if(inDF$hhveh[i]/inDF$hdrivers[i]>=2){autoPerDrv[i]=2}
		}
	}
	return(autoPerDrv)
}


distCat <- function(inDF){
	dcat = rep(0,nrow(inDF))
	
	for(i in 1:nrow(inDF)){
		dcat[i] = ceiling(inDF$dist[i])
		#if(dcat[i]>){ dcat[i]= }
	}

	return(dcat)
}