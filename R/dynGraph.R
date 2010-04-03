dynGraph <- function(res) {
	
	#ligneDir = paste(.find.package(package="FactoMineR")[1],"/etc/dynGraph/",sep="")
	ligneDir = paste(.find.package(package="dynGraph")[1],"/etc/dynGraph/",sep="")
	if(.Platform$OS.type!="unix"){
		ligneDir=gsub("/","\\\\\\\\",ligneDir)
		ligneDir=gsub("\\\\\\\\","\\\\",ligneDir)
		ligneDir=gsub("\\\\","\\\\\\\\",ligneDir)
	}	
	dir=ligneDir
	

	#chargement de la librarie qui dialogue entre R et java
	library(rJava)
	#initialisation de la machine virtuelle avec mes classes
	.jinit(classpath=paste(dir,"dynGraph.jar",sep=""))
	#fonction dynGraph
	
	
	#trouve le groupe d'une variable (continue ou nominale) dont le nom est passÃ© en paramÃ¨tre
	trouveGroupe<-function(var){
		val=0
		if(mfa){
			if(!is(res$summary.quanti,"NULL")){		
				for(i in 1:dim(res$summary.quanti)[1]){
					if(identical(res$summary.quanti[i,2],var)){ val = res$summary.quanti[i,1]}
				}
			}
			if(!is(res$summary.quali,"NULL") && dim(res$summary.quali)[1]>0){			
				for(i in 1:dim(res$summary.quali)[1]){
					if(identical(res$summary.quali[i,2],var)){ val = res$summary.quali[i,1]}
				}
			}
		}else if(afdm){
			val=position(res$call$name.group,var)
			val=contient(res$call$num.group.sup,p)
		}
		return(val)
	}
	
	#teste si vect contient val
	contient<-function(vect,val){
		for(i in vect){
			if(i==val){return(TRUE)}
		}
		return(FALSE)
	}
	
	#trouve la position de val dans vect
	position<-function(vect,val){
		for(i in 1:length(vect)){
			if(vect[i]==val){return(i)}
		}
		return(0)
	}
	
	#retourne vect2 moins les Ã©lÃ©ments de vect1
	difference<-function(vect1, vect2) {
		n=1
		vect=c()
		for(i in vect2){
			if(contient(vect1,i)){
				
			}else{
				if(!is(vect,"NULL")){
					vect<-c(vect,i)
				}else{
					vect<-c(i)
				}
			}
		}
		return(vect)
	}
	
	#trouve la nominale d'une modalitÃ©
	trouveNom<-function(var){
		val=0
		if(mfa){
			for(i in 1:dim(res$summary.quali)[1]){
				if(identical(res$summary.quali[i,3],var)){ val = res$summary.quali[i,2]}
			}
		}else if(afdm){
			for(i in 1: dim(res$call$quali.sup$quali.sup)[2]){
				if(contient(as.vector(res$call$quali.sup$quali.sup[,i]),var)
						){ val =  names(res$call$quali.sup$quali.sup)[i]}
			}
		}
		return(val)
	}
	
	trouveDec<-function(numNom){
		decalage=1
		while(numNom%/%10^decalage>0){
			decalage=decalage+1
			}
		return(decalage)				
	}
	
	ajouteNumCont<-function(listeNumCont,liste){	
		for(nom in liste){
			listeNumCont=c(listeNumCont,position(colnames(res$call$X),nom))
			}
		return(listeNumCont)
	}
	
	############################
	#  calculs prÃ©liminaires   #
	############################
	
	
	#type d'analyse :
	mfa=inherits(res,"MFA") && 
			(!is(res$summary.quanti,"NULL") ||
				(!is(res$summary.quali,"NULL") && dim(res$summary.quali)[1]>0))
	pca=inherits(res,"PCA")
	mca=inherits(res,"MCA")
	ca=inherits(res,"CA")
	afdm=inherits(res,"MFA") && 
			(is(res$summary.quanti,"NULL") &&
				is(res$summary.quali,"NULL"))
	dataframe=inherits(res,"data.frame")
    catego=inherits(res,"catego")	
	if(mfa){
		typeAnalyse="MFA"
	}else if(pca){
		typeAnalyse="PCA"
	}else if(mca){
		typeAnalyse="MCA"
	}else if(ca){
		typeAnalyse="CA"
	}else if(afdm){
		typeAnalyse="AFDM"
	}else if(dataframe){
		typeAnalyse="dataframe"
	}else if(catego){
        typeAnalyse="catego"
    }
	#calcul du nombre d'axes
	if(dataframe){
		nbAxe=0
	}else{
		nbAxe<-res$call$ncp
	}
	
	#calcul du nombre de variables continues 
	
	#nombre de variables continues illustratives	
	if(pca || mca){
		if(!is(res$quanti.sup,"NULL")){
			nbContIllus=dim(res$quanti.sup$coord)[1]
		}else{
			nbContIllus=0
		}
	}else if(mfa || afdm){
		if(!is(res$quanti.var.sup,"NULL")){
			nbContIllus=dim(res$quanti.var.sup$coord)[1]
		}else{
			nbContIllus=0
		}
	}else if(ca){
		nbContIllus=0
	}else if(catego){
        nbContIllus=0
    }else if(dataframe){
       	nbContIllus=0
    }
	
	#nombre de variables continues actives
	if(mfa){#en MFA
		nbContAct=0
		if(!is(res$summary.quanti,"NULL")){
			for(i in 1:dim(res$quanti.var$coord)[1]){
				groupe=trouveGroupe(rownames(res$quanti.var$coord)[i])
				if(!contient(res$call$num.group.sup,groupe)){
					nbContAct=nbContAct+1
				}
			}	
		}	
	} else if(pca){#en PCA
		nbContAct<-dim(res$var$contrib)[1]
	}else if(mca){#en MCA
		nbContAct=0
	}else if(ca){#en CA
		nbContAct=0
	}else if(afdm){
		nbContAct=dim(res$quanti.var$contrib)[1]
	}else if(catego){
        nbContAct=0
    }else if(catego){
        nbContAct=0
    }else if(dataframe){
        nbContAct=0
    }
	
	#nombre de variables continues 
	nbCont=nbContAct + nbContIllus
	
	listeNumCont=c()
	if(pca || mca){
		if(nbContAct>0){
			listeNumCont=ajouteNumCont(listeNumCont,rownames(res$var$coord))		
		}
		if(nbContIllus>0){
			listeNumCont=ajouteNumCont(listeNumCont,rownames(res$quanti.sup$coord))
		}
	}else if(mfa || afdm){
		if(nbContAct>0){
			listeNumCont=ajouteNumCont(listeNumCont,rownames(res$quanti.var$coord))		
		}
		if(nbContIllus>0){
			listeNumCont=ajouteNumCont(listeNumCont,rownames(res$quanti.var.sup$coord))
		}
	}
	
	
	
		
	#calcul du nombre de variables nominales
	
	
	#nombre de variables nominales actives
	if(mca || catego){
		nbNomIllus=length(res$call$quali.sup)
		nbNomAct=dim(res$call$X)[2]-nbCont-nbNomIllus
	}else if(mfa){	
		nbNomAct=0
		listeNomAct=c()
		listeNomIllus=c()
		listeNom=c()
		
		if((!is(res$summary.quali,"NULL") && dim(res$summary.quali)[1]>0)){
			for(i in 1:dim(res$call$quali.sup$quali.sup)[2]){
				groupe=trouveGroupe(colnames(res$call$quali.sup$quali.sup)[i])
				if(!contient(res$call$num.group.sup,groupe)){
					nbNomAct=nbNomAct+1
					listeNomAct=c(listeNomAct,i)
				}else{
					listeNomIllus=c(listeNomIllus,i)
				}
				listeNom=c(listeNom,i)
			}
		}
	}else if(pca){#pas de variables nominales actives en PCA
		nbNomAct=0
	}else if(ca){
		nbNomAct=2
	}else if(afdm){
		nbNomAct=0
		listeNomAct=c()
		listeNomIllus=c()
		listeNom=c()
		for(i in 1:length(res$call$type)[1]){
			if(res$call$type[i]=="n"){
				#il s'agit d'une nominale
				if(!contient(res$call$num.group.sup,i)){
					nbNomAct=nbNomAct+1
					listeNomAct=c(listeNomAct,i)
				}else{
					listeNomIllus=c(listeNomIllus,i)						
				}
				listeNom=c(listeNom,i)
			}
		}
	}else if(dataframe){
        nbNomAct=0
        nbNom=0
    }
	
	#nombre de variables nominales
	if(!ca){
		nbNom=dim(res$call$X)[2]-nbCont #taille du tableau de donnÃ©es - nombre de continues
	}else{
		nbNom=2
	}
	
	
	#nombre de variables nominales illustratives	
	nbNomIllus=nbNom-nbNomAct
	
	
	#traitement particulier du dataframe
	if(dataframe){
		listeNom=c()
		nbNom=0
		listeNumCont=c()
		nbCont=0
		for(i in 1:dim(res)[2]){
			if(inherits(res[1,i],"factor")){
				nbNom=nbNom+1
				listeNom=c(listeNom,i)		
			}else{
				nbCont=nbCont+1
				listeNumCont=c(listeNumCont,i)				
			}	
		}
	}
	
	
	
	#calcul du nombre de colonnes dans le tableau Ã  construire
	nbCol<-4+3*nbAxe+nbCont
	
	
	#calcul des noms de colonnes
	nomsVal=c("Statut","Groupe","Nombre_axes",paste("Axe",names(res$ind$coord[1,])),paste("Contrib",names(res$ind$contrib[1,])),paste("Cos2",names(res$ind$cos2[1,])),"Nombre_var",names(res$call$X[listeNumCont]))
	if(pca || afdm){
		nomsAnnot=c("Annotation", colnames(res$call$quali.sup$quali.sup), NA)
	}else if(mfa){
		nomsAnnot=c("Annotation")
		for(i in colnames(res$call$quali$quali)){
			nomsAnnot=c(nomsAnnot,i)
		}
		nomsAnnot=c(nomsAnnot,NA)
	} else if(mca || catego){
		estActive <- function(mod) {
			return(contient(names(res$call$marge.col),mod))
		}
		
		nominales=difference(rownames(res$quanti.sup$coord), colnames(res$call$X))
		
		nominalesActives=c()
		for(i in 1:dim(res$call$X)[2]){
			if(estActive(res$call$X[1,i])){nominalesActives=c(nominalesActives,colnames(res$call$X)[i])}			
			
		}
		
		nominalesIllustratives=difference(nominalesActives,nominales)
		
		nomsAnnot=c("Annotation",nominales,NA)
	} else if(ca){
		nomsAnnot=c("Annotation","row","col",NA)
	} else if(dataframe){
		nomsAnnot=c("Annotation",colnames(res)[listeNom],NA)
	}
	
	
	if(mfa || afdm || catego){
		#calcul des nombres de groupes
		nbGroupes=length(res$call$group)
		nbGroupesIllus=length(res$call$num.group.sup)
		nbGroupesAct=nbGroupes-nbGroupesIllus		
	}
	
	#initialisation du parser :
	###########################
	obj<-.jnew("parse",nomsVal,nomsAnnot,dir)
	.jcall(obj,,"setType",typeAnalyse)
	
	
	############################
	#construction des tableaux #
	############################
	
	
	
	# inerties portÃ©es par les axes
	###############################
	
	tab=c(0,0,nbAxe)
	for(i in 1:nbAxe){
		tab=c(tab,res$eig[i,2])		
	}
	.jcall(obj,,"addVal","inerties",as.matrix(tab))
	
	
	# Axes partiels (AFM)
	######################
	if(mfa){
		for(i in 1:dim(res$partial.axes$coord)[1]){
			coord=unlist(res$partial.axes$coord[i,])
			cos2=c()
			for(n in 1:nbAxe){
				cos2=c(cos2,res$partial.axes$cor[i,n]*res$partial.axes$cor[i,n])
			}
			contrib=unlist(res$partial.axes$contrib[i,])			
			nom=rownames(res$partial.axes$coord)[i]
			liste=unlist(strsplit(nom,".",fixed=TRUE))
			nomGr=liste[2]
			if(length(liste)>2){#si le nom de groupe a la bonne idée de contenir des '.', il faut tout reconstruire
				for(j in 3:length(liste)){
					nomGr=paste(nomGr,liste[j],sep=".")
				}
			}
			numeroGr=position(res$call$name.group,nomGr)
			
			tab<-as.matrix(c(0,numeroGr,nbAxe,coord,contrib,cos2))
			.jcall(obj,,"addVal",nom,tab)
		}				
	}
	
	
	
	
	# Groupes (AFM ou AFDM)
	###################	
	
	
	if(mfa || afdm || catego){
		
		numGrAct=1
		numGrIllus=1
		for(i in 1:length(res$call$name.group)){
			
			if(contient(res$call$num.group.sup,i)){				
				coord=unlist(res$group$coord.sup[numGrIllus,])		
				contrib=unlist(res$group$contrib.sup[numGrIllus,])
				cos2=unlist(res$group$cos2.sup[numGrIllus,])
				
				tab<-as.matrix(c(10,0,nbAxe,coord,contrib,cos2,0))
				#appel parser
				.jcall(obj,,"addVal",res$call$name.group[i],tab)	
				numGrIllus=numGrIllus+1
			}else{
				coord=unlist(res$group$coord[numGrAct,])		
				contrib=unlist(res$group$contrib[numGrAct,])
				cos2=unlist(res$group$cos2[numGrAct,])
				
				tab<-as.matrix(c(9,0,nbAxe,coord,contrib,cos2,0))
				#appel parser
				.jcall(obj,,"addVal",res$call$name.group[i],tab)	
				numGrAct=numGrAct+1
				
				
				
			}
			
		}
		
		
	}	
	
	
	
	
	if(nbNom>0){
		
		if(pca){
			
			# modalitÃ©s moyennes illustratives (pas de partielles, et pas de nominales actives en ACP)
			#############################################################################################
			
			
			quali=res$call$quali.sup$quali.sup
			
			numMod=0
			for(numNom in 1:nbNom){
				mod=c()
				
				decalage=trouveDec(numNom)
				
				for(i in 1:dim(quali)[1]){
					if(!contient(mod,as.integer(quali[i,numNom]))){
						mod=c(mod,as.integer(quali[i,numNom]))
						numMod=numMod+1					
						
						coord=unlist(res$quali.sup$coord[numMod,])	
						#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹				
						contrib=c(1:nbAxe)
						cos2=unlist(res$quali.sup$cos2[numMod,])
						
						tab<-c(8*10^decalage+numNom,0,nbAxe,coord,contrib,cos2)					
						
						.jcall(obj,,"addVal",as.character(rownames(res$quali.sup$coord)[numMod]),as.matrix(tab))
					}
					
				}
				
			}
		}else if(mfa || afdm){	
			
			# modalitÃ©s moyennes actives 
			#############################
			quali=res$call$quali.sup$quali.sup
			
			numNom=0
			nom=c()
			decalage=1			
			if(nbNomAct>0){
				for(i in 1:dim(res$quali.var$coord)[1]){
					nomMod=rownames(res$quali.var$coord)[i]
					nomNom=trouveNom(nomMod)
					if(!contient(nom,nomNom)){
						nom=c(nom,nomNom)
						numNom=position(nomsAnnot,nomNom)-1
						decalage=floor(log(numNom)/log(10))+1
					}
					coord=unlist(res$quali.var$coord[i,])				
					contrib=unlist(res$quali.var$contrib[i,])
					cos2=unlist(res$quali.var$cos2[i,])
					
					ident=7*10^decalage+numNom
					
					tab<-c(ident,0,nbAxe,coord,contrib,cos2)					
					
					.jcall(obj,,"addVal",as.character(nomMod),as.matrix(tab))
					
					if(mfa){
						gr=1
						for(j in ((i-1)*nbGroupesAct+1):(i*nbGroupesAct)){
							coord=unlist(res$quali.var$coord.partiel[j,])	
							contrib=1:nbAxe
							cos2=1:nbAxe					
							tab2<-c(ident,gr,nbAxe,coord,contrib,cos2,nbCont)
							.jcall(obj,,"addVal",as.character(nomMod),tab2)
							gr=gr+1
						}
					}
					
					
				}
			}
			
			
			# modalitÃ©s moyennes illustratives
			###################################
			
			quali=res$call$quali.sup$quali.sup
			
			decalage=1
			if(nbNomIllus>0){
				for(i in 1:dim(res$quali.var.sup$coord)[1]){
					nomMod=rownames(res$quali.var.sup$coord)[i]
					nomNom=trouveNom(nomMod)
					if(!contient(nom,nomNom)){
						nom=c(nom,nomNom)
						numNom=position(nomsAnnot,nomNom)-1
						decalage=floor(log(numNom)/log(10))+1
					}
					
					coord=unlist(res$quali.var.sup$coord[i,])	
					#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹				
					contrib=c(1:nbAxe)
					cos2=unlist(res$quali.var.sup$cos2[i,])
					
					ident=8*10^decalage+numNom
					
					tab<-c(ident,0,nbAxe,coord,contrib,cos2)					
					
					.jcall(obj,,"addVal",as.character(nomMod),as.matrix(tab))	
					
					
					if(mfa){	
						gr=1
						for(j in ((i-1)*nbGroupesAct+1):(i*nbGroupesAct)){
							coord=unlist(res$quali.var.sup$coord.partiel[j,])	
							contrib=1:nbAxe
							cos2=1:nbAxe					
							tab2<-c(ident,gr,nbAxe,coord,contrib,cos2,nbCont)
							.jcall(obj,,"addVal",as.character(nomMod),tab2)
							gr=gr+1
						}
					}
					
				}
			}
			
			
			
			
			
		}else if(mca || catego){
			
			# modalitÃ©s moyennes (pas de partielles en ACM)
			################################################
			
			
			#modalitÃ©s actives
			####################
			
				
			mod=c()
			
			for(j in 1:dim(res$call$X)[2]){
				
				if(contient(nominales,colnames(res$call$X)[j])){
					numNom=position(nomsAnnot,colnames(res$call$X)[j])-1
					
					if(contient(nominalesActives,colnames(res$call$X)[j])){
						decalage=trouveDec(numNom)
						mod=c()	
						
						for(i in 1:dim(res$call$X)[1]){
							if(!contient(mod,as.integer(res$call$X[i,j]))){
								mod=c(mod,res$call$X[i,j])
								numMod=position(rownames(res$var$coord),res$call$X[i,j])				
								
								coord=unlist(res$var$coord[numMod,])	
								contrib=unlist(res$var$contrib[numMod,])
								cos2=unlist(res$var$cos2[numMod,])
								statut=7*10^decalage+numNom
								tab<-c(statut,0,nbAxe,coord,contrib,cos2)					
								
								.jcall(obj,,"addVal",as.character(rownames(res$var$coord)[numMod]),as.matrix(tab))
							}
							
						}
					}
					
				}
				
			}
			
			#modalitÃ©s illustratives
			####################
			
			if(nbNomIllus>0){
				for(j in 1:dim(res$call$X)[2]){			
					mod=c()
					if(contient(nominales,colnames(res$call$X)[j])){
						numNom=position(nomsAnnot,colnames(res$call$X)[j])-1
						if(contient(nominalesIllustratives,colnames(res$call$X)[j])){
							
							decalage=trouveDec(numNom)
							
							for(i in 1:dim(res$call$X)[1]){
								if(!contient(mod,as.integer(res$call$X[i,j]))){
									mod=c(mod,res$call$X[i,j])
									numMod=position(rownames(res$quali.sup$coord),res$call$X[i,j])			
									
									coord=unlist(res$quali.sup$coord[numMod,])	
									#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹				
									contrib=c(1:nbAxe)
									cos2=unlist(res$quali.sup$cos2[numMod,])
									statut=8*10^decalage+numNom
									tab<-c(statut,0,nbAxe,coord,contrib,cos2)					
									
									.jcall(obj,,"addVal",as.character(rownames(res$quali.sup$coord)[numMod]),as.matrix(tab))
								}							
							}
													
						}
					}				
				}
			}
			
			
		}else if(ca){
			
			#lignes
			#########################################
			
			#donnÃ©es actives
			for( i in 1:dim(res$row$coord)[1]){
				coord=unlist(res$row$coord[i,])	
				cos2=unlist(res$row$cos2[i,])	
				contrib=unlist(res$row$contrib[i,])	
				statut=71
				tab<-c(statut,0,nbAxe,coord,contrib,cos2)											
				.jcall(obj,,"addVal",rownames(res$row$coord)[i],as.matrix(tab))
			}
			
			#donnÃ©es illustratives
			if(!is(res$row.sup,"NULL")){
				for( i in 1:dim(res$row.sup$coord)[1]){
					coord=unlist(res$row.sup$coord[i,])	
					cos2=unlist(res$row.sup$cos2[i,])	
					#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹				
					contrib=c(1:nbAxe)
					statut=81
					tab<-c(statut,0,nbAxe,coord,contrib,cos2)											
					.jcall(obj,,"addVal",rownames(res$row.sup$coord)[i],as.matrix(tab))
				}		
			}
			
			#colonnes
			#########################################		
			
			#donnÃ©es actives		
			for( i in 1:dim(res$col$coord)[1]){
				coord=unlist(res$col$coord[i,])	
				cos2=unlist(res$col$cos2[i,])	
				contrib=unlist(res$col$contrib[i,])	
				statut=72
				tab<-c(statut,0,nbAxe,coord,contrib,cos2)											
				.jcall(obj,,"addVal",rownames(res$col$coord)[i],as.matrix(tab))
			}
			
			#donnÃ©es illustratives
			if(!is(res$col.sup,"NULL")){
				for( i in 1:dim(res$col.sup$coord)[1]){
					coord=unlist(res$col.sup$coord[i,])	
					cos2=unlist(res$col.sup$cos2[i,])	
					#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹				
					contrib=c(1:nbAxe)
					statut=82
					tab<-c(statut,0,nbAxe,coord,contrib,cos2)											
					.jcall(obj,,"addVal",rownames(res$col.sup$coord)[i],as.matrix(tab))
				}		
			}		
			
		}else if(dataframe){
			
			for(j in listeNom){
				numNom=position(nomsAnnot,colnames(res)[j])-1
				mod=c()
				decalage=trouveDec(numNom)
				statut=7*10^decalage+numNom
				for(i in 1:dim(res)[1]){
					if(!contient(mod,as.integer(res[i,j]))){
						mod=c(mod,res[i,j])
						tab<-c(statut,0,nbAxe)
						.jcall(obj,,"addVal",as.character(res[i,j]),as.matrix(tab))
					}				
				}			
			}		
		}
	}
	
	
	
	
	
	if(pca || mca || catego){
		
		
		# individus moyens (pas de partiels en ACP et ACM)
		###################################################
		nbIndiv=dim(res$ind$coord)[1]
		listeIndIllus=res$call$ind.sup
		listeIndAct=difference(listeIndIllus,1:nbIndiv)
		i=0
		
		#individus actifs
		for(numInd in listeIndAct)
		{
			i=i+1
			#valeurs
			coord=unlist(res$ind$coord[i,])					
			contrib=unlist(res$ind$contrib[i,])
			cos2=unlist(res$ind$cos2[i,])
			
			num=listeIndAct[i]
			
			valContinues=unlist(res$call$X[num,listeNumCont])
			
			
			tab<-c(1,0,nbAxe,coord,contrib,cos2,nbCont,valContinues)
			
			#annotations
			annot=matrix(NA,1,2+nbNom)
			annot[1,1]=rownames(res$ind$coord)[i]
			annot[1,2+nbNom]=NA
			if(pca && nbNom > 0){					
				for(j in 1:dim(res$call$quali.sup$quali.sup)[2]){
					annot[1,j+1]=as.character(res$call$quali.sup$quali.sup[i,j])
				}
			}else{
				n=2
				for(j in res$call$quali){
					annot[1,n]=as.character(res$call$X[numInd ,j])
					n=n+1
				}
				for(j in res$call$quali.sup){
					annot[1,n]=as.character(res$call$X[numInd ,j])
					n=n+1							
				}
			}
			
			#appel parser
			.jcall(obj,,"addVal",annot[1,1],tab)
			.jcall(obj,,"addHabVal",annot)
			
			
		}#fin for i
		
		
		#individus illustratifs, s'il y en a
		if(!is(res$ind.sup,"NULL")){
			i=0
			for(numInd in listeIndIllus)
			{
				i=i+1
				coord=unlist(res$ind.sup$coord[i,])
				#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹
				contrib=unlist(res$ind$contrib[1,])				
				cos2=unlist(res$ind.sup$cos2[i,])
				
				num=listeIndIllus[i]
				
				valContinues=unlist(res$call$X[num,listeNumCont])
				
				
				tab<-as.matrix(c(2,0,nbAxe,coord,contrib,cos2,nbCont,valContinues))
				
				#annotations
				annot=matrix(NA,1,2+nbNom)
				annot[1,1]=rownames(res$ind.sup$coord)[i]
				annot[1,2+nbNom]=NA
				if(pca && nbNom > 0){					
					for(j in 1:dim(res$call$quali.sup$quali.sup)[2]){
						annot[1,j+1]=as.character(res$call$quali.sup$quali.sup[i,j])
					}
				}else{
					n=2
					for(j in res$call$quali){
						annot[1,n]=as.character(res$call$X[numInd ,j])
						n=n+1
					}
					for(j in res$call$quali.sup){
						annot[1,n]=as.character(res$call$X[numInd ,j])
						n=n+1
					}
				}
				
				#appel parser
				.jcall(obj,,"addVal",annot[1,1],tab)
				.jcall(obj,,"addHabVal",annot)
				
			}#fin for i
		}
		
	}else if(mfa || afdm){
		
		# individus
		#############
		nbIndiv=dim(res$ind$coord)[1]
		listeIndIllus=res$call$ind.sup
		listeIndAct=difference(listeIndIllus,1:nbIndiv)		
		i=0
		
		#individus actifs
		for(numInd in listeIndAct)
		{
			i=i+1
			#valeurs
			coord=unlist(res$ind$coord[i,])					
			contrib=unlist(res$ind$contrib[i,])
			cos2=unlist(res$ind$cos2[i,])
			
			num=listeIndAct[i]
			
			valContinues=unlist(res$call$X[num,listeNumCont])
			
			
			tab<-c(1,0,nbAxe,coord,contrib,cos2,nbCont,valContinues)
			
			#annotations
			annot=matrix(NA,1,2+nbNom)
			annot[1,1]=rownames(res$call$X)[numInd]
			
			j=2
			for(nom in listeNom){
				annot[1,j]=as.character(res$call$X[numInd,nom])
				j=j+1
			}
			
			#appel parser
			.jcall(obj,,"addVal",annot[1,1],tab)
			.jcall(obj,,"addHabVal",annot)
			
			#partiels
			if(mfa){
				gr=1
				for(j in ((i-1)*nbGroupesAct+1):(i*nbGroupesAct)){
					coord=unlist(res$ind$coord.partiel[j,])	
					contrib=1:nbAxe
					cos2=1:nbAxe					
					tab2<-c(1,gr,nbAxe,coord,contrib,cos2,nbCont,valContinues)
					.jcall(obj,,"addVal",annot[1,1],tab2)
					.jcall(obj,,"addHabVal",annot)
					gr=gr+1
				}
				
			}
		}#fin for i		
		
		
		
		i=0
		if(!is(res$ind.sup,"NULL")){
			#individus illustratifs
			for(numInd in listeIndIllus)
			{
				i=i+1
				coord=unlist(res$ind.sup$coord[i,])
				#pas de contrib normalement, mais on fait du bourrage au cas oÃ¹
				contrib=unlist(res$ind$contrib[1,])				
				cos2=unlist(res$ind.sup$cos2[i,])
				
				num=listeIndIllus[i]
				
				valContinues=unlist(res$call$X[num,listeNumCont])
				
				
				tab<-as.matrix(c(2,0,nbAxe,coord,contrib,cos2,nbCont,valContinues))
				
				#annotations
				annot=matrix(NA,1,2+nbNom)
				annot[1,1]=rownames(res$call$X)[numInd]
				j=2
				for(nom in listeNom){
					annot[1,j]=as.character(res$call$X[numInd,nom])
					j=j+1
				}
				
				#appel parser
				.jcall(obj,,"addVal",annot[1,1],tab)
				.jcall(obj,,"addHabVal",annot)
				
				
				#partiels
				if(mfa){
					gr=1
					for(j in ((i-1)*nbGroupesAct+1):(i*nbGroupesAct)){
						coord=unlist(res$ind.sup$coord.partiel[j,])	
						contrib=1:nbAxe
						cos2=1:nbAxe					
						tab2<-c(1,gr,nbAxe,coord,contrib,cos2,nbCont,valContinues)
						.jcall(obj,,"addVal",annot[1,1],tab2)
						.jcall(obj,,"addHabVal",annot)
						gr=gr+1
					}
				}
				
			}#fin for i		
			
		}
		
	}else if(dataframe){
	
	
		for(i in 1:dim(res)[1]){
			valContinues=unlist(res[i,listeNumCont])
			tab<-c(1,0,nbAxe,c(),c(),c(),nbCont,valContinues)
			#annotations
			annot=matrix(NA,1,2+nbNom)
			annot[1,1]=rownames(res)[i]
			j=2
			for(nom in listeNom){
				annot[1,j]=as.character(res[i,nom])
				j=j+1
			}
			#appel parser
			.jcall(obj,,"addVal",annot[1,1],tab)
			.jcall(obj,,"addHabVal",annot)
		}
		
	}
	
	
	# variables continues moyennes (pas de partielles en ACP, AFM, ACM)
	####################################################################
	
	norme = res$call$scale.unit #les variables sont elles norm�es ?
	if(is(norme,'NULL') || (mfa || afdm)){norme=TRUE}#norme prend�vrai si on n'a pas trouv� l'info ou si l'on est en AFM ou AFDM
	
	if(pca){			
		listeContAct=res$var
		listeNumContAct=1:dim(listeContAct$coord)[1]
		#variables continues actives
		if(norme){
			statut=3
		} else {
			statut=5
		}
		
		for(i in listeNumContAct)
		{
			coord=unlist(listeContAct$coord[i,])		
			contrib=unlist(listeContAct$contrib[i,])
			cos2=unlist(listeContAct$cos2[i,])
			
			tab<-as.matrix(c(statut,0,nbAxe,coord,contrib,cos2,0))
			#appel parser
			.jcall(obj,,"addVal",rownames(listeContAct$coord)[i],tab)
		}
		
	}else if((mfa || afdm)){
		listeContAct=res$quanti.var
		if (!is(listeContAct,'NULL')) {
			listeNumContAct=1:dim(listeContAct$coord)[1]
			#variables continues actives
			if(norme){
				statut=3
			} else {
				statut=5
			}
			
			for(i in listeNumContAct)
			{
				coord=unlist(listeContAct$cor[i,])		
				contrib=unlist(listeContAct$contrib[i,])
				cos2=unlist(listeContAct$cos2[i,])
				if(!is(res$summary.quanti,"NULL")){
					groupe=trouveGroupe(rownames(listeContAct$coord)[i])
				}else{
					groupe=0
				}
				if(!contient(res$call$num.group.sup,groupe)){
					tab<-as.matrix(c(statut,groupe,nbAxe,coord,contrib,cos2,0))
					#appel parser
					.jcall(obj,,"addVal",rownames(listeContAct$coord)[i],tab)
				}
			}
		}
	}else if(dataframe){
		statut=5
		for(i in listeNumCont)
		{
			groupe=0
			tab<-as.matrix(c(statut,groupe,nbAxe,c(),c(),c(),0))
			#appel parser
			.jcall(obj,,"addVal",colnames(res)[i],tab)			
		}
	
	
	}
	
	if(nbContIllus > 0){
		if(pca||mca){
			#variables continues illustratives
			if(norme){statut=4}
			else{statut=6}
			
			listeContIllus=res$quanti.sup
			listeNumContIllus=1:dim(listeContIllus$coord)[1]
			
			for(i in listeNumContIllus)
			{
				coord=unlist(listeContIllus$coord[i,])		
				contrib=unlist(listeContIllus$cor[i,])
				cos2=unlist(listeContIllus$cos2[i,])
				
				tab<-as.matrix(c(statut,0,nbAxe,coord,contrib,cos2,0))
				
				#appel parser
				.jcall(obj,,"addVal",rownames(listeContIllus$coord)[i],tab)
			}
			
		}else if((mfa || afdm) && !is(res$summary.quanti,"NULL")){
			listeContIllus=res$quanti.var.sup
			listeNumContIllus=1:dim(listeContIllus$coord)[1]
			if(norme){statut=4}
			else{statut=6}
			
			for(i in listeNumContIllus)
			{
				coord=unlist(listeContIllus$coord[i,])		
				contrib=unlist(listeContIllus$cor[i,])
				cos2=unlist(listeContIllus$cos2[i,])
				if(!is(res$summary.quanti,"NULL")){			
					groupe=trouveGroupe(rownames(listeContIllus$coord)[i])
				}else{
					groupe=0
				}
				if(contient(res$call$num.group.sup,groupe)){
					tab<-as.matrix(c(statut,groupe,nbAxe,coord,contrib,cos2,0))
					
					#appel parser
					.jcall(obj,,"addVal",rownames(listeContIllus$coord)[i],tab)
				}
			}
			
		}
	}
	
	library(FactoMineR)
	
	#catdes
	###################################
	if(mca||pca||mfa||afdm||dataframe){
	  	if (!dataframe) tablo = res$call$X
	  	else tablo = res
	  	for (i in 1:ncol(tablo)){
		  	if(is.factor(tablo[,i])){
		    	XXdes<-catdes(tablo,i,proba=1)
		     	nmod=nlevels(tablo[,i])
		     	if(!is.null(XXdes$test.chi)) {
		          	Xdes=XXdes$test.chi
		     	for (k in 1:nrow(Xdes))  .jcall(obj,,
					"add_description_nom_par_nom", colnames(tablo)[i], rownames(Xdes)[k], Xdes[k,1])
		         }
		     	for (j in 1:nmod){
		        	if(!is.null(XXdes$test.chi)) {
		       			Xdes=XXdes$category[[j]]
		       			if(!is.null(Xdes)){
			       			for (k in 1:nrow(Xdes)){
			       			if(abs(Xdes[k,5])<1.96){ 
                     .jcall(obj,,"add_description_mod_par_mod", names(XXdes$category[j]),strsplit(rownames(Xdes)[k],"=")[[1]][2],0)
                                      }else{
                                             if(abs(Xdes[k,5])>4){
                     .jcall(obj,,"add_description_mod_par_mod", names(XXdes$category[j]),strsplit(rownames(Xdes)[k],"=")[[1]][2],5)
                                                                 }else{
                     .jcall(obj,,"add_description_mod_par_mod", names(XXdes$category[j]),strsplit(rownames(Xdes)[k],"=")[[1]][2],2)                                           
                                                                       }
                                            }
								                          }
                #On caract�rise par les valeurs tests
					      #strsplit(rownames(Xdes)[k],"=")[[1]][2], Xdes[k,5])
						                      }
		           	}
		           	if (!is.null(XXdes$quanti)){
		            	Xdes = XXdes$quanti[[j]]
		       			if(!is.null(Xdes)){
			         		for (k in 1:nrow(Xdes))  .jcall(obj,,
								"add_description_mod_par_cont", names(XXdes$quanti[j]), rownames(Xdes)[k], Xdes[k,1])
						}
		          }
		     	}
		   	}
		}
	}
	


	#dimdesc, il faut r�gler le cas de l'AC
	###################################
	if(pca || mca || mfa || ca || afdm){
		if(!ca){
			Axedesc<-res$ind$coord
		}else{
			Axedesc<-res$row$coord
		}
		
		for (i in 1:ncol(Axedesc)){
			tablo=cbind.data.frame(Axedesc[,i],res$call$X)
			Xdesc=condes(tablo,1,proba=1)		
			if(ca){
				Xdesc$category=Xdesc$quanti
			}
		 	if(!is.null(Xdesc$quanti) && !ca){
		    	for (j in 1:NROW(Xdesc$quanti)){
		           .jcall(obj,, "add_description_dim_par_cont",	i, rownames(Xdesc$quanti)[j], Xdesc$quanti[j,2]*sign(Xdesc$quanti[j,1]))
		       }
		    }
		   	if(!is.null(Xdesc$quali)){		
		    	for (j in 1:NROW(Xdesc$quali)){
		           	.jcall(obj,, "add_description_dim_par_nom",
						#i, rownames(Xdesc$quali)[j], Xdesc$quali[j,1])
						i, rownames(Xdesc$quali)[j], Xdesc$quali[j,2])
		       }
		
				for (j in 1:NROW(Xdesc$category)){
				  if(Xdesc$category[j,2]>0.05){
				        .jcall(obj,, "add_description_dim_par_mod",i, rownames(Xdesc$category)[j], 0)
				                              }else{
				  if(Xdesc$category[j,2]<0.00005){
				        .jcall(obj,, "add_description_dim_par_mod",i, rownames(Xdesc$category)[j], 5)
                                         }else{
                .jcall(obj,, "add_description_dim_par_mod",i, rownames(Xdesc$category)[j], 2)                                        
                                              }
                                            }				                              
          #On fait trois niveaux de probas critiques
          #else{
		      #     	.jcall(obj,, "add_description_dim_par_mod",i, rownames(Xdesc$category)[j], (1/(10*(Xdesc$category[j,2]))))
     	    #       .jcall(obj,, "add_description_dim_par_mod",i, rownames(Xdesc$category)[j], sign(Xdesc$category[j,1])*(1/(10*(Xdesc$category[j,2]))))
		      #     	}
				  #i, rownames(Xdesc$category)[j], sign(Xdesc$category[j,1])*(1/(10*(Xdesc$category[j,2]))))
			    #i, rownames(Xdesc$category)[j], sign(Xdesc$category[j,1])*0.005)
				  #i, rownames(Xdesc$category)[j], 1/(10*(Xdesc$category[j,2])))
		       }                                  
		
		    }
		}
	}
	
	
	
	#desco
	#####################	
	if(mca||pca||mfa||afdm||dataframe){	
		if(dataframe){
			tablo=res
		}else{
			tablo=res$call$X
		}
		for (k in 1:ncol(tablo)){
	    	if( is.numeric(tablo[,k])){
	       		Xdesc<-condes(tablo,k,proba=1)
	            if(!is.null(Xdesc$quanti)){
	            	for (j in 1:NROW(Xdesc$quanti)){
	           			.jcall(obj,, "add_description_cont_par_cont",
							colnames(tablo)[k], rownames(Xdesc$quanti)[j], Xdesc$quanti[j,2]*sign(Xdesc$quanti[j,1]))
	       			}
	     		}
	   			if(!is.null(Xdesc$quali)){	
	     			for (j in 1:NROW(Xdesc$quali)){
	           			.jcall(obj,, "add_description_cont_par_nom",
							colnames(tablo)[k], rownames(Xdesc$quali)[j], Xdesc$quali[j,2])
	       			}
	
					for (j in 1:NROW(Xdesc$category)){
									  if(Xdesc$category[j,2]>0.05){
				        .jcall(obj,, "add_description_cont_par_mod",colnames(tablo)[k], rownames(Xdesc$category)[j], 0)
				                              }else{
				  if(Xdesc$category[j,2]<0.00005){
				        .jcall(obj,, "add_description_cont_par_mod",colnames(tablo)[k], rownames(Xdesc$category)[j], 6)
                                         }else{
                .jcall(obj,, "add_description_cont_par_mod",colnames(tablo)[k], rownames(Xdesc$category)[j], 3)                                        
                                              }
                                            }	
      			   #.jcall(obj,, "add_description_cont_par_mod",colnames(tablo)[k], rownames(Xdesc$category)[j], abs(Xdesc$category[j,1]))
      			   #Cette partie sert aux axes dans les dataframes
      			   #Les changements de couleurs ne marchent pas
	       			                              }
	
	     		}
			}
		}
	}
	
	#appel parser	
	#.jcall(obj,,"fenetre")
	.jcall(obj,,"generate")	
}
