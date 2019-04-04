#Subscriber R
ID=as.character(sample(100:200,1))

library(jsonlite)
library(httr)
library(caret)


#########################################################################################ù
 df=read.table("dataset.csv", sep=",")
	intrain <- createDataPartition(y = df$V4, p= 0.7, list = FALSE)
	training <- df[intrain,]
	testing <- df[-intrain,]
	if(!(anyNA(df))) {
		trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
		dtree_fit <<- train(V4 ~., data = training, method = "rpart",
					parms = list(split = "information"),
					trControl=trctrl,
					tuneLength = 10)
	}
	test_pred <- predict(dtree_fit, newdata = testing)
	print(confusionMatrix(test_pred, testing$V4 ))
	#print(test_pred)
#########################################################################################

#* Echo back the input
#* @post /publish
Publish<- function(req){

	
	body=fromJSON(req$postBody,simplifyDataFrame=TRUE)
	print(paste(body$clientId,body$topic,body$data,sep=" "))
	h=strsplit(as.character(body$data),split=" ")
	data=h[[1]][1]
	id=strsplit(h[[1]][2],split=":")
	df=data.frame(body$clientId,body$topic,data,id[[1]][2])
	names(df)<- c("ClientId","topic","data","id")
	#print(body$clientId,body$topic,body$data)
	write.table(df, "dati.csv", row.names=FALSE, col.names=FALSE, sep=",",append="TRUE")
	
	return ("ok")
}

#* Ritorna gli ultimi valori
#* @json
#* @get /lastdata
LastData<- function(){
	
	df=read.table("dati.csv", sep=",")
	
	x=df[nrow(df),]
	names(x)<- c("id_dato","ClientID","topic","dato")
	return (x)
}

#* Plot a histogram
#* @param topic 
#* @png
#* @get /plot
Grafico<- function(topic){
	color=c("red","blue","green","black","yellow","violet")
	i=0
	j=0
	n=NULL
	leg=NULL
	df=read.table("dati.csv", sep=",")
	id=NULL
	#colleziono gli id 
	for (x in df$V4) {
		j=j+1
		for (y in id){ 
			if (x==y && df$V2[j]==topic) i=1
		}
		if(i==0 && df$V2[j]==topic) id=c(id,x)
		i=0
	}
	#print(id)
	i=0
	j=0
	l=0
	#creo il grafico raccogliendo il dato per id e topic
	for (y in id){
		j=j+1
		for (x in df$V2){
			i=i+1
			if (x==topic && df$V4[i]==y) n=c(n,as.numeric(df$V3[i])) 
		}
		
		if (j==1) { 
			plot(n, type="b", col=color[j],axes=FALSE, ann=FALSE)
			
			
		}
		else {
			lines(n, type="b", col=color[j])
		}
		if(l<length(n)) l=length(n)
		i=0
		#text(c(1:length(n)),n,n,col = "black",cex = 0.6,pos=2)
		n=NULL
		
	}
	if(j>0){
	box()
	for (x in id) leg=c(leg,paste("id",x,sep=":"))
	print(leg)
	if(topic=="temperature"){ 
		title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="Gradi Celsius")
		axis(1, at=seq(1, l, 1))
		axis(2, at=seq(18, 35, 1))
	}
	if(topic=="brightness"){ 
		title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="LX")
		axis(1, at=seq(1, l, 1))
		axis(2, at=seq(90, 420, 25))
	}
	if(topic=="umidity"){ 
		title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="%")
		axis(1, at=seq(1, l, 1))
		axis(2, at=seq(8, 120, 10))
	}
	legend("bottomright",leg,col=color[1:length(id)],bg = "white",title="Sensori",lty=1,pch=1)
	}
	else {
		plot(c(2,4), type="l", col="white")
		text(1.5,3,labels="Non ci sono ancora dati per questo topic",col = "red",cex = 0.9)
	}
}

#* @param id
#* @param topic
#* @png
#* @get /plotid
PlotID<- function(id,topic){

	i=0
	n=NULL
	df=read.table("dati.csv", sep=",")
	for (x in df$V4) {
		i=i+1
		if (x==id && df$V2[i]==topic)  n=c(n,as.numeric(df$V3[i]))
	}
	
	if(length(n)>=1){
		plot(n, type="b", col="red",axes=FALSE, ann=FALSE)
		box()
		if(topic=="temperature"){ 
			title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="Gradi Celsius")
			axis(1, at=seq(1, length(n), 1))
			axis(2, at=seq(18, 35, 1))
		}
		if(topic=="brightness"){ 
			title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="LX")
			axis(1, at=seq(1, length(n), 1))
			axis(2, at=seq(90, 420, 25))
		}
		if(topic=="umidity"){ 
			title(main=topic, col.main="blue", font.main="4", xlab="N° Dati", ylab="%")
			axis(1, at=seq(1, length(n), 1))
			axis(2, at=seq(8, 120, 10))
		}
		#legend("bottomright",leg,col=color[1:length(id)],bg = "white",title="Sensori",lty=1,pch=1)
		text(c(1:length(n)),n,n,col = "black",cex = 0.6,pos=2)
		}
	else {
		plot(c(2,4), type="l", col="white")
		text(1.5,3,labels="Non ci sono ancora dati per questo topic/id",col = "red",cex = 0.9)
	}
}

#* @post /connect
Connect<- function(){
	
	resp <- POST("http://localhost:9003/api/connect",
                body=list("clientId"=ID,"clientUrl"="http://localhost:8000/publish"),encode="json")
				
	return(status_code(resp))
}

#* @post /subscribe
Subscribe<- function(req){

	resp <- POST("http://localhost:9003/api/subscribe",
                body=list("clientId"=ID,"topic"=fromJSON(req$postBody)),
				encode="json")
				
	return(status_code(resp))

}

#* Echo back the input
#* @param ClientID The ID of node
#* @post /disconnect
Disconnect<-function(){

	resp <- POST("http://localhost:9003/api/disconnect",
                body=list("clientId"=ID),
				encode="json")
				
	return(status_code(resp))
	
}

#* @post /unsubscribe
Unsubscribe<-function(req){

	resp <- POST("http://localhost:9003/api/unsubscribe",
                body=list("clientId"=ID,"topic"=fromJSON(req$postBody)),
				encode="json")
				
	return(status_code(resp))
	
}

#creo il dataset dal file csv
Dataset<- function(){
	
	temp=NULL
	luce=NULL
	umi=NULL
	et=NULL
	
	for (i in 1:100){
		
		temp=c(temp,sample(20:30,1))
		luce=c(luce,sample(100:400,1))
		umi=c(umi,sample(10:100,1))
		if (temp[i]>=23 && temp[i]<=26) et=c(et,"high")
		if (temp[i]<23) et=c(et,"med")
		if (temp[i]>26) et=c(et,"low")
		
	}
	df=data.frame(temp,luce,umi,et)
	write.table(df, "dataset.csv", row.names=FALSE, col.names=FALSE, sep=",",append="TRUE")
	
}

#vedo la classificazione dell'ultimo dato
#* @get /salute
Salute<- function(){

	df=read.table("dati.csv", sep=",")
	n=c(0,0,0)
	i=0
	#colleziono i topic
	for (x in df$V2) {
		i=i+1
		if(x=="temperature")n[1]=as.numeric(df$V3[i])
		if(x=="brightness") n[2]=as.numeric(df$V3[i])
		if(x=="umidity") n[3]=as.numeric(df$V3[i])
		
		
	}
	
	for(i in 1:3){
		
		if (i==1 && n[i]==0) n[i]=25
		if (i==2 && n[i]==0) n[i]=250
		if (i==3 && n[i]==0) n[i]=50
		
	}
	t=data.frame(n[1],n[2],n[3])
	names(t)<- c("V1","V2","V3")
	print(test_pred <- predict(dtree_fit, newdata = t))
	return(test_pred)
}

#training del classificatore
Training<- function(){

	df=read.table("dataset.csv", sep=",")
	intrain <- createDataPartition(y = df$V4, p= 0.7, list = FALSE)
	training <- car_df[intrain,]
	testing <- car_df[-intrain,]
	if(!(anyNA(car_df))) {
		trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
		dtree_fit <<- train(V4 ~., data = training, method = "rpart",
					parms = list(split = "information"),
					trControl=trctrl,
					tuneLength = 10)
	}
	test_pred <- predict(dtree_fit, newdata = testing)
	print(test_pred)
}
