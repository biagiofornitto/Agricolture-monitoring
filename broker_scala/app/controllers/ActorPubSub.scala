package controllers


import akka.actor.{Actor, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, HttpResponse}
import play.api.libs.json.Json
import utilities.PayloadTypes._
import utilities.Structures._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global


object  ActorPubSub {
  //metodo che ritorna le proprietà dell'oggetto necessario per la creazione dell'actor
  def props = Props[ActorPubSub]
}



class ActorPubSub extends Actor{

  def send(uriApi:String,data:String) ={
    val responseFuture: Future[HttpResponse] = Http(context.system)
      .singleRequest(HttpRequest(POST,uri= uriApi,entity = HttpEntity(ContentTypes.`application/json`, data)))
    //code status of request timeout
    //var resCode=408
    responseFuture onComplete {
      case Success(res) => {
        println("Response code: "+res.status+ " "+res.entity)
       // resCode=res.status.intValue()

      }
      case Failure(t) => {
        println("An error has occured: " + t.getMessage)

      }
    }
    //return resCode
  }




  override def receive: Receive ={
   /* case ConnectPayload(clientId,clientUrl)=> {
                println("ConnectPayload ricevuto dal client: "+ clientId +" che ha url: "+clientUrl)

    }
    case DisconnectPayload(clientId) =>println("DisconnectPayload ricevuto dal client: "+ clientId )*/
    case PublishPayload(clientId,topic,data)=> {
      println("PublishPayload ricevuto dal client: " + clientId + " topic: " + topic + " data: " + data)
      if (hashMapTopicSub.contains(topic)) {
                                val clients=hashMapTopicSub.apply(topic)
                                for ( c <- clients) {
                                        println("Send publish to: "+ c )
                                        send(hashMapClients.apply(c),Json.toJson(PublishPayload(c,topic,data)).toString() )
                                }
      }

    }
    case SubscribePayload(clientId,topic)=> {
      println("SubscribePayload ricevuto dal client: " + clientId + " topic: " + topic)
      /*se il topic non esiste viene creata la relativa entry*/
      if (!hashMapTopicSub.contains(topic))
        hashMapTopicSub.put(topic, ArrayBuffer(clientId))

      else
        /*se il client non si era già sottoscritto al topic viene inserito*/
        if (!hashMapTopicSub.apply(topic).contains(clientId))
                        hashMapTopicSub.apply(topic) += clientId
      // hashMapTopicSub.apply(topic).append(clientId)


    }
    case UnsubscribePayload(clientId,topic)=>{

      /*si cancella il client dalla lista dei subscriber del relativo topic*/
    if (hashMapTopicSub.contains(topic)){
              hashMapTopicSub.apply(topic) -=clientId

        }

  }


  }


  }

