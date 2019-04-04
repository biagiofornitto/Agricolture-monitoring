package interfaces


import akka.actor.{Actor}
import utilities.PayloadTypes._
import play.api.libs.json.Json
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import utilities.Configuration._

trait SensorsFunctions extends Actor{

  def read():String

  var interrupt:Boolean=false




  def on(): Unit = {
        //println("sono nell'on")
       if (interrupt.equals(false)) {
          interrupt = true
          incConnections()
         println("Numero sensori connessi="+connectionsSensors)
         if (connectionsSensors==1) {
           val a = connect
           //println(a)
           if (a< 400) isConnect=true
         }
         if (isConnect==true)
                self ! "publish"


        }

  }


  def off: Unit ={
    if (interrupt.equals(true)) {
      interrupt = false
      decConnections()
      if (connectionsSensors==0) {
        disconnect
        isConnect=false
      }
    }
  }

  //def send(uriApi:String,data:String): Int
  def send(uriApi:String,data:String): Int ={
    val responseFuture: Future[HttpResponse] = Http(context.system)
      .singleRequest(HttpRequest(POST,uri= uriApi,entity = HttpEntity(ContentTypes.`application/json`, data)))
    //code status of request timeout
    var resCode=408
    responseFuture onComplete {
      case Success(res) => {
        println("Response code: "+res.status+ " "+res.entity)
        resCode=res.status.intValue()

      }
      case Failure(t) => {
        println("An error has occured: " + t.getMessage)

      }
    }
    Thread.sleep(rate_invio)
    return resCode
  }


  def connect=send(urlBrokerConnect,Json.toJson(ConnectPayload(clientId,clientUrl)).toString() )
  def disconnect=send(urlBrokerDisconnect,Json.toJson(DisconnectPayload(clientId)).toString() )
  def publish(topic:String,data:String)=send(urlBrokerPublish,Json.toJson(PublishPayload(clientId,topic,data)).toString() )


  def genSensorValue()



  override def receive() = {


    case "on" => {
      on()

    }

    case "publish"=>{

      if (interrupt) {
        genSensorValue
        self ! "publish"
      }
    }
    case "off" =>off
    case "read" =>sender ! read

  }







}
