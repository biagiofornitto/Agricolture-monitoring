package controllers

import akka.actor.ActorSystem
import javax.inject._
import play.api.libs.json._
import play.api.mvc._
import utilities.Structures._
import utilities.PayloadTypes._

import scala.collection.mutable.ArrayBuffer
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents,
                               system: ActorSystem
                               ) extends AbstractController(cc) {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  val pub_sub=system.actorOf(ActorPubSub.props, "pubsub-Actor")



  def index() = Action { implicit request: Request[AnyContent] =>

    //esempi di sottoscrizioni
    /*hashMapClients.put("clientsub","http://localhost:8000/Publish")
    hashMapTopicSub.put("temperature",ArrayBuffer("clientsub"))
    hashMapTopicSub.put("brightness",ArrayBuffer("clientsub"))
    hashMapTopicSub.put("umidity",ArrayBuffer("clientsub"))*/
    Ok(views.html.index())
  }


  def connect() = Action(parse.json) {implicit request=>

    val dati: JsValue = request.body
    val connectPacket:ConnectPayload=dati.as[ConnectPayload]
    //println(connectPacket.clientId+" "+connectPacket.clientUrl)

    //pub_sub ! connectPacket
    val clientId=connectPacket.clientId
    val clientUrl=connectPacket.clientUrl
    println("ConnectPayload ricevuto dal client: "+ clientId +" che ha url: "+clientUrl)

    if (!hashMapClients.contains(clientId)) {

      //insert the new client in the map
      hashMapClients.put(clientId, clientUrl)
      Ok("Got request [" + request + "] Connection Accepted")
    }
    else  BadRequest("Connection Refused")
  }

  def publish()=Action(parse.json) { implicit request=>
    val dati: JsValue = request.body
    val publishPacket:PublishPayload=dati.as[PublishPayload]

    if(hashMapClients.contains(publishPacket.clientId)) {
                pub_sub ! publishPacket
                Ok("Got request [" + request + "] Publish Accepted")
    }
    else  BadRequest("Publish Refused")
  }


  def subscribe()=Action(parse.json) { implicit request=>
    val dati: JsValue = request.body
    val subscribePacket:SubscribePayload=dati.as[SubscribePayload]

    if(hashMapClients.contains(subscribePacket.clientId)) {
      pub_sub ! subscribePacket
      Ok("Got request [" + request + "] Subscribe Accepted")
    }
    else  BadRequest("Subscribe Refused")
  }


  def unsubscribe()=Action(parse.json) { implicit request=>
    val dati: JsValue = request.body
    val unsubscribePacket:UnsubscribePayload=dati.as[UnsubscribePayload]

    if(hashMapClients.contains(unsubscribePacket.clientId)) {
      pub_sub ! unsubscribePacket
      Ok("Got request [" + request + "] Unsubscribe Accepted")
    }
    else  BadRequest("Unsubscribe Refused")
  }




  def disconnect()=Action(parse.json) { implicit request=>
    val dati: JsValue = request.body
    val disconnectPacket:DisconnectPayload=dati.as[DisconnectPayload]
    println("DisconnectPayload ricevuto dal client: "+ disconnectPacket.clientId)
    if(hashMapClients.contains(disconnectPacket.clientId)) {
      hashMapClients.remove(disconnectPacket.clientId)


      /*si deve fare l'unsubscribe di tutti i topic a cui si era sottoscritto il client*/
      hashMapTopicSub.foreach((e: (String, ArrayBuffer[String]))=>if (e._2.contains(disconnectPacket.clientId))
                                                                            e._2 -=disconnectPacket.clientId)


      Ok("Got request [" + request + "] Disconnect Accepted")
    }
    else  BadRequest("Disconnect Refused")
  }















}
