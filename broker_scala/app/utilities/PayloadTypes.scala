package utilities

import play.api.libs.json.{JsPath, Json, Writes}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{__, Reads}


object PayloadTypes {

  case class ConnectPayload(clientId: String,clientUrl:String)

  implicit val connectReads: Reads[ConnectPayload] = (
    (JsPath \ "clientId").read[String] and
      (JsPath \ "clientUrl").read[String]
    )(ConnectPayload.apply _)

  implicit val connectWrites = new Writes[ConnectPayload] {
    def writes(connect: ConnectPayload) = Json.obj(
      "clientId" -> connect.clientId,
      "clientUrl" -> connect.clientUrl
    )
  }

  case class DisconnectPayload(clientId: String)


  implicit val reads: Reads[DisconnectPayload] = (__ \ "clientId")
      .read[String](maxLength[String](2000)).map {clientId => DisconnectPayload(clientId)}



  implicit val disconnectWrites = new Writes[DisconnectPayload] {
    def writes(disconnect: DisconnectPayload) = Json.obj(
      "clientId" -> disconnect.clientId
    )
  }


  case class PublishPayload(clientId: String,topic:String,data:String)

  implicit val publishReads: Reads[PublishPayload] = (
    (JsPath \ "clientId").read[String] and
      (JsPath \ "topic").read[String] and
      (JsPath \ "data").read[String]
    )(PublishPayload.apply _)

  implicit val publishWrites = new Writes[PublishPayload] {
    def writes(publish: PublishPayload) = Json.obj(
      "clientId" -> publish.clientId,
      "topic" -> publish.topic,
      "data" -> publish.data
    )
  }


  case class SubscribePayload(clientId: String,topic:String)

  implicit val subscribeReads: Reads[SubscribePayload] = (
    (JsPath \ "clientId").read[String] and
      (JsPath \ "topic").read[String]
    )(SubscribePayload.apply _)

  implicit val subscribeWrites = new Writes[SubscribePayload] {
    def writes(subscribe: SubscribePayload) = Json.obj(
      "clientId" -> subscribe.clientId,
      "topic" -> subscribe.topic
    )
  }

  case class UnsubscribePayload(clientId: String,topic:String)
  implicit val unsubscribeReads: Reads[UnsubscribePayload] = (
    (JsPath \ "clientId").read[String] and
      (JsPath \ "topic").read[String]
    )(UnsubscribePayload.apply _)

  implicit val unsubscribeWrites = new Writes[UnsubscribePayload] {
    def writes(unsubscribe: UnsubscribePayload) = Json.obj(
      "clientId" -> unsubscribe.clientId,
      "topic" -> unsubscribe.topic
    )
  }





}
