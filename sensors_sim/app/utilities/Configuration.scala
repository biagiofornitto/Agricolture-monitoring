package utilities

object Configuration {
   val urlBrokerConnect="http://localhost:9003/api/connect"
   val urlBrokerPublish="http://localhost:9003/api/publish"
   val urlBrokerSubscribe="http://localhost:9003/api/subscribe"
   val urlBrokerUnsbscribe="http://localhost:9003/api/unsubscribe"
   val urlBrokerDisconnect="http://localhost:9003/api/disconnect"
   val clientId="client1"
   val clientUrl="http://localhost:9001"

   //specificare ogni quanti sencondi
   val rate_invio=4000

   var connectionsSensors=0
   var isConnect=false
   def incConnections()={connectionsSensors=connectionsSensors+1}
   def decConnections()={connectionsSensors=connectionsSensors-1}

   /*sensori di default per tipologia*/
   var numberSensorsTemp=1
   var numberSensorsBright=1
   var numberSensorsUmi=1

}
