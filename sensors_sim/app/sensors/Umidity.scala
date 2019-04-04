package sensors

import akka.actor.{Props}
import interfaces.SensorsFunctions
import utilities.Configuration._


object  Umidity {
  def props = Props[Umidity]
}


class Umidity extends SensorsFunctions {


  var umidityNumber: Int =0
  var umidity: String = "sensor still off"

  override def read: String =umidity

  override def genSensorValue= {

    for (a <- 1 to numberSensorsUmi) {
      //generation sensors's value
      umidityNumber = 10 + new scala.util.Random().nextInt(90)
      umidity = umidityNumber.toString+" ID:"+a
      println("Generate umid: " + umidity)

      val resCode = publish("umidity", umidity)
      //in caso di publish Refused si riprova la connessione
      if (resCode >= 400) {
        connect
      }

    }


  }



}
