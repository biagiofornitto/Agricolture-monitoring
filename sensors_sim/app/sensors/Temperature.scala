package sensors

import akka.actor.{ Props}
import interfaces.SensorsFunctions
import utilities.Configuration._


object  Temperature {
  def props = Props[Temperature]
}



class Temperature extends SensorsFunctions  {


  var temperatureNumber: Int = 0
  var temperature: String = "sensor still off"

  override def read: String = temperature
  override def genSensorValue= {

    for (a <- 1 to numberSensorsTemp) {
      //generation sensors's value
      temperatureNumber = 20 + new scala.util.Random().nextInt(10)
      temperature = temperatureNumber.toString+" ID:"+a
      println("Generate temp: " + temperature)

      val resCode = publish("temperature", temperature)
      //in caso di publish Refused si riprova la connessione
      if (resCode >= 400) {
        connect
      }
    }
  }

}