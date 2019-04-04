package sensors

import akka.actor.{Props}
import interfaces.SensorsFunctions
import utilities.Configuration._

object  Brightness {
  def props = Props[Brightness]


}


class Brightness extends SensorsFunctions {

  var brightnessNumber: Int=0
  var brightness: String="sensor still off"



  override def read: String =brightness

  override def genSensorValue={
    //generate sensor's value
    for( a <- 1 to numberSensorsBright) {
      brightnessNumber = 100 + new scala.util.Random().nextInt(300)
      brightness = brightnessNumber.toString+" ID:"+a
      println("Generate bright: " + brightness)

      //avviare il metodo che pubblica

      val resCode = publish("brightness", brightness)
      //in caso di publish Refused si riprova la connessione
      if (resCode >= 400) {
        connect
      }
    }

  }














}
