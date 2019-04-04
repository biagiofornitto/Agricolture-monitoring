package controllers


import akka.actor.ActorSystem
import akka.util.Timeout
import javax.inject._
import play.api._
import play.api.mvc._
import sensors._

import scala.concurrent.duration._
import akka.pattern.ask
import play.api.data._
import play.api.data.Forms._
import play.api.i18n._

import scala.concurrent.Await
import utilities.Configuration._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

@Singleton
class HomeController @Inject()(cc: ControllerComponents,
                               system: ActorSystem,
                                messagesApi: MessagesApi
                              ) extends AbstractController(cc)  with I18nSupport {



  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */

  //vengono definiti gli attori, dell'injection se ne occupa il controller
  val temp=system.actorOf(Temperature.props, "temp-Actor")
  val bright=system.actorOf(Brightness.props, "bright-Actor")
  val umid=system.actorOf(Umidity.props, "umid-Actor")


  val userForm = Form(mapping("numberSensors"->number(min=1, max=10))(UserData.apply)(UserData.unapply))


//funzione per settare numero di sensori per tipologia
  def setSensors = Action {
    implicit request =>
      userForm.bindFromRequest.fold(

        formWithErrors => {
          BadRequest("It is not possible to set this value")
        },
        userData => {
          println(request.uri)
          val newUser = UserData(userData.numberSensors)
          println(userData.numberSensors)
          matchsetSensors(request.uri,userData.numberSensors)
          //Redirect(routes.HomeController.index())
          Ok("Got request [" + request + "]"+" N°Sensori="+userData.numberSensors)

        })

  }








  def index() = Action {implicit request: Request[AnyContent] =>

    Ok(views.html.index(userForm))

  }

  def on()=Action{request =>
      matchOn(request.uri)
      Ok("Got request [" + request + "]")
  }

  def off()=Action{request =>
    matchOff(request.uri)
    Ok("Got request [" + request + "]")
  }

  def read()=Action{request =>
    var result=matchRead(request.uri)
    Ok("Got request [" + request + "]"+"\n"+ "Response: "+ result )
  }

  /*def setSensors()=Action{request =>
    //var result=matchsetSensors(request.uri)
    Ok("Got request [" + request + "]"+"\n"+ "Response: "+ request.body )
  }*/


  val PatternTemp= "/api/temp"
  val PatternBright="/api/bright"
  val PatternUmid="/api/umi"


  def matchOn(req:String) = req match {
    case r if r.startsWith(PatternTemp)=>temp ! "on"
    case r if r.startsWith(PatternBright)=>bright ! "on"
    case r if r.startsWith(PatternUmid)=>umid ! "on"

  }

  def matchOff(req: String) = req match {

    case r if r.startsWith(PatternTemp)=> temp ! "off"
    case r if r.startsWith(PatternBright)=>bright ! "off"
    case r if r.startsWith(PatternUmid)=>umid ! "off"

  }

  def matchsetSensors(req:String,nSensors:Int)=req match{
    case r if r.startsWith(PatternTemp)=>numberSensorsTemp=nSensors
    case r if r.startsWith(PatternBright)=>numberSensorsBright=nSensors
    case r if r.startsWith(PatternUmid)=>numberSensorsUmi=nSensors



  }


  //timeout per la request di read
  implicit val timeout = Timeout(20 seconds)


  def matchRead(req: String): String = req match {

    case r if r.startsWith(PatternTemp)=> {
                       val future = temp ? "read"
                       return Await.result(future, timeout.duration).asInstanceOf[String]

       }
    case r if r.startsWith(PatternBright)=>{
                      val future = bright ? "read"
                       return Await.result(future, timeout.duration).asInstanceOf[String]

    }
    case r if r.startsWith(PatternUmid)=>{
                      val future = umid ? "read"
                      return Await.result(future, timeout.duration).asInstanceOf[String]

    }
  }








}
