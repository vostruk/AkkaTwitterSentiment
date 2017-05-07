package classify

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global


case object PingMessage

class Printer(testingActor: ActorRef) extends Actor {
  implicit val duration: Timeout = 100 seconds;

  override def receive = {
    case PingMessage =>
      val accuracyFuture = testingActor ? GetAccuracy
      println(Await.result(accuracyFuture, 1000 seconds))
      Thread.sleep(1000)
      self ! PingMessage
  }
}


class Learner(routerActor: ActorRef) extends Actor {
  val source = scala.io.Source.fromFile("newsgroups_dataset.txt")
  val inputText = try source.mkString.toLowerCase finally source.close()
  val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
  implicit val duration: Timeout = 100 seconds;

  override def receive = {
    case PingMessage =>
      for (line <- lines) {
        routerActor ! DocumentCategoryMessage(line(1), line(0))
      }
      println("Learning Done!")
  }
}


object Main extends App {
  val system = ActorSystem("SAGSystem")
  val categoriesRepository = system.actorOf(Props(new CategoriesRepositoryActor()))
  val routerActor = system.actorOf(Props(new NaiveBayesModelRouterActor(categoriesRepository)))
  routerActor ! SetWorkersNumber(10)
  val testingActor = system.actorOf(Props(new TestingActor("newsgroups_dataset.txt", routerActor)))
  val printer = system.actorOf(Props(new Printer(testingActor)))
  val learner = system.actorOf(Props(new Learner(routerActor)))

  printer ! PingMessage
  testingActor ! StartEvaluatingModel
  learner ! PingMessage
  Thread.sleep(15000)
  println("Changing parameters")
  categoriesRepository ! GoodTuringSmoothingModel(2, 10)
  Thread.sleep(5000)
  learner ! PingMessage
  Thread.sleep(15000)
  println("Stop")
  testingActor ! StopEvaluatingModel
}
