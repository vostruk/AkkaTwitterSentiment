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
      val accuracyFuture = testingActor ? EvaluateModel
      println(Await.result(accuracyFuture, 1000 seconds))
      Thread.sleep(1000)
      self ! PingMessage
  }
}


class Learner(categoriesRepositoryActor: ActorRef) extends Actor {
  val source = scala.io.Source.fromFile("newsgroups_dataset.txt")
  val inputText = try source.mkString.toLowerCase finally source.close()
  val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
  implicit val duration: Timeout = 100 seconds;

  override def receive = {
    case PingMessage =>
      val nbActorFuture = (categoriesRepositoryActor ? CreateNaiveBayesModelActor).mapTo[ActorRef]
      nbActorFuture.map {
        nbActor =>
          for (line <- lines) {
            nbActor ! DocumentCategoryMessage(line(1), line(0))
          }
          println("Learning Done!")
      }
  }
}


object Main extends App {
  val system = ActorSystem("SAGSystem")
  val cr = system.actorOf(Props(new CategoriesRepositoryActor()))
  val testingActor = system.actorOf(Props(new TestingActor("newsgroups_dataset.txt", cr)))
  val printer = system.actorOf(Props(new Printer(testingActor)))
  val learner = system.actorOf(Props(new Learner(cr)))

  printer ! PingMessage
  learner ! PingMessage
  Thread.sleep(15000)
  println("Changing parameters")
  cr ! GoodTuringSmoothingModel(2, 10)
  Thread.sleep(5000)
  learner ! PingMessage
}
