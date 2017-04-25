package classify

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.Await


case object PingMessage

class Printer(testingActor: ActorRef) extends Actor {
  implicit val duration: Timeout = 100 seconds;

  override def receive = {
    case PingMessage =>
      val accuracyFuture = testingActor ? EvaluateModel
      println(Await.result(accuracyFuture, 1000 seconds))
      Thread.sleep(20000)
      self ! PingMessage
  }
}

object Main extends App {
  val source = scala.io.Source.fromFile("newsgroups_dataset.txt")
  val inputText = try source.mkString.toLowerCase finally source.close()
  val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))

  val system = ActorSystem("SAGSystem")
  val dp = new DocumentPreprocessor(2)
  val cr = system.actorOf(Props(new CategoriesRepositoryActor(() => new LaplaceSmoothingCategoryModel(0.1, dp))))
  val a1 = system.actorOf(Props(new NaiveBayesModelActor(dp, cr)), name = "dpa1")
  val a2 = system.actorOf(Props(new NaiveBayesModelActor(dp, cr)), name = "dpa2")
  val testingActor = system.actorOf(Props(new TestingActor("newsgroups_dataset.txt", a2)))
  val printer = system.actorOf(Props(new Printer(testingActor)))

  printer ! PingMessage

  for (line <- lines) {
    a1 ! DocumentCategoryMessage(line(1), line(0))
    Thread.sleep(300)
  }
}
