package classify

import akka.actor._

case object PingMessage
case object PongMessage
case object StartMessage
case object StopMessage
case class PreprocessDocumentMessage(document: String)


class Printer(testDocuments: Iterable[String], nbma: ActorRef) extends Actor {
  override def receive = {
    case CategoryMessage(category) => println(category)
    case _ => testDocuments.foreach(nbma ! ClassifyDocumentMessage(_))
  }
}

object Main extends App {
  val source = scala.io.Source.fromFile("newsgroups_dataset.txt")
  val inputText = try source.mkString.toLowerCase finally source.close()
  val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
  val (train, test) = lines.partition(_ => scala.util.Random.nextDouble < 0.7)
  val (test_labels, test_documents) = test.map((line) => (line(0), line(1))).unzip

  val system = ActorSystem("SAGSystem")
  val dp = new DocumentPreprocessor(2)
  val cr = system.actorOf(Props(new CategoriesRepositoryActor(() => new LaplaceSmoothingCategoryModel(0.5, dp))))
  val nbm1 = new NaiveBayesModel(dp, cr)
  val nbm2 = new NaiveBayesModel(dp, cr)
  val nbm3 = new NaiveBayesModel(dp, cr)
  val a1 = system.actorOf(Props(new NaiveBayesModelActor(nbm1)), name = "dpa1")
  val a2 = system.actorOf(Props(new NaiveBayesModelActor(nbm2)), name = "dpa2")
  val a3 = system.actorOf(Props(new NaiveBayesModelActor(nbm3)), name = "dpa3")
  val printer = system.actorOf(Props(new Printer(test_documents, a1)))

  for (line <- train) {
    a1 ! DocumentCategoryMessage(line(1), line(0))
    a2 ! DocumentCategoryMessage(line(1), line(0))
    a3 ! DocumentCategoryMessage(line(1), line(0))
  }

  printer ! 1
  //val errors = test.map((line) => line(0) != naiveBayesModel.classifyDocument(line(1))).count(_ == true)

}
