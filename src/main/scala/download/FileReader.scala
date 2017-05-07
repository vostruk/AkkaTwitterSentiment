package download

import akka.actor.{Actor, ActorRef}
import classify.DocumentCategoryMessage

import scala.io.Source


case class StartLearningFromFile(fileName: String)
case object ReadNextLine
case object StopLearningFromFile

class FileReader(naiveBayesActor: ActorRef) extends Actor {
  var linesIteratorOption: Option[Iterator[String]] = None

  def receive = {
    case StartLearningFromFile(fileName) =>
      val linesIteratorOption = Source.fromFile(fileName).getLines
      self ! ReadNextLine

    case ReadNextLine =>
      linesIteratorOption match {
        case None => Unit
        case Some(linesIterator) =>
          if (linesIterator.hasNext) {
            val line = linesIterator.next()
            val categoryDocumentPair = line.split("\\t")
            naiveBayesActor ! DocumentCategoryMessage(categoryDocumentPair(1), categoryDocumentPair(0))
            self ! ReadNextLine
          }
          else {
            linesIteratorOption = None
          }
      }

    case StopLearningFromFile =>
      linesIteratorOption = None
  }
}