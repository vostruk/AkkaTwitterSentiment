package download

import akka.actor.{Actor, ActorRef}
import classify.DocumentCategoryMessage
import sentimentgui.sentimentgui.guiAlert

import scala.io.Source


case class StartLearningFromFile(fileName: String)
case object ReadNextLine
case object StopLearningFromFile

class FileReader(naiveBayesActor: ActorRef, GuiActorInstance : ActorRef) extends Actor {
  var linesIteratorOption: Option[Iterator[String]] = None

  def receive = {
    case StartLearningFromFile(fileName) =>
      linesIteratorOption = Some(Source.fromFile(fileName).getLines)
      println("reading from "+fileName)
      self ! ReadNextLine

    case ReadNextLine =>
      linesIteratorOption match {
        case None => self ! StopLearningFromFile
        case Some(linesIterator) =>
          if (linesIterator.hasNext) {
            val line = linesIterator.next()
            val categoryDocumentPair = line.split("\\t")
            naiveBayesActor ! DocumentCategoryMessage(categoryDocumentPair(1), categoryDocumentPair(0))
            self ! ReadNextLine
          }
          else {
            println("done")
            GuiActorInstance ! guiAlert("Reading file is done!")
            linesIteratorOption = None
          }
      }

    case StopLearningFromFile =>
      linesIteratorOption = None
      println("stopped reading")
  }
}