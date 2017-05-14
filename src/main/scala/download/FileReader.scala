package download

import akka.actor.{Actor, ActorRef}
import classify.DocumentCategoryMessage
import sentimentgui.sentimentgui.guiAlert

import scala.io.Source


case class StartLearningFromFile(fileName: String)
case object ReadNextLine
case object StopLearningFromFile
case object GetLearningProgress

class FileReader(naiveBayesActor: ActorRef, GuiActorInstance : ActorRef) extends Actor {
  var linesIteratorOption: Option[Iterator[String]] = None
  var currentLine: Int = 0
  var currentFileNumberOfLines: Int = 0

  def receive = {
    case StartLearningFromFile(fileName) =>
      currentLine = 0
      currentFileNumberOfLines = Source.fromFile(fileName).getLines.size
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
            currentLine += 1
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

    case GetLearningProgress =>
      sender ! (currentLine.toFloat / currentFileNumberOfLines)
  }
}
