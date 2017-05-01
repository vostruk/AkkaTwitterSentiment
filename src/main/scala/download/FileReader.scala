package download

import akka.actor.{Actor, ActorRef}
import classify.DocumentCategoryMessage

import scala.io.Source


class FileReader(naiveBayesActor: ActorRef) extends Actor {

  def receive = {
    case (filenameToRead: String) => {
      Source
        .fromFile(filenameToRead) //ex "categories_init_learn.txt"
        .getLines
        .foreach { line =>
          val (cat, tw) = line.split("\\t")
          naiveBayesActor ! DocumentCategoryMessage(tw, cat)
        }

    }
  }
}