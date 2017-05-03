package classify

import akka.actor._

import scala.collection.mutable


case object CreateNaiveBayesModelActor
case class NewCategory(category: String, categoryActor: ActorRef)
case class GetCategoryActor(category: String)
case class GetAllCategoriesActors(categoriesActors: mutable.Map[String, ActorRef])
case object ClearTrainedModel
abstract class SmoothingModelParameters
case class SetLaplaceSmoothingModel(ngramOrder: Int, pseudoCount: Double) extends SmoothingModelParameters
case class SetGoodTuringSmoothingModel(ngramOrder: Int, frequencyThreshold: Int) extends SmoothingModelParameters


class CategoriesRepositoryActor(categoryModelFactory: () => CategoryModel) extends Actor {

  val categoryActors: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()
  val naiveBayesModelActors: mutable.Set[ActorRef] = mutable.Set[ActorRef]()

  def clearTrainedModel(): Unit = {
    categoryActors.clear()
    naiveBayesModelActors.foreach(_ ! ClearTrainedModel)
  }

  override def receive = {
    case CreateNaiveBayesModelActor =>
      val newActor = context.actorOf(Props(new NaiveBayesModelActor(self)))
      naiveBayesModelActors.add(newActor)
      sender ! newActor

    case GetCategoryActor(category) =>
      if (!categoryActors.contains(category)) {
        val newCategory = context.actorOf(Props(new CategoryModelActor(categoryModelFactory())))
        categoryActors(category) = newCategory
        naiveBayesModelActors.foreach(_ ! NewCategory(category, newCategory))
      }
      sender ! categoryActors(category)
  }
}
