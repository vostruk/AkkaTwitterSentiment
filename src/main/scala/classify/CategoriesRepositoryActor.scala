package classify

import akka.actor._

import scala.collection.mutable


case object CreateNaiveBayesModelActor
case object DestroyNaiveBayesModelActor
case class ModelActorCreated(actorRef: ActorRef)
case class ModelActorDestroyed(actorRef: ActorRef)
case class NewCategory(category: String, categoryActor: ActorRef)
case class GetCategoryActor(category: String)
case class GetAllCategoriesActors(categoriesActors: mutable.Map[String, ActorRef])
case object ClearTrainedModel
abstract class SmoothingModelParameters
case class LaplaceSmoothingModel(ngramOrder: Int, pseudoCount: Double) extends SmoothingModelParameters
case class GoodTuringSmoothingModel(ngramOrder: Int, frequencyThreshold: Int) extends SmoothingModelParameters


class CategoriesRepositoryActor extends Actor {

  val categoryActors: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()
  val naiveBayesModelActors: mutable.Set[ActorRef] = mutable.Set[ActorRef]()
  var categoryModelFactory: () => CategoryModel = () => new LaplaceSmoothingCategoryModel(0.5, new DocumentPreprocessor(2))
  var currentParameters: SmoothingModelParameters = LaplaceSmoothingModel(2, 0.5)

  def clearTrainedModel(): Unit = {
    categoryActors.clear()
    broadcastToNaiveBayesActors(ClearTrainedModel)
  }

  def broadcastToNaiveBayesActors(message: Any): Unit = {
    naiveBayesModelActors.foreach(_ ! message)
  }

  override def receive = {
    case CreateNaiveBayesModelActor =>
      val newActor = context.actorOf(Props(new NaiveBayesModelActor(self, categoryActors)))
      naiveBayesModelActors.add(newActor)
      sender ! ModelActorCreated(newActor)

    case DestroyNaiveBayesModelActor =>
      val destroyedActor = naiveBayesModelActors.last
      naiveBayesModelActors -= destroyedActor
      destroyedActor ! PoisonPill
      sender ! ModelActorDestroyed(destroyedActor)

    case GetCategoryActor(category) =>
      if (!categoryActors.contains(category)) {
        val newCategory = context.actorOf(Props(new CategoryModelActor(categoryModelFactory())))
        categoryActors(category) = newCategory
        broadcastToNaiveBayesActors(NewCategory(category, newCategory))
      }
      sender ! categoryActors(category)

    case ClearTrainedModel =>
      clearTrainedModel()

    case newParameters@LaplaceSmoothingModel(ngramOrder, pseudoCount) =>
      categoryModelFactory = () => new LaplaceSmoothingCategoryModel(pseudoCount, new DocumentPreprocessor(ngramOrder))
      broadcastToNaiveBayesActors(SetNGramOrder(ngramOrder))
      currentParameters match {
        case LaplaceSmoothingModel(currentNGramOrder, _) if currentNGramOrder == ngramOrder => Unit
        case default => clearTrainedModel()
      }
      currentParameters = newParameters

    case newParameters@GoodTuringSmoothingModel(ngramOrder, frequencyThreshold) =>
      categoryModelFactory = () => new GoodTuringSmoothingCategoryModel(frequencyThreshold, new DocumentPreprocessor(ngramOrder))
      broadcastToNaiveBayesActors(SetNGramOrder(ngramOrder))
      currentParameters match {
        case GoodTuringSmoothingModel(currentNGramOrder, _) if currentNGramOrder == ngramOrder => Unit
        case default => clearTrainedModel()
      }
      currentParameters = newParameters
  }
}
