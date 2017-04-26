package classify

import akka.actor._

import scala.collection.mutable


case object CreateNaiveBayesModelActor
case class NewCategory(category: String, categoryActor: ActorRef)
case class GetCategoryActor(category: String)
case class GetAllCategoriesActors(categoriesActors: mutable.Map[String, ActorRef])


class CategoriesRepositoryActor(documentPreprocessor: DocumentPreprocessor, categoryModelFactory: () => CategoryModel) extends Actor {

  val categoryActors: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()
  val naiveBayesModelActors: mutable.Set[ActorRef] = mutable.Set[ActorRef]()

  override def receive = {
    case CreateNaiveBayesModelActor =>
      val newActor = context.actorOf(Props(new NaiveBayesModelActor(documentPreprocessor, self)))
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
