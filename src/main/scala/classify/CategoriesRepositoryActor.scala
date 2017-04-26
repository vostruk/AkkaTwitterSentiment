package classify

import akka.actor._

import scala.collection.mutable


case class GetCategoryActor(category: String)
case class GetAllCategoriesActors(categoriesActors: mutable.Map[String, ActorRef])


class CategoriesRepositoryActor(categoryModelFactory: () => CategoryModel) extends Actor {

  val categoryActors: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()

  override def receive = {
    case GetCategoryActor(category) =>
      if (!categoryActors.contains(category)) {
        categoryActors(category) = context.actorOf(Props(new CategoryModelActor(categoryModelFactory())))
      }
      sender ! categoryActors(category)
  }
}
