import akka.actor._

import scala.collection.mutable


case class GetCategoryActor(category: String)


class CategoriesRepositoryActor(categoryModelFactory: () => CategoryModel, categoryAgents: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()) extends Actor {
  override def receive = {
    case GetCategoryActor(category) =>
      if (!categoryAgents.contains(category)) {
        categoryAgents(category) = context.actorOf(Props(new CategoryModelActor(categoryModelFactory())))
      }
      sender ! categoryAgents(category)
  }
}
