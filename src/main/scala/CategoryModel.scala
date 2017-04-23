import scala.collection.mutable
import akka.actor._


case class AddDocumentMessage(document: String)
case class GetNGramProbablityMessage(ngram: String)
case object GetDocumentsCount


class CategoryModelActor(categoryModel: CategoryModel) extends Actor {
  override def receive = {
    case AddDocumentMessage(document) => categoryModel.addDocument(document)
    case GetNGramProbablityMessage(ngram) =>
      val ngramProbability = categoryModel.getNGramProbability(ngram)
      sender ! ngramProbability
    case GetDocumentsCount =>
      sender ! categoryModel.documentsCount
  }
}


abstract class CategoryModel(val ngramCounts: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0), var documentsCount: Int = 0, var ngramsCount: Int = 0, documentPreprocessor: DocumentPreprocessor) {
  def addDocument(document: String): Unit = {
    for (ngram <- documentPreprocessor.getTermsFromDocument(document)) {
      ngramCounts(ngram) += 1
      ngramsCount += 1
    }
    documentsCount += 1
  }

  def getNGramProbability(ngram: String): Double
}


class LaplaceSmoothingCategoryModel(
                                     val pseudoCount: Double,
                                     documentPreprocessor: DocumentPreprocessor,
                                     var vocabulary: mutable.Set[String] = mutable.Set[String]()
                                   ) extends CategoryModel(documentPreprocessor=documentPreprocessor) {
  override def getNGramProbability(ngram: String): Double = (ngramCounts(ngram) + pseudoCount) / (ngramsCount + vocabulary.size)

  override def addDocument(document: String): Unit = {
    vocabulary ++= documentPreprocessor.getTermsFromDocument(document)
    super.addDocument(document)
  }
}


class GoodTuringSmoothingCategoryModel(
                                        frequencyThreshold: Int,
                                        documentPreprocessor: DocumentPreprocessor,
                                        countCounts: mutable.Map[Int, Int] = mutable.Map().withDefaultValue(0),
                                        unigramVocabulary: mutable.Set[String] = mutable.Set[String](),
                                        var vocabulary: mutable.Set[String] = mutable.Set[String]()
                                      ) extends CategoryModel(documentPreprocessor = documentPreprocessor) {
  override def addDocument(document: String): Unit = {
    for (ngram <- documentPreprocessor.getTermsFromDocument(document)) {
      val oldCount = ngramCounts(ngram)
      countCounts(oldCount) -= 1
      countCounts(oldCount + 1) += 1
      ngramCounts(ngram) = oldCount + 1
      ngramsCount += 1
      unigramVocabulary ++= ngram.split(" ")
      vocabulary += ngram
    }
    documentsCount += 1
  }

  override def getNGramProbability(ngram: String): Double = {
    val count = ngramCounts(ngram)
    if (count >= frequencyThreshold) {
      return count
    }
    val divisor = if (count == 0)
      10 + math.pow(unigramVocabulary.size, documentPreprocessor.ngramOrder) - vocabulary.size
    else
      countCounts(count).toDouble
    (count + 1) * countCounts(count + 1).toDouble / (ngramsCount * divisor)
  }
}

