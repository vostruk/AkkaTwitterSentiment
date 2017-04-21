import scala.collection.JavaConverters._
import scala.collection.mutable
import java.io.StringReader
import scala.math.log

import edu.stanford.nlp.util.StringUtils


class DocumentPreprocessor(val ngramOrder: Int) {
  def getTermsFromDocument(document: String): List[String] = {
    val coreNlpPreprocessor = new edu.stanford.nlp.process.DocumentPreprocessor(new StringReader(document))
    for {
      sentence <- coreNlpPreprocessor.asScala.toList
      words = sentence.asScala.map(_.word()).toList
      ngram <- StringUtils.getNgrams(words.asJava, ngramOrder, ngramOrder).asScala
    } yield ngram
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
  override def getNGramProbability(ngram: String) = (ngramCounts(ngram) + pseudoCount) / (ngramsCount + vocabulary.size)

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


class NaiveBayesModel(documentPreprocessor: DocumentPreprocessor, categoryModelFactory: () => CategoryModel, languageModels: mutable.Map[String, CategoryModel] = mutable.Map[String, CategoryModel]()) {

  def addDocument(document: String, category: String): Unit = {
    if (!languageModels.contains(category)) {
      languageModels(category) = categoryModelFactory()
    }
    languageModels(category).addDocument(document)
  }

  def classifyDocument(document: String) = {
    val allNGrams = documentPreprocessor.getTermsFromDocument(document)
    val documentsCount = languageModels.values.map(_.documentsCount).sum
    val scoredCategories = for {
      (category, languageModel) <- languageModels
      loglikelihood = allNGrams.map(languageModel.getNGramProbability).map(log).sum + log(languageModel.documentsCount.toDouble / documentsCount)
    } yield (category, loglikelihood)
    scoredCategories.maxBy(_._2)._1
  }
}


object Hi {

  def main(args: Array[String]): Unit = {
    val documentPreprocessor = new DocumentPreprocessor(1)
    val naiveBayesModel = new NaiveBayesModel(documentPreprocessor, () => new GoodTuringSmoothingCategoryModel(6, documentPreprocessor))
    val source = scala.io.Source.fromFile("newsgroups_dataset.txt")
    val inputText = try source.mkString.toLowerCase finally source.close()
    val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
    val (train, test) = lines.partition(_ => util.Random.nextDouble() < 0.7)
    for (line <- train) {
      naiveBayesModel.addDocument(line(1), line(0))
    }
    val errors = test.map((line) => line(0) != naiveBayesModel.classifyDocument(line(1))).count(_ == true)
    println(errors.toDouble / test.length)
  }
}
