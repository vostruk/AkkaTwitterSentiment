package classify

import java.io.StringReader

import edu.stanford.nlp.process.PTBTokenizer

import scala.collection.JavaConverters._
import edu.stanford.nlp.util.StringUtils


class DocumentPreprocessor(val ngramOrder: Int) {
  def getTermsFromDocument(document: String): List[String] = {
    val coreNlpPreprocessor = new edu.stanford.nlp.process.DocumentPreprocessor(new StringReader(
      document.replaceAll("[^\u0000-\uFFFF]", " ")) // replace all non-lingual characters with space
    )
    val factory = PTBTokenizer.factory()
    factory.setOptions("untokenizable=noneDelete")
    coreNlpPreprocessor.setTokenizerFactory(factory)
    for {
      sentence <- coreNlpPreprocessor.asScala.toList
      words = sentence.asScala.map(_.word()).toList
      ngram <- StringUtils.getNgrams(words.asJava, ngramOrder, ngramOrder).asScala
    } yield ngram
  }
}
