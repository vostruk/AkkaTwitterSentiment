package sentimentgui

/**
  * Created by Stefan on 2017-05-13.
  */
class inputValidator {
  var testLabels: List[String] = List[String]()
  var testDocuments: List[String] = List[String]()
  var listOfAcceptedEmotions: List[String] = List[String]("happiness" , "surprise" , "sadness" , "anger" , "disgust" , "fear")

  def validate(testDataFile : java.io.File): Boolean ={
    val source = scala.io.Source.fromFile(testDataFile.getAbsolutePath)
    val inputText = try source.mkString.toLowerCase finally source.close()
    try {
      val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2)).toList
      testLabels = lines.map(_ (0))
      testDocuments = lines.map(_ (1))


      for (el <- testLabels){
        if (!listOfAcceptedEmotions.contains(el)) {
          return false
        }
      }
      return true

    } catch{
      case ex :ArrayIndexOutOfBoundsException=>
        println(ex.toString)
        return false
    }

    return true
  }

}
