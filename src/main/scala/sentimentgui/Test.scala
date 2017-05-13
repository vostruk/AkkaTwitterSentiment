package sentimentgui



object Test extends App {
  println(".")
  val vali = new inputValidator()
  val fil = new java.io.File("./emoTweets_test.txt")

  println(vali.validate(fil))
}
