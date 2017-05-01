package sentimentgui

import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import classify._
import com.danielasfregola.twitter4s.entities.{AccessToken, ConsumerToken}
import download.{TweetDatesRangeDownloader,  OnlineTweetStreamer}

import scala.collection.immutable
import scala.collection.mutable.Map
import scala.concurrent.Await
import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.control.Button
import scalafx.scene.chart.PieChart
import scalafx.scene.control.TextField
import scalafx.scene.control.CheckBox
import scalafx.scene.text.Text
import breeze.linalg._
import breeze.plot._
import scalafx.scene.control.DatePicker
import java.time.LocalDate
import scalafx.scene.control.Slider
import scalafx.scene.control.ComboBox
import javafx.collections.FXCollections
import scala.util.Random
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.control.{Button, CheckBox, Label, TextField}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.text.Text
import scalafx.scene.{Group, Scene}

import javafx.stage.FileChooser
import javafx.stage.FileChooser.ExtensionFilter
import javafx.scene.control.TitledPane

object sentimentgui extends JFXApp {

  //================================ACTORS here =======================

  val CKey = "9DZO2bQPgmXO4r2eML5yVE7tb";
  val CSecret = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW";
  val AToken = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0";
  val ASecret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x";

  val consumerToken = ConsumerToken( CKey, CSecret)
  val accessToken = AccessToken(AToken, ASecret)

  //=============================================

  var AgentEmotionsList : List[String] = "happiness" :: "surprise" :: "sadness" :: "anger" :: "disgust" :: "fear" :: Nil

  val system = ActorSystem("DownloadSystem")

  val dp = new DocumentPreprocessor(2)
  val cr = system.actorOf(Props(new CategoriesRepositoryActor(dp, () => new LaplaceSmoothingCategoryModel(0.5, dp))))
  val NbMActor = system.actorOf(Props(new NaiveBayesModelActor(dp, cr)), name = "dpa1")

  val streamActor = system.actorOf(Props(new OnlineTweetStreamer(consumerToken, accessToken, NbMActor)), name = "streamActor")

  streamActor ! AgentEmotionsList

  val TweetDatesRangeDownloaderActor = system.actorOf(Props(new TweetDatesRangeDownloader(CKey, CSecret, AToken, ASecret, NbMActor)), name = "DownloadActor")
  var ActorTweetsDataReceved: List[List[Double]] = Nil

  def getclassifiedDataFromActor() = {

    implicit val duration: Timeout = 500 seconds

    //"2017-03-31"
    val scp = getScopeFromInput()
    val dfr = getDateFromInput().minusDays(scp.toLong)
    val dto = getDateFromInput().plusDays(scp.toLong)

    val fr = dfr.getYear().toString()+"-"+dfr.getMonthValue().toString()+"-"+dfr.getDayOfMonth().toString()//"2017-04-20"
    ActorTweetsDataReceved = Nil

    val t =  dto.getYear().toString()+"-"+dto.getMonthValue().toString()+"-"+dto.getDayOfMonth().toString()//"2017-04-24"

    val ans = Await.result(TweetDatesRangeDownloaderActor ? (getHashtagFromInput(), fr, t), 500.seconds).asInstanceOf[scala.collection.mutable.Map[String, Map[String, Int]]]//.category.get.toString()
    //println(ans)
    //implicit def intlist2dlist(il: List[Int]): List[Double] = il.map(_.toDouble)
    for ((k, v) <- ans)
    {
      val (keys, vals) = v.toSeq.sortBy(_._1).unzip
      ActorTweetsDataReceved :+ (vals map (_.toDouble) )
      println(k)
      println(vals.size)
    }
  }

  def refreshGui(): Unit = {

    sentimentPieChart.title = "Sentiment pie chart for #" + getHashtagFromInput() + ""
    loadData(ActorTweetsDataReceved)//genRandomData(getScopeFromInput()))

    f.clearPlot(0)
    p = f.subplot(0)
    if (AngerCheckBox.selected()) {
      p += plot(days2, sampleAnger, name = "Anger")
    }
    if (DisgustCheckBox.selected()) {
      p += plot(days2, sampleDisgust, name = "Disgust")
    }
    if (FearCheckBox.selected()) {
      p += plot(days2, sampleFear, name = "Fear")
    }
    if (HappinesCheckBox.selected()) {
      p += plot(days2, sampleHappines, name = "Happines")
    }
    if (SadnessCheckBox.selected()) {
      p += plot(days2, sampleSadness, name = "Sadness")
    }
    if (SurpriseCheckBox.selected()) {
      p += plot(days2, sampleSurprise, name = "Surprise")
    }

    sentimentPieChart.data = ObservableBuffer(dataPairs.map { case (x, y) => PieChart.Data(x, y) })
    p.xlabel = getUnitFromInput() + " around " + getDateFromInput()
    p.ylabel = "Sentiment"
    p.legend = true
    f.refresh()
  }

  def loadData (inputData: List[List[Double]]) : Unit = {
    //list of 6 lists of any length
    if (inputData.size != 6) {return}
    s = List()
    for (i <- -(inputData(0).size)/2 to (inputData(0).size)/2){
      s = List.concat(s, List(i.toDouble))
    }
    days2 = DenseVector(s.toArray)
    //println(days2)
    sampleAnger = DenseVector(inputData(0).toArray)
    //println(sampleAnger)
    sampleDisgust = DenseVector(inputData(1).toArray)
    sampleFear = DenseVector(inputData(2).toArray)
    sampleHappines = DenseVector(inputData(3).toArray)
    sampleSadness = DenseVector(inputData(4).toArray)
    sampleSurprise = DenseVector(inputData(5).toArray)
    dataPairs = Seq(("Anger",sumArray(inputData(0).toArray)), ("Disgust", sumArray(inputData(1).toArray)), ("Fear", sumArray(inputData(2).toArray)), ("Happines", sumArray(inputData(3).toArray)), ("Sadness", sumArray(inputData(4).toArray)), ("Surprise", sumArray(inputData(5).toArray)))
  }
  def getHashtagFromInput () : String ={
    return hashtagInput.getText()
  }
  def getDateFromInput () : LocalDate ={
    return dateInput.getValue()

  }
  def getUnitFromInput () : String ={
    return dhComboBox.value.value.toString

  }
  def getScopeFromInput () : Double ={
    return sliderInput.value.value.toInt.toDouble
  }

  def getCKeyFromInput () : String ={
    return CKeyInput.getText()
  }
  def getCSecretFromInput () : String ={
    return CSecretInput.getText()
  }
  def getATokenFromInput () : String ={
    return ATokenInput.getText()
  }
  def getASecretFromInput () : String ={
    return ASecretInput.getText()
  }


  //utility
  def sumArray(input:Array[Double]): Double = {
    var i=0
    var sum = 0.0
    while (i < input.length) {
      sum += input(i)
      i += 1
    }
    return sum
  }
  def genRandomData(range: Double): List[List[Double]]= {
    val randomGen = scala.util.Random
    var s : List[Double] = List()
    var sampleAngerList : List[Double] = List()
    var sampleDisgustList : List[Double] = List()
    var sampleFearList : List[Double] = List()
    var sampleHappinesList : List[Double] = List()
    var sampleSadnessList : List[Double] = List()
    var sampleSurpriseList : List[Double] = List()
    for (i <- -range.toInt to range.toInt){
//      println(i)
      s = List.concat(s, List(i.toDouble))
      sampleAngerList = List.concat(sampleAngerList, List(randomGen.nextDouble()*5+30.0))
      sampleDisgustList = List.concat(sampleDisgustList, List(randomGen.nextDouble()*5+40.0))
      sampleFearList = List.concat(sampleFearList, List(randomGen.nextDouble()*5+50.0))
      sampleHappinesList = List.concat(sampleHappinesList, List(randomGen.nextDouble()*5+60.0))
      sampleSadnessList = List.concat(sampleSadnessList, List(randomGen.nextDouble()*5+70.0))
      sampleSurpriseList = List.concat(sampleSurpriseList, List(randomGen.nextDouble()*5+80.0))
    }
    val sampleInput = List(sampleAngerList,
      sampleDisgustList,
      sampleFearList,
      sampleHappinesList,
      sampleSadnessList,
      sampleSurpriseList)
    return sampleInput
  }

  var plotUnit = "Days"
  val randomGen = scala.util.Random
  var s : List[Double] = List()
  var sampleAngerList : List[Double] = List()
  var sampleDisgustList : List[Double] = List()
  var sampleFearList : List[Double] = List()
  var sampleHappinesList : List[Double] = List()
  var sampleSadnessList : List[Double] = List()
  var sampleSurpriseList : List[Double] = List()

  //temp
  var range = 5.0
  for (i <- -range.toInt to range.toInt){
    s = List.concat(s, List(i.toDouble))
    sampleAngerList = List.concat(sampleAngerList, List(randomGen.nextDouble()*5+30.0))
    sampleDisgustList = List.concat(sampleDisgustList, List(randomGen.nextDouble()*5+40.0))
    sampleFearList = List.concat(sampleFearList, List(randomGen.nextDouble()*5+50.0))
    sampleHappinesList = List.concat(sampleHappinesList, List(randomGen.nextDouble()*5+60.0))
    sampleSadnessList = List.concat(sampleSadnessList, List(randomGen.nextDouble()*5+70.0))
    sampleSurpriseList = List.concat(sampleSurpriseList, List(randomGen.nextDouble()*5+80.0))
  }
  var sampleInput = List(sampleAngerList,
    sampleDisgustList,
    sampleFearList,
    sampleHappinesList,
    sampleSadnessList,
    sampleSurpriseList)

  var days2 = DenseVector(s.toArray)
  var sampleAnger = DenseVector(sampleAngerList.toArray)
  var sampleDisgust = DenseVector(sampleDisgustList.toArray)
  var sampleFear = DenseVector(sampleFearList.toArray)
  var sampleHappines = DenseVector(sampleHappinesList.toArray)
  var sampleSadness = DenseVector(sampleSadnessList.toArray)
  var sampleSurprise = DenseVector(sampleSurpriseList.toArray)
  //cal.add(Calendar.DATE, 1)
  //println(cal.getTime())
  var dataPairs = Seq(("Anger", 1.0), ("Disgust", 17.0), ("Fear", 25.0), ("Happines", 27.0), ("Sadness", 5.0), ("Surprise", 5.0))
  //temp


  val f = Figure("Twitter Plot")
  var p = f.subplot(0)
  val x = linspace(0.0, 1.0)
  p += plot(days2, sampleAnger)
  p.xlabel = "Days"
  p.ylabel = "Sentiment"

 //!!@@

  val hashtagConfirm = new Button {
    text = "Ok"
    onAction = { ae =>
      getclassifiedDataFromActor()
      refreshGui()
    }
  }

  val loadDataConfirm = new Button {
    text = "TrainOnline"
    onAction = { ae =>
      streamActor ! ("start", 400)
    }
  }

  val hashtagInput = new TextField{
    text = "Hashtag"
  }

  val sentimentPieChart = new PieChart {
    title = "Pie chart example"
    clockwise = false
    data = ObservableBuffer(dataPairs.map { case (x, y) => PieChart.Data(x, y) })
  }

  val AngerCheckBox = new CheckBox {
    text = "Anger"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val DisgustCheckBox = new CheckBox {
    text = "Disgust"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val FearCheckBox = new CheckBox {
    text = "Fear"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val HappinesCheckBox = new CheckBox {
    text = "Happines"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val SadnessCheckBox = new CheckBox {
    text = "Sadness"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val SurpriseCheckBox = new CheckBox {
    text = "Surprise"
    selected = true
    onAction = { ae =>
      refreshGui()
    }
  }
  val dateInput = new DatePicker(LocalDate.now()) {

  }

  val sliderInput = new Slider(1.0,3.0,2.0) {
    onMouseReleased = { ae =>
      scopeField.text = value.value.toInt.toString
      //scopeField.text = dhComboBox.value.toString
      //println(dhComboBox.value.value.toString)
    }
  }

  val scopeField = new TextField{
    disable = true
    text = "2"
    maxWidth = 40
  }

  val dhComboBox = new ComboBox[String](){

      items = ObservableBuffer("days","hours")
      value = "days"
      disable = true
    onAction = { ae =>
      if (value.value.toString == "days"){
        sliderInput.value = 1.0
        sliderInput.max = 3.0
        scopeField.text = "1"
      }
      if (value.value.toString == "hours"){
        sliderInput.value = 1.0
        sliderInput.max = 24.0
        scopeField.text = "1"
      }

    }
  }

  val inputFileChooser = new FileChooser()
  inputFileChooser.setTitle("Learn from a file")
  inputFileChooser.extensionFilters ++= Seq(
    new ExtensionFilter("Text Files", "*.txt"))

  val fileConfirm = new Button {
    text = "File"
    onAction = { ae =>
      var file = inputFileChooser.showOpenDialog(stage)
    }
  }
  val CKeyInput = new TextField{
    text = "9DZO2bQPgmXO4r2eML5yVE7tb"
    minWidth = 400
  }
  val CSecretInput = new TextField{
    text = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW"
    minWidth = 400
  }
  val ATokenInput = new TextField{
    text = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0"
    minWidth = 400
  }
  val ASecretInput = new TextField{
    text = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x"
    minWidth = 400
  }
  val keyTitledPane = new TitledPane("API keys",
    new VBox(0,
      new HBox(10,
        new Text("    CKey :"),
        CKeyInput
      ),
      new HBox(10,
        new Text("CSecret :"),
        CSecretInput
      ),
      new HBox(10,
        new Text("AToken :"),
        ATokenInput
      ),
      new HBox(10,
        new Text("ASecret :"),
        ASecretInput
      )
    )
  )
  keyTitledPane.expanded = false

  val resetModelConfirm = new Button {
    text = "Reset"
    onAction = { ae =>

    }
  }

  val holdTrainingConfirm = new Button {
    text = "Hold"
    onAction = { ae =>

    }
  }

  val pseudocountInput = new TextField{
    text = "2"
    maxWidth = 30
  }
  val frequencyThresholdInput = new TextField{
    text = "2"
    maxWidth = 30
  }
  val ngramInput = new TextField{
    text = "2"
    maxWidth = 30
  }

  val smoothingSelectionComboBox = new ComboBox[String]() {
    items = ObservableBuffer("Laplace", "Good-Turing")
    value = "Laplace"
    //    disable = true
    onAction = { ae =>
      if (value.value.toString == "Laplace"){

      }
      if (value.value.toString == "Good-Turing"){

      }

    }
  }

  val advancedClasifierOptionsTitledPane = new TitledPane("Advanced clasifier options",
    new HBox(5,
      new Text("Smoothing:"),
      smoothingSelectionComboBox,
      new Text("pseudocount:"),
      pseudocountInput,
      new Text("freq. threshold:"),
      frequencyThresholdInput,
      new Text("n-grams:"),
      ngramInput
    )
  )
  advancedClasifierOptionsTitledPane.expanded = false

  val clasifierQualityField = new TextField{
    disable = true
    text = "0"
    maxWidth = 30
  }


  stage = new JFXApp.PrimaryStage {
    title = "Twitter Sentiment Analyzer"
    resizable = false
    scene = new Scene {
      root = new VBox(10,
        new HBox(
          sentimentPieChart
        ),
        new HBox(10,
          AngerCheckBox,
          DisgustCheckBox,
          FearCheckBox,
          HappinesCheckBox,
          SadnessCheckBox,
          SurpriseCheckBox
        ),
        keyTitledPane,
        new HBox(10,
          new Text("Train model :"),
          resetModelConfirm,
          loadDataConfirm,
          holdTrainingConfirm,
          fileConfirm,
          new Text("Quality:"),
          clasifierQualityField
        ),
        advancedClasifierOptionsTitledPane,
        new HBox(20,
          new Text("Hashtag :"),
          hashtagInput,
          hashtagConfirm,
          dateInput
        ),
        new HBox(20,
          new Text("Scope :"),
          sliderInput,
          scopeField,
          dhComboBox
        )
//        new HBox(20,
//          new Text("File input:"),
//          fileConfirm
//
//        )
      )
    }
    onCloseRequest = handle {
      println("app is closing")
      //there is no Figure.close() !!!
      //f.close()
      System.exit(0)
    }
  }



}