package sentimentgui

//import akka.actor.Status.{Success, Failure}
import akka.actor.{Actor, ActorSystem, Kill, OneForOneStrategy, Props}
import akka.util.Timeout
import classify._
import com.danielasfregola.twitter4s.entities.{AccessToken, ConsumerToken}
import download._

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{ListBuffer, Map}
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

import scala.util.{Failure, Random, Success}
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
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.Alert.AlertType.INFORMATION

import akka.actor.SupervisorStrategy._
import akka.routing.{RoundRobinGroup, RoundRobinPool}
import classify.Main.categoriesRepository

import java.util.concurrent._
import java.util.Timer

import scala.concurrent.duration._

import java.io.PrintWriter
import scala.io.Source



object sentimentgui extends JFXApp {

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

  def setCKeyInInput (in : String) : Unit ={
    CKeyInput.text = in
  }
  def setCSecretInInput (in : String) : Unit ={
    CSecretInput.text = in
  }
  def setATokenInInput (in : String) : Unit ={
    ATokenInput.text = in
  }
  def setASecretInInput (in : String) : Unit ={
    ASecretInput.text = in
  }

  class authKey {
    var CKey : String = "-"
    var CSecret : String = "-"
    var AToken : String = "-"
    var ASecret : String = "-"

    def setKeys(ck : String, cs : String, at : String, as : String){
      this.CKey = ck
      this.CSecret = cs
      this.AToken = at
      this.ASecret = as
    }
    def print(): String ={
     val s = this.CKey + "\t"+ this.CSecret + "\t" + this.AToken + "\t" + this.ASecret + "\n"
//      println(this.CKey)
//      println(this.CSecret)
//        println(this.AToken)
//        println(this.ASecret)
      return s
    }
  }
  var authList = new ListBuffer[authKey]()


  val system = ActorSystem("DownloadSystem")

  //================================ACTORS here =======================

  val CKey = "9DZO2bQPgmXO4r2eML5yVE7tb"; // getCKeyFromInput() //
  val CSecret = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW"; //getCSecretFromInput() //
  val AToken =  "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0"; //getATokenFromInput() //
  val ASecret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x"; //getASecretFromInput() //
  authList += new authKey()
  authList(0).setKeys(CKey,CSecret,AToken,ASecret)
  var defaultAuthList = authList

  val consumerToken = ConsumerToken( CKey, CSecret)
  val accessToken = AccessToken(AToken, ASecret)

  //=============================================

  var GlobalEmojiMap = immutable.Map("happiness" -> immutable.Set("😀"),  "surprise" -> immutable.Set("😯"), "sadness"  -> immutable.Set("☹️"),  "anger" ->  immutable.Set("😠"), "disgust" -> immutable.Set("\uD83D\uDE12") ,  "fear"  -> immutable.Set("\uD83D\uDE31"))

  val categoriesRepository = system.actorOf(Props(new CategoriesRepositoryActor()))
  val routerActor = system.actorOf(Props(new NaiveBayesModelRouterActor(categoriesRepository)))
  routerActor ! SetWorkersNumber(3)
  val TweetDatesRangeDownloaderActor = system.actorOf(Props(new TweetDatesRangeDownloader(CKey, CSecret, AToken, ASecret, routerActor)), name = "DownloadActor")
  val streamActor = system.actorOf(Props(new OnlineTweetStreamer(consumerToken, accessToken, routerActor)), name = "streamActor")
  //val TestingActor = system.actorOf(Props(new TestingActor(testingFileName,routerActor)))
  val FileReaderActor = system.actorOf(Props(new FileReader(routerActor)))
  val TestingActorInstance = system.actorOf(Props(new TestingActor(routerActor)))
  //FileReaderActor ! StartLearningFromFile("categories_init_learn.txt")

  def getclassifiedDataFromActor() = {

    //"2017-03-31"
    val scp = getScopeFromInput()
    val dfr = getDateFromInput().minusDays(scp.toLong)
    val dto = getDateFromInput().plusDays(scp.toLong)

    val fr = dfr.getYear().toString()+"-"+dfr.getMonthValue().toString()+"-"+dfr.getDayOfMonth().toString()//"2017-04-20"
    val t =  dto.getYear().toString()+"-"+dto.getMonthValue().toString()+"-"+dto.getDayOfMonth().toString()//"2017-04-24"

    //val ans = Await.result(TweetDatesRangeDownloaderActor ? (getHashtagFromInput(), fr, t), 500.seconds).asInstanceOf[scala.collection.mutable.Map[String, Map[String, Int]]]//.category.get.toString()
    TweetDatesRangeDownloaderActor ! (getHashtagFromInput(), fr, t)
    //implicit def intlist2dlist(il: List[Int]): List[Double] = il.map(_.toDouble)

  }

  def castMapToList(MP: scala.collection.mutable.Map[String, Map[String, Int]], range: Double): List[List[Double]]= {


    //for (i <- -range.toInt to range.toInt){

    // s = List.concat(s, List(i.toDouble))

    //NEED TO GET ALL DATES FROM RANGE and then just count
    val allDates = new ListBuffer[String]()
    for ((k, v) <- MP)
    {
      val (keys, vals) = v.toSeq.sortBy(_._1).unzip
      keys.foreach(kk => if(!allDates.contains(kk)){allDates+=kk} )
    }

    var sampleAngerList = new ListBuffer[Double]()
    var sampleDisgustList = new ListBuffer[Double]()
    var sampleFearList = new ListBuffer[Double]()
    var sampleHappinesList = new ListBuffer[Double]()
    var sampleSadnessList = new ListBuffer[Double]()
    var sampleSurpriseList = new ListBuffer[Double]()



     allDates.foreach( dd => {
       if(MP.contains("anger") && MP("anger").contains(dd)) {
         sampleAngerList+=MP("anger")(dd)
       } else {sampleAngerList+=0}

      if(MP.contains("disgust") && MP("disgust").contains(dd)) {
        sampleDisgustList+=MP("disgust")(dd)
      } else {sampleDisgustList+=0}

      if(MP.contains("happiness") && MP("happiness").contains(dd)) {
        sampleHappinesList+=MP("happiness")(dd)
      } else {sampleHappinesList+=0}

      if(MP.contains("sadness") && MP("sadness").contains(dd)) {
        sampleSadnessList+=MP("sadness")(dd)
      } else {sampleSadnessList+=0}

       if(MP.contains("fear") && MP("fear").contains(dd)) {
         sampleFearList+=MP("fear")(dd)
       } else {sampleFearList+=0}


       if(MP.contains("surprise") && MP("surprise").contains(dd)) {
          sampleSurpriseList+=MP("surprise")(dd)
        } else {sampleSurpriseList+=0}})

    //   sampleDisgustList = List.concat(sampleDisgustList, List(randomGen.nextDouble()*5+40.0))
    //  }
    println(MP)
    var sampleInput = List(
      sampleAngerList.toList,
      sampleDisgustList.toList,
      sampleFearList.toList,
      sampleHappinesList.toList,
      sampleSadnessList.toList,
      sampleSurpriseList.toList)
    println(sampleInput)

    if (sampleInput(0).isEmpty){
      sampleInput = List( List(0.0),List(0.0),List(0.0),List(0.0),List(0.0),List(0.0))
    }
    println(sampleInput)
    return sampleInput
  }

  def refreshGui(): Unit = {
    implicit val duration: Timeout = 50 seconds
    val ans = Await.result(TweetDatesRangeDownloaderActor ? GetDateToStatMessage, 50.seconds).asInstanceOf[scala.collection.mutable.Map[String, Map[String, Int]]]//.category.get.toString()

    /*for ((k, v) <- ans)
    {
      val (keys, vals) = v.toSeq.sortBy(_._1).unzip
      ActorTweetsDataReceved :+ (vals map (_.toDouble) )
      println(k)
      println(vals.size)
    }*/
    sentimentPieChart.title = "Sentiment pie chart for #" + getHashtagFromInput() + ""
    loadData(castMapToList(ans, getScopeFromInput()*2))//genRandomData(getScopeFromInput()))

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

  def isAllDigits(x: String) = x forall Character.isDigit
  def getPseudocountFromInput () : Double ={
    val s = pseudoOrFreqInput.getText()
    if (isAllDigits(s)){
      val t = s.toDouble
      if (t<=0) {new Alert(AlertType.INFORMATION, "pseudocount has to be a positive number").showAndWait(); return -1.0}
      return t
    }
    else{
      new Alert(AlertType.INFORMATION, "pseudocount has to be a positive number").showAndWait()
      return -1.0
    }

  }
  def getFrequencyThresholdFromInput () : Int ={
    val s = pseudoOrFreqInput.getText()
    if (isAllDigits(s)){
      val t = s.toInt
      if (t<=0) {new Alert(AlertType.INFORMATION, "Frequency threshold has to be a positive integer").showAndWait(); return -1}
      return t
    }
    else{
      new Alert(AlertType.INFORMATION, "Frequency threshold has to be a positive integer").showAndWait()
      return -1
    }

//    return frequencyThresholdInput.getText()
  }
  def getNgramInput () : Int ={
    val s = ngramInput.getText()
    if (isAllDigits(s)){
      val t = s.toInt
      if (t<=0) {new Alert(AlertType.INFORMATION, "Ngram has to be a positive integer").showAndWait(); return -1}
      return t
    }
    else{
      new Alert(AlertType.INFORMATION, "Ngram has to be a positive integer").showAndWait()
      return -1
    }
//    return ngramInput.getText()
  }

  def getNumberOfWorkersFromInput () : Int ={
    val s = numberOfWorkersInput.getText()
    if (isAllDigits(s)){
      val t = s.toInt
      if (t<=0) {new Alert(AlertType.INFORMATION, "Number of workers has to be a positive integer").showAndWait(); return -1}
      return t
    }
    else{
      new Alert(AlertType.INFORMATION, "Number of workers has to be a positive integer").showAndWait()
      return -1
    }
    //    return ngramInput.getText()
  }

  def setQualityField (in: String) : Unit ={
    clasifierQualityField.text = in
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
    disable = true
    onAction = { ae =>
      TweetDatesRangeDownloaderActor ! SetUserKeysDownloader(authList(0).CKey,authList(0).CSecret,authList(0).AToken,authList(0).ASecret)
      getclassifiedDataFromActor()
      refreshGui()
    }
  }

  val loadDataConfirm = new Button {
    text = "Train Online"
    onAction = { ae =>
      disable = true
      resetModelConfirm.setDisable(true)
      hashtagConfirm.setDisable(true)
      //disableLearningConfirm()
      //disableTestingConfirm()
      enableHoldLearningConfirm()

      val ct = ConsumerToken( authList(0).CKey, authList(0).CSecret)
      val at = AccessToken(authList(0).AToken, authList(0).ASecret)
      streamActor ! SetUserKeys(ct,at)
      streamActor ! StartStreamingMessage("happiness" :: "surprise" :: "sadness" :: "anger" :: "disgust" :: "fear" :: Nil, GlobalEmojiMap )
    }
  }

  def disableTestingConfirm(): Unit ={
    fileTestingConfirm.setDisable(true)
  }
  def disableLearningConfirm(): Unit ={
    fileLearningConfirm.setDisable(true)
  }
  def enableHoldLearningConfirm(): Unit ={
    holdTrainingConfirm.setDisable(false)
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

  val sliderInput = new Slider(0.0,3.0,1.0) {
    onMouseReleased = { ae =>
      scopeField.text = value.value.toInt.toString
      //scopeField.text = dhComboBox.value.toString
      //println(dhComboBox.value.value.toString)
    }
  }

  val scopeField = new TextField{
    disable = true
    text = "1"
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
  val defaultDirectory = new java.io.File(".")
  inputFileChooser.setInitialDirectory(defaultDirectory)

  val fileLearningConfirm = new Button {
    text = "Learn"
    onAction = { ae =>
//      var file = inputFileChooser.showOpenDialog(stage)
//      if (file != null) {
//       //process it
//      }
      resetModelConfirm.setDisable(true)
      hashtagConfirm.setDisable(true)
      disable = true
      //loadDataConfirm.setDisable(true)
      //disableTestingConfirm()
      enableHoldLearningConfirm()

      var file = inputFileChooser.showOpenDialog(stage)
      if (file != null) {
        //println("fileok")
        FileReaderActor ! StartLearningFromFile(file.getAbsolutePath) // other file ?
      }


    }
  }

  val fileTestingConfirm = new Button {
    text = "Test"
    onAction = { ae =>
      resetModelConfirm.setDisable(true)
      hashtagConfirm.setDisable(true)
      disable = true
      //loadDataConfirm.setDisable(true)
      //disableLearningConfirm()
      enableHoldLearningConfirm()

            var file = inputFileChooser.showOpenDialog(stage)
            if (file != null) {
              //println("fileok")
              TestingActorInstance ! SetTestDataFile(file)
              TestingActorInstance ! StartEvaluatingModel
            }

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

  var selectedKey = 0

  val currentKeyField = new TextField{
    disable = true
    text = (selectedKey+1).toString + " of " + authList.size.toString
    maxWidth = 60
  }
  def refreshKeyField(): Unit ={
    currentKeyField.text = (selectedKey+1).toString + " of " + authList.size.toString
  }
  def showCurrentKey(current : Int): Unit ={
    setCKeyInInput(authList(selectedKey).CKey)
    setCSecretInInput(authList(selectedKey).CSecret)
    setATokenInInput(authList(selectedKey).AToken)
    setASecretInInput(authList(selectedKey).ASecret)
  }


  val prevKeyButton = new Button {
    text = "Previous"
    onAction = { ae =>
      if (selectedKey>0){
        selectedKey = selectedKey -1
      }
      showCurrentKey(selectedKey)
      refreshKeyField()
    }
  }

  val nextKeyButton = new Button {
    text = "Next"
    onAction = { ae =>
      if (selectedKey+1<authList.size){
        selectedKey = selectedKey +1
      }
      showCurrentKey(selectedKey)
      refreshKeyField()
    }
  }

  val addKeyButton = new Button {
    text = "Add"
    onAction = { ae =>
      authList += new authKey()
      refreshKeyField()
    }
  }

  val setKeyButton = new Button {
    text = "Set"
    onAction = { ae =>
      authList(selectedKey).setKeys(getCKeyFromInput(),getCSecretFromInput(),getATokenFromInput(),getASecretFromInput())
    }
  }

  val delKeyButton = new Button {
    text = "Delete"
    onAction = { ae =>
      if (authList.size != 1){
      authList.remove(selectedKey)
      if (selectedKey==authList.size){
        selectedKey = selectedKey -1
      }
      showCurrentKey(selectedKey)
      refreshKeyField()
    }
    }
  }

  val saveKeyButton = new Button {
    text = "Save"
    onAction = { ae =>
      new PrintWriter("keysConfig.txt") {
//        write(authList(0).print())
        for (el <- authList){
          write(el.print())
        }
        close
      }

    }
  }

  val loadKeyButton = new Button {
    text = "Load"
    onAction = { ae =>
      val source = Source.fromFile("keysConfig.txt")

      val inputText = try source.mkString finally source.close()
      try {
        val lines = inputText.split("\n").map(_.split("\t", 4)).toList
        val cks = lines.map(_ (0))
        val css = lines.map(_ (1))
        val ats = lines.map(_ (2))
        val ass = lines.map(_ (3))
     if (cks.size == css.size && css.size == ats.size && ats.size == ass.size ){
          println(cks)
          println(css)
          println(ats)
          println(ass)
          println (ass.size)
       authList = new ListBuffer[authKey]()
       var it = 0
       for (key<- cks){
         authList += new authKey()
         authList(it).CKey = key
         it = it + 1
       }
       it = 0
       for (key<- css){
         authList(it).CSecret = key
         it = it + 1
       }
       it = 0
       for (key<- ats){
         authList(it).AToken = key
         it = it + 1
       }
       it = 0
       for (key<- ass){
         authList(it).ASecret = key
         it = it + 1
       }
       it = 0
       selectedKey = 0
       showCurrentKey(selectedKey)
       refreshKeyField()
        }else
        {
          authList = defaultAuthList
          selectedKey = 0
          showCurrentKey(selectedKey)
          refreshKeyField()
          new Alert(AlertType.INFORMATION, "keyCofing file has incorrect format. Setting default keys").showAndWait()
        }
      } catch{
        case ex :ArrayIndexOutOfBoundsException=>
          authList = defaultAuthList
          selectedKey = 0
          showCurrentKey(selectedKey)
          refreshKeyField()
          new Alert(AlertType.INFORMATION, "keyCofing file has incorrect format!\n" +ex.toString).showAndWait()
      }

//      val lines = source.mkString
//      println (lines.split("\n"))

    }
  }



  val keyTitledPane = new TitledPane("API keys",
    new VBox(0,
      new HBox(10,
        prevKeyButton,
        currentKeyField,
        nextKeyButton,
        addKeyButton,
        setKeyButton,
        delKeyButton,
        saveKeyButton,
        loadKeyButton
      ),
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
        hashtagConfirm.setDisable(true)
        println("Clearing trained model.")
        categoriesRepository ! ClearTrainedModel
        //cq ! GuiUpdateQuality("test")
    }
  }

  val holdTrainingConfirm = new Button {
    text = "Hold"
    disable = true
    onAction = { ae =>
      hashtagConfirm.setDisable(false)
      resetModelConfirm.setDisable(false)
      loadDataConfirm.setDisable(false)
      fileLearningConfirm.setDisable(false)
      fileTestingConfirm.setDisable(false)
      disable = true


      implicit val timeout = Timeout(50 seconds)
      TestingActorInstance ! StopEvaluatingModel
      FileReaderActor ! StopLearningFromFile
      system.actorSelection("/user/streamActor").resolveOne().onComplete {
        case Success(st) => st ! StopStreamingMessage
        case Failure(ex) => println("Actor u wanna kill doesn't exist")
      }
    }
  }

  val pseudoOrFreqInput = new TextField{
    text = "2"
    maxWidth = 30
  }
  val pseudoOrFreqInputLabel = new Text("           pseudocount:")


  val ngramInput = new TextField{
    text = "2"
    maxWidth = 30
  }

  val numberOfWorkersInput = new TextField{
    text = "4"
    maxWidth = 40
  }

  val smoothingSelectionComboBox = new ComboBox[String]() {
    items = ObservableBuffer("Laplace", "Good-Turing")
    value = "Laplace"
    //    disable = true
    onAction = { ae =>
      if (value.value.toString == "Laplace"){
        pseudoOrFreqInputLabel.text = "           pseudocount:"
        pseudoOrFreqInput.text = "2"
      }
      if (value.value.toString == "Good-Turing"){
        pseudoOrFreqInputLabel.text = "frequency threshold:"
        pseudoOrFreqInput.text = "10"
      }

    }
  }

  val setParamsButton = new Button {
    text = "Set advanced options"
    onAction = { ae =>
      //println("parameters set")

      val ngram = getNgramInput()
      if (ngram != -1) {

        if (smoothingSelectionComboBox.value.value.toString == "Laplace"){
          val pseudo = getPseudocountFromInput()
          if (pseudo != -1) {
            println("setting to Laplace")
            categoriesRepository ! LaplaceSmoothingModel(ngram, pseudo)
          }
        }
        if (smoothingSelectionComboBox.value.value.toString == "Good-Turing"){
          val freq = getFrequencyThresholdFromInput()
          if (freq != -1.0) {
            println("setting to Good-Turing")
            categoriesRepository ! GoodTuringSmoothingModel(ngram, freq)
          }
        }
      }

      val nrWork = getNumberOfWorkersFromInput()
      if (nrWork != -1.0) {
        //println("Setting numbe")
        routerActor ! SetWorkersNumber(nrWork)
      }


    }
  }

  val advancedClasifierOptionsTitledPane = new TitledPane("Advanced clasifier options",
    new VBox(5,
    new HBox(5,
      new Text("Smoothing:"),
      smoothingSelectionComboBox,
      new Text("n-grams:"),
      ngramInput,
      pseudoOrFreqInputLabel,
      pseudoOrFreqInput

    ),
      new HBox(5,
        new Text("Number of workers: "),
        numberOfWorkersInput,
        setParamsButton
      )
    )
  )
  advancedClasifierOptionsTitledPane.expanded = false

  val clasifierQualityField = new TextField{
    disable = true
    text = "0"
    maxWidth = 60
  }







//  abstract class QualityIn
//  case class GuiUpdateQuality(input :String) extends QualityIn
//  class ShowClassificationQualityActor extends Actor {
//
//    override def receive = {
//      case GuiUpdateQuality(inputString :String) =>
//        setQualityField(inputString)
//    }
//  }
//  val cq = system.actorOf(Props(new ShowClassificationQualityActor()))


  val executor = new ScheduledThreadPoolExecutor(1)
  val task = new Runnable {
    def run() = {
      implicit val timeout = Timeout(5 seconds)
      val future = TestingActorInstance ? GetAccuracy
      var result = Await.result(future, timeout.duration).asInstanceOf[String]
      //println(result)
      setQualityField(result)
    }
  }
  val sched = executor.scheduleAtFixedRate(task, 1, 1, TimeUnit.SECONDS)
//  //sched.cancel(false)


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
          new Text("Files:"),
          fileLearningConfirm,
          fileTestingConfirm
        ),
        new HBox(10,
          new Text("Quality of classification:"),
          clasifierQualityField
        )
        ,
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