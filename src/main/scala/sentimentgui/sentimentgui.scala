package sentimentgui

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

object sentimentgui extends JFXApp {

  def refreshGui(): Unit = {

    sentimentPieChart.title = "Sentiment pie chart for #" + getHashtagFromInput() + ""
    loadData(genRandomData(getScopeFromInput()))

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

  val hashtagConfirm = new Button {
    text = "Ok"
    onAction = { ae =>
      refreshGui()

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

  val sliderInput = new Slider(1.0,24.0,1.0) {
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
      value = "hours"
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

  stage = new JFXApp.PrimaryStage {
    title = "Twitter Sentiment Analyzer"
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
        new HBox(
          new HBox(20,
            new Text("Hashtag :"),
            hashtagInput,
            hashtagConfirm,
            dateInput
          )
        ),
        new HBox(20,
          new Text("Scope :"),
          sliderInput,
          scopeField,
          dhComboBox
        )
      )
    }
  }
}