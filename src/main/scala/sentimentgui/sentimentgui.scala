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
import java.util.Calendar
import scala.util.Random

object sentimentgui extends JFXApp {

  def refreshGui(): Unit = {

    // temp
    dataPairs = Seq(("Anger", 25), ("Disgust", 17), ("Fear", 25), ("Happines", 27), ("Sadness", 5), ("Surprise", 5))
    //temp

    sentimentPieChart.title = "Sentiment pie chart for #" + hashtagInput.getText() + ""

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
    p.xlabel = "Days"
    p.ylabel = "Sentiment"
    p.legend = true
    f.refresh()
  }



  //temp
  var days = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0)
  val cal = Calendar.getInstance()

  val randomGen = scala.util.Random

  var s : List[Double] = List()
  var sampleAngerList : List[Double] = List()
  var sampleDisgustList : List[Double] = List()
  var sampleFearList : List[Double] = List()
  var sampleHappinesList : List[Double] = List()
  var sampleSadnessList : List[Double] = List()
  var sampleSurpriseList : List[Double] = List()

  for (i <- 1 to 17){
    s = List.concat(s, List(i.toDouble))
    sampleAngerList = List.concat(sampleAngerList, List(randomGen.nextDouble()*5+30.0))
    sampleDisgustList = List.concat(sampleDisgustList, List(randomGen.nextDouble()*5+40.0))
    sampleFearList = List.concat(sampleFearList, List(randomGen.nextDouble()*5+50.0))
    sampleHappinesList = List.concat(sampleHappinesList, List(randomGen.nextDouble()*5+60.0))
    sampleSadnessList = List.concat(sampleSadnessList, List(randomGen.nextDouble()*5+70.0))
    sampleSurpriseList = List.concat(sampleSurpriseList, List(randomGen.nextDouble()*5+80.0))
  }




  var days2 = DenseVector(s.toArray)
  var sampleAnger = DenseVector(sampleAngerList.toArray)
  var sampleDisgust = DenseVector(sampleDisgustList.toArray)
  var sampleFear = DenseVector(sampleFearList.toArray)
  var sampleHappines = DenseVector(sampleHappinesList.toArray)
  var sampleSadness = DenseVector(sampleSadnessList.toArray)
  var sampleSurprise = DenseVector(sampleSurpriseList.toArray)


  val exSent2 = DenseVector(22.3,50.3,66.3)
  val exSent = DenseVector(75.31, 74.72, 74.47, 76.21, 76.4, 75.69, 76.22, 76.54, 76.39, 75.18, 74.82, 74.43, 74.14, 72.32, 71.73, 72.9, 73.36)

  //cal.add(Calendar.DATE, 1)
  //println(cal.getTime())
  var dataPairs = Seq(("Anger", 1), ("Disgust", 17), ("Fear", 25), ("Happines", 27), ("Sadness", 5), ("Surprise", 5))
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

  val hashtagInput = new TextField()

  val sentimentPieChart = new PieChart {
    title = "Pie chart example"
    clockwise = false
    data = ObservableBuffer(dataPairs.map { case (x, y) => PieChart.Data(x, y) })
  }

  val AngerCheckBox = new CheckBox {
    text = "Anger"
    onAction = { ae =>
      refreshGui()
    }
  }
  val DisgustCheckBox = new CheckBox {
    text = "Disgust"
    onAction = { ae =>
      refreshGui()
    }
  }
  val FearCheckBox = new CheckBox {
    text = "Fear"
    onAction = { ae =>
      refreshGui()
    }
  }
  val HappinesCheckBox = new CheckBox {
    text = "Happines"
    onAction = { ae =>
      refreshGui()
    }
  }
  val SadnessCheckBox = new CheckBox {
    text = "Sadness"
    onAction = { ae =>
      refreshGui()
    }
  }
  val SurpriseCheckBox = new CheckBox {
    text = "Surprise"
    onAction = { ae =>
      refreshGui()
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
            hashtagConfirm
          )
        )
      )
    }
  }
}