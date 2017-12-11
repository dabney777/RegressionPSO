package com.regression.regressionMLLib

import java.io.File
import breeze.optimize.LBFGS
import org.apache.spark.ml.classification.LogisticRegression
import org.apache.spark.ml.regression.LinearRegression
import org.apache.spark.mllib.classification.LogisticRegressionWithLBFGS
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.{LinearRegressionWithSGD, LabeledPoint}
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.{SparkContext, SparkConf}
import com.coefficientPSO.Coefficient

/**
 * Created by root on 12/4/17.
 */
class LRModel {
  val irisPath = "./src/main/resources/iris.data"
  val features_test = "./src/main/resources/new_features"


}

object LRModel{
  def main (args: Array[String]): Unit ={
    val lrModel = new LRModel()
    val conf = new SparkConf setAppName("MLlibRegression") setMaster("local")
    val sc = new SparkContext(conf)

    Coefficient.readData()
    val label = Coefficient.Y
    val vec = Coefficient.X map { Vectors.dense _ }
    var lpList = List[LabeledPoint]()
    for(i <- 0 until Coefficient.X.length){
      lpList = LabeledPoint(label(i), vec(i)):: lpList
    }
    val usedTime = System.currentTimeMillis()

    val data = sc.parallelize(lpList reverse).cache()

    val splits = data.randomSplit(Array(0.7, 0.3), seed = 1L)
    val training = splits(0).cache()
    val test = splits(1)

    //Run training algorithm to build the model
    val model = new LogisticRegressionWithLBFGS().setNumClasses(2).run(training)

    val predictionAndLabels = test.map {case LabeledPoint(label, features) =>
      val prediction = model.predict(features)
      (prediction, label)
    }

    val metrics = new MulticlassMetrics(predictionAndLabels)
    val accuracy = metrics.accuracy
    val recall = metrics.recall(1)
    println(s"Accuracy = $accuracy\nRecall = $recall")
    System.err.print("caculation used time: " + ((System.currentTimeMillis() - usedTime) / 1000) + "\n")
  }
}
