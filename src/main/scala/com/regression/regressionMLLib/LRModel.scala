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

/**
 * Created by root on 12/4/17.
 */
class LRModel {
  val irisPath = "./src/main/resources/iris.data"


}

object LRModel{
  def main (args: Array[String]): Unit ={
    val lrModel = new LRModel()
    val conf = new SparkConf setAppName("MLlibRegression") setMaster("local")
    val sc = new SparkContext(conf)
    val rawData = sc.textFile(lrModel.irisPath).cache()

    val data = rawData.map{line =>
      val rawFeatures = line.split(",")
      val label = categorize(rawFeatures(4))
      val v = rawFeatures.slice(0, 4).map{_.toDouble}
      LabeledPoint(label, Vectors.dense(v))
    }
    val splits = data.randomSplit(Array(0.7, 0.3), seed = 1L)
    val training = splits(0).cache()
    val test = splits(1)

    //Run training algorithm to build the model
    val model = new LogisticRegressionWithLBFGS().setNumClasses(4).run(training)

    val predictionAndLabels = test.map {case LabeledPoint(label, features) =>
      val prediction = model.predict(features)
      (prediction, label)
    }

    val metrics = new MulticlassMetrics(predictionAndLabels)
    val accuracy = metrics.accuracy
    val recall = metrics.recall(1)
    println(s"Accuracy = $accuracy\nRecall = $recall")
  }


  def categorize(s: String):Int = {
    if(s.equals("Iris-setosa")) return 1
    if(s.equals("Iris-versicolor")) return 2
    if(s.equals("Iris-virginica")) return 3
    0
  }


}
