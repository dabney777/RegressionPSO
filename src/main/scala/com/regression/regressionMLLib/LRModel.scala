package com.regression.regressionMLLib

import java.io.File
import breeze.optimize.LBFGS
import org.apache.spark.ml.regression.LinearRegression
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
    val data = sc.textFile(lrModel.irisPath)

    val parsedData = data.map { line =>
      val parts = line.split(',').toList
      val data = parts.slice(0, 4).map(_.toDouble)
      val v = Vectors.dense(data.toArray)
      LabeledPoint(categorize(parts(4)).toDouble, Vectors.dense(data.toArray))
    }.cache()

    


    val valuesAndPreds = parsedData.map{ point =>
      val prediction = model.predict(point.features)
      (point.label, 1.0)
    }

    val MSE = valuesAndPreds.map{ case(v, p) => math.pow((v-p), 2)}.sum()
    println("training MSE = " + MSE)




  }

  def categorize(s: String):Int = {
    if(s.equals("Iris-setosa")) return 1
    if(s.equals("Iris-versicolor")) return 2
    if(s.equals("Iris-virginica")) return 3
    0
  }


}
