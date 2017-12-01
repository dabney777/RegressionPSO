package com.coefficientPSO

import java.io._
import java.time
import scala.collection.parallel.mutable.ParArray
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

import scala.util.Random

/**
  * Created by Dabney on 4/20/2017.
  */
class Coefficient {

}
object Coefficient{
  //Iris: numberOfinstaces = 150, rawDataDimensions = 4
  //letter-recognition: numberOfinstaces = 20000, rawDataDimensions = 16
  //letterA: numberOfInstances = 1200, rawDataDimensions = 10
  //mouse: numberOf_POS_Instances = 2000, numberOf_NEG_Instances = 2000, dimensions = 412
  var numberOfInstances = 38000
  val Y = new Array[Double](numberOfInstances)
  var X = List[Array[Double]]()
  val rawDataDimensions = 900

  //read RAWdata and put input in X output in Y
  def readData()={
    val pathIris = ".\\src\\main\\resources\\iris.data"
    val pathLetters = ".\\src\\main\\resources\\letter-recognition.data"
    val pathLetterTrain = ".\\src\\main\\resources\\letterA\\TrainData"
    val pathMouse = ".\\src\\main\\resources\\mouse_features\\TrainData"
    val pathMouseTest = ".\\src\\main\\resources\\mouse_features\\TestData"
    val features_test = "./src/main/resources/new_features"
    val f = new File(features_test)
    val r = new RandomAccessFile(f,"r")
    // val r = new BufferedReader(new InputStreamReader(new FileInputStream(f), "utf-8"))
    val gap = 10
    val t_read_start = System.currentTimeMillis()
    for(i <- 0 until numberOfInstances){
      if(i % 3800 == 0){print(" finishing " + i + "\n")}
      if(i % gap == 0 ){
        val rawLine = r.readLine()
        val temp = new Array[Double](rawDataDimensions)
        if(rawLine.length() > 1){
          val line = rawLine.split(",")
          for(p <- 0 until rawDataDimensions){
            temp(p) = line(p).toDouble
//            try {
//              if (line(p).matches(".*[0-9]+*")) {
//                temp(p) = line(p).toDouble
//              }
//            }catch{
//              case _ => {
//                print(line.length)
//                throw new Exception("an error")
//              }

//            }
          }
        }
        X = temp::X
      }
    }
    System.err.print("used time: " + ((System.currentTimeMillis() - t_read_start) / 1000) + "\n")
    numberOfInstances /= gap

    X = X.reverse

    r.seek(0)
    for(i <- 0 until numberOfInstances) {
      //letters: r.readLine().split(",")(rawDataDimensions).trim == "A"
      if(i< numberOfInstances/2){
        Y(i) = 1
      }else{
        Y(i) = 0
      }
    }
    r.close()
//    for(i <- 0 until X(0).length){
//      X = normalizeColumn(X,i)
//    }
  }

  def changeTestData(newElem:List[Array[Int]],numberOfTestInstance:Int)={
    val pathIris = ".\\src\\main\\resources\\iris.data"
    val pathLetters = ".\\src\\main\\resources\\letter-recognition.data"
    val pathLetterTest = ".\\src\\main\\resources\\letterA\\TestData"
    val pathMouse = ".\\src\\main\\resources\\mouse_features\\mouse_train"
    val pathMouseTest = ".\\src\\main\\resources\\mouse_features\\mouse_train_test"
    val f = new File(pathMouse)
    val r = new RandomAccessFile(f,"r")
   // val r = new Buffereder(new InputStreamReader(new FileInputStream(f), "utf-8"))

    for(i <- 0 until numberOfTestInstance){
      val line = r.readLine().split(",")
      val temp = new Array[Double](rawDataDimensions)
      for(p <- 0 until rawDataDimensions){
        if(line(p).matches("[0-9]+.*")){
          temp(p) = line(p).toDouble
        }
      }
      X = temp::X
    }

    X = X.reverse

    r.seek(0)
    for(i <- 0 until numberOfTestInstance) {
      if(r.readLine().split(",")(rawDataDimensions).trim == "A"){
        Y(i) = 1
      }else{
        Y(i) = 0
      }
    }
    r.close()
    for(i <- 0 until X(0).length){
      X = normalizeColumn(X, i, numberOfTestInstance)
    }
    if(!newElem.isEmpty) {
      for (i <- newElem) {
        addNewX(i, numberOfTestInstance)
      }
    }
    persistList(X, ".\\src\\main\\resources\\letterA\\TestDataNormed")
  }

  //add new value to X in MEMORY insted rawData
  def addNewX(power:Array[Int], number:Int = numberOfInstances) = {
    val sizeOfXArray = X(0).length
    var temp = List[Array[Double]]()
    for(i <- 0 until X.length){
      var sum = 1d
      for(j <- 0 until rawDataDimensions){
        sum = sum * Particle.powerA(X(i)(j), power(j))
      }
      val arr = new Array[Double](sizeOfXArray + 1)
      System.arraycopy(X(i), 0, arr, 0, sizeOfXArray)
      arr(sizeOfXArray) = sum
      temp =  arr :: temp
    }
    temp = temp.reverse
    temp = normalizeColumn(temp, sizeOfXArray, number)
    X = temp


  }

  def normalizeColumn(src:List[Array[Double]], no:Int, number:Int = numberOfInstances) ={
    var normalization = new Array[Double](number)
    for(i <- 0 until number){
      normalization(i) = src(i)(no)
    }
    normalization = normalize(normalization)
    for(i <- 0 until number){
      src(i)(no) = normalization(i)
    }
    src
  }

  def normalize(src:Array[Double]
              ):Array[Double] = {
    var max:Double = Double.MinValue
    var min:Double = Double.MaxValue
    for(i <- src){
      if(max < i ) max = i
      if(min > i ) min = i
    }
    val range = max - min
    val output = new Array[Double](src.length)
    if(range <= Double.MinPositiveValue)
        System.err.print("this array is all zero")

    for(i <- 0 until src.length){
      output(i) = ( src(i) - min ) / range
    }
    output//All NaN!!##
  }

  def persistList[T](array:List[Array[T]], path:String)={
    val f = new File(path)
    val cleaner = new FileWriter(f)
    cleaner.write("")
    val ranf = new RandomAccessFile(f, "rws")
    var str = ""
    for(i <- array){
      str = i(0).toString
      for(j <- 1 until i.length){
        str = str + "," + i(j)
      }
      str = str + "\n"
      ranf.write(str.getBytes("utf-8"))
    }
  }

  def findBestCov(maxPower:Int) = {
    val t0 = System.currentTimeMillis()
    val numberOfParticles = 700
    val particles = new ParArray[Particle](numberOfParticles)
    print("Particles has been created!")
    for(i <- 0 until numberOfParticles){
//      maxPower should be used in here
      particles(i) = new Particle(new Array[Int](rawDataDimensions), i % 10)
    }
    val completelyParallel = false
    if(completelyParallel){
      val totalTimes = 1000
      val t1 = System.currentTimeMillis()
      particles(0).processIndicator = true
      particles.map(_.movRepeatedly(totalTimes))
      print(totalTimes + "movements finished in " + (System.currentTimeMillis() - t1) / 1000 + "\n")
    }else{
      for(i <- 0 until 100){
        val t1 = System.currentTimeMillis()
        particles.map(_.mov())
        print("epoch" + i + "finished in " + (System.currentTimeMillis() - t1) / 1000 + "\n")
      }
    }
    var Cov = List[(Double,Array[Int])]()
    for(i <- 0 until particles.length){
      Cov = (particles(i).individualBestCov, particles(i).individualBestCoefficients) +: Cov
    }
    Cov = Cov.sortWith((a,b) => a._1 > b._1)
    val fileCoe = new RandomAccessFile("./Coe", "rw")
    fileCoe.setLength(0)
    val fileCov = new RandomAccessFile("./Cov", "rw")
    fileCov.setLength(0)
    for(i <- 0 until 300){
      var covariance = Cov(i)._1
      var coeValue = ""
      for(i <- Cov(i)._2){
        coeValue = coeValue + i + ","
      }
      fileCoe.write(coeValue.getBytes("utf-8"))
      fileCoe.write("\n".getBytes("utf-8"))
      fileCov.write((covariance + "\n").getBytes("utf-8"))

    }
    print(Cov(numberOfParticles - 1)._1)
    print(Cov(Cov.length-1)._1)
    fileCoe.close()
    fileCov.close()
    val t_mov = (System.currentTimeMillis() - t0) / 1000
    print("done in " + t_mov / 3600 + "h" + (t_mov / 60) % 60 + "m" + (t_mov % 60) + "s")
  }

  def main(args:Array[String]) = {
    //changeTestData(List(Array(2, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0)), 300)
    //changeTestData(List(Array(0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0)), 300)
    //changeTestData(List(Array(0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0)), 300)
    //changeTestData(List(Array(0, 1, 0, 0, 0, 0, 0, 0, 0, 1)), 300)
    //changeTestData(List(Array(1, 0, 0, 0, 0, 0, 1, 0, 0, 0)), 300)
    //changeTestData(List(Array(0, 0, 0, 0, 0, 2, 0, 0, 0, 0)), 300)
    //changeTestData(List(Array(0, 0, 0, 0, 1, 0, 0, 0, 1, 2)), 300)
    //changeTestData(List(Array(1,1,0,1,0,0,1,0,0,0)), 300)

    readData()
    findBestCov(4)

    /*
    readData()
    var sum = 0d
    val c = Array(14, 6, 4, 0)
    val result = new Array[Double](numberOfInstances)
    for(i <- 0 until numberOfInstances){
      result(i) = sum
      sum = 0
      for(j <- 0 until dimensions){
        sum = sum + Particle.powerA(X(i)(j), c(j))
      }
    }
    for(i <- 0 until numberOfInstances){
      val str = new DecimalFormat("0.00").format(result(i).toDouble)
      println(i+" : " + str + "," + Y(i))
    }
    */
  }



}
