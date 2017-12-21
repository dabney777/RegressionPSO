package com.regression.regressionPSO

import java.io._

import scala.collection.parallel.mutable.ParArray
import scala.util.Random

/**
  * Created by Dabney on 4/4/2017.
  */
class Regression{
}
object  Regression{
  val k = 386
  val instanceNumber = 15000  //All: 53499
  var Pgd = new Array[Double](k-1)
  var PgdLoss = Double.MaxValue
  val Y = new Array[Double](instanceNumber) // Regression labeled value
  var rawData = List[Array[Double]]()     // subscript from 1 ~ 384 is Training data
  val rand = new Random()


  def NDimensionArrayCreator(n:Int) = {

    val a = new Array[Double](n)
    for(i <- 0 until n){
      a(i) = rand.nextDouble()*2 - 1
    }
    a
  }

  def main(args:Array[String]) = {


    val file = new File("./src/main/resources/slice_localization_data.csv")
    val r = new BufferedReader(new InputStreamReader(new FileInputStream(file)))

    var data = new Array[Double](k+1)
    var line = r.readLine()
    for(i <- 0 until instanceNumber){
      line = r.readLine()
      val temp = line.split(',').map(_.toDouble)
      Y(i) = temp(k - 1)
      rawData = List(temp) ::: rawData// subscript from 1 ~ 384 is Training data
    }


    for(w <- Range(4,20,2)){
     for(i <- Range(2,30,2)) {
        var beg = System.currentTimeMillis()
        Regression.PgdLoss = Double.MaxValue
        val iterationNumber = 50
        val numberOfParticles = 300
        val Particles = new ParArray[Particle](numberOfParticles)
        for (i <- 0 until numberOfParticles) {

          Particles(i) = new Particle(w, NDimensionArrayCreator(385), NDimensionArrayCreator(385), NDimensionArrayCreator(385), NDimensionArrayCreator(385), i/10d, i/10d)

        }
        println("Particles have been created in " + System.currentTimeMillis()/1000)
        for (i <- 0 until iterationNumber) {
          Particles.par.foreach(_.move())
          println("I am running in" + System.currentTimeMillis()/1000)
        }
        println("This parameters has been caculted in " + System.currentTimeMillis()/1000)
        val usedTime = "Used Time : " + ((System.currentTimeMillis() - beg) /1000).toInt
        val LValue = "MinLoss: " + PgdLoss.toInt

        val ran = new RandomAccessFile("./src/main/resources/Result","rws")
        ran.seek(ran.length())
        ran.write((usedTime + "\n" + LValue + "\nw: " + w + "\nc1 = c2 = " + i/10d + "\n\n").getBytes())
        ran.close()

      }

    }

  }
}

