package com.coefficientPSO

import java.text.DecimalFormat

import scala.util.Random

import java.time

/**
  * Created by Dabney on 4/19/2017.
  */
/**
  *
  * @param myPowerCoefficient: the number of power above X
  * @param maxPower: all the power above X 's sum in an Item
  */
class Particle(val myPowerCoefficient:Array[Int],
               val maxPower:Int) {
  val powerCoefficient = myPowerCoefficient.clone()
  val rand = new Random()
  var temp:Int = 0
  var individualPosition:Array[Int] = new Array[Int](powerCoefficient.length + 1)
  val Ymean = calculateYmean()
  var individualBestCov = 0d
  var individualBestCoefficients = new Array[Int](powerCoefficient.length)
  var movCount:Int = 0
  var processIndicator = false
  //Initializer
  for(i <- 0 until maxPower){
    val x = (Random.nextDouble() * powerCoefficient.length).toInt
    powerCoefficient(x) = powerCoefficient(x) + 1
  }

  def calculateYmean()={
    var sum = 0D
    for(i <- 0 until Coefficient.Y.length){
      sum = sum + Coefficient.Y(i)
    }
    sum / (Coefficient.Y.length + 1)
  }


  def movRepeatedly(times:Int){

    for(i <- 0 until times ){
      if(processIndicator){
        if(i % 10 == 0){
          print("now process: " + (i.toDouble/times).formatted("%.2f") + "%\n")
        }
      }
      mov()

    }

  }

//    var str = "Cov : " + new DecimalFormat("0.00").format(individualBestCov) + "\tCoefficient : "

  def mov(): Unit = {
    var speed:Int = 0
    var minuend:Int = 0
    minuend = (Random.nextDouble() * powerCoefficient.length).toInt
    while( powerCoefficient(minuend) == 0 ){
      minuend = minuend + 1
      if(minuend >= powerCoefficient.length -1 )
        return Unit
      speed = (powerCoefficient(minuend) * Random.nextDouble()).toInt
    }
    //perturbation
    if (powerCoefficient(minuend) <= 0){
        powerCoefficient(minued) = powerCoeffcient((int)Random.nextDouble() * powerCoefficient.length)
    }
    if (speed <0){
    speed = -1 * speed
    }
    if(speed == 0 ) speed = 1
    if(
    powerCoefficient(minuend) = powerCoefficient(minuend) - speed
    for(i <- 0 until speed){
      temp = (Random.nextDouble() * powerCoefficient.length).toInt
      powerCoefficient(temp) = powerCoefficient(temp) + 1
    }
    val cov = calculateCov()

    if(cov > individualBestCov){
      individualBestCov = cov
      for(i <- powerCoefficient.indices){
         individualBestCoefficients(i) = powerCoefficient(i)
      }
    }
    Unit
  }

  //calculate X^i * X^j * X^k * etc...
  def calculateNewXValue(x:Array[Double])={
    var temp = 1d
    var powerItem = 1d
    for(i <- 0 until powerCoefficient.length){
      powerItem = Particle.powerA(x(i), powerCoefficient(i))
      temp = temp * powerItem
    }
    temp
  }

  def calculateCov()={
    var cov = 0D
    var newXValue = new Array[Double](Coefficient.numberOfInstances)
    for(i <- 0 until Coefficient.numberOfInstances) {
      newXValue(i) = calculateNewXValue(Coefficient.X(i))
    }
    Coefficient.normalize(newXValue)
    newXValue = Coefficient.normalize(newXValue)
    var sum = 0d
    for(i <- 0 until newXValue.length){
      sum += newXValue(i)
    }
    val Xmean = sum / Coefficient.numberOfInstances
    for(i <- 0 until Coefficient.numberOfInstances) {
      cov = cov + (newXValue(i) -Coefficient.Y(i))
      //cov = cov + (newXValue(i) - Xmean) * (Coefficient.Y(i) - Ymean) * 100 //magnify every cov to 100 times
    }

//    if(cov < 0){
//      cov = cov * (-1)
//      cov / Coefficient.numberOfInstances
//    }else{
//      cov / Coefficient.numberOfInstances
//    }
    100000-cov
  }

}
object Particle{
  //calculate x-th power of a
  def powerA(a:Double,x:Int):Double = {
    var t = 1D
    if(x == 0) {
      return 1d}
    for(i <- 0 until x){
      t = t * a * 100
    }
    t
  }
  def main(args:Array[String])= {



  }
}
