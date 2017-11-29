package com.regression.regressionPSO

/**
  * Created by Dabney on 4/10/2017.
  */
/**
  *
  * @param w : inertia weight
  * @param X : current position
  * @param V : current velocity
  * @param Pid : BEST position found by individual
  */
class Particle(var w:Double,
               val X:Array[Double],
               val V:Array[Double],
               val Pid:Array[Double],
               val Vid:Array[Double],
               val c1:Double, val c2:Double ){

  var currentLoss = Double.MaxValue
  var PidLoss = Double.MaxValue
  val rand = Regression.rand

  def move() = {
    for(i <- X.indices ){
      X(i) = X(i) + V(i)
      V(i) = w * V(i) + c1 * (Pid(i) - X(i)) * rand.nextDouble() + c2 * (Regression.Pgd(i) - X(i)) * rand.nextDouble()
      w = w*0.95
    }

    currentLoss = caculateLoss()

    compareAndChangePd()

  }

  def outPut(Attr:Array[Double])={
    var out = 0d
    for(i <- 0 until X.length - 1){
      out = out + X(i) * Attr(i+1)    // calibration for rawData because training Set start from subscript 1
    }
    out + X(X.length-1)               // the last number of X is zero power item coefficient b
  }

  def caculateLoss() = {
    var loss = 0d
    for(i <- 0 until Regression.instanceNumber){
      val er = outPut(Regression.rawData(i)) - Regression.Y(i)
      loss = loss + er * er
    }
    loss
  }

  def compareAndChangePd() ={
    if(currentLoss < PidLoss){
      for(i <- X.indices){
        Pid(i) = X(i)
      }
      PidLoss = currentLoss
    }
    if(currentLoss < Regression.PgdLoss) {
      for (i <- Regression.Pgd.indices) {
        Regression.Pgd(i) = X(i)
      }
      Regression.PgdLoss = currentLoss
    }
  }

}