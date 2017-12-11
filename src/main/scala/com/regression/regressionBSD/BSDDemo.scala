package com.regression.regressionBSD

import com.coefficientPSO._
import java.time
/**
  * Created by Dabney on 4/21/2017.
  */
class BSDDemo {

}
object BSDDemo{
  var featuresMatrix: List[List[Double]] = List(List(1, 1), List(2, 2), List(5, 5), List(4, 4))//rawData
  var labelMatrix: List[Double] = List(1, 2, 5, 4)//真实值向量
  var theta: List[Double] = List(0, 0)  //参数 theta 的初始值
  var loss: Double = 10.0
  val alpha = 0.0001 //学习率
  var numberOfInstances = 0
  var dimensions = 0

  def readData()={
    Coefficient.readData()
    //Iris
    //Coefficient.addNewX(Array(4, 1, 4, 1))
    //Coefficient.addNewX(Array(0, 4, 1, 0))
    //Coefficient.addNewX(Array(1, 3, 1, 0))
    //letters
//    Coefficient.addNewX(Array(2, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0))
//    Coefficient.addNewX(Array(0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0))
//    Coefficient.addNewX(Array(0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0))
//    Coefficient.addNewX(Array(0,1,0,0,0,0,0,0,0,1))
//    Coefficient.addNewX(Array(1,0,0,0,0,0,1,0,0,0))
//    Coefficient.addNewX(Array(0,0,0,0,0,2,0,0,0,0))
//    Coefficient.addNewX(Array(0, 0, 0, 0, 1, 0, 0, 0, 1, 2))
//    Coefficient.addNewX(Array(1,1,0,1,0,0,1,0,0,0))
//    Coefficient.persistList(Coefficient.X, ".\\src\\main\\resources\\normedData")


    numberOfInstances = Coefficient.numberOfInstances
    dimensions = Coefficient.X(0).length

    featuresMatrix = List[List[Double]]()
    for(i <- Coefficient.X){
      featuresMatrix = i.toList +: featuresMatrix
    }
    featuresMatrix = featuresMatrix.reverse

    labelMatrix = Coefficient.Y.toList
    theta =  List.fill(dimensions + 1)(0.5)

  }
  def findCoefficientInBSD():Unit={
    readData()
    var duration = 0
    for (i <- 0 until 3000){ //迭代次数  - until: not including end
      for(temp <- 0 until 15){
        var error = 0.0 //第j个样本的预测误差：  labelMatrix(j) - h(j)
        var j = i % numberOfInstances
        var h_j = 0.0  //第j个样本的预测值：h(j)

        //calculate the hypothesis(i) and the result is ：[Double]
        for (k <- 0 until dimensions) { // <dimensions> is the number of features in erery raw point
         h_j += featuresMatrix(j)(k) * theta(k)
        }
        //constant item
        h_j = h_j + theta(dimensions)


        error = labelMatrix(j) - h_j //计算给出的测试数据集中类标签与计算的类标签的误差值

        var cacheTheta: List[Double] = List()

        //更新权重向量  - 运算结果是一个theta向量：[List[Double]]
        for (k <- 0 until dimensions ) {
          val updatedTheta = theta(k) + alpha * error * featuresMatrix(j)(k)
          cacheTheta = updatedTheta +: cacheTheta
        }

        cacheTheta =   cacheTheta :+ (theta(dimensions) + theta(dimensions) * error * alpha)
//        PRINT: coefficient
//        cacheTheta.foreach(t => print(t + ","))
//        print("\n")
        theta = cacheTheta

      //update loss function's value
      var SumLoss: Double = 0

      for (j <- 0 until 4) {
        var current_h = 0.0
        for (k <- 0 until dimensions) { current_h += featuresMatrix(j)(k) * theta(k) }  //计算第j个样本点的特征加权和，即预测值h(j)
        SumLoss += (current_h - labelMatrix(j)) * (current_h - labelMatrix(j))
      }
      var oldloss = loss
      loss = SumLoss / dimensions
      var change = oldloss - loss
      if(change<0)  change = -change
      if(change < 0.005 * loss){
        duration += 1
        if(duration > 100){
          return Unit
        }
      }
      println("loss->>>>" + loss + " ,i->>>>>" + i + "\n")
    }
  }
  }
  def main(args: Array[String]): Unit = {
    val t0 = System.currentTimeMillis()
    findCoefficientInBSD()
      print((System.currentTimeMillis() - t0) / 1000)
  }
}