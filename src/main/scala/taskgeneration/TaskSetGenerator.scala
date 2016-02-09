package main.scala.taskgeneration


import main.scala.taskmodel.{Task, TaskSet}
import main.scala.utils.Numbers
import taskgeneration.{PrecedenceGeneration, SimpleErdösRényi, TGeneration}

import scala.util.Random


object TaskSetGenerator {

  val tFactor = 10

  def genTaskSet(nTasks: Int, uFactor: Double, dMin: Double, dMax: Double,
                 asynchronous: Boolean, periodGen: TGeneration, nDiffPeriods: Int,
                 uFactorGen: UGeneration): TaskSet = {

    val uValues = uFactorGen(nTasks, uFactor)
    val periods = periodGen(nTasks, nDiffPeriods)

    val set: Seq[Task] =
      for{
        i <- 0 until nTasks
        name = genName(i)
        t = (periods(i) * tFactor).toInt
        c = (uValues(i) * t).toInt
        d = ((t - c) * (dMin + (dMax - dMin) * Random.nextDouble())).toInt + c
        oMin = 0
        oMax = t
        o = if(asynchronous) (oMin + (oMax - oMin) * Random.nextDouble()).toInt else 0
      }yield Task(name, c, d, t, o)

    TaskSet(set)
  }


  /**
    * Generate a string from a number on base 26(ex:"a"=0,"bb"= 27)
    * @param index idx
    * @return
    */
  private def genName(index: Int): String = {
    val baseConverted = Numbers.baseConv(index, 26)
    baseConverted.map(nb => ('a' + nb).toChar).mkString
  }



}
