/*
 * Copyright (c) CNRS - CRIStAL Laboratory - Emeraude Team
 * contributor: Antoine Bertout (2012-2015)
 * Copyright (c) Antoine Bertout (2015-2016)
 *
 * bertout.antoine@gmail.com
 *
 * This software is a computer program whose purpose is to minimize the
 * number of tasks of a real-time system by clustering.
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 */

package main.scala.taskgeneration


import main.scala.taskmodel.{Task, TaskSet}
import main.scala.utils.Numbers
import taskgeneration.TGeneration

import scala.annotation.tailrec
import scala.util.Random


object TaskSetGenerator {

  val derivPercentage = 0.05d /* Tolerated percentage of variation from the target utilizzation factor */
  val discardLimit = 1000

  def genTaskSet(nTasks: Int, uFactor: Double, dMin: Double, dMax: Double,
                 asynchronous: Boolean, periodGen: TGeneration, nDiffPeriods: Int,
                 uFactorGen: UGeneration): TaskSet = {

    val tFactor = nTasks
    val uValues = uFactorGen(nTasks, uFactor)
    val periods = periodGen(nTasks, nDiffPeriods)

    val set: Seq[Task] =
      for{
        i <- 0 until nTasks
        name = genName(i)
        t = (periods(i) * tFactor).toInt /* factor inflates the periods to avoid too much execution times equal to zero due to integer rounding */
        c = (uValues(i) * t).toInt
        d = ((t - c) * (dMin + (dMax - dMin) * Random.nextDouble())).toInt + c
        oMin = 0
        oMax = t
        o = if(asynchronous) (oMin + (oMax - oMin) * Random.nextDouble()).toInt else 0
      }yield Task(name, c, d, t, o)

    val generatedTaskSet =  TaskSet(set)
    if(generatedTaskSet.uFactor < uFactor - derivPercentage && generatedTaskSet.uFactor > uFactor + derivPercentage){
      maxAttempts(genTaskSet(nTasks, uFactor, dMin, dMax, asynchronous, periodGen, nDiffPeriods, uFactorGen), discardLimit)
    }
    else generatedTaskSet
  }


  /**
    * Generate a string from a number on base 26(ex:"a"=0,"bb"= 27)
    *
    * @param index idx
    * @return
    */
  private def genName(index: Int): String = {
    val baseConverted = Numbers.baseConv(index, 26)
    baseConverted.map(nb => ('a' + nb).toChar).mkString
  }

  /**
    *
    * @param f a function of type A
    * @param max maximum number of attemps allowed
    * @tparam A any type
    * @return
    */
  def maxAttempts[A](f: A, max: Int): A = recMaxAttempts(f, max, 0)

  @tailrec
  private def recMaxAttempts[A](f: A, max: Int, count: Int): A = {
    if(count == max) throw new Exception("Discard limit exceeded")
    recMaxAttempts(f, max, count + 1)
  }


}
