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

import main.scala.utils.Numbers

import scala.util.Random


trait TGeneration extends ((Int,Int) => Vector[Double]){

  val matrix = Array(
    Array(1, 2, 2, 4, 4, 4, 8),
    Array(1, 3, 3, 3, 3, 9, 9),
    Array(1, 5, 5, 5, 5),
    Array(1, 1, 7, 7, 7),
    Array(1, 1, 1, 1, 11)
  )


  /**
    * Based on Goossens, J., & Macq, C. (2001). Limitation of the hyper-period in real-time periodic task set generation.
    * In Proceedings of the RTS Embedded System (RTS’01).
    *
    * @return a limited period
    */
  def limitedHPPeriod: Double = {
    matrix.indices.foldLeft(1){
      case(acc,line) => acc * matrix(line)(Random.nextInt(matrix(line).length))
    }
  }


}

object LimitedHPDistinctPeriods extends TGeneration {

  def apply(nTask: Int, nDiffPeriods: Int): Vector[Double] = {

    def distinctPeriods(n: Int, seq: Seq[Double]): Seq[Double] = {
      if (seq.size == n) return seq
      val limitedP = limitedHPPeriod
      if (!seq.contains(limitedP)) return distinctPeriods(n, seq.:+(limitedP))
      distinctPeriods(n, seq)
    }

    val dispPeriods = distinctPeriods(nDiffPeriods, Seq.empty)
    val occurences = Numbers.partitionsWithoutZero(dispPeriods.size, nTask)

    val periods = (dispPeriods zip occurences).flatMap {
      case (p, nb) => List.fill(nb)(p)
    }.toVector

    Random.shuffle(periods)
  }
}

object UniformDistinctPeriods extends TGeneration {

    def apply(nTask: Int, nDiffPeriods: Int): Vector[Double] = {

      val dispPeriods = rndUniform(10, 1000, nDiffPeriods)
      val occurences = Numbers.partitionsWithoutZero(dispPeriods.size, nTask)
      val periods = (dispPeriods zip occurences).flatMap{
        case (p, nb) =>  List.fill(nb)(p)
      }
      Random.shuffle(periods)
    }

    /**
      * Draw samples from a uniform distribution.
      * Samples are uniformly distributed over the half-open interval [low, high) .
      * Identical to python method random.uniform from numpy library
      *
      * @param low lower bound
      * @param high upper bound
      * @param size number of samples
      * @return a uniform distribution
      */
    def rndUniform(low: Int, high: Int, size: Int): Vector[Double] =
      Vector.fill(size)(low + Random.nextInt(high))

  }
