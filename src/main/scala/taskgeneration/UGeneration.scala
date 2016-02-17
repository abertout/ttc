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

import breeze.linalg.{DenseMatrix, DenseVector}
import main.scala.utils.Matrix

import scala.annotation.tailrec
import scala.util.Random

trait UGeneration extends ((Int, Double) => Vector[Double])

object UUnifast extends UGeneration{
  /**
    * Implements the UUnifast method to generate unbiased and random uniform distribution of Ui
    * @return an array with all Ui
    */
  def apply(nTasks: Int, uFactor: Double): Vector[Double] = {
    require(nTasks > 0, "number of tasks can not be equal to zero")
    require(uFactor > 0, "the utilization factor must be superior than 0")
    require(nTasks.toDouble >= uFactor, "Task set utilisation must be less than or equal to number of tasks")

    val (lastRest, partialVec) = (0 until nTasks - 1).foldLeft(uFactor, Vector.empty[Double]) {
      case ((acc, vec), idx) =>
        val nextSumU = acc * math.pow(Random.nextDouble, 1d / (nTasks - (idx + 1)))
        (nextSumU, vec.:+(acc - nextSumU))
      }
    partialVec :+ lastRest
  }

}

object UUnifastDiscard extends UGeneration{

  /**
    * Implements the UUnifast method simply adapted to multiprocessor
    * Davis, R. I., & Burns, A. (2011).
    * Improved priority assignment for global fixed priority pre-emptive scheduling in multiprocessor real-time systems.
    * Real-Time Systems, 47(1), 1-40.
    * @param nTasks number of utilization factors needed
    * @param uFactor the total utilization factor
    * @return an array of Double with nTask utilization factors
    */
  @tailrec
  def apply(nTasks: Int, uFactor: Double): Vector[Double] = {
    /* require(nTasks > 0, "number of tasks can not be equal to zero")
       require(nTasks.toDouble >= uFactor, "Task set utilisation must be less than or equal to number of tasks")

        if(nTasks == 1)
          Vector(uFactor)
    */
    val u = UUnifast(nTasks, uFactor)
    if(u.exists(_ > 1))
      UUnifastDiscard(nTasks, uFactor)
    else u



    /*val vectU: Array[Double] = new Array[Double](nTasks)
    var sumU = uFactor
    var nextSumU = 0d
    var discard: Boolean = false

    do {
      discard = false
      sumU = uFactor
      nextSumU = 0d
      for (i <- 0 until nTasks - 1) {
        nextSumU = sumU * math.pow(Random.nextDouble, 1d / (nTasks - (i + 1)))
        vectU(i) = sumU - nextSumU
        if(vectU(i) > 1)
          discard = true
        sumU = nextSumU
      }
      vectU(nTasks - 1) = sumU
      if(vectU(nTasks - 1) > 1)
        discard = true
    }while(discard)
    vectU.toVector
    */
  }
}



object RandFixedSum extends UGeneration {
  /**
    * Implementation of Stafford random fixed sum algorithm adapted specifically for the purpose of taskset generation
    * with fixed total utilisation value. Popularized in real-time community in
    * Emberson, P., Stafford, R., & Davis, R. I. (2010, July).
    * Techniques for the synthesis of multiprocessor tasksets.
    * In proceedings 1st International Workshop on Analysis Tools and Methodologies for Embedded and Real-time Systems (WATERS 2010) (pp. 6-11).
    * Require breeze library https://github.com/scalanlp/breeze
    * @param nTasks number of tasks
    * @param uFactor array of uFactors
    * @return
    */

  def apply(nTasks: Int, uFactor: Double): Vector[Double] = {
    require(nTasks > 0, "number of tasks can not be equal to zero")
    require(nTasks.toDouble >= uFactor, "Task set utilisation must be less than or equal to number of tasks")
    if(nTasks == 1)
      Vector(uFactor)

    val nSets = 1
    val k: Double = math.floor(uFactor)
    val s: Double = uFactor
    val step1: Double =  if(k < (k - nTasks + 1)) 1 else -1
    val s1 = s - DenseVector((k until k - nTasks + 1 + step1 by step1).toArray)
    val step2: Double = if((k + nTasks) < (k - nTasks + 1)) 1 else -1
    val s2 = DenseVector(((k + nTasks) until (k + 1) + step2 by step2).toArray) - s

    val tiny = Double.MinPositiveValue
    val huge = Double.MaxValue

    val w = DenseMatrix.zeros[Double](nTasks, nTasks + 1)
    w(0, 1) = huge
    val t = DenseMatrix.zeros[Double](nTasks - 1, nTasks)


    for(i <- 2 until nTasks + 1){
      val tmp1 = w(i - 2, 1 until (i + 1)) :* s1(0 until i).t / i.toDouble
      val tmp2 = w(i - 2, 0 until i) :* s2(nTasks - i until nTasks).t / i.toDouble
      w(i - 1, 1 until i + 1) := tmp1 + tmp2
      val tmp3 = w(i - 1, 1 until i + 1) + tiny
      val tmp34 = (s2(nTasks - i until nTasks) :> s1(0 until i)).toDenseVector
      val tmp4 = DenseVector.zeros[Double](tmp34.size)
      val notTmp4 = DenseVector.zeros[Double](tmp34.size)

      //Convert DenseVector[Boolean] to DenseVector[Double] because no implicit conversion between exists
      var cpt = 0
      while(cpt < tmp4.length) {
        if (tmp34(cpt)) {
          tmp4(cpt) = 1d
          notTmp4(cpt) = 0d
        }else {
          tmp4(cpt) = 0d
          notTmp4(cpt) = 1d
        }
        cpt += 1
      }

      val tmp45 = (tmp2 :/ tmp3) :* tmp4.t
      val vector1d = DenseVector.fill(tmp1.t.length)(1d)
      val tmp13 = vector1d - (tmp1 :/ tmp3).t :* notTmp4
      t(i - 2, 0 until i) := tmp45 + tmp13.t
    }

    val x = DenseMatrix.zeros[Double](nTasks, nSets)
    val rt = DenseMatrix.rand(nTasks - 1, nSets)
    val rs = DenseMatrix.rand(nTasks - 1, nSets)
    var ss = DenseVector.fill(nSets)(s)
    val j: DenseVector[Int] = DenseVector.fill(nSets, (k + 1).toInt)
    var sm = DenseVector.fill(nSets)(0d)
    var pr = DenseVector.fill(nSets)(1d)

    for(i <- (nTasks - 1) until 0 by -1){
      val e: DenseVector[Boolean] = (rt(nTasks - i - 1, ::) :<= t(i - 1, j(0) - 1)).t.toDenseVector
      val eDouble: DenseVector[Double] = Matrix.toNumericDenseVector[Double](e)
      val eInt: DenseVector[Int]= Matrix.toNumericDenseVector[Int](e)
      val sx: DenseVector[Double] = (rs(nTasks - i - 1, ::) :^ (1 / i.toDouble)).t
      val vector1d: DenseVector[Double] = DenseVector.fill(sx.length)(1d)
      sm :+= (vector1d :- sx):* pr :* ss / (i + 1).toDouble
      pr :*= sx
      x(nTasks - i - 1, ::) := (sm :+ (pr :* eDouble)).t
      ss :-= eDouble
      j :-= eInt
    }

    x(nTasks - 1, ::) := (sm :+ (pr :* ss)).t

    var i = 0
    while(i < nSets){
      val rdmPermutat = Matrix.rdmPermutation(nTasks)
      val b: DenseVector[Double] = x(::,i).slice(0, x.size)
      val permutB: Vector[Double] = rdmPermutat.map(b(_))
      x(::, i) := DenseVector(permutB.toArray)
      i += 1
    }
    x.toArray.toVector
  }
}




