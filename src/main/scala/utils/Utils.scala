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

package main.scala.utils

import breeze.linalg.DenseVector
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math.BigInt
import scala.reflect.ClassTag
import scala.util.Random




object Numbers {

  def lcm(x:BigInt, y:BigInt): BigInt = {
    var tmp: BigInt = x
    while (tmp % y != 0)
      tmp += x
    tmp
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def getLCM(args: BigInt*) = args.reduce((a,b) => lcm(a,b))

  def baseConv(nb: Int, base: Int) = baseConvRec(nb, base, Seq.empty[Int])

  @tailrec
  private def baseConvRec(nb: Int, base: Int, seq: Seq [Int]): Seq[Int] = {
    if(nb < base)return seq.+:(nb)
    val r = nb % base
    baseConvRec(nb / base, base, seq.+:(r))
  }

  /**
    * Generate a random partition from a given sum
    * @param n partition size
    * @param total given sum
    * @return
    */
  def partitions(n: Int, total: Int): Seq[Int] = {
    val ascRdmList: List[Int] = (0 :: List.tabulate(n - 1)(x => 1 + ((total - 1) * Random.nextFloat()).toInt).sorted):+total
    for(i <- 1 until ascRdmList.size)
      yield ascRdmList(i) - ascRdmList(i - 1)
  }

  /**
    * Repeat partitions method until having no zero
    * @param n number of partitions
    * @param total sum of the partitions
    * @return
    */
  def partitionsWithoutZero(n: Int, total: Int): Seq[Int] = {
    val part =  Numbers.partitions(n, total)
    if(part.contains(0)) partitionsWithoutZero(n, total)
    else part
  }
}

object MapUtils {

  def getKey[K, V](map: Map[K, V], value: V): Option[K] = {
    for ((k, v) <- map)
      if (v == value) return Some(k)
    None
  }

  /**
    * Remove from a map all occurrences of elements that are not in a seq
    * @param seq initial seq
    * @param m given map
    * @tparam A type
    * @return
    */
  def filterMap[A](seq: Seq[A], m: Map[A,Set[A]]): Map[A,Set[A]]
  = m.filterKeys(seq.contains(_)).map(kv => (kv._1, kv._2.intersect(seq.toSet)))

}

object Matrix {

  /**
    * Generate a random permutation of an array filled from 0d until n by step of 1d
    * Equivalent than python numpy library method "random.permutation"
    * @param n superior bound
    * @return
    */
  def rdmPermutation(n: Int): Vector[Int] = scala.util.Random.shuffle(0 until n toVector)

  /**
    * Convert DenseVector[Numeric] to DenseVector[Boolean] because no implicit conversion between exists
    * @param boolVector boolean DenseVector
    * @param numeric numeric DenseVector
    * @tparam A type
    * @return
    */
  def toNumericDenseVector[A: ClassTag](boolVector: DenseVector[Boolean])(implicit numeric: Numeric[A]): DenseVector[A] = {
    boolVector.map {
      case true => numeric.one
      case false => numeric.zero
    }
  }

}

trait Implicits {

  implicit class SeqImprovements[T](val s: Seq[T]){
    def containsDuplicate: Boolean = s.view.distinct.length != s.length
  }

}

object AllImplicits extends Implicits
