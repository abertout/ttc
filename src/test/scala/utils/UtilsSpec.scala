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

package test.scala.utils

import breeze.linalg.DenseVector
import main.scala.utils.AllImplicits.SeqImprovements
import main.scala.utils.{MapUtils, Matrix, Numbers}
import test.scala.UnitSpec


class UtilsSpec extends UnitSpec{


  def fixture =
    new {
      val myMap: Map[String, Int] = Map("platon" -> 1, "socrate" -> 2, "aristote" -> 3)
    }

  "Method containsDuplicate" should "recognize duplicates" in {
    val list = List(3,5,4,4,1)
    list.containsDuplicate shouldEqual true

  }

  it should "recognize no duplicate" in {
    val list2 = List("j","k","m","a")
    list2.containsDuplicate shouldEqual false
  }


  "Method getKey" should "find the first key if present" in {
    val f = fixture
    MapUtils.getKey(f.myMap, 1) should be(Some("platon"))
  }

  it should "returns None if the key is not present" in {
    val f = fixture
    MapUtils.getKey(f.myMap, 5) should be(None)
  }

  "2 params lcm method" should "work" in {
    Numbers.lcm(22,3) should be (66)
  }

  "2 params gcd" should "work" in {

    Numbers.gcd(35,22) should be (1)
    Numbers.gcd(35,90) should be (5)

  }

  "n params lcm method" should "work" in {
    Numbers.getLCM(66, 34, 22, 3) should be (1122)
  }

  "Method baseConv" should "convert the number in the given base" in {
    val seq = Numbers.baseConv(28, 25)
    seq shouldEqual Seq(1, 3)
  }

  "Method partitions" should "generate a random partition from a given sum" in {
    val seq = Numbers.partitions(4, 15)
    seq.size shouldEqual 4
    seq.sum shouldEqual 15
  }

  "Method partitionsWithoutZero" should "generate a random partition from a given sum" in {
    val seq = Numbers.partitionsWithoutZero(6, 15)
    seq.contains(0) shouldEqual false
  }


  "Method filterMap" should "do the job correctly" in {
    val m = Map(1 -> Set(3, 6, 9, 4),
      8 -> Set(1, 5, 9, 0),
      3 -> Set(1, 2, 7, 100),
      2 -> Set(5, 9, 2, 1, 0),
      12 -> Set(3, 45, 7, 23 ),
      5 -> Set(6, 2, 44, 7, 8),
      4 -> Set(1, 8, 4, 12 ))

    val s = Seq(1, 3, 4)

    val expMap = Map(1 -> Set(3, 4),
      4 -> Set(1, 4),
      3 -> Set(1))

    expMap shouldEqual MapUtils.filterMap(s, m)

  }

  "Method randomPermutation" should "generate random permutation of 0 to n-1" in {
    val p10 =  Matrix.rdmPermutation(10)
    p10.sum shouldEqual 45
    p10.length shouldEqual 10
    val p5 = Matrix.rdmPermutation(5)
    p5.sum shouldEqual 10
    p5.length shouldEqual 5

  }

  "Method toNumericDenseVector" should "convert a boolean DenseVector to a numeric DenseVector" in {
    val boolDenseVector = DenseVector[Boolean](true, false, true, true, true, false)
    val boolDenseVector2 = DenseVector[Boolean](false, false, true, false, true, true)
    val expIntDenseVector = DenseVector[Int](1, 0 , 1, 1, 1, 0)
    val expDoubleDenseVector = DenseVector[Double](0d, 0.0 , 1d, 0.0, 1d, 1.0)
    val intDenseVector: DenseVector[Int] = Matrix.toNumericDenseVector(boolDenseVector)
    val doubleDenseVector: DenseVector[Double] = Matrix.toNumericDenseVector(boolDenseVector2)
    expIntDenseVector shouldEqual intDenseVector
    expDoubleDenseVector shouldEqual doubleDenseVector
  }






}
