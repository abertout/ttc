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

package test.scala.taskgeneration

import main.scala.taskgeneration.{RandFixedSum, UUnifastDiscard, UUnifast}
import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec

class UGenerationSpec extends UnitSpec{

  def fixture = {
    new {
      val a = Task("a", 3777, 15043, 39600, 37099, None)
      val b = Task("b", 3742, 4598, 39600, 23165, None)
      val c = Task("c", 181, 1115, 39600, 13429, None)
      val d = Task("d", 1, 43, 60, 57, None)
      val e = Task("e", 38, 219, 2100, 31, None)
      val f = Task("f", 92, 641, 2100, 1181, None)
      val g = Task("g", 7, 84, 120, 100, None)
      val h = Task("h", 41, 536, 1050, 444, None)
      val i = Task("i", 1, 18, 120, 18, None)
      val j = Task("j", 5, 30, 60, 5, None)
      val k = Task("k", 77, 301, 2100, 1644, None)
      val l = Task("l", 318, 326, 2100, 940, None)
      val m = Task("m", 7, 1011, 2100, 474, None)
      val n = Task("n", 115, 208, 2100, 2024, None)
      val o = Task("o", 61, 1720, 2100, 707, None)
      val p = Task("p", 1, 118, 120, 103, None)
      val q = Task("q", 1756, 37033, 39600, 38364, None)
      val r = Task("r", 995, 16400, 39600, 27157, None)
      val s = Task("s", 0, 32, 60, 37, None)
      val t = Task("t", 711, 1327, 39600, 26565, None)

      val taskSet = TaskSet(set = Seq(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
    }
  }


  "UUnifast method" should "generate unbiased task set utilization factors" in {

    val nTasks = 6
    val uFactor: Double = 0.90
    val u: Vector[Double] = UUnifast(nTasks, uFactor)
    val sumValid = u.sum >= 0.89 && u.sum <= 0.91
    sumValid shouldEqual true
    a [IllegalArgumentException] should be thrownBy UUnifast(6, -0.1)
    a [IllegalArgumentException] should be thrownBy UUnifast(0, 0.5)
  }

  "UUnifastDiscard method" should "generate unbiased task set utilization factors" in {
    val nTasks = 6
    val uFactor: Double = 3.90
    val u: Vector[Double] = UUnifastDiscard(nTasks, uFactor)
    val sumValid = u.sum >= 3.89 && u.sum <= 3.91
    sumValid shouldEqual true
    u.exists(ui => ui > 1.0) shouldEqual false
    a [IllegalArgumentException] should be thrownBy UUnifastDiscard(0, 1.01)
    a [IllegalArgumentException] should be thrownBy UUnifastDiscard(2, 3)
  }

  "RandFixedSum method" should "generate unbiased task set utilization factors" in {
    val nTasks = 6
    val uFactor: Double = 3.90
    val u: Vector[Double] = RandFixedSum(nTasks, uFactor)
    val sumValid = u.sum >= 3.89 && u.sum <= 3.91
    sumValid shouldEqual true
    u.exists(ui => ui > 1.0) shouldEqual false
    a [IllegalArgumentException] should be thrownBy RandFixedSum(0, 1.01)
    a [IllegalArgumentException] should be thrownBy RandFixedSum(2, 3)
  }




}
