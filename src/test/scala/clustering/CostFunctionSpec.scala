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

package ttc.clustering

import ttc.clustering._
import ttc.scheduling.EDFresponseTimeAnalysisGuan
import ttc.taskmodel.{TaskSet, Task}
import ttc.UnitSpec


class CostFunctionSpec extends UnitSpec{

  def fixture =
    new {

      val a = Task("a", 1,19,40)
      val b = Task("b", 3,34,50)
      val c = Task("c", 4,33,33)
      val d = Task("d", 11,90,100)
      val e = Task("e", 1,9,10)

      val taskSet = TaskSet(Seq(a,b,c,d,e))

      val a2 = Task("a", 1,33,50)
      val b2 = Task("b", 2,44,44)
      val c2 = Task("c", 40,100,105)
      val d2 = Task("d", 2,12,15)
      val e2 = Task("e", 2,10,40)

      val taskSet2 = TaskSet(Seq(a2,b2,c2,d2,e2))

      val a3 = Task("a", 1,33,55)
      val b3 = Task("b", 2,35,40)
      val c3 = Task("c", 5,50,55)
      val d3 = Task("d", 2,50,75)
      val e3 = Task("e", 10,60,65)

      val taskSet3 = TaskSet(Seq(a3,b3,c3,d3,e3))

      val a4 = Task("a", 1,39,40)
      val b4 = Task("b", 3,34,34)
      val c4 = Task("c", 4,39,33)
      val d4 = Task("d", 20,90,100)
      val e4 = Task("e", 1,30,30)

      val taskSet4 = TaskSet(Seq(a4,b4,c4,d4,e4))

      val allTaskSet = Seq(taskSet, taskSet2, taskSet3, taskSet4)

    }

  "An AvgResponseTime cost function" should "select the taskset with the minimal average response time" in {
    val f = fixture
    val taskSet = EDFresponseTimeAnalysisGuan(f.taskSet)._2
    val allTaskSetWithR = f.allTaskSet.map(EDFresponseTimeAnalysisGuan(_)._2)
    AvgResponseTime(allTaskSetWithR).equalContent(taskSet) shouldEqual true
  }


  "An MaxDensity cost function" should "select the taskset with the maximal density" in {
    val f = fixture
    MaxDensity(f.allTaskSet).equalContent(f.taskSet2) shouldBe true
  }

  "An MinDensity cost function" should "select the taskset with the minimal density" in {
    val f = fixture
    MinDensity(f.allTaskSet).equalContent(f.taskSet3) shouldBe true
  }

  "An random cost function" should "select the taskset randomly" in {
    val f = fixture
    val rdmTaskSet = Random(f.allTaskSet)
    f.allTaskSet.exists(taskSet => taskSet.equalContent(rdmTaskSet)) shouldBe true
  }

  "An ResponseTimeDensity cost function" should "select the taskset with the minimal density computed on the response time" in {
    val f = fixture
    val taskSet = EDFresponseTimeAnalysisGuan(f.taskSet3)._2
    val allTaskSetWithR = f.allTaskSet.map(EDFresponseTimeAnalysisGuan(_)._2)
    ResponseTimeDensity(allTaskSetWithR).equalContent(taskSet) shouldEqual true
  }

}
