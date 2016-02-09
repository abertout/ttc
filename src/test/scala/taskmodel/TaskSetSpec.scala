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

package test.scala.taskmodel

import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec


class TaskSetSpec extends UnitSpec{

  def fixture =
    new {
      val tauA = Task("a", 1, 5, 50, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 1, 10, 50, 0)
      val tauE = Task("e", 3, 12, 50, 0)
      val tauF = Task("f", 3, 9, 30, 0)
      val tauG = Task("g", 2, 16, 30, 5)
      val tauH = Task("h", 1, 13, 30, 0)
      val tauI = Task("i", 4, 20, 40, 0)

      val dep = Map(
        tauA -> Set(tauB, tauE),
        tauG -> Set(tauE),
        tauD -> Set(tauF, tauI)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH))
      val taskSetDep = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    }

  def fixture2 =
    new {
      val tauA = Task("a", 1, 5, 50, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 1, 10, 50, 0)
      val tauE = Task("e", 3, 12, 50, 0)
      val tauF = Task("f", 3, 9, 30, 0)
      val tauG = Task("g", 2, 16, 30, 5)
      val tauH = Task("h", 1, 13, 30, 0)
      val tauI = Task("i", 4, 20, 40, 0)

      val dep = Map(
        tauD -> Set(tauF, tauI),
        tauA -> Set(tauB, tauE),
        tauG -> Set(tauE)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH))
      val taskSetDep = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    }

  def fixture3 =
    new {
      val tauA = Task("a", 1, 5, 50, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 1, 10, 50, 0)
      val tauE = Task("e", 3, 12, 50, 0)
      val tauF = Task("f", 3, 9, 30, 0)
      val tauG = Task("g", 2, 16, 30, 5)
      val tauH = Task("h", 1, 12, 30, 0)
      val tauI = Task("i", 4, 20, 40, 0)

      val dep = Map(
        tauA -> Set(tauB, tauE, tauC),
        tauG -> Set(tauE),
        tauD -> Set(tauF, tauI)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH))
      val taskSetDep = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    }

  def fixture4 =
    new {

      val encodedTauA = Task("a", 1, 3, 50, 0)
      val encodedTauB = Task("b", 2, 7, 50, 5)
      val encodedTauC = Task("c", 2, 5, 50, 1)
      val encodedTauD = Task("d", 1, 9, 50, 7)
      val encodedTauE = Task("e", 3, 12, 50, 8)
      val encodedTauF = Task("f", 3, 9, 30, 0)
      val encodedTauG = Task("g", 2, 12, 30, 5)
      val encodedTauH = Task("h", 1, 13, 30, 7)
      val encodedTauI = Task("i", 4, 20, 40, 0)


      val encTaskSet = TaskSet(set = Seq(encodedTauA, encodedTauB, encodedTauC, encodedTauD, encodedTauE, encodedTauF, encodedTauG, encodedTauH, encodedTauI), Some(fixture.dep))

    }

  def fixture5 =
    new {
      val tauA = Task("a", 1, 5, 50, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 1, 10, 50, 0)
      val tauF = Task("f", 3, 9, 30, 0)
      val tauG = Task("g", 2, 16, 30, 5)
      val tauH = Task("h", 1, 13, 30, 0)

      val dep = Map(
        tauA -> Set(tauB),
        tauD -> Set(tauF)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauA, tauD, tauF, tauG, tauH), Some(dep))

    }


  def fixture6 =
    new {
      val tauA = Task("a", 7, 30, 30, 0)
      val tauB = Task("b", 4, 40, 50, 0)
      val tauC = Task("c", 5, 25, 30, 0)
      val tauD = Task("d", 7, 40, 50, 0)
      val tauE = Task("e", 4, 50, 50, 0)
      val tauF = Task("f", 6, 43, 50, 0)
      val tauG = Task("g", 7, 28, 30, 0)
      val tauH = Task("h", 4, 50, 50, 0)
      val tauI = Task("i", 3, 30, 50, 0)
      val tauJ = Task("j", 1, 20, 20, 0)
      val tauK = Task("k", 4, 18, 20, 0)
      val tauL = Task("l", 4, 15, 20, 0)


      val dep = Map(
        tauA  -> Set(tauB),
        tauB  -> Set(tauC),
        tauD -> Set(tauI, tauG),
        tauG -> Set(tauH),
        tauJ -> Set(tauK, tauL)
      )

      val dep2 = Map(
        tauA  -> Set(tauB),
        tauB  -> Set(tauC),
        tauD -> Set(tauG),
        tauG -> Set(tauH)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH, tauJ, tauK, tauL), Some(dep))
      val taskSet2 = TaskSet(set = Seq(tauB, tauC, tauA, tauD, tauG, tauH), Some(dep2))

    }




  "A TaskSet" should "give the right utilization factor" in {
    val f = fixture
    (math.abs(f.taskSet.uFactor - 0.48d) <= 0.001d) should be (true)
  }

  it should "be equal to an other with the same set of tasks and preds" in {
    val f = fixture
    val f2 = fixture2
    val f3 = fixture3
    val f4 = fixture4

    f.taskSet should not equal f.taskSetDep
    f.taskSetDep shouldEqual f2.taskSetDep
    f.taskSetDep should not equal f3.taskSetDep
    f.taskSet shouldEqual f2.taskSet
    f.taskSetDep shouldEqual f4.encTaskSet
  }

  it should "not be empty" in {
    val set: Seq[Task] = Seq.empty
    a [IllegalArgumentException] should be thrownBy TaskSet(set)
  }

  it should "contains a duplicate" in {
    val tau1 = Task("a", 1, 5, 10)
    val tau2 = Task("a", 1, 5, 10)
    val set = Seq(tau1, tau2)
    a [IllegalArgumentException] should be thrownBy TaskSet(set)
  }


  it should "be correctly compared on content" in {
    val f = fixture
    val f2 = fixture2
    val f4 = fixture4
    val eqContent = f.taskSetDep equalContent f4.encTaskSet
    eqContent shouldEqual false
    val eqContent2 = f.taskSetDep equalContent f2.taskSetDep
    eqContent2 shouldEqual true
  }


  it should "be correctly printed" in {
    val f = fixture
    val s = "TaskSet[set((b(2, 7, 50, 5, -1), c(2, 5, 50, 0, -1), i(4, 20, 40, 0, -1), a(1, 5, 50, 0, -1), d(1, 10, 50, 0, -1), f(3, 9, 30, 0, -1), e(3, 12, 50, 0, -1), g(2, 16, 30, 5, -1), h(1, 13, 30, 0, -1))), Dep(a -> (b,e), g -> (e), d -> (f,i))]"
    f.taskSetDep.toString shouldEqual s
    val s2 = "TaskSet[set((b(2, 7, 50, 5, -1), c(2, 5, 50, 0, -1), i(4, 20, 40, 0, -1), a(1, 5, 50, 0, -1), d(1, 10, 50, 0, -1), f(3, 9, 30, 0, -1), e(3, 12, 50, 0, -1), g(2, 16, 30, 5, -1), h(1, 13, 30, 0, -1))), Dep()]"
    f.taskSet.toString shouldEqual s2
  }



  it should "count the right number of different periods" in {fixture.taskSet.nPeriods should be(3)}

  it should "compute the right LCM of periods" in {fixture.taskSet.periodsLCM should be(600)}

  it should "have a size that correponds to its number of tasks" in {
    val f = fixture
    f.taskSet.size shouldEqual 9
    f.taskSet.size shouldEqual f.taskSet.set.length
  }

  it should "have the adjacency map correctly initialized" in {
    val f = fixture
    val initAdjMap = PrivateMethod[(Map[Task, Int],Map[Int, Task])]('initAdjMap)
    val adjMaps: (Map[Task, Int],Map[Int, Task]) =  f.taskSet invokePrivate initAdjMap()
    adjMaps._1 shouldEqual adjMaps._2.map(_.swap)
    adjMaps._1.values.toSet shouldEqual Set(0, 1, 2, 3, 4, 5, 6, 7, 8)
    adjMaps._1.keySet shouldEqual f.taskSet.set.toSet

  }

  it should "have the adjacency matrix properly initialized" in {
    val f = fixture
    val initAdjMatrix = PrivateMethod[Vector[Vector[Int]]]('initAdjMatrix)
    val adjMatrix: Vector[Vector[Int]] =  f.taskSetDep invokePrivate initAdjMatrix(Some(f.dep))
    val expectedMatrix = Vector(
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 0, 0, 1, 0, 0),
      Vector(0, 0, 1, 0, 0, 1, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 1, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    expectedMatrix shouldEqual adjMatrix
  }


  it should "links tasks correctly" in {
    val f = fixture
    f.taskSetDep.directPreds(f.tauE).toSet shouldEqual Set(f.tauA, f.tauG)
    f.taskSetDep.directSuccs(f.tauA).toSet shouldEqual Set(f.tauB, f.tauE)
    f.taskSetDep.directSuccs(f.tauF).toSet shouldEqual Set.empty
    f.taskSetDep.directPreds(f.tauA).toSet shouldEqual Set.empty
  }


  it should "give a correct direct precedence constraint relation between a predecessor and a successor" in {
    val f = fixture
    f.taskSetDep.directPredRelation(f.tauA, f.tauC) shouldEqual false
    f.taskSetDep.directPredRelation(f.tauA, f.tauB) shouldEqual true
    f.taskSetDep.directPredRelation(f.tauB, f.tauA) shouldEqual false
    f.taskSetDep.directPredRelation(f.tauG, f.tauB) shouldEqual false
  }

  it should "give a correct direct precedence constraint relation between a successor and a predecessor" in {
    val f = fixture
    f.taskSetDep.directSuccRelation(f.tauB, f.tauA) shouldEqual true
    f.taskSetDep.directSuccRelation(f.tauA, f.tauB) shouldEqual false
    f.taskSetDep.directSuccRelation(f.tauA, f.tauC) shouldEqual false
    f.taskSetDep.directSuccRelation(f.tauG, f.tauB) shouldEqual false
  }

  it should "give the tasks without successor" in{
    val f = fixture
    f.taskSetDep.tasksWithoutSucc() shouldEqual Set(f.tauB, f.tauC, f.tauE, f.tauF, f.tauH, f.tauI)
  }

  it should "remove tasks and related dependencies" in {
    val f1 = fixture
    val f5 = fixture5
    val taskSet = f1.taskSetDep
    taskSet.remove(f1.tauE, f1.tauI) shouldEqual f5.taskSet
  }

  it should "restrict to the given tasks and related dependencies" in {
    val f1 = fixture
    val f5 = fixture5
    val taskSet = f1.taskSetDep
    taskSet.restrictedTo(f1.tauA, f1.tauB, f1.tauC, f1.tauD, f1.tauF, f1.tauG, f1.tauH) shouldEqual f5.taskSet
    val f6 = fixture6
    f6.taskSet.restrictedTo(f6.tauA, f6.tauB, f6.tauC, f6.tauD, f6.tauG, f6.tauH) shouldEqual f6.taskSet2

  }

  it should "have a correct seq of absolute deadlines" in{
    val f = fixture
    f.taskSet.absDeadlines(100) shouldEqual Seq(5, 9, 10, 12, 13, 20, 21, 39, 43, 51, 55, 60, 62, 69, 73, 81, 99, 100)
  }


}
