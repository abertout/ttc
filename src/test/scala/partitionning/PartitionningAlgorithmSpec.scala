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

package test.scala.partitionning

import main.scala.scheduling.EDFqPA
import main.scala.taskmodel.{TaskSet, Task}
import test.scala.UnitSpec
import main.scala.partitionning.{ButtazoHeuristicH2, GlobalHeuristic, ButtazoHeuristicH1, PartitionningAlgorithm}

class PartitionningAlgorithmSpec extends UnitSpec{


  def fixture =
    new {
      val tauA = Task("a", 6, 50, 50, 0)
      val tauB = Task("b", 3, 50, 50, 0)
      val tauC = Task("c", 4, 50, 50, 0)
      val tauD = Task("d", 3, 50, 50, 0)
      val tauE = Task("e", 2, 50, 50, 0)
      val tauF = Task("f", 1, 50, 50, 0)
      val tauG = Task("g", 7, 50, 50, 5)
      val tauH = Task("h", 1, 50, 50, 0)
      val tauI = Task("i", 1, 50, 50, 0)

      val dep = Map(
        tauC  -> Set(tauD, tauF),
        tauD  -> Set(tauE, tauI),
        tauG -> Set(tauH)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    }

  def fixture2 =
    new {
      val tauA = Task("a", 6, 40, 50, 0)
      val tauB = Task("b", 3, 50, 50, 0)
      val tauC = Task("c", 4, 50, 50, 0)
      val tauD = Task("d", 3, 50, 50, 0)
      val tauE = Task("e", 2, 30, 50, 0)
      val tauF = Task("f", 1, 40, 50, 0)
      val tauG = Task("g", 7, 50, 50, 5)
      val tauH = Task("h", 1, 50, 50, 0)
      val tauI = Task("i", 1, 45, 50, 0)

      val dep = Map(
        tauC  -> Set(tauD, tauF),
        tauD  -> Set(tauE, tauI),
        tauG -> Set(tauH)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    }

  def fixture3 =
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

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH, tauJ, tauK, tauL), Some(dep))

    }

  "A partitionning method" should "should be able to detect the critical path" in {
    val f = fixture
    val criticalPathMethod = PrivateMethod[Seq[Task]]('criticalPath)
    val criticalPath: Seq[Task] = PartitionningAlgorithm invokePrivate criticalPathMethod(f.taskSet)
    criticalPath shouldEqual Seq(f.tauC, f.tauD, f.tauE)
  }

  it should "compute the utilization factor of a set of tasks correctly" in {
    val f = fixture
    ButtazoHeuristicH1.uFactor(f.taskSet.set) shouldEqual 0.56d
  }

  "The lower bound on the number of flows" should "should be correct" in {
    val f = fixture
    val lowerBound = ButtazoHeuristicH1.lowerBoundOnFlows(f.taskSet)
    lowerBound shouldEqual 1
  }

  "The particular density" should "should be correct" in {
    val f = fixture2
    val particularDensityMethod = PrivateMethod[Double]('particularDensity)
    val particularDensity: Double = ButtazoHeuristicH1  invokePrivate particularDensityMethod(f.taskSet)
    val inRange = particularDensity <= 0.594d && particularDensity >= 0.593d
    inRange shouldEqual true
  }

  "The fitElseNew method" should "should be correct" in {
    ???
  }

  "The ButtazoHeuristicH1 partitionning heuristic" should "work correctly" in {
    val f = fixture
    val vec = ButtazoHeuristicH1.partitionning(f.taskSet, EDFqPA)
    ???
  }

  it should "not generate unschedulable flows" in {
    val f = fixture3
    val vec = ButtazoHeuristicH1.partitionning(f.taskSet, EDFqPA)
    vec shouldBe defined
  }


  "The ButtazoHeuristicH2 partitionning heuristic" should "work correctly" in {
    val f = fixture
    val vec = ButtazoHeuristicH2.partitionning(f.taskSet, EDFqPA)
    println(vec.mkString)
    ???
  }


  it should "not generate unschedulable flows" in {
    val f = fixture3
    val vec = ButtazoHeuristicH2.partitionning(f.taskSet, EDFqPA)
    vec shouldBe defined
  }


  "The GlobalPartitioning partitionning heuristic" should "work correctly" in {
    val f = fixture
    val vec = GlobalHeuristic.partitionning(f.taskSet, EDFqPA)
    ???

  }


  it should "not generate unschedulable flows" in {
    val f = fixture3
    val vec = GlobalHeuristic.partitionning(f.taskSet, EDFqPA)
    vec shouldBe defined
  }



}
