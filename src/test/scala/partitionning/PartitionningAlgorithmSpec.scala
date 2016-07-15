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

package ttc.partitionning

import ttc.partitionning.{ButtazoHeuristicH1, ButtazoHeuristicH2, GlobalHeuristic, PartitionningAlgorithm}
import ttc.scheduling.{DMresponseTimeAnalysis, EDFqPA}
import ttc.taskmodel.{Task, TaskSet}
import ttc.UnitSpec

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

  it should "behave correctly with an empty dependent set of tasks" in {
    val a = Task("a", 18, 257, 560, 0, None)
    val b = Task("b", 75, 364, 560, 0, None)
    val c = Task("c", 184, 2097, 2400, 0, None)
    val d = Task("d", 3580, 89731, 92400, 0, None)
    val e = Task("e", 195, 83486, 92400, 0, None)
    val f = Task("f", 188, 1615, 2400, 0, None)
    val g = Task("g", 4288, 16463, 92400, 0, None)
    val h = Task("h", 23353, 80236, 92400, 0, None)
    val i = Task("i", 28, 1281, 2400, 0, None)
    val j = Task("j", 23987, 45078, 92400, 0, None)
    val k = Task("k", 6742, 29671, 277200, 0, None)
    val l = Task("l", 4576, 88943, 92400, 0, None)
    val m = Task("m", 2729, 54909, 92400, 0, None)
    val n = Task("n", 11, 195, 560, 0, None)
    val o = Task("o", 40, 326, 560, 0, None)
    val p = Task("p", 35, 481, 560, 0, None)
    val q = Task("q", 51, 216, 560, 0, None)
    val r = Task("r", 4948, 121252, 277200, 0, None)
    val s = Task("s", 13199, 48138, 92400, 0, None)
    val t = Task("t", 14107, 43030, 92400, 0, None)
    val taskSet = TaskSet(set = Seq(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))

    val part = ButtazoHeuristicH1.partitionning(taskSet,DMresponseTimeAnalysis).get
    part.size shouldEqual 2
    part.map(_.size).sum shouldEqual taskSet.size


  }

  it should "behave correctly with an empty independent set of tasks" in {
    val a = Task("a", 69, 206, 400, 0, None)
    val b = Task("b", 615, 725, 1680, 0, None)
    val c = Task("c", 201, 344, 400, 0, None)
    val d = Task("d", 3700, 4354, 25200, 0, None)
    val e = Task("e", 725, 2676, 25200, 0, None)

    val dep = Map(
      a -> Set(b,c),
      b -> Set(d),
      c -> Set(d),
      d -> Set(e)
    )
    val depTaskSet = TaskSet(set = Seq(a,b,c,d,e), Some(dep))
    val part = ButtazoHeuristicH1.partitionning(depTaskSet,DMresponseTimeAnalysis).get

    part.size shouldEqual 2
    part.map(_.size).sum shouldEqual depTaskSet.size
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
