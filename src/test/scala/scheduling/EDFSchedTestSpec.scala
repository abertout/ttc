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

package test.scala.scheduling

import test.scala.UnitSpec
import main.scala.scheduling.{EDFresponseTimeAnalysisGuan, EDFresponseTimeAnalysisSpuri, EDFsufficientTestDevi, EDFSchedTest}
import main.scala.taskmodel.{Task, TaskSet}


class EDFSchedTestSpec extends UnitSpec{


  def fixture =
    new {

      val EDFsched = new EDFSchedTest {
        override def apply(v1: TaskSet): Boolean = true
      }

      val tauA = Task("a", 21, 347, 720)
      val tauB = Task("b", 653, 26010, 26400)
      val tauC = Task("c", 874, 3513, 10080)
      val tauD = Task("d", 2384, 9748, 10080)
      val tauE = Task("e", 161, 8969, 12600)
      val tauF = Task("f", 33, 532, 840)
      val tauG = Task("g", 50, 386, 1200)
      val tauH = Task("h", 56, 166, 1680)
      val tauI = Task("i", 29, 134, 1200)
      val tauJ = Task("j", 23, 583, 1200)
      val tauK = Task("k", 78, 10258, 19800)
      val tauL = Task("l", 23, 633, 1200)
      val tauM = Task("m", 36, 1488, 1680)
      val tauN = Task("n", 4, 436, 840)
      val tauO = Task("o", 21, 82, 720)
      val tauP = Task("p", 6, 391, 1680)
      val tauQ = Task("q", 19, 407, 720)
      val tauR = Task("r", 5, 6729, 10080)
      val tauS = Task("s", 717, 2915, 19800)
      val tauT = Task("t", 1, 77, 200)

      val taskSet = TaskSet(set = Seq(tauA, tauB, tauC, tauD, tauE, tauF, tauG, tauH, tauI, tauJ, tauK, tauL, tauM, tauN, tauO, tauP, tauQ, tauR, tauS, tauT))

      /**
       * Spuri article example of table 1
       */
      val tau1 = Task("1", 1, 4, 4, 11)
      val tau2 = Task("2", 2, 9, 6, 6)
      val tau3 = Task("3", 2, 6, 8, 9)
      val tau4 = Task("4", 2, 12, 16, 3)

      val spuriTaskSet = TaskSet(set = Seq(tau1, tau2, tau3, tau4))

      val synchronousTau1 = Task("1", 1, 4, 4)
      val synchronousTau2 = Task("2", 2, 9, 6)
      val synchronousTau3 = Task("3", 2, 6, 8)
      val synchronousTau4 = Task("4", 2, 12, 16)

      val synchronouSpuriTaskSet = TaskSet(set = Seq(synchronousTau1, synchronousTau2, synchronousTau3, synchronousTau4))


      /**
       * Guan article example III.3
       */
      val tau1Guan = Task("1", 1, 4, 4)
      val tau2Guan = Task("2", 1, 12 ,12)
      val tau3Guan = Task("3", 3, 16, 16)

      val guanTaskSet = TaskSet(set = Seq(tau1Guan, tau2Guan, tau3Guan))

      /**
       * Zhang & Burns QPA Example
       */

      val t1 = Task("1", 6000, 18000, 31000)
      val t2 = Task("2", 2000, 9000, 9800)
      val t3 = Task("3", 1000, 12000, 17000)
      val t4 = Task("4", 90, 3000, 4200)
      val t5 = Task("5", 8, 78, 96)
      val t6 = Task("6", 2, 16, 12)
      val t7 = Task("7", 10, 120, 280)
      val t8 = Task("8", 26, 160, 660)

      val zhangBurnsTaskSet = TaskSet(set = Seq(t1, t2, t3, t4, t5, t6, t7, t8))

    }


  "The sufficient schedulability of Devi" should "decide the schedulability of task set" in {
    val f = fixture
    val tauA = Task("a", 2, 10, 15)
    val tauB = Task("b", 5, 15, 17)
    val tauC = Task("c", 4, 12, 13)

    val taskSet = new TaskSet(set = Seq(tauA, tauB, tauC))
    EDFsufficientTestDevi(taskSet) shouldEqual true

  }

  "The exact schedulability test of Spuri" should "compute the right response times" in {
    val f = fixture
    val spuriTaskSet = f.spuriTaskSet


    //TODO vérifier pour chaque tâche pour spuri avec plus de tâches

    EDFresponseTimeAnalysisSpuri(spuriTaskSet) shouldEqual true
    ???

  }


  "The exact schedulability test of Guan" should "compute the right response times" in {
    val f = fixture
    val guanTaskSet = f.guanTaskSet


    //TODO vérifier pour chaque tâche pour guan avec plus de tâches

    EDFresponseTimeAnalysisGuan(guanTaskSet) shouldEqual true
    ???
  }

  "EDFSched" should "compute the right demand bound function (dbf)" in {

    //Exemples from http://retis.sssup.it/~lipari/courses/str07/edf-handout.pdf
    val f = fixture
    val EDFsched = f.EDFsched

    val tau1 = Task("1", 1, 4, 6)
    val tau2 = Task("2", 2, 6, 8)
    val tau3 = Task("3", 3, 5, 10)
    val taskSet = TaskSet(set = Seq(tau1, tau2, tau3))

    val tau4 = Task("4", 1, 2, 4)
    val tau5 = Task("5", 2, 4 ,5)
    val tau6 = Task("6", 5, 8, 15)
    val taskSet2 = TaskSet(set = Seq(tau4, tau5, tau6))

    val synchronouSpuriTaskSet = f.synchronouSpuriTaskSet

    EDFsched.demandBoundFunction(taskSet, 4) shouldEqual 1
    EDFsched.demandBoundFunction(taskSet, 5) shouldEqual 4
    EDFsched.demandBoundFunction(taskSet, 6) shouldEqual 6
    EDFsched.demandBoundFunction(taskSet, 10) shouldEqual 7

    EDFsched.demandBoundFunction(taskSet2, 2) shouldEqual 1
    EDFsched.demandBoundFunction(taskSet2, 4) shouldEqual 3
    EDFsched.demandBoundFunction(taskSet2, 6) shouldEqual 4
    EDFsched.demandBoundFunction(taskSet2, 8) shouldEqual 9

    //Results can not be null
    EDFsched.demandBoundFunction(synchronouSpuriTaskSet, 0) shouldEqual 0
    EDFsched.demandBoundFunction(synchronouSpuriTaskSet, 1) shouldEqual 0
    EDFsched.demandBoundFunction(synchronouSpuriTaskSet, 2) shouldEqual 0
  }

  it should "compute the right request bound function (rbf)" in {

    //Exemples from http://retis.sssup.it/~lipari/courses/str07/edf-handout.pdf
    val f = fixture
    val EDFsched = f.EDFsched

    val tau1 = Task("1", 1, 4, 6)
    val tau2 = Task("2", 2, 6, 8)
    val tau3 = Task("3", 3, 5, 10)
    val taskSet = TaskSet(set = Seq(tau1, tau2, tau3))

    EDFsched.requestBoundFunction(taskSet, 0) shouldEqual 6
    EDFsched.requestBoundFunction(taskSet, 4) shouldEqual 6
    EDFsched.requestBoundFunction(taskSet, 5) shouldEqual 6
    EDFsched.requestBoundFunction(taskSet, 6) shouldEqual 7
    EDFsched.requestBoundFunction(taskSet, 10) shouldEqual 12

  }

  it should "compute the right mixed bound function (mbf)" in {
    val f = fixture
    val EDFsched = f.EDFsched

    val tau1 = Task("1", 1, 4, 6)
    val tau2 = Task("2", 2, 6, 8)
    val tau3 = Task("3", 3, 5, 10)
    val taskSet = TaskSet(set = Seq(tau1, tau2, tau3))

    //TODO test that it's not min(dbf,rbf) but sum des min(dbfi,rbfi)
    intercept [IllegalArgumentException]{ EDFsched.mixedBoundFunction(taskSet, 5, 8) }
    intercept [IllegalArgumentException]{ EDFsched.mixedBoundFunction(taskSet, -1, 0) }
    intercept [IllegalArgumentException]{ EDFsched.mixedBoundFunction(taskSet, 5, -1) }
  }


}
