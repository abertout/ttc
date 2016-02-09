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


import main.scala.scheduling.{DMsufficientSchedulabilityTest, DMresponseTimeAnalysis}
import main.scala.taskmodel.{TaskSet, Task}
import test.scala.UnitSpec


class DMSchedTestSpec extends UnitSpec{

  def fixture =
    new {
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

      val tau1 = Task("1", 2, 10, 15)
      val tau2 = Task("2", 5, 15, 17)
      val tau3 = Task("3", 4, 12, 13)
      val simpleTaskSet = TaskSet(set = Seq(tau1, tau2, tau3))


    }

  "The RTA for DM" should "compute the right response times" in {
    val f = fixture
    val taskSet = f.taskSet

    DMresponseTimeAnalysis(taskSet) shouldEqual true

    taskSet.set.head.r should equal(Some(128))
    taskSet.set(1).r should equal(Some(7091))
    taskSet.set(2).r should equal(Some(2567))
    taskSet.set(3).r should equal(Some(6159))
    taskSet.set(4).r should equal(Some(2734))
    taskSet.set(5).r should equal(Some(241))
    taskSet.set(6).r should equal(Some(178))
    taskSet.set(7).r should equal(Some(107))
    taskSet.set(8).r should equal(Some(51))
    taskSet.set(9).r should equal(Some(264))
    taskSet.set(10).r should equal(Some(6238))
    taskSet.set(11).r should equal(Some(287))
    taskSet.set(12).r should equal(Some(323))
    taskSet.set(13).r should equal(Some(208))
    taskSet.set(14).r should equal(Some(22))
    taskSet.set(15).r should equal(Some(184))
    taskSet.set(16).r should equal(Some(204))
    taskSet.set(17).r should equal(Some(2572))
    taskSet.set(18).r should equal(Some(1142))
    taskSet.set(19).r should equal(Some(1))

  }

  "The sufficient test for DM" should "decide the schedulability of task set" in {
    val f = fixture
    val taskSet = f.simpleTaskSet
    DMsufficientSchedulabilityTest(taskSet) shouldEqual true
  }


}
