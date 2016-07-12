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

package ttc.scheduling

import ttc.scheduling.Encoding
import ttc.taskmodel.{Task, TaskSet}
import ttc.UnitSpec

class EncodingSpec extends UnitSpec{

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



      val taskSetDep = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))
      val indepTaskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), None)
    }

  def fixture2 =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 2, 5, 40, 0)
      val tauE = Task("e", 2, 4, 50, 0)



      val dep = Map(
        tauA -> Set(tauB),
        tauB -> Set(tauC, tauD),
        tauD -> Set(tauE)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauA, tauD, tauE), Some(dep))
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
      val tauH = Task("h", 1, 13, 30, 0)
      val tauI = Task("i", 4, 20, 40, 0)



      val dep = Map(
        tauA -> Set(tauC, tauD),
        tauB -> Set(tauD),
        tauC -> Set(tauE),
        tauD -> Set(tauE),
        tauF -> Set(tauG),
        tauG -> Set(tauH)
      )


      val taskSetDep = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

    val encodedTauA = Task("a", 1, 3, 50, 0)
    val encodedTauB = Task("b", 2, 7, 50, 5)
    val encodedTauC = Task("c", 2, 5, 50, 1)
    val encodedTauD = Task("d", 1, 9, 50, 7)
    val encodedTauE = Task("e", 3, 12, 50, 8)
    val encodedTauF = Task("f", 3, 9, 30, 0)
    val encodedTauG = Task("g", 2, 12, 30, 5)
    val encodedTauH = Task("h", 1, 13, 30, 7)
    val encodedTauI = Task("i", 4, 20, 40, 0)

      val depEnc = Map(
        encodedTauA -> Set(encodedTauC, encodedTauD),
        encodedTauB -> Set(encodedTauD),
        encodedTauC -> Set(encodedTauE),
        encodedTauD -> Set(encodedTauE),
        encodedTauF -> Set(encodedTauG),
        encodedTauG -> Set(encodedTauH)
      )


    val encTaskSet = TaskSet(set = Seq(encodedTauA, encodedTauB, encodedTauC, encodedTauD, encodedTauE, encodedTauF, encodedTauG, encodedTauH, encodedTauI), Some(depEnc))

  }

  "The tasks " should "be encoded following the topologic order" in {
    val f = fixture
    val f2 = fixture2
    val q1 = Encoding.topologicSort(f.taskSetDep)
    val q2 = Encoding.topologicSort(f2.taskSet)

    val f1PosA = q1.indexOf(f.tauA)
    val f1PosB = q1.indexOf(f.tauB)
    val f1PosC = q1.indexOf(f.tauC)
    val f1PosD = q1.indexOf(f.tauD)
    val f1PosE = q1.indexOf(f.tauE)
    val f1PosF = q1.indexOf(f.tauF)
    val f1PosG = q1.indexOf(f.tauG)
    val f1PosH = q1.indexOf(f.tauH)
    val f1PosI = q1.indexOf(f.tauI)

    f1PosA < f1PosB && f1PosA < f1PosE shouldEqual true
    f1PosG < f1PosE shouldEqual true
    f1PosD < f1PosI && f1PosD < f1PosF shouldEqual true


    val f2PosA = q2.indexOf(f2.tauA)
    val f2PosB = q2.indexOf(f2.tauB)
    val f2PosC = q2.indexOf(f2.tauC)
    val f2PosD = q2.indexOf(f2.tauD)
    val f2PosE = q2.indexOf(f2.tauE)

    f2PosA shouldEqual 0
    f2PosB shouldEqual 1
    f2PosB < f2PosC && f2PosB < f2PosD shouldEqual true
    f2PosC < f2PosE && f2PosD < f2PosE shouldEqual true
    f2PosE shouldEqual q2.size - 1

  }

  "The Chetto's encoding" should "be correct" in {
    val f3 = fixture3
    val taskSet = f3.taskSetDep
    val expEncTaskSet = f3.encTaskSet
    val encTaskSet =  Encoding.predsEncoding(taskSet)
    val eqContent = encTaskSet equalContent expEncTaskSet
    eqContent shouldEqual true
  }

  "The Chetto's encoding" should "not modified a task set without dependence" in {
    val f1 = fixture
    val taskSet = f1.indepTaskSet
    val encTaskSet =  Encoding.predsEncoding(taskSet)
    val eqContent = taskSet equalContent encTaskSet
    eqContent shouldEqual true
  }
}
