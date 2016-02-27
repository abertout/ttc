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

package ttc.taskmodel

import ttc.taskmodel.Task
import ttc.UnitSpec


class TaskSpec extends UnitSpec{

  def fixture =
    new {
      val synchronousTau1 = Task("1", 1, 4, 4)
      val asynchronousTau2 = Task("2", 2, 9, 6, 2)
      val synchronousTau3 = Task("3", 2, 6, 8, 0, Some(3))
      val synchronousTau4 = Task("4", 2, 12, 16)
    }


  "A Task" should "be correctly printed" in {
    val f = fixture
    f.synchronousTau1.toString should be ("1(1, 4, 4, 0, -1)")
    f.asynchronousTau2.toString should be ("2(2, 9, 6, 2, -1)")
    f.synchronousTau3.toString should be ("3(2, 6, 8, 0, 3)")
    f.synchronousTau4.toString should be ("4(2, 12, 16, 0, -1)")
  }

  it should "should be compared on name or parameters" in {
    val f = fixture
    val sTau1 = Task("1", 3, 4, 5)
    val sTau2 = Task("1", 1, 4, 4)
    f.synchronousTau1 shouldEqual sTau1
    val eqContent = f.synchronousTau1.equalContent(sTau1)
    eqContent should be (false)
    f.synchronousTau1 should not be theSameInstanceAs (sTau2)
  }


  it should "compute the right absolute deadline from a time" in {
    val f = fixture
    val tau5 = Task("1", 2, 6, 10, 3)
    f.synchronousTau1.getNextAbsoluteDeadline(16) should equal(20)
    f.asynchronousTau2.getNextAbsoluteDeadline(17) should equal(23)
    f.synchronousTau3.getNextAbsoluteDeadline(5) should equal(6)
    f.synchronousTau4.getNextAbsoluteDeadline(55) should equal(60)
    tau5.getNextAbsoluteDeadline(48) should equal(49)
  }


}
