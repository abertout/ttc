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

package test.scala.clustering

import main.scala.clustering.{MinDensity, MonoClustering}
import main.scala.scheduling.DMresponseTimeAnalysis
import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec


class MonoClusteringSpec extends UnitSpec{


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



    }

  "A uniprocessor clustering " should "minimize the number of tasks" in {

    val f = fixture
    val minTaskSet = MonoClustering.greedyBFSClustering(f.taskSet, DMresponseTimeAnalysis, MinDensity, None)
    //println(minTaskSet)
    ???

  }

}
