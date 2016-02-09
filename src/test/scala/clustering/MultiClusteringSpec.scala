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

package clustering

import main.scala.clustering.MinDensity
import main.scala.partitionning.ButtazoHeuristicH1
import main.scala.scheduling.EDFqPA
import main.scala.taskmodel.{TaskSet, Task}
import test.scala.UnitSpec

class MultiClusteringSpec extends UnitSpec{

  def fixture =
    new {
      val tauA = Task("a", 6, 50, 50, 0)
      val tauB = Task("b", 3, 50, 50, 0)
      val tauC = Task("c", 4, 50, 50, 0)
      val tauD = Task("d", 3, 50, 50, 0)
      val tauE = Task("e", 2, 50, 50, 0)
      val tauF = Task("f", 1, 50, 50, 0)
      val tauG = Task("g", 7, 50, 50, 0)
      val tauH = Task("h", 1, 50, 50, 0)
      val tauI = Task("i", 1, 50, 50, 0)

      val dep = Map(
        tauA  -> Set(tauB),
        tauB  -> Set(tauC),
        tauD -> Set(tauI, tauG),
        tauG -> Set(tauH)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

      val flows = Vector(Seq(tauA, tauB), Seq(tauC, tauD, tauI), Seq(tauE, tauF), Seq(tauG, tauH))

    }

  def fixture2 =
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


  def fixture3 =
    new {
      val a = Task("a", 207, 1867, 3150, 0, None)
      val b = Task("b", 1, 17, 150, 0, None)
      val c = Task("c", 1032, 3270, 12600, 0, None)
      val d = Task("d", 1, 76, 150, 0, None)
      val e = Task("e", 1, 5, 30, 0, None)
      val f = Task("f", 3, 134, 150, 0, None)
      val g = Task("g", 10, 128, 150, 0, None)
      val h = Task("h", 18, 111, 150, 0, None)
      val i = Task("i", 1, 84, 150, 0, None)
      val j = Task("j", 0, 19, 30, 0, None)
      val k = Task("k", 53, 89, 150, 0, None)
      val l = Task("l", 25, 3047, 4200, 0, None)
      val m = Task("m", 30, 1133, 1200, 0, None)
      val n = Task("n", 100, 3091, 3150, 0, None)
      val o = Task("o", 20, 119, 150, 0, None)
      val p = Task("p", 20, 132, 150, 0, None)
      val q = Task("q", 71, 430, 1200, 0, None)
      val r = Task("r", 2, 23, 30, 0, None)
      val s = Task("s", 7, 92, 150, 0, None)
      val t = Task("t", 8, 159, 600, 0, None)
      val u = Task("u", 9, 54, 150, 0, None)
      val v = Task("v", 1080, 1408, 3150, 0, None)
      val w = Task("w", 14, 136, 150, 0, None)
      val x = Task("x", 0, 98, 150, 0, None)
      val y = Task("y", 2, 41, 150, 0, None)
      val z = Task("z", 0, 451, 3150, 0, None)
      val ba = Task("ba", 2, 28, 30, 0, None)
      val bb = Task("bb", 99, 6850, 12600, 0, None)
      val bc = Task("bc", 179, 2475, 3150, 0, None)
      val bd = Task("bd", 28, 136, 150, 0, None)
      val be = Task("be", 2, 10, 30, 0, None)
      val bf = Task("bf", 0, 118, 150, 0, None)
      val bg = Task("bg", 7, 149, 150, 0, None)
      val bh = Task("bh", 26, 2585, 3150, 0, None)
      val bi = Task("bi", 18, 304, 400, 0, None)
      val bj = Task("bj", 11, 35, 150, 0, None)
      val bk = Task("bk", 39, 155, 840, 0, None)
      val bl = Task("bl", 391, 1199, 3150, 0, None)
      val bm = Task("bm", 772, 5813, 12600, 0, None)
      val bn = Task("bn", 0, 8, 30, 0, None)
      val bo = Task("bo", 11, 112, 150, 0, None)
      val bp = Task("bp", 10, 124, 150, 0, None)
      val bq = Task("bq", 15, 15, 150, 0, None)
      val br = Task("br", 4, 117, 150, 0, None)
      val bs = Task("bs", 17, 48, 150, 0, None)
      val bt = Task("bt", 17, 106, 150, 0, None)
      val bu = Task("bu", 14, 126, 150, 0, None)
      val bv = Task("bv", 0, 144, 150, 0, None)
      val bw = Task("bw", 0, 112, 150, 0, None)
      val bx = Task("bx", 81, 3346, 8400, 0, None)

      val dep = Map(
        a -> Set(n),
        b -> Set(u),
        d -> Set(i),
        e -> Set(bn),
        f -> Set(w,bd),
        g -> Set(w,bd),
        k -> Set(s),
        p -> Set(w,bd),
        u -> Set(d),
        v -> Set(n),
        w -> Set(bg,bv),
        y -> Set(u),
        bc -> Set(n),
        bd -> Set(bg,bv),
        bf -> Set(o,bp,bu),
        bh -> Set(n),
        bj -> Set(u),
        bq -> Set(u),
        br -> Set(o,bp,bu),
        bs -> Set(u)
      )
      val taskSetM = TaskSet(set = Seq(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx), Some(dep))

    }

  "A multiprocessor clustering " should "minimize the number of tasks" in {
    val f = fixture3
    val test = MultiClustering.cluster(f.taskSetM, ButtazoHeuristicH1, EDFqPA, MinDensity, EDFqPA, None)
  }


  "The partitionFlows method" should "separate dependent and independent flows" in {
    val f = fixture
    val taskSet =  f.taskSet
    val flows = f.flows
    val partitionFlowsMethod = PrivateMethod[((Vector[Seq[Task]],Vector[Seq[Task]]))]('partitionFlows)
    val (indepFlow, depFlows) = MultiClustering invokePrivate partitionFlowsMethod(taskSet, flows)
    indepFlow shouldEqual Vector(Seq(f.tauE, f.tauF))
    depFlows shouldEqual Vector(Seq(f.tauA, f.tauB), Seq(f.tauC, f.tauD, f.tauI), Seq(f.tauG, f.tauH))
  }



}
