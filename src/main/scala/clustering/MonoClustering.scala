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

import ttc.scheduling.{Encoding, ResponseTimeAnalysis, SchedTest}
import ttc.taskmodel.{Task, TaskSet}

import scala.collection.mutable.ArrayBuffer


object MonoClustering {


  /**
    * Greedy Best First Search heuristic that minimizes the number of tasks by successive clustering
    * @param taskSet task set
    * @param schedTest a schedulability test
    * @param costFunction a cost function to choose between several local schedulable candidates
    * @param rta an additionnal response time analysis test applied once (optional if schedtest is a rta)
    * @return
    */
  def greedyBFSClustering(taskSet: TaskSet, schedTest: SchedTest, costFunction: CostFunction, rta: Option[ResponseTimeAnalysis]): TaskSet = {
    val taskSetRTA: TaskSet = rta match {
      case None => taskSet
      case Some(rt) if(taskSet.tasksAndSuccs.isEmpty)  =>  rt(taskSet)._2
      case Some(rt) => //RTA on encoded but original task set parameters should not be modified
        val encoded = Encoding.predsEncoding(taskSet)
        val rtaEncoded = rt(encoded)._2
        val sorted = taskSet.set.sortBy(_.name)
        val depTaskSetwithR = sorted.zip(rtaEncoded.set.sortBy(_.name)).map{
          case (tau,tauEnc) => Task(tau.name,tau.c,tau.d,tau.t,tau.o,tauEnc.r)
        }
        TaskSet(depTaskSetwithR,taskSet.tasksAndSuccs)
    }

    val sortedTaskSet: TaskSet = TaskSet(taskSetRTA.set.sortBy(_.d), taskSetRTA.tasksAndSuccs)
    var j = sortedTaskSet.size - 1
    var setMinCost: Option[TaskSet] = None
    var children = new ArrayBuffer[(TaskSet, Int, Int, ClusterDeadline.Value)]()

    while (j >= 0) {
      var i = j - 1
      while (i >= 0) {
        val tauJ = sortedTaskSet.set(j)
        val tauI = sortedTaskSet.set(i)
        val comp = ClusteringCompanion(sortedTaskSet)
        if (comp.regroupable(tauJ, tauI)) {
          if (taskSet.isolatedTask(tauJ, tauI)) {
            if ((tauJ.c + tauI.c <= tauJ.d) && ((tauJ.r.isDefined && tauJ.r.get - tauJ.c <= tauI.d) || (tauJ.d - tauJ.c <= tauI.d))) { //Case 1.a
              val newTaskSet = comp.fusion(tauJ, tauI, ClusterDeadline.DlMax)
              return greedyBFSClustering(newTaskSet, schedTest, costFunction, rta)
            } else if(tauJ.c + tauI.c <= tauI.d){ //Case 1.b
              children += ((sortedTaskSet, j, i, ClusterDeadline.DlMin))
            }
          } else {
            val iPredOfJ = sortedTaskSet.directPredRelation(tauJ, tauI)
            val iSuccOfJ = sortedTaskSet.directSuccRelation(tauJ, tauI)
            if (iPredOfJ || iSuccOfJ) {
              val pred = if (iPredOfJ) tauJ else tauI
              val succ = if (iSuccOfJ) tauJ else tauI

              //Check the conditions
              if (pred.d > succ.d && pred.c + succ.c <= succ.d) { //Case 2.a
                children += ((sortedTaskSet, j, i, ClusterDeadline.DlSucc))
              } else {
                if (pred.c + succ.c <= succ.d && ((succ.r.isDefined && succ.r.get - succ.c <= pred.d) || (succ.d - succ.c <= pred.d))) //Case 2.b
                  children += ((sortedTaskSet, j, i, ClusterDeadline.DlSucc))
                else if(pred.c + succ.c <= pred.d && (!((succ.r.isDefined && succ.r.get - succ.c <= pred.d) || (succ.d - succ.c <= pred.d)))) //Case 2.c
                  children += ((sortedTaskSet, j, i, ClusterDeadline.DlPred))
              }

            } else {
              /*not isolated but no direct precedence relation */
              if (tauJ.c + tauI.c <= tauJ.d && ((tauJ.r.isDefined && tauJ.r.get - tauJ.c <= tauI.d) || (tauJ.d - tauJ.c <= tauI.d)))
                children += ((sortedTaskSet, j, i, ClusterDeadline.DlMax))
              else if(tauJ.c + tauI.c <= tauI.d && (!(tauJ.r.isDefined && tauJ.r.get - tauJ.c <= tauI.d) || (tauJ.d - tauJ.c <= tauI.d)))
                children += ((sortedTaskSet, j, i, ClusterDeadline.DlMin))
            }
          }
        }
        i -= 1
      }
      j -= 1
    }

    val schedulableChildren = new ArrayBuffer[TaskSet]()
    for (tuple <- children) {
      val (ts, ii, jj, dl) =  tuple
      val comp = new ClusteringCompanion(ts)
      val newTaskSet = comp.fusion(ts.set(ii), ts.set(jj), dl)
      val newEncodedTaskSet = Encoding.predsEncoding(newTaskSet) //After fusion we ensure that no redundant edge exist

      if (schedTest(newEncodedTaskSet)._1) {
        //If task set is schedulable
        schedulableChildren += newTaskSet
      }
    }

    if (schedulableChildren.nonEmpty)
      setMinCost = Some(costFunction(schedulableChildren))


    if (setMinCost.isDefined) greedyBFSClustering(setMinCost.get, schedTest, costFunction, rta) //If we found at least one schedulable child, let's make a recursive call on the most promising child
    else taskSet //Otherwise we return the father
  }


  /**
    * Minimize the number of tasks by clustering
    * @param taskSet taskSet
    * @param costFunction cost function
    * @param schedTest schedulability test used for the clustering
    * @param rta response time analysis test used to allow more clustering
    * @return set of schedulable flows where the number of tasks is minimizes
    */
  def cluster(taskSet: TaskSet, schedTest: SchedTest, costFunction: CostFunction, rta: Option[ResponseTimeAnalysis]): TaskSet = {
    require(schedTest(Encoding.predsEncoding(taskSet))._1, "the initial task set must be schedulable")
    greedyBFSClustering(taskSet, schedTest, costFunction, rta)
  }
}