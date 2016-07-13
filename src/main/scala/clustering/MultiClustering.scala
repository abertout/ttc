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

import ttc.partitionning.PartitionningAlgorithm
import ttc.scheduling.{Encoding, ResponseTimeAnalysis, SchedTest}
import ttc.taskmodel.{Task, TaskSet}

import scala.collection.mutable.ArrayBuffer

object MultiClustering {

  private def partitionFlows(taskSet: TaskSet, flows: Vector[Seq[Task]]): (Vector[Seq[Task]],Vector[Seq[Task]]) = {

    def isIndep(flow: Seq[Task]): Boolean = {
      for(task <- flow){
        for(succ <- taskSet.directSuccs(task))
          if(!flow.contains(succ))
            return false
        for(pred <- taskSet.directPreds(task))
          if(!flow.contains(pred))
            return false
      }
      true
    }
    flows.partition(isIndep)
  }

  /**
    * Minimize the number of tasks by clustering using a partitionned approach
    * @param taskSet taskSet
    * @param partitionningMethod partitionning method used
    * @param partSchedTest schedulability test used for the partitionning
    * @param costFunction cost function
    * @param clusSchedTest schedulability test used for the clustering
    * @param rta response time analysis test used to allow more clustering
    * @return set of schedulable flows where the number of tasks is minimizes
    */
  def cluster(taskSet: TaskSet, partitionningMethod: PartitionningAlgorithm, partSchedTest: SchedTest,
              costFunction: CostFunction, clusSchedTest: SchedTest, rta: Option[ResponseTimeAnalysis]): Seq[TaskSet] = {


    val optFlows = partitionningMethod.partitionning(taskSet, partSchedTest)
    println(optFlows)
    optFlows match {
      case Some(flows) =>
        val (depFlows, indepFlows) = partitionFlows(taskSet, flows)
        val clusteredIndepFlows: Seq[TaskSet] = indepFlows.map(flow => MonoClustering.greedyBFSClustering(taskSet.restrictedTo(flow:_*),clusSchedTest, costFunction, rta))
        val depFlowsTaskSet: Vector[TaskSet] = depFlows.map(flow => taskSet.restrictedTo(flow:_*))
        val clusteredDepFlows: Seq[TaskSet] =
          if(depFlowsTaskSet.nonEmpty) greedyBFSHeuristic(taskSet, depFlowsTaskSet, clusSchedTest, costFunction, rta)
          else Seq.empty
        clusteredDepFlows ++ clusteredIndepFlows

      case None => throw new Exception("Partitionning infeasible")
    }

  }

  /***
    * Implements a greedy Best-First Search heuristic
    * @param taskSet taskSet
    * @param flows taskSet partitionned in flows
    * @param schedTest schedulability test used for the clustering
    * @param costFunction cost function
    * @param rta set of schedulable flows where the number of tasks is minimizes
    * @return
    */
  def greedyBFSHeuristic(taskSet: TaskSet, flows: Vector[TaskSet], schedTest: SchedTest, costFunction: CostFunction,
                         rta: Option[ResponseTimeAnalysis]): Seq[TaskSet] = {

    val resultFlows: Array[TaskSet] = flows.toArray
    var currFlowIdx = 0
    var currentFlow: TaskSet = resultFlows(currFlowIdx)
    var taskSetMinCost: Option[TaskSet] = Some(taskSet)
    var nInfeasibleFlows = 0

    while(nInfeasibleFlows < flows.length) {

      val sortedFLow: TaskSet = TaskSet(currentFlow.set.sortBy(_.d), currentFlow.tasksAndSuccs)
      var i = sortedFLow.size - 1
      var flowMinCost: Option[TaskSet] = None
      var children = new ArrayBuffer[(TaskSet, Int, Int, ClusterDeadline.Value)]()
      currentFlow =  resultFlows(currFlowIdx)
      var freeGrouping = false

      while (i >= 0 && !freeGrouping) {
        var j = i - 1
        while (j >= 0 && !freeGrouping) {
          val tauI = sortedFLow.set(i)
          val tauJ = sortedFLow.set(j)
          val comp = ClusteringCompanion(sortedFLow)
          if (comp.regroupable(tauI, tauJ)) {
            if (taskSetMinCost.get.isolatedTask(tauI, tauJ)) {
              if ((tauI.c + tauJ.c <= tauI.d) && (tauI.r.isDefined && tauI.r.get - tauI.c <= tauJ.d) || (tauI.d - tauI.c <= tauJ.d)) {
                val newTaskSet = comp.fusion(tauI, tauJ, ClusterDeadline.DlMax)
                val newSortedTs = TaskSet(newTaskSet.set.sortBy(_.d), newTaskSet.tasksAndSuccs)
                resultFlows(currFlowIdx) = newSortedTs
                val gobalComp = ClusteringCompanion(taskSetMinCost.get)
                taskSetMinCost = Some(gobalComp.fusion(tauI, tauJ, ClusterDeadline.DlMax))
                freeGrouping = true
              } else {
                children += ((sortedFLow, i, j, ClusterDeadline.DlMin))
              }
            } else {
              val iPredOfJ = sortedFLow.directPredRelation(tauI, tauJ)
              val iSuccOfJ = sortedFLow.directSuccRelation(tauI, tauJ)
              if (iPredOfJ || iSuccOfJ) {
                val pred = if (iPredOfJ) tauI else tauJ
                val succ = if (iSuccOfJ) tauI else tauJ

                //Check the conditions
                if (pred.d > succ.d && pred.c + succ.c <= pred.d) {
                  children += ((sortedFLow, i, j, ClusterDeadline.DlMin))
                } else {
                  if (pred.c + succ.c <= succ.d && ((succ.r.isDefined && succ.r.get - succ.c <= pred.d) || (succ.d - succ.c <= pred.d)))
                    children += ((sortedFLow, i, j, ClusterDeadline.DlSucc))
                  else if(pred.c + succ.c <= pred.d && (!((succ.r.isDefined && succ.r.get - succ.c <= pred.d) || (succ.d - succ.c <= pred.d))))
                    children += ((sortedFLow, i, j, ClusterDeadline.DlPred))
                }
              } else {
                /*not isolated but no direct precedence relation */
                if (tauI.c + tauJ.c <= tauJ.d && ((tauI.r.isDefined && tauI.r.get - tauI.c <= tauJ.d) || (tauI.d - tauI.c <= tauJ.d)))
                  children += ((sortedFLow, i, j, ClusterDeadline.DlMax))
                else if(tauI.c + tauJ.c <= tauI.d && (!(tauI.r.isDefined && tauI.r.get - tauI.c <= tauJ.d) || (tauI.d - tauI.c <= tauJ.d)))
                  children += ((sortedFLow, i, j, ClusterDeadline.DlMin))

              }
            }
          }
          j -= 1
        }
        i -= 1
      }

      if(!freeGrouping){

        val schedulableChildren = new ArrayBuffer[(TaskSet,Array[TaskSet])]()

        for (tuple <- children) {
          val (ts, ii, jj, dl) =  tuple
          val comp = ClusteringCompanion(ts)
          val globalComp =  ClusteringCompanion(taskSetMinCost.get)
          val updatedFlow = comp.fusion(ts.set(ii), ts.set(jj), dl)
          val updatedTaskSet = globalComp.fusion(ts.set(ii), ts.set(jj), dl)
          val updatedEncTaskSet = Encoding.predsEncoding(updatedTaskSet)
          val updatedFlows: Array[TaskSet] = resultFlows.updated(currFlowIdx, updatedFlow)
          val updatedEncFlows =  updatedFlows.map(ts => updatedEncTaskSet.restrictedTo(ts.set:_*))

          val nonSched = updatedEncFlows.exists(!schedTest(_)._1)

          if (!nonSched) {              //If task set is schedulable
            schedulableChildren += ((updatedTaskSet, updatedFlows))
          }
        }
        var currGlobalTaskSet: Option[TaskSet] = taskSetMinCost


        if (schedulableChildren.nonEmpty) {
          val currFlow = schedulableChildren.map(_._2(currFlowIdx))

          flowMinCost = Some(costFunction(currFlow))
          val idxFMinCost = currFlow.indexOf(flowMinCost.get)
          currGlobalTaskSet = Some(schedulableChildren(idxFMinCost)._1)
          if (flowMinCost.isDefined) {
            rta match {
              case Some(rtaTest) =>
                val encoded = Encoding.predsEncoding(flowMinCost.get)
                val encodedRTA = rtaTest(encoded)._2
                for (task <- flowMinCost.get.set; encTask <- encodedRTA.set) {
                  if (task.equals(encTask)) {
                    task.r = encTask.r
                  }
                }
              case _ =>
            }
          }
        }

        if (flowMinCost.isDefined) {
          resultFlows(currFlowIdx) = flowMinCost.get
          taskSetMinCost = currGlobalTaskSet
        }
        else nInfeasibleFlows += 1

      }

      currFlowIdx = (currFlowIdx + 1) % resultFlows.length
      currentFlow = resultFlows(currFlowIdx)
    }

    resultFlows
  }

}
