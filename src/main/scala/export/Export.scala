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

package main.scala.export

import java.io.{File, PrintWriter}

import main.scala.taskmodel.{Task, TaskSet}

import scala.sys.process._
import scala.xml.{Unparsed, XML}


object Export{


  /**
    * Export taskset on Cheddar software 3.0 format (uniprocessor)
    *
    * @param taskSet taskset
    * @param sched scheduling algorithm
    * @param outPath path of generated file
    */
  def toCheddarXml(taskSet: TaskSet, sched: String, outPath: String): Unit = {
    val cheddarDM = "DEADLINE_MONOTONIC_PROTOCOL"
    val cheddarEDF = "EARLIEST_DEADLINE_FIRST_PROTOCOL"

    val cheddarSched = sched match {
      case "DM" => cheddarDM
      case "EDF" => cheddarEDF
      case _ => throw new Exception(s"Scheduling algorithm $sched not available")
    }
    val coreId = Unparsed("id_1")
    val processorId = Unparsed("id_3")
    val addressSpace = Unparsed("id_4")
    val cpuName = "p1"
    val addressSpaceName = "a1"

    val tasksIdMap:Map[Task, Unparsed] = (taskSet.set zip (5 to taskSet.size + 5).toList).map(kv => (kv._1, Unparsed("id_"+kv._2))).toMap

    val cheddar = {
      <cheddar>
        <core_units>
          <core_unit id={coreId}>
            <object_type>CORE_OBJECT_TYPE</object_type>
            <name>one</name>
            <scheduling>
              <scheduling_parameters>
                <scheduler_type>{cheddarSched}</scheduler_type>
                <quantum>0</quantum>
                <preemptive_type>PREEMPTIVE</preemptive_type>
                <capacity>0</capacity>
                <period>0</period>
                <priority>0</priority>
                <start_time>0</start_time>
              </scheduling_parameters>
            </scheduling>
            <speed>0.00000</speed>
          </core_unit>
        </core_units>
        <processors>
          <mono_core_processor id={processorId}>
            <object_type>PROCESSOR_OBJECT_TYPE</object_type>
            <name>{cpuName}</name>
            <processor_type>MONOCORE_TYPE</processor_type>
            <migration_type>NO_MIGRATION_TYPE</migration_type>
            <core ref={coreId}>
            </core>
          </mono_core_processor>
        </processors>
        <address_spaces>
          <address_space id={addressSpace}>
            <object_type>ADDRESS_SPACE_OBJECT_TYPE</object_type>
            <name>{addressSpaceName}</name>
            <cpu_name>{cpuName}</cpu_name>
            <text_memory_size>0</text_memory_size>
            <stack_memory_size>0</stack_memory_size>
            <data_memory_size>0</data_memory_size>
            <heap_memory_size>0</heap_memory_size>
            <scheduling>
              <scheduling_parameters>
                <scheduler_type>NO_SCHEDULING_PROTOCOL</scheduler_type>
                <quantum>0</quantum>
                <preemptive_type>PREEMPTIVE</preemptive_type>
                <capacity>0</capacity>
                <period>0</period>
                <priority>0</priority>
                <start_time>0</start_time>
              </scheduling_parameters>
            </scheduling>
          </address_space>
        </address_spaces>
        <tasks>
          {
          for(task <- taskSet.set) yield
            <periodic_task id={tasksIdMap(task)}>
              <object_type>TASK_OBJECT_TYPE</object_type>
              <name>{task.name}</name>
              <task_type>PERIODIC_TYPE</task_type>
              <cpu_name>{cpuName}</cpu_name>
              <address_space_name>a1</address_space_name>
              <capacity>{task.c}</capacity>
              <deadline>{task.d}</deadline>
              <start_time>{task.o}</start_time>
              <priority>1</priority>
              <blocking_time>0</blocking_time>
              <policy>SCHED_FIFO</policy>
              <text_memory_size>0</text_memory_size>
              <stack_memory_size>0</stack_memory_size>
              <criticality>0</criticality>
              <context_switch_overhead>0</context_switch_overhead>
              <period>{task.t}</period>
              <jitter>0</jitter>
              <every>0</every>
            </periodic_task>

          }
        </tasks>{taskSet.tasksAndSuccs match {
        case Some(dep) =>
          <dependencies>
            {for ((task, succsSet) <- dep; succ <- succsSet) yield
            <dependency>
              <type_of_dependency>PRECEDENCE_DEPENDENCY</type_of_dependency>
              <precedence_sink ref={tasksIdMap(succ)}>
              </precedence_sink>
              <precedence_source ref={tasksIdMap(task)}>
              </precedence_source>
            </dependency>}
          </dependencies>

        case None =>
      }
        }
      </cheddar>
    }
    XML.save(outPath, cheddar.head, /*encoding*/"utf-8", xmlDecl = true)

  }

  /**
    * Export taskSet on SimSo software 0.7 format (uniprocessor)
    *
    * @param taskSet taskset
    * @param sched scheduling algorithm
    * @param outPath path of generated file
    * @param simuDuration simulation duration
    */
  def toSimSoXml(taskSet: TaskSet, sched: String, outPath: String, simuDuration: Option[Int] = None): Unit = {
    val cycles =  simuDuration.getOrElse(if(!taskSet.set.exists(_.o != 0)) taskSet.periodsLCM else taskSet.periodsLCM * 2 + taskSet.set.maxBy(_.o).o)
    val (schedClass, className) = sched match {
      case "EDF" => (Unparsed("simso.schedulers.EDF"), Unparsed(""))
      case "DM"  => throw new Exception("DM is not natively available in Simso")
      case _ => throw new Exception(s"Scheduling algorithm $sched not available")
    }
    val tasksIdMap:Map[Task, Unparsed] = (taskSet.set zip (1 to taskSet.size).toList).map(kv => (kv._1, Unparsed(kv._2.toString))).toMap

    val simso = {
      <simulation cycles_per_ms="1" duration={Unparsed(simuDuration.getOrElse(1000).toString)} etm="wcet">
        <sched class={schedClass} className={className} overhead="0" overhead_activate="0" overhead_terminate="0"/>
        <caches memory_access_time="100"/>
        <processors>
          <processor cl_overhead="0" cs_overhead="0" id="1" name="CPU 1" speed="1.0"/>
        </processors>
        <tasks>
          {for (task <- taskSet.set) yield
            <task ACET="0" WCET={Unparsed(task.c.toFloat.toString)} abort_on_miss="yes" activationDate={Unparsed(task.o.toString)} base_cpi="1.0" deadline={Unparsed(task.d.toFloat.toString)} et_stddev="0" id={tasksIdMap(task)}
                  instructions="0" list_activation_dates="" mix="0.5" name={Unparsed(task.name)} period={Unparsed(task.t.toFloat.toString)} preemption_cost="0" task_type="Periodic"/>
          }
        </tasks>
      </simulation>
    }

    XML.save(outPath, simso.head, /*encoding*/"utf-8", xmlDecl = true)

  }

  /**
    * Draw a picture of the graph using a dot description
    * require "eog" and "dot" softwares
    *
    * @param taskSet task set
    * @return
    */
  def displayGraph(taskSet: TaskSet) = {


    val outDir = "/tmp/"
    def exportFileFormat: String = "svg"
    val graphName =  System.currentTimeMillis+""
    val dotPath = outDir+graphName+".dot"
    val writer = new PrintWriter(new File(dotPath))

    writer.write(toDot(taskSet, graphName))
    writer.close()


    val cmd1 = Seq("dot","-T", exportFileFormat,"-o",s"$outDir$graphName.$exportFileFormat", s"$outDir$graphName.dot")
    cmd1.!
    val cmd2 = Seq("eog", s"$outDir$graphName.$exportFileFormat")
    cmd2.!

  }

  /**
    * Export the taskset to a .dot description file
    *
    * @param taskSet task set
    * @param graphName path
    * @return
    */

  def toDot(taskSet: TaskSet, graphName: String): String = {

    val exportFileFormat: String = "svg"
    val idDotPrefix = "id"
    val output = new StringBuffer
    output.append("digraph "+graphName+" {")
    for (ts <- taskSet.set){
      if (taskSet.directSuccs(ts).isEmpty)
        output.append("\n"+idDotPrefix+ts.name+" [label = \""+ts+"\""+"];")
      else
        for(succ <- taskSet.directSuccs(ts)){
          output.append("\n"+idDotPrefix+ts.name+" -> "+idDotPrefix+succ.name+";")
          output.append("\n"+idDotPrefix+ts.name+" [label = \""+ts+"\""+"];")
          output.append("\n"+idDotPrefix+succ.name+" [label = \""+succ+"\""+"];")
        }
    }

    output.append("\n}")
    output.toString

  }

  /**
    * Generates the set of declarations in scala to form the taskSet
    *
    * @param taskSet taskSet
    * @param taskSetVarName name of taskSet variable
    * @return a String with the complete declaration to copy past
    */
  def genScalaTaskSetDecl(taskSet: TaskSet, taskSetVarName: String): String = {
    val sb = new StringBuilder
    for(t <- taskSet.set)
      sb.append("val "+t.name+ " = Task(\""+t.name+"\", "+t.c+", "+t.d+", "+t.t+", "+t.o+", "+t.r+")\n")



    taskSet.tasksAndSuccs match{
      case Some(smtg) =>
        sb.append("\n")
        sb.append("val dep = Map(\n")
        val truc =  taskSet.set.filter(task => taskSet.directSuccs(task).nonEmpty)
        val truc2 = truc.map(
          task => s"\t${task.name} -> Set(${taskSet.directSuccs(task).map(_.name).mkString(",")})"
        )
        sb.append(truc2.mkString(",\n"))
        sb.append("\n)\n")

      case _ =>
    }

    val dep = taskSet.tasksAndSuccs match {
      case Some(d) => ", Some(dep))"
      case _ => ")"
    }

    sb.append(s"val $taskSetVarName = TaskSet(set = Seq(")
    sb.append(s"${taskSet.set.head.name}")

    for(i <- 1 until taskSet.set.length) sb.append(","+taskSet.set(i).name)
    sb.append(s")$dep")
    sb.toString
  }

}
