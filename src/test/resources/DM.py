#!/usr/bin/python
# coding=utf-8

from core import Scheduler

class DM(Scheduler):
    """ Deadline monotonic """
    def init(self):
        self.ready_list = []

    def on_activate(self, job):
        self.ready_list.append(job)
        job.cpu.resched()

    def on_terminated(self, job):
        job.cpu.resched()

    def schedule(self, cpu):
        if len(self.ready_list) > 0:
            # Get a free processor or a processor running a low priority job.
            key = lambda x: (-x.running.task.deadline if x.running else None,
                    0 if x is cpu else 1)
            cpu_min = min(self.processors, key=key)

            # Job with highest priority.
            job = min(self.ready_list, key=lambda x: x.task.deadline)

            if (cpu_min.running is None or 
                        cpu_min.running.task.deadline > job.task.deadline):
                self.ready_list.remove(job)
                if cpu_min.running:
                    self.ready_list.append(cpu_min.running)
                return (job, cpu_min)

        return None
