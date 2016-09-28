#! /usr/bin/env python

from waflib import Task
from waflib.TaskGen import extension, feature, after_method
from waflib.Build import BuildContext

class dfu_programmer(Task.Task):
    def run(self):
        cmd = str(self.env.DFU_PROGRAMMER[0])
        cmd += ' ' + str("at90usb162")
        cmd += ' erase --force'
        if self.exec_command(cmd):
            return 1
        cmd = str(self.env.DFU_PROGRAMMER[0])
        cmd += ' ' + str("at90usb162")
        cmd += ' flash ' + str(self.inputs[0])
        if self.exec_command(cmd):
            return 1
        cmd = str(self.env.DFU_PROGRAMMER[0])
        cmd += ' ' + str("at90usb162")
        cmd += ' launch --no-reset'
        return self.exec_command(cmd)

    def runnable_status(self):
		ret = super(dfu_programmer, self).runnable_status()
		if ret == Task.SKIP_ME:
			return Task.RUN_ME
		return ret

def configure(ctx):
    ctx.find_program('dfu-programmer', var = 'DFU_PROGRAMMER')

@extension('.hex')
def upload_hex(tskg, node):
    return tskg.create_task('dfu_programmer', node, '')
