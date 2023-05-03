#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt

benchmark = sys.argv[1]
data_sizes = map(int, sys.argv[2:])

opencl_filename = '{}-opencl.json'.format(benchmark)
c_filename = '{}-c.json'.format(benchmark)

opencl_json = json.load(open(opencl_filename))
c_json = json.load(open(c_filename))

opencl_measurements = opencl_json['{}.fut'.format(benchmark)]['datasets']
c_measurements = c_json['{}.fut'.format(benchmark)]['datasets']

opencl_runtimes = [ np.mean(opencl_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]
c_runtimes = [ np.mean(c_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
               for n in data_sizes ]
speedups = map(lambda x, y: x / y, c_runtimes, opencl_runtimes)

fig, ax1 = plt.subplots()
opencl_runtime_plot = ax1.plot(data_sizes, opencl_runtimes, 'b-', label='OpenCL runtime')
c_runtime_plot = ax1.plot(data_sizes, c_runtimes, 'g-', label='Sequential runtime')
ax1.set_xlabel('Input size')
ax1.set_ylabel('Runtime (ms)', color='k')
ax1.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot(data_sizes, speedups, 'k-', label='OpenCL speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = opencl_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

fig.tight_layout()
plt.show()

plt.rc('text')
plt.savefig('{}.pdf'.format(benchmark), bbox_inches='tight')
