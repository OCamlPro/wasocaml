import os
import json
import matplotlib.pyplot as plt
import numpy as np

# Directory containing JSON files
json_dir = './'

# Dictionary to store benchmark data
data = {}

# Read each JSON file in the directory
for filename in os.listdir(json_dir):
    if filename.endswith('.json'):
        benchmark_name = filename.split('.')[0]  # Extract benchmark name from filename
        with open(os.path.join(json_dir, filename), 'r') as f:
            json_data = json.load(f)
            # Extract command names and mean execution times
            commands = [result['command'] for result in json_data['results']]
            means = [result['mean'] for result in json_data['results']]
            data[benchmark_name] = (commands, means)

# Prepare the plot
fig, ax = plt.subplots(figsize=(12, 6))
benchmarks = list(data.keys())
commands = data[benchmarks[0]][0]  # Get the list of tools from the first benchmark
num_benchmarks = len(benchmarks)
num_commands = len(commands)

# Set bar width and positions with additional space between groups
bar_width = 0.15
group_gap = 0.4  # Gap between benchmark groups
x = np.arange(num_benchmarks) * (num_commands * bar_width + group_gap)

# Plot bars for each command
for i, command in enumerate(commands):
    # Get means for each benchmark
    means = [data[benchmark][1][i] for benchmark in benchmarks]
    ax.bar(x + i * bar_width, means, bar_width, label=command)

# Customize plot
ax.set_xticks(x + bar_width * (num_commands - 1) / 2)
ax.set_xticklabels(benchmarks, rotation=45, ha='right')
ax.set_ylabel('Execution Time (s)')
ax.legend(title="Tool")
plt.title("Benchmark Execution Times for Various OCaml compilers")
plt.tight_layout()

# Save as SVG
plt.savefig('benchmark_execution_times.svg', format='svg')
plt.show()
