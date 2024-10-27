import os
import json
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogLocator, FormatStrFormatter

# Directory containing JSON files
json_dir = './'

# Dictionary to store benchmark data
data = {}

# Desired order of tools
commands = ["OCaml native", "Wasocaml", "Wsoo", "Jsoo", "Bytecode"]

# Read each JSON file in the directory
for filename in os.listdir(json_dir):
    if filename.endswith('.json'):
        benchmark_name = filename.split('.')[0]  # Extract benchmark name from filename
        with open(os.path.join(json_dir, filename), 'r') as f:
            json_data = json.load(f)
            # Extract command names and mean execution times
            commands_data = {result['command']: result['mean'] for result in json_data['results']}
            # Calculate ratios with respect to OCaml native if it exists
            if "OCaml native" in commands_data:
                native_mean = commands_data["OCaml native"]
                ratios = {command: commands_data.get(command, np.nan) / native_mean for command in commands}
            else:
                ratios = {command: np.nan for command in commands}  # If "OCaml native" is missing, set all to NaN
            data[benchmark_name] = ratios

# Sort benchmarks lexicographically
benchmarks = sorted(data.keys())
num_benchmarks = len(benchmarks)
num_commands = len(commands)

# Prepare the plot
fig, ax = plt.subplots(figsize=(12, 6))

# Set bar width and positions with additional space between groups
bar_width = 0.15
group_gap = 0.4  # Gap between benchmark groups
x = np.arange(num_benchmarks) * (num_commands * bar_width + group_gap)

# Plot bars for each command
for i, command in enumerate(commands):
    # Get ratios for each benchmark
    ratios = [data[benchmark].get(command, np.nan) for benchmark in benchmarks]
    ax.bar(x + i * bar_width, ratios, bar_width, label=command)

# Customize plot
ax.set_yscale('log')  # Set y-axis to logarithmic scale

# Add minor ticks between powers of ten on the y-axis
ax.yaxis.set_major_locator(LogLocator(base=2.0, numticks=64))  # Major ticks at 10^0, 10^1, etc.
ax.yaxis.set_major_formatter(FormatStrFormatter('%d'))
#ax.yaxis.set_minor_locator(LogLocator(base=2.0, subs=np.arange(2, 10) * 0.1, numticks=10))  # Minor ticks at 2, 3, ..., 9
#ax.yaxis.set_minor_formatter(FormatStrFormatter('%d'))

# Set y-axis tick labels to avoid the repeated 0
#ax.set_ylim(0.9, 70)  # Set limits slightly below 10^0 to avoid duplication
#ax.yaxis.get_minor_ticks()[0].label1.set_visible(False)  # Hide the extra "0" tick

ax.set_xticks(x + bar_width * (num_commands - 1) / 2)
ax.set_xticklabels(benchmarks, rotation=45, ha='right')
ax.set_ylabel('Execution Time Ratio (Relative to OCaml Native)')
ax.legend(title="Tool")
plt.title("Benchmark Execution Time Ratios for Various Compilers (Relative to OCaml Native)")
plt.tight_layout()

# Save as SVG
plt.savefig('benchmark_execution_time_ratios.svg', format='svg')
plt.show()
