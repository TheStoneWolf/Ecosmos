# Build hardware description binary
pl-build:
  @echo "Building PL..."
  cd PL && stack run clash -- Example.Project --vhdl -fclash-hdldir "../VM_Export/"

# Run all PL tests
pl-test *PARAMETERS:
  @echo "Testing PL..."
  cd PL && stack test {{PARAMETERS}}

# Display PL waveforms in Surfer
[arg("TARGET", pattern="[^./]+")]
pl-surfer TARGET:
  kitten @ launch --type=tab --cwd="$PWD" bash -lc "surfer 'PL/waveforms/{{TARGET}}.vcd' --command-file 'surfer.sucl'"

# End-to-end build: HDL and SW to final binary 
build: pl-build
  @echo "Building all..."

# Run bottleneck analysis script
[working-directory: 'Analysis']
py-throughput *PARAMETERS:
  . .venv/bin/activate
  pip install -r requirements.txt
  python memoryThroughput.py {{PARAMETERS}}


