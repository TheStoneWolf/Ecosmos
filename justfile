pl-build:
  @echo "Building PL..."
  cd PL && stack run clash -- Example.Project --vhdl -fclash-hdldir "../VM_Export/"

pl-test *PARAMETERS:
  @echo "Testing PL..."
  cd PL && stack test {{PARAMETERS}}

[arg("TARGET", pattern="[^./]+")]
pl-surfer TARGET:
  kitten @ launch --type=tab --cwd="$PWD" bash -lc "surfer 'PL/waveforms/{{TARGET}}.vcd' --command-file 'surfer.sucl'"

build: pl-build
  @echo "Building all..."

[working-directory: 'Analysis']
py-throughput *PARAMETERS:
  . .venv/bin/activate
  pip install -r requirements.txt
  python memoryThroughput.py {{PARAMETERS}}


