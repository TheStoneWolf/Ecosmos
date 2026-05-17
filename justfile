pl-build:
  echo "Building PL..."
  cd PL && stack run clash -- Example.Project --vhdl -fclash-hdldir "VM_Export/"

build: pl-build
  echo "Building all..."
