pl-build:
  echo "Building PL..."
  cd PL && stack run clash -- Example.Project --vhdl -fclash-hdldir "../VM_Export/"

pl-test:
  echo "Testing PL..."
  cd PL && stack test

build: pl-build
  echo "Building all..."
