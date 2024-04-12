with import <nixpkgs> {
  crossSystem = {
    config = "riscv32-unknown-linux-gnu";
  };
};

mkShell {
  buildInputs = [ ];
}
