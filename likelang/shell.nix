with import <nixpkgs> {};
let
  pythonEnv = python39.withPackages (ps: [
    ps.lark-parser
    ps.colorama
    ps.flake8
    ps.isort
  ]);
in mkShell {
  packages = [
    pythonEnv
    python39
    black
    nodejs
  ];
}
