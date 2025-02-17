{ mkDerivation, base, boxes, directory, filepath, lib
, optparse-applicative, process, split, temporary, text-ansi, time
}:
mkDerivation {
  pname = "journalh";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base time ];
  executableHaskellDepends = [
    base boxes directory filepath optparse-applicative process split
    temporary text-ansi time
  ];
  license = "unknown";
  mainProgram = "jnlh";
}
