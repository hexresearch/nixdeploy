{ mkDerivation, async, base, bytestring, containers, directory
, enclosed-exceptions, exceptions, filepath, hspec, hspec-contrib
, HUnit, lifted-async, lifted-base, monad-control, mtl, process
, stdenv, system-fileio, system-filepath, text, time, transformers
, transformers-base, unix-compat
}:
mkDerivation {
  pname = "shelly";
  version = "1.8.0";
  sha256 = "737f51e5f4d98d72012d905d3f2b78b620c5c15292bb77e38264fa4478bb08f8";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers directory enclosed-exceptions
    exceptions lifted-async lifted-base monad-control mtl process
    system-fileio system-filepath text time transformers
    transformers-base unix-compat
  ];
  testHaskellDepends = [
    async base bytestring containers directory enclosed-exceptions
    exceptions filepath hspec hspec-contrib HUnit lifted-async
    lifted-base monad-control mtl process system-fileio system-filepath
    text time transformers transformers-base unix-compat
  ];
  homepage = "https://github.com/yesodweb/Shelly.hs";
  description = "shell-like (systems) programming in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
