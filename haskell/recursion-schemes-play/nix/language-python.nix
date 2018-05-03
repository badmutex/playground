{ mkDerivation, alex, array, base, containers, fetchgit, happy
, monads-tf, pretty, stdenv, transformers, utf8-string
}:
mkDerivation {
  pname = "language-python";
  version = "0.5.5";
  src = fetchgit {
    url = "git://github.com/bjpop/language-python.git";
    sha256 = "183rvdamlwp7lwryhrri35x8cpyb8ds1s2qi8xyv4wv8ng3ybbds";
    rev = "a5aeabfa58b65a742a6a6745720b4864c0ea770c";
  };
  libraryHaskellDepends = [
    array base containers monads-tf pretty transformers utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  homepage = "http://github.com/bjpop/language-python";
  description = "Parsing and pretty printing of Python code";
  license = stdenv.lib.licenses.bsd3;
}
