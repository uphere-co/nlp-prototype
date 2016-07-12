{
  bllipparser = super.buildPythonPackage {
    name = "bllipparser-2015.12.3";
    buildInputs = with self; [];
    doCheck = false;
    propagatedBuildInputs = with self; [six];
    src = fetchurl {
      url = "https://pypi.python.org/packages/50/e1/39852026449d1ae146d3e7a26a531b5b4fc6c3bcdc0b3951d9ee41ed44fd/bllipparser-2015.12.3.tar.gz";
      md5 = "5ec1e27e5d36b26709b0e67052273174";
    };
  };
  six = super.buildPythonPackage {
    name = "six-1.10.0";
    buildInputs = with self; [];
    doCheck = false;
    propagatedBuildInputs = with self; [];
    src = fetchurl {
      url = "https://pypi.python.org/packages/b3/b2/238e2590826bfdd113244a40d9d3eb26918bd798fc187e2360a8367068db/six-1.10.0.tar.gz";
      md5 = "34eed507548117b2ab523ab14b2f8b55";
    };
  };
}
