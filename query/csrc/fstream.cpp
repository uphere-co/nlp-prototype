#include <iostream>
#include <fstream>
#include <ext/stdio_filebuf.h>

extern "C" {
  std::istream* make_istream_from_fd ( int fd );
  std::ostream* make_ostream_from_fd ( int fd );
  void delete_istream( std::istream* is );
  void delete_ostream( std::ostream* os );
}


std::istream* make_istream_from_fd ( int fd )
{
  __gnu_cxx::stdio_filebuf<char> buf (fd, std::ios::in);
  std::istream* is = new std::istream(&buf);
  return is;
}

std::ostream* make_ostream_from_fd ( int fd )
{
  __gnu_cxx::stdio_filebuf<char> buf(fd, std::ios::out);
  std::ostream* os = new std::ostream(&buf);
  return os;
}

void delete_istream( std::istream* is )
{
  delete is;
}

void delete_ostream( std::ostream* os )
{
  delete os;
}

