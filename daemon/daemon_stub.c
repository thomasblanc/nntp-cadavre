#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

value as_daemon ( value keep_dir, value keep_fd)
{
 int r = daemon ( Bool_val(keep_dir),Bool_val(keep_fd));
 
 if (r == -1)
  caml_failwith ("daemon");
 return Val_int(0);
}
